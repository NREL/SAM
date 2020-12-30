#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Pvsamv1.h"

SAM_EXPORT int SAM_Pvsamv1_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("pvsamv1", data, verbosity, err);
}

SAM_EXPORT void SAM_Pvsamv1_SolarResource_albedo_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "albedo", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SolarResource_irrad_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "irrad_mode", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SolarResource_sky_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sky_model", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SolarResource_solar_resource_data_tset(SAM_table ptr, SAM_table tab, SAM_error *err){
	SAM_table_set_table(ptr, "solar_resource_data", tab, err);
}



SAM_EXPORT void SAM_Pvsamv1_SolarResource_solar_resource_file_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "solar_resource_file", str);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SolarResource_use_wf_albedo_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "use_wf_albedo", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_acwiring_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "acwiring_loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_dcoptimizer_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dcoptimizer_loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_en_snow_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_snow_model", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_subarray1_dcwiring_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray1_dcwiring_loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_subarray1_diodeconn_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray1_diodeconn_loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_subarray1_mismatch_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray1_mismatch_loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_subarray1_nameplate_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray1_nameplate_loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_subarray1_rear_irradiance_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray1_rear_irradiance_loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_subarray1_soiling_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "subarray1_soiling", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_subarray1_tracking_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray1_tracking_loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_subarray2_dcwiring_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray2_dcwiring_loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_subarray2_diodeconn_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray2_diodeconn_loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_subarray2_mismatch_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray2_mismatch_loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_subarray2_nameplate_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray2_nameplate_loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_subarray2_rear_irradiance_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray2_rear_irradiance_loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_subarray2_soiling_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "subarray2_soiling", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_subarray2_tracking_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray2_tracking_loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_subarray3_dcwiring_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray3_dcwiring_loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_subarray3_diodeconn_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray3_diodeconn_loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_subarray3_mismatch_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray3_mismatch_loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_subarray3_nameplate_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray3_nameplate_loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_subarray3_rear_irradiance_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray3_rear_irradiance_loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_subarray3_soiling_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "subarray3_soiling", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_subarray3_tracking_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray3_tracking_loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_subarray4_dcwiring_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray4_dcwiring_loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_subarray4_diodeconn_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray4_diodeconn_loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_subarray4_mismatch_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray4_mismatch_loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_subarray4_nameplate_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray4_nameplate_loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_subarray4_rear_irradiance_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray4_rear_irradiance_loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_subarray4_soiling_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "subarray4_soiling", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_subarray4_tracking_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray4_tracking_loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_transformer_load_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "transformer_load_loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_transformer_no_load_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "transformer_no_load_loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Losses_transmission_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "transmission_loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Lifetime_ac_lifetime_losses_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ac_lifetime_losses", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Lifetime_analysis_period_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "analysis_period", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Lifetime_dc_degradation_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dc_degradation", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Lifetime_dc_lifetime_losses_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dc_lifetime_losses", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Lifetime_en_ac_lifetime_losses_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_ac_lifetime_losses", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Lifetime_en_dc_lifetime_losses_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_dc_lifetime_losses", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Lifetime_inflation_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inflation_rate", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Lifetime_save_full_lifetime_variables_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "save_full_lifetime_variables", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Lifetime_system_use_lifetime_output_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_use_lifetime_output", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_enable_mismatch_vmax_calc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "enable_mismatch_vmax_calc", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_inverter_count_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inverter_count", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray1_azimuth_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray1_azimuth", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray1_backtrack_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray1_backtrack", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray1_gcr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray1_gcr", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray1_modules_per_string_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray1_modules_per_string", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray1_monthly_tilt_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "subarray1_monthly_tilt", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray1_mppt_input_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray1_mppt_input", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray1_nstrings_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray1_nstrings", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray1_rotlim_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray1_rotlim", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray1_tilt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray1_tilt", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray1_tilt_eq_lat_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray1_tilt_eq_lat", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray1_track_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray1_track_mode", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray2_azimuth_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray2_azimuth", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray2_backtrack_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray2_backtrack", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray2_enable_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray2_enable", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray2_gcr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray2_gcr", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray2_modules_per_string_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray2_modules_per_string", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray2_monthly_tilt_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "subarray2_monthly_tilt", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray2_mppt_input_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray2_mppt_input", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray2_nstrings_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray2_nstrings", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray2_rotlim_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray2_rotlim", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray2_tilt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray2_tilt", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray2_tilt_eq_lat_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray2_tilt_eq_lat", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray2_track_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray2_track_mode", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray3_azimuth_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray3_azimuth", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray3_backtrack_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray3_backtrack", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray3_enable_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray3_enable", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray3_gcr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray3_gcr", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray3_modules_per_string_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray3_modules_per_string", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray3_monthly_tilt_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "subarray3_monthly_tilt", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray3_mppt_input_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray3_mppt_input", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray3_nstrings_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray3_nstrings", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray3_rotlim_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray3_rotlim", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray3_tilt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray3_tilt", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray3_tilt_eq_lat_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray3_tilt_eq_lat", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray3_track_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray3_track_mode", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray4_azimuth_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray4_azimuth", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray4_backtrack_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray4_backtrack", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray4_enable_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray4_enable", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray4_gcr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray4_gcr", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray4_modules_per_string_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray4_modules_per_string", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray4_monthly_tilt_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "subarray4_monthly_tilt", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray4_mppt_input_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray4_mppt_input", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray4_nstrings_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray4_nstrings", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray4_rotlim_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray4_rotlim", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray4_tilt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray4_tilt", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray4_tilt_eq_lat_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray4_tilt_eq_lat", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray4_track_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray4_track_mode", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SystemDesign_system_capacity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_capacity", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Shading_subarray1_shade_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray1_shade_mode", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Shading_subarray1_shading_azal_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "subarray1_shading:azal", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Shading_subarray1_shading_diff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray1_shading:diff", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Shading_subarray1_shading_mxh_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "subarray1_shading:mxh", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Shading_subarray1_shading_string_option_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray1_shading:string_option", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Shading_subarray1_shading_timestep_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "subarray1_shading:timestep", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Shading_subarray2_shade_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray2_shade_mode", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Shading_subarray2_shading_azal_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "subarray2_shading:azal", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Shading_subarray2_shading_diff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray2_shading:diff", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Shading_subarray2_shading_mxh_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "subarray2_shading:mxh", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Shading_subarray2_shading_string_option_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray2_shading:string_option", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Shading_subarray2_shading_timestep_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "subarray2_shading:timestep", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Shading_subarray3_shade_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray3_shade_mode", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Shading_subarray3_shading_azal_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "subarray3_shading:azal", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Shading_subarray3_shading_diff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray3_shading:diff", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Shading_subarray3_shading_mxh_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "subarray3_shading:mxh", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Shading_subarray3_shading_string_option_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray3_shading:string_option", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Shading_subarray3_shading_timestep_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "subarray3_shading:timestep", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Shading_subarray4_shade_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray4_shade_mode", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Shading_subarray4_shading_azal_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "subarray4_shading:azal", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Shading_subarray4_shading_diff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray4_shading:diff", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Shading_subarray4_shading_mxh_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "subarray4_shading:mxh", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Shading_subarray4_shading_string_option_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray4_shading:string_option", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Shading_subarray4_shading_timestep_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "subarray4_shading:timestep", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Layout_module_aspect_ratio_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "module_aspect_ratio", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Layout_subarray1_mod_orient_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray1_mod_orient", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Layout_subarray1_nmodx_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray1_nmodx", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Layout_subarray1_nmody_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray1_nmody", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Layout_subarray2_mod_orient_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray2_mod_orient", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Layout_subarray2_nmodx_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray2_nmodx", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Layout_subarray2_nmody_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray2_nmody", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Layout_subarray3_mod_orient_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray3_mod_orient", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Layout_subarray3_nmodx_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray3_nmodx", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Layout_subarray3_nmody_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray3_nmody", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Layout_subarray4_mod_orient_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray4_mod_orient", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Layout_subarray4_nmodx_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray4_nmodx", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Layout_subarray4_nmody_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subarray4_nmody", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Module_module_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "module_model", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_a_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spe_a", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_area_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spe_area", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_b_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spe_b", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_bifacial_ground_clearance_height_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spe_bifacial_ground_clearance_height", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_bifacial_transmission_factor_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spe_bifacial_transmission_factor", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_bifaciality_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spe_bifaciality", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_dT_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spe_dT", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spe_eff0", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spe_eff1", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spe_eff2", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spe_eff3", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spe_eff4", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_fd_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spe_fd", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_is_bifacial_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spe_is_bifacial", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_module_structure_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spe_module_structure", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spe_rad0", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spe_rad1", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spe_rad2", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spe_rad3", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spe_rad4", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_reference_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spe_reference", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_temp_coeff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spe_temp_coeff", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_vmp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spe_vmp", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_voc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spe_voc", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_a_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_a_ref", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_adjust_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_adjust", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_alpha_sc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_alpha_sc", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_area_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_area", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_array_cols_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_array_cols", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_array_rows_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_array_rows", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_backside_temp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_backside_temp", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_beta_oc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_beta_oc", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_bifacial_ground_clearance_height_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_bifacial_ground_clearance_height", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_bifacial_transmission_factor_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_bifacial_transmission_factor", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_bifaciality_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_bifaciality", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_gamma_r_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_gamma_r", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_gap_spacing_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_gap_spacing", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_heat_transfer_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_heat_transfer", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_height_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_height", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_l_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_i_l_ref", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_mp_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_i_mp_ref", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_o_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_i_o_ref", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_sc_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_i_sc_ref", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_is_bifacial_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_is_bifacial", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_module_length_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_module_length", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_module_width_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_module_width", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_mounting_config_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_mounting_config", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_mounting_orientation_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_mounting_orientation", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_n_s_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_n_s", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_r_s_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_r_s", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_r_sh_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_r_sh_ref", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_standoff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_standoff", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_t_noct_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_t_noct", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_temp_corr_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_temp_corr_mode", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_transient_thermal_model_unit_mass_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_transient_thermal_model_unit_mass", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_v_mp_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_v_mp_ref", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_v_oc_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cec_v_oc_ref", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_aisc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "6par_aisc", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_area_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "6par_area", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_bifacial_ground_clearance_height_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "6par_bifacial_ground_clearance_height", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_bifacial_transmission_factor_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "6par_bifacial_transmission_factor", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_bifaciality_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "6par_bifaciality", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_bvoc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "6par_bvoc", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_celltech_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "6par_celltech", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_gpmp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "6par_gpmp", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_imp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "6par_imp", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_is_bifacial_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "6par_is_bifacial", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_isc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "6par_isc", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_mounting_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "6par_mounting", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_nser_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "6par_nser", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_standoff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "6par_standoff", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_tnoct_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "6par_tnoct", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_transient_thermal_model_unit_mass_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "6par_transient_thermal_model_unit_mass", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_vmp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "6par_vmp", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_voc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "6par_voc", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_a", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_a0", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_a1", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_a2", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_a3", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_a4", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_aimp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_aimp", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_aisc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_aisc", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_area_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_area", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_b", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_b0", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_b1", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_b2", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_b3", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_b4", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_b5", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_bvmpo_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_bvmpo", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_bvoco_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_bvoco", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_c0", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_c1", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_c2", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_c3", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_c4", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_c5", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c6_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_c6", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c7_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_c7", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_dtc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_dtc", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_fd_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_fd", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_impo_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_impo", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_isco_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_isco", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_ixo_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_ixo", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_ixxo_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_ixxo", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_mbvmp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_mbvmp", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_mbvoc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_mbvoc", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_module_structure_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_module_structure", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_n_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_n", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_ref_a_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_ref_a", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_ref_b_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_ref_b", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_ref_dT_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_ref_dT", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_series_cells_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_series_cells", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_transient_thermal_model_unit_mass_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_transient_thermal_model_unit_mass", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_vmpo_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_vmpo", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_voco_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "snl_voco", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_AMa0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sd11par_AMa0", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_AMa1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sd11par_AMa1", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_AMa2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sd11par_AMa2", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_AMa3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sd11par_AMa3", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_AMa4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sd11par_AMa4", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Egref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sd11par_Egref", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Il_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sd11par_Il", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Imp0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sd11par_Imp0", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Io_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sd11par_Io", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Isc0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sd11par_Isc0", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Vmp0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sd11par_Vmp0", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Voc0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sd11par_Voc0", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_alphaIsc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sd11par_alphaIsc", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_area_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sd11par_area", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_c1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sd11par_c1", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_c2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sd11par_c2", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_c3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sd11par_c3", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_d1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sd11par_d1", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_d2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sd11par_d2", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_d3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sd11par_d3", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_glass_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sd11par_glass", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_mounting_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sd11par_mounting", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_n_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sd11par_n", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_nser_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sd11par_nser", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_standoff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sd11par_standoff", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_tnoct_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sd11par_tnoct", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_AM_c_lp0", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_AM_c_lp1", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_AM_c_lp2", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_AM_c_lp3", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_AM_c_lp4", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_AM_c_lp5", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_sa0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_AM_c_sa0", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_sa1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_AM_c_sa1", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_sa2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_AM_c_sa2", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_sa3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_AM_c_sa3", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_sa4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_AM_c_sa4", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_AM_mode", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_D2MuTau_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_D2MuTau", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_E_g_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_E_g", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_as_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_IAM_c_as", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_cs_iamValue_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "mlm_IAM_c_cs_iamValue", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_cs_incAngle_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "mlm_IAM_c_cs_incAngle", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_IAM_c_sa0", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_IAM_c_sa1", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_IAM_c_sa2", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_IAM_c_sa3", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_IAM_c_sa4", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_IAM_c_sa5", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_IAM_mode", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_I_mp_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_I_mp_ref", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_I_sc_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_I_sc_ref", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_Length_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_Length", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_N_diodes_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_N_diodes", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_N_parallel_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_N_parallel", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_N_series_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_N_series", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_R_s_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_R_s", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_R_sh0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_R_sh0", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_R_shexp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_R_shexp", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_R_shref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_R_shref", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_S_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_S_ref", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_fa_U0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_T_c_fa_U0", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_fa_U1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_T_c_fa_U1", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_fa_alpha_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_T_c_fa_alpha", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_no_mounting_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_T_c_no_mounting", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_no_standoff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_T_c_no_standoff", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_no_tnoct_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_T_c_no_tnoct", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_T_mode", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_T_ref", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_V_mp_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_V_mp_ref", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_V_oc_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_V_oc_ref", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_Width_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_Width", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_alpha_isc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_alpha_isc", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_beta_voc_spec_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_beta_voc_spec", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_groundRelfectionFraction_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_groundRelfectionFraction", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_mu_n_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_mu_n", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_n_0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mlm_n_0", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Inverter_inv_cec_cg_eff_cec_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_cec_cg_eff_cec", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Inverter_inv_cec_cg_paco_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_cec_cg_paco", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Inverter_inv_ds_eff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_ds_eff", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Inverter_inv_ds_paco_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_ds_paco", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Inverter_inv_num_mppt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_num_mppt", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Inverter_inv_pd_eff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_pd_eff", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Inverter_inv_pd_paco_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_pd_paco", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Inverter_inv_snl_eff_cec_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_snl_eff_cec", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Inverter_inv_snl_paco_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_snl_paco", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Inverter_inverter_count_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inverter_count", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Inverter_inverter_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inverter_model", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Inverter_mppt_hi_inverter_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mppt_hi_inverter", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Inverter_mppt_low_inverter_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mppt_low_inverter", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_c0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_snl_c0", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_c1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_snl_c1", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_c2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_snl_c2", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_c3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_snl_c3", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_paco_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_snl_paco", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_pdco_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_snl_pdco", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_pnt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_snl_pnt", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_pso_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_snl_pso", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_vdcmax_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_snl_vdcmax", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_vdco_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_snl_vdco", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_tdc_cec_db_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "inv_tdc_cec_db", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_c0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_cec_cg_c0", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_c1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_cec_cg_c1", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_c2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_cec_cg_c2", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_c3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_cec_cg_c3", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_paco_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_cec_cg_paco", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_pdco_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_cec_cg_pdco", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_pnt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_cec_cg_pnt", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_psco_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_cec_cg_psco", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_vdcmax_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_cec_cg_vdcmax", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_vdco_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_cec_cg_vdco", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_tdc_cec_cg_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "inv_tdc_cec_cg", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterDatasheet_inv_ds_eff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_ds_eff", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterDatasheet_inv_ds_paco_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_ds_paco", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterDatasheet_inv_ds_pnt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_ds_pnt", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterDatasheet_inv_ds_pso_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_ds_pso", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterDatasheet_inv_ds_vdcmax_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_ds_vdcmax", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterDatasheet_inv_ds_vdco_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_ds_vdco", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterDatasheet_inv_tdc_ds_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "inv_tdc_ds", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_efficiency_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "inv_pd_efficiency", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_paco_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_pd_paco", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_partload_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "inv_pd_partload", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_pdco_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_pd_pdco", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_pnt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_pd_pnt", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_vdcmax_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_pd_vdcmax", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_vdco_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_pd_vdco", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterPartLoadCurve_inv_tdc_plc_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "inv_tdc_plc", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_Aux_Loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ond_Aux_Loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_CompPMax_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "ond_CompPMax", str);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_CompVMax_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "ond_CompVMax", str);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_IMaxAC_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ond_IMaxAC", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_IMaxDC_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ond_IMaxDC", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_INomAC_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ond_INomAC", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_INomDC_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ond_INomDC", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_ModeAffEnum_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "ond_ModeAffEnum", str);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_ModeOper_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "ond_ModeOper", str);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_NbInputs_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ond_NbInputs", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_NbMPPT_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ond_NbMPPT", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_Night_Loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ond_Night_Loss", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PLim1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ond_PLim1", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PLimAbs_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ond_PLimAbs", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PMaxDC_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ond_PMaxDC", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PMaxOUT_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ond_PMaxOUT", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PNomConv_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ond_PNomConv", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PNomDC_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ond_PNomDC", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PSeuil_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ond_PSeuil", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_TPLim1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ond_TPLim1", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_TPLimAbs_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ond_TPLimAbs", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_TPMax_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ond_TPMax", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_TPNom_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ond_TPNom", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_VAbsMax_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ond_VAbsMax", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_VMPPMax_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ond_VMPPMax", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_VMppMin_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ond_VMppMin", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_VNomEff_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ond_VNomEff", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_VOutConv_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ond_VOutConv", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_doAllowOverpower_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ond_doAllowOverpower", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_doUseTemperatureLimit_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ond_doUseTemperatureLimit", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_effCurve_Pac_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ond_effCurve_Pac", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_effCurve_Pdc_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ond_effCurve_Pdc", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_effCurve_elements_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ond_effCurve_elements", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_effCurve_eta_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ond_effCurve_eta", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_lossRAc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ond_lossRAc", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_lossRDc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ond_lossRDc", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_ac_dc_efficiency_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_ac_dc_efficiency", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_ac_or_dc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_ac_or_dc", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_computed_bank_capacity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_computed_bank_capacity", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_computed_series_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_computed_series", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_computed_strings_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_computed_strings", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_current_charge_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_current_charge_max", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_current_choice_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_current_choice", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_current_discharge_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_current_discharge_max", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_dc_ac_efficiency_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_dc_ac_efficiency", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_dc_dc_efficiency_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_dc_dc_efficiency", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_inverter_efficiency_cutoff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_inverter_efficiency_cutoff", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_loss_choice_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_loss_choice", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_losses_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_losses", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_losses_charging_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_losses_charging", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_losses_discharging_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_losses_discharging", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_losses_idle_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_losses_idle", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_mass_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_mass", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_meter_position_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_meter_position", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_power_charge_max_kwac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_power_charge_max_kwac", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_power_charge_max_kwdc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_power_charge_max_kwdc", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_power_discharge_max_kwac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_power_discharge_max_kwac", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_power_discharge_max_kwdc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_power_discharge_max_kwdc", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_replacement_capacity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_replacement_capacity", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_replacement_option_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_replacement_option", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_replacement_schedule_percent_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_replacement_schedule_percent", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_surface_area_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_surface_area", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatterySystem_en_batt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_batt", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatterySystem_om_replacement_cost1_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_replacement_cost1", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Load_crit_load_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "crit_load", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Load_load_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "load", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_Load_load_escalation_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "load_escalation", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_LeadAcid_q10_computed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "LeadAcid_q10_computed", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_LeadAcid_q20_computed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "LeadAcid_q20_computed", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_LeadAcid_qn_computed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "LeadAcid_qn_computed", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_LeadAcid_tn_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "LeadAcid_tn", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_C_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_C_rate", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_Cp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_Cp", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_Qexp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_Qexp", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_Qfull_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_Qfull", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_Qfull_flow_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_Qfull_flow", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_Qnom_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_Qnom", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_Vexp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_Vexp", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_Vfull_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_Vfull", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_Vnom_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_Vnom", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_Vnom_default_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_Vnom_default", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_calendar_a_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_calendar_a", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_calendar_b_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_calendar_b", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_calendar_c_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_calendar_c", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_calendar_choice_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_calendar_choice", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_calendar_lifetime_matrix_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "batt_calendar_lifetime_matrix", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_calendar_q0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_calendar_q0", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_chem_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_chem", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_h_to_ambient_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_h_to_ambient", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_initial_SOC_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_initial_SOC", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_lifetime_matrix_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "batt_lifetime_matrix", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_maximum_SOC_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_maximum_SOC", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_minimum_SOC_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_minimum_SOC", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_minimum_modetime_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_minimum_modetime", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_resistance_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_resistance", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_room_temperature_celsius_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_room_temperature_celsius", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_voltage_choice_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_voltage_choice", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_voltage_matrix_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "batt_voltage_matrix", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryCell_cap_vs_temp_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "cap_vs_temp", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_auto_gridcharge_max_daily_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_auto_gridcharge_max_daily", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_custom_dispatch_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_custom_dispatch", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_cycle_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_cycle_cost", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_cycle_cost_choice_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_cycle_cost_choice", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_dispatch_auto_can_charge_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_dispatch_auto_can_charge", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_dispatch_auto_can_clipcharge_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_dispatch_auto_can_clipcharge", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_dispatch_auto_can_fuelcellcharge_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_dispatch_auto_can_fuelcellcharge", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_dispatch_auto_can_gridcharge_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_dispatch_auto_can_gridcharge", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_dispatch_choice_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_dispatch_choice", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_dispatch_update_frequency_hours_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_dispatch_update_frequency_hours", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_look_ahead_hours_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_look_ahead_hours", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_pv_ac_forecast_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_pv_ac_forecast", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_pv_clipping_forecast_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_pv_clipping_forecast", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_target_choice_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_target_choice", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_target_power_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_target_power", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_target_power_monthly_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_target_power_monthly", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_dispatch_manual_charge_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_manual_charge", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_dispatch_manual_discharge_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_manual_discharge", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_dispatch_manual_fuelcellcharge_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_manual_fuelcellcharge", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_dispatch_manual_gridcharge_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_manual_gridcharge", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_dispatch_manual_percent_discharge_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_manual_percent_discharge", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_dispatch_manual_percent_gridcharge_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_manual_percent_gridcharge", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_dispatch_manual_sched_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "dispatch_manual_sched", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_dispatch_manual_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "dispatch_manual_sched_weekend", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_FuelCell_fuelcell_power_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "fuelcell_power", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_PriceSignal_dispatch_factors_ts_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_factors_ts", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_PriceSignal_dispatch_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "dispatch_sched_weekday", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_PriceSignal_dispatch_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "dispatch_sched_weekend", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_PriceSignal_dispatch_tod_factors_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_tod_factors", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_PriceSignal_forecast_price_signal_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "forecast_price_signal_model", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_PriceSignal_mp_ancserv1_revenue_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "mp_ancserv1_revenue", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_PriceSignal_mp_ancserv2_revenue_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "mp_ancserv2_revenue", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_PriceSignal_mp_ancserv3_revenue_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "mp_ancserv3_revenue", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_PriceSignal_mp_ancserv4_revenue_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "mp_ancserv4_revenue", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_PriceSignal_mp_enable_ancserv1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mp_enable_ancserv1", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_PriceSignal_mp_enable_ancserv2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mp_enable_ancserv2", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_PriceSignal_mp_enable_ancserv3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mp_enable_ancserv3", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_PriceSignal_mp_enable_ancserv4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mp_enable_ancserv4", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_PriceSignal_mp_enable_energy_market_revenue_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mp_enable_energy_market_revenue", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_PriceSignal_mp_energy_market_revenue_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "mp_energy_market_revenue", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_PriceSignal_ppa_multiplier_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ppa_multiplier_model", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_PriceSignal_ppa_price_input_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ppa_price_input", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_rate_escalation_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "rate_escalation", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_annual_min_charge_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_annual_min_charge", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_dc_enable_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_enable", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_dc_flat_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_dc_flat_mat", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_dc_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_dc_sched_weekday", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_dc_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_dc_sched_weekend", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_dc_tou_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_dc_tou_mat", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_ec_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_ec_sched_weekday", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_ec_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_ec_sched_weekend", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_ec_tou_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_ec_tou_mat", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_en_ts_buy_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_en_ts_buy_rate", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_en_ts_sell_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_en_ts_sell_rate", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_metering_option_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_metering_option", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_monthly_fixed_charge_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_monthly_fixed_charge", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_monthly_min_charge_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_monthly_min_charge", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_nm_credit_month_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_nm_credit_month", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_nm_credit_rollover_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_nm_credit_rollover", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_nm_yearend_sell_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_nm_yearend_sell_rate", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_sell_eq_buy_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_sell_eq_buy", number);
	});
}

SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_ts_buy_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ur_ts_buy_rate", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_ts_sell_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ur_ts_sell_rate", arr, length);
	});
}

SAM_EXPORT double* SAM_Pvsamv1_SolarResource_albedo_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "albedo", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "albedo");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SolarResource_irrad_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "irrad_mode", &result))
		make_access_error("SAM_Pvsamv1", "irrad_mode");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SolarResource_sky_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sky_model", &result))
		make_access_error("SAM_Pvsamv1", "sky_model");
	});
	return result;
}



SAM_EXPORT SAM_table SAM_Pvsamv1_SolarResource_solar_resource_data_tget(SAM_table ptr, SAM_error *err){
	SAM_table result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_table(ptr, "solar_resource_data");
	if (!result)
		make_access_error("SAM_Pvsamv1", "solar_resource_data");
	});
	return result;
}



SAM_EXPORT const char* SAM_Pvsamv1_SolarResource_solar_resource_file_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "solar_resource_file");
	if (!result)
		make_access_error("SAM_Pvsamv1", "solar_resource_file");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SolarResource_use_wf_albedo_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "use_wf_albedo", &result))
		make_access_error("SAM_Pvsamv1", "use_wf_albedo");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Losses_acwiring_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "acwiring_loss", &result))
		make_access_error("SAM_Pvsamv1", "acwiring_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Losses_dcoptimizer_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dcoptimizer_loss", &result))
		make_access_error("SAM_Pvsamv1", "dcoptimizer_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Losses_en_snow_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_snow_model", &result))
		make_access_error("SAM_Pvsamv1", "en_snow_model");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Losses_subarray1_dcwiring_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray1_dcwiring_loss", &result))
		make_access_error("SAM_Pvsamv1", "subarray1_dcwiring_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Losses_subarray1_diodeconn_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray1_diodeconn_loss", &result))
		make_access_error("SAM_Pvsamv1", "subarray1_diodeconn_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Losses_subarray1_mismatch_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray1_mismatch_loss", &result))
		make_access_error("SAM_Pvsamv1", "subarray1_mismatch_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Losses_subarray1_nameplate_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray1_nameplate_loss", &result))
		make_access_error("SAM_Pvsamv1", "subarray1_nameplate_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Losses_subarray1_rear_irradiance_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray1_rear_irradiance_loss", &result))
		make_access_error("SAM_Pvsamv1", "subarray1_rear_irradiance_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Losses_subarray1_soiling_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_soiling", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_soiling");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Losses_subarray1_tracking_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray1_tracking_loss", &result))
		make_access_error("SAM_Pvsamv1", "subarray1_tracking_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Losses_subarray2_dcwiring_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray2_dcwiring_loss", &result))
		make_access_error("SAM_Pvsamv1", "subarray2_dcwiring_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Losses_subarray2_diodeconn_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray2_diodeconn_loss", &result))
		make_access_error("SAM_Pvsamv1", "subarray2_diodeconn_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Losses_subarray2_mismatch_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray2_mismatch_loss", &result))
		make_access_error("SAM_Pvsamv1", "subarray2_mismatch_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Losses_subarray2_nameplate_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray2_nameplate_loss", &result))
		make_access_error("SAM_Pvsamv1", "subarray2_nameplate_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Losses_subarray2_rear_irradiance_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray2_rear_irradiance_loss", &result))
		make_access_error("SAM_Pvsamv1", "subarray2_rear_irradiance_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Losses_subarray2_soiling_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_soiling", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_soiling");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Losses_subarray2_tracking_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray2_tracking_loss", &result))
		make_access_error("SAM_Pvsamv1", "subarray2_tracking_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Losses_subarray3_dcwiring_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray3_dcwiring_loss", &result))
		make_access_error("SAM_Pvsamv1", "subarray3_dcwiring_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Losses_subarray3_diodeconn_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray3_diodeconn_loss", &result))
		make_access_error("SAM_Pvsamv1", "subarray3_diodeconn_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Losses_subarray3_mismatch_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray3_mismatch_loss", &result))
		make_access_error("SAM_Pvsamv1", "subarray3_mismatch_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Losses_subarray3_nameplate_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray3_nameplate_loss", &result))
		make_access_error("SAM_Pvsamv1", "subarray3_nameplate_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Losses_subarray3_rear_irradiance_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray3_rear_irradiance_loss", &result))
		make_access_error("SAM_Pvsamv1", "subarray3_rear_irradiance_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Losses_subarray3_soiling_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_soiling", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_soiling");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Losses_subarray3_tracking_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray3_tracking_loss", &result))
		make_access_error("SAM_Pvsamv1", "subarray3_tracking_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Losses_subarray4_dcwiring_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray4_dcwiring_loss", &result))
		make_access_error("SAM_Pvsamv1", "subarray4_dcwiring_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Losses_subarray4_diodeconn_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray4_diodeconn_loss", &result))
		make_access_error("SAM_Pvsamv1", "subarray4_diodeconn_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Losses_subarray4_mismatch_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray4_mismatch_loss", &result))
		make_access_error("SAM_Pvsamv1", "subarray4_mismatch_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Losses_subarray4_nameplate_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray4_nameplate_loss", &result))
		make_access_error("SAM_Pvsamv1", "subarray4_nameplate_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Losses_subarray4_rear_irradiance_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray4_rear_irradiance_loss", &result))
		make_access_error("SAM_Pvsamv1", "subarray4_rear_irradiance_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Losses_subarray4_soiling_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_soiling", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_soiling");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Losses_subarray4_tracking_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray4_tracking_loss", &result))
		make_access_error("SAM_Pvsamv1", "subarray4_tracking_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Losses_transformer_load_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "transformer_load_loss", &result))
		make_access_error("SAM_Pvsamv1", "transformer_load_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Losses_transformer_no_load_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "transformer_no_load_loss", &result))
		make_access_error("SAM_Pvsamv1", "transformer_no_load_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Losses_transmission_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "transmission_loss", &result))
		make_access_error("SAM_Pvsamv1", "transmission_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Lifetime_ac_lifetime_losses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ac_lifetime_losses", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "ac_lifetime_losses");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Lifetime_analysis_period_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "analysis_period", &result))
		make_access_error("SAM_Pvsamv1", "analysis_period");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Lifetime_dc_degradation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dc_degradation", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "dc_degradation");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Lifetime_dc_lifetime_losses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dc_lifetime_losses", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "dc_lifetime_losses");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Lifetime_en_ac_lifetime_losses_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_ac_lifetime_losses", &result))
		make_access_error("SAM_Pvsamv1", "en_ac_lifetime_losses");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Lifetime_en_dc_lifetime_losses_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_dc_lifetime_losses", &result))
		make_access_error("SAM_Pvsamv1", "en_dc_lifetime_losses");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Lifetime_inflation_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inflation_rate", &result))
		make_access_error("SAM_Pvsamv1", "inflation_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Lifetime_save_full_lifetime_variables_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "save_full_lifetime_variables", &result))
		make_access_error("SAM_Pvsamv1", "save_full_lifetime_variables");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Lifetime_system_use_lifetime_output_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_use_lifetime_output", &result))
		make_access_error("SAM_Pvsamv1", "system_use_lifetime_output");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_enable_mismatch_vmax_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "enable_mismatch_vmax_calc", &result))
		make_access_error("SAM_Pvsamv1", "enable_mismatch_vmax_calc");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_inverter_count_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inverter_count", &result))
		make_access_error("SAM_Pvsamv1", "inverter_count");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray1_azimuth_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray1_azimuth", &result))
		make_access_error("SAM_Pvsamv1", "subarray1_azimuth");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray1_backtrack_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray1_backtrack", &result))
		make_access_error("SAM_Pvsamv1", "subarray1_backtrack");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray1_gcr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray1_gcr", &result))
		make_access_error("SAM_Pvsamv1", "subarray1_gcr");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray1_modules_per_string_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray1_modules_per_string", &result))
		make_access_error("SAM_Pvsamv1", "subarray1_modules_per_string");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_SystemDesign_subarray1_monthly_tilt_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_monthly_tilt", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_monthly_tilt");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray1_mppt_input_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray1_mppt_input", &result))
		make_access_error("SAM_Pvsamv1", "subarray1_mppt_input");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray1_nstrings_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray1_nstrings", &result))
		make_access_error("SAM_Pvsamv1", "subarray1_nstrings");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray1_rotlim_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray1_rotlim", &result))
		make_access_error("SAM_Pvsamv1", "subarray1_rotlim");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray1_tilt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray1_tilt", &result))
		make_access_error("SAM_Pvsamv1", "subarray1_tilt");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray1_tilt_eq_lat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray1_tilt_eq_lat", &result))
		make_access_error("SAM_Pvsamv1", "subarray1_tilt_eq_lat");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray1_track_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray1_track_mode", &result))
		make_access_error("SAM_Pvsamv1", "subarray1_track_mode");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray2_azimuth_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray2_azimuth", &result))
		make_access_error("SAM_Pvsamv1", "subarray2_azimuth");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray2_backtrack_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray2_backtrack", &result))
		make_access_error("SAM_Pvsamv1", "subarray2_backtrack");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray2_enable_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray2_enable", &result))
		make_access_error("SAM_Pvsamv1", "subarray2_enable");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray2_gcr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray2_gcr", &result))
		make_access_error("SAM_Pvsamv1", "subarray2_gcr");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray2_modules_per_string_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray2_modules_per_string", &result))
		make_access_error("SAM_Pvsamv1", "subarray2_modules_per_string");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_SystemDesign_subarray2_monthly_tilt_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_monthly_tilt", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_monthly_tilt");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray2_mppt_input_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray2_mppt_input", &result))
		make_access_error("SAM_Pvsamv1", "subarray2_mppt_input");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray2_nstrings_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray2_nstrings", &result))
		make_access_error("SAM_Pvsamv1", "subarray2_nstrings");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray2_rotlim_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray2_rotlim", &result))
		make_access_error("SAM_Pvsamv1", "subarray2_rotlim");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray2_tilt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray2_tilt", &result))
		make_access_error("SAM_Pvsamv1", "subarray2_tilt");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray2_tilt_eq_lat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray2_tilt_eq_lat", &result))
		make_access_error("SAM_Pvsamv1", "subarray2_tilt_eq_lat");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray2_track_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray2_track_mode", &result))
		make_access_error("SAM_Pvsamv1", "subarray2_track_mode");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray3_azimuth_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray3_azimuth", &result))
		make_access_error("SAM_Pvsamv1", "subarray3_azimuth");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray3_backtrack_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray3_backtrack", &result))
		make_access_error("SAM_Pvsamv1", "subarray3_backtrack");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray3_enable_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray3_enable", &result))
		make_access_error("SAM_Pvsamv1", "subarray3_enable");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray3_gcr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray3_gcr", &result))
		make_access_error("SAM_Pvsamv1", "subarray3_gcr");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray3_modules_per_string_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray3_modules_per_string", &result))
		make_access_error("SAM_Pvsamv1", "subarray3_modules_per_string");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_SystemDesign_subarray3_monthly_tilt_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_monthly_tilt", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_monthly_tilt");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray3_mppt_input_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray3_mppt_input", &result))
		make_access_error("SAM_Pvsamv1", "subarray3_mppt_input");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray3_nstrings_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray3_nstrings", &result))
		make_access_error("SAM_Pvsamv1", "subarray3_nstrings");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray3_rotlim_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray3_rotlim", &result))
		make_access_error("SAM_Pvsamv1", "subarray3_rotlim");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray3_tilt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray3_tilt", &result))
		make_access_error("SAM_Pvsamv1", "subarray3_tilt");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray3_tilt_eq_lat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray3_tilt_eq_lat", &result))
		make_access_error("SAM_Pvsamv1", "subarray3_tilt_eq_lat");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray3_track_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray3_track_mode", &result))
		make_access_error("SAM_Pvsamv1", "subarray3_track_mode");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray4_azimuth_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray4_azimuth", &result))
		make_access_error("SAM_Pvsamv1", "subarray4_azimuth");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray4_backtrack_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray4_backtrack", &result))
		make_access_error("SAM_Pvsamv1", "subarray4_backtrack");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray4_enable_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray4_enable", &result))
		make_access_error("SAM_Pvsamv1", "subarray4_enable");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray4_gcr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray4_gcr", &result))
		make_access_error("SAM_Pvsamv1", "subarray4_gcr");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray4_modules_per_string_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray4_modules_per_string", &result))
		make_access_error("SAM_Pvsamv1", "subarray4_modules_per_string");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_SystemDesign_subarray4_monthly_tilt_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_monthly_tilt", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_monthly_tilt");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray4_mppt_input_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray4_mppt_input", &result))
		make_access_error("SAM_Pvsamv1", "subarray4_mppt_input");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray4_nstrings_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray4_nstrings", &result))
		make_access_error("SAM_Pvsamv1", "subarray4_nstrings");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray4_rotlim_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray4_rotlim", &result))
		make_access_error("SAM_Pvsamv1", "subarray4_rotlim");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray4_tilt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray4_tilt", &result))
		make_access_error("SAM_Pvsamv1", "subarray4_tilt");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray4_tilt_eq_lat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray4_tilt_eq_lat", &result))
		make_access_error("SAM_Pvsamv1", "subarray4_tilt_eq_lat");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray4_track_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray4_track_mode", &result))
		make_access_error("SAM_Pvsamv1", "subarray4_track_mode");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SystemDesign_system_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_Pvsamv1", "system_capacity");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Shading_subarray1_shade_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray1_shade_mode", &result))
		make_access_error("SAM_Pvsamv1", "subarray1_shade_mode");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Shading_subarray1_shading_azal_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "subarray1_shading:azal", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_shading:azal");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Shading_subarray1_shading_diff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray1_shading:diff", &result))
		make_access_error("SAM_Pvsamv1", "subarray1_shading:diff");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Shading_subarray1_shading_mxh_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "subarray1_shading:mxh", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_shading:mxh");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Shading_subarray1_shading_string_option_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray1_shading:string_option", &result))
		make_access_error("SAM_Pvsamv1", "subarray1_shading:string_option");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Shading_subarray1_shading_timestep_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "subarray1_shading:timestep", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_shading:timestep");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Shading_subarray2_shade_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray2_shade_mode", &result))
		make_access_error("SAM_Pvsamv1", "subarray2_shade_mode");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Shading_subarray2_shading_azal_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "subarray2_shading:azal", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_shading:azal");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Shading_subarray2_shading_diff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray2_shading:diff", &result))
		make_access_error("SAM_Pvsamv1", "subarray2_shading:diff");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Shading_subarray2_shading_mxh_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "subarray2_shading:mxh", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_shading:mxh");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Shading_subarray2_shading_string_option_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray2_shading:string_option", &result))
		make_access_error("SAM_Pvsamv1", "subarray2_shading:string_option");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Shading_subarray2_shading_timestep_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "subarray2_shading:timestep", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_shading:timestep");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Shading_subarray3_shade_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray3_shade_mode", &result))
		make_access_error("SAM_Pvsamv1", "subarray3_shade_mode");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Shading_subarray3_shading_azal_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "subarray3_shading:azal", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_shading:azal");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Shading_subarray3_shading_diff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray3_shading:diff", &result))
		make_access_error("SAM_Pvsamv1", "subarray3_shading:diff");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Shading_subarray3_shading_mxh_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "subarray3_shading:mxh", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_shading:mxh");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Shading_subarray3_shading_string_option_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray3_shading:string_option", &result))
		make_access_error("SAM_Pvsamv1", "subarray3_shading:string_option");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Shading_subarray3_shading_timestep_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "subarray3_shading:timestep", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_shading:timestep");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Shading_subarray4_shade_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray4_shade_mode", &result))
		make_access_error("SAM_Pvsamv1", "subarray4_shade_mode");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Shading_subarray4_shading_azal_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "subarray4_shading:azal", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_shading:azal");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Shading_subarray4_shading_diff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray4_shading:diff", &result))
		make_access_error("SAM_Pvsamv1", "subarray4_shading:diff");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Shading_subarray4_shading_mxh_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "subarray4_shading:mxh", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_shading:mxh");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Shading_subarray4_shading_string_option_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray4_shading:string_option", &result))
		make_access_error("SAM_Pvsamv1", "subarray4_shading:string_option");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Shading_subarray4_shading_timestep_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "subarray4_shading:timestep", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_shading:timestep");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Layout_module_aspect_ratio_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "module_aspect_ratio", &result))
		make_access_error("SAM_Pvsamv1", "module_aspect_ratio");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Layout_subarray1_mod_orient_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray1_mod_orient", &result))
		make_access_error("SAM_Pvsamv1", "subarray1_mod_orient");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Layout_subarray1_nmodx_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray1_nmodx", &result))
		make_access_error("SAM_Pvsamv1", "subarray1_nmodx");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Layout_subarray1_nmody_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray1_nmody", &result))
		make_access_error("SAM_Pvsamv1", "subarray1_nmody");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Layout_subarray2_mod_orient_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray2_mod_orient", &result))
		make_access_error("SAM_Pvsamv1", "subarray2_mod_orient");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Layout_subarray2_nmodx_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray2_nmodx", &result))
		make_access_error("SAM_Pvsamv1", "subarray2_nmodx");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Layout_subarray2_nmody_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray2_nmody", &result))
		make_access_error("SAM_Pvsamv1", "subarray2_nmody");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Layout_subarray3_mod_orient_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray3_mod_orient", &result))
		make_access_error("SAM_Pvsamv1", "subarray3_mod_orient");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Layout_subarray3_nmodx_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray3_nmodx", &result))
		make_access_error("SAM_Pvsamv1", "subarray3_nmodx");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Layout_subarray3_nmody_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray3_nmody", &result))
		make_access_error("SAM_Pvsamv1", "subarray3_nmody");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Layout_subarray4_mod_orient_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray4_mod_orient", &result))
		make_access_error("SAM_Pvsamv1", "subarray4_mod_orient");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Layout_subarray4_nmodx_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray4_nmodx", &result))
		make_access_error("SAM_Pvsamv1", "subarray4_nmodx");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Layout_subarray4_nmody_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray4_nmody", &result))
		make_access_error("SAM_Pvsamv1", "subarray4_nmody");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Module_module_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "module_model", &result))
		make_access_error("SAM_Pvsamv1", "module_model");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_a_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spe_a", &result))
		make_access_error("SAM_Pvsamv1", "spe_a");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spe_area", &result))
		make_access_error("SAM_Pvsamv1", "spe_area");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_b_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spe_b", &result))
		make_access_error("SAM_Pvsamv1", "spe_b");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_bifacial_ground_clearance_height_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spe_bifacial_ground_clearance_height", &result))
		make_access_error("SAM_Pvsamv1", "spe_bifacial_ground_clearance_height");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_bifacial_transmission_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spe_bifacial_transmission_factor", &result))
		make_access_error("SAM_Pvsamv1", "spe_bifacial_transmission_factor");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_bifaciality_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spe_bifaciality", &result))
		make_access_error("SAM_Pvsamv1", "spe_bifaciality");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_dT_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spe_dT", &result))
		make_access_error("SAM_Pvsamv1", "spe_dT");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spe_eff0", &result))
		make_access_error("SAM_Pvsamv1", "spe_eff0");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spe_eff1", &result))
		make_access_error("SAM_Pvsamv1", "spe_eff1");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spe_eff2", &result))
		make_access_error("SAM_Pvsamv1", "spe_eff2");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spe_eff3", &result))
		make_access_error("SAM_Pvsamv1", "spe_eff3");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spe_eff4", &result))
		make_access_error("SAM_Pvsamv1", "spe_eff4");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_fd_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spe_fd", &result))
		make_access_error("SAM_Pvsamv1", "spe_fd");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_is_bifacial_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spe_is_bifacial", &result))
		make_access_error("SAM_Pvsamv1", "spe_is_bifacial");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_module_structure_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spe_module_structure", &result))
		make_access_error("SAM_Pvsamv1", "spe_module_structure");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spe_rad0", &result))
		make_access_error("SAM_Pvsamv1", "spe_rad0");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spe_rad1", &result))
		make_access_error("SAM_Pvsamv1", "spe_rad1");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spe_rad2", &result))
		make_access_error("SAM_Pvsamv1", "spe_rad2");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spe_rad3", &result))
		make_access_error("SAM_Pvsamv1", "spe_rad3");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spe_rad4", &result))
		make_access_error("SAM_Pvsamv1", "spe_rad4");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_reference_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spe_reference", &result))
		make_access_error("SAM_Pvsamv1", "spe_reference");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_temp_coeff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spe_temp_coeff", &result))
		make_access_error("SAM_Pvsamv1", "spe_temp_coeff");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_vmp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spe_vmp", &result))
		make_access_error("SAM_Pvsamv1", "spe_vmp");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_voc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spe_voc", &result))
		make_access_error("SAM_Pvsamv1", "spe_voc");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_a_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_a_ref", &result))
		make_access_error("SAM_Pvsamv1", "cec_a_ref");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_adjust_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_adjust", &result))
		make_access_error("SAM_Pvsamv1", "cec_adjust");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_alpha_sc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_alpha_sc", &result))
		make_access_error("SAM_Pvsamv1", "cec_alpha_sc");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_area", &result))
		make_access_error("SAM_Pvsamv1", "cec_area");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_array_cols_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_array_cols", &result))
		make_access_error("SAM_Pvsamv1", "cec_array_cols");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_array_rows_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_array_rows", &result))
		make_access_error("SAM_Pvsamv1", "cec_array_rows");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_backside_temp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_backside_temp", &result))
		make_access_error("SAM_Pvsamv1", "cec_backside_temp");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_beta_oc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_beta_oc", &result))
		make_access_error("SAM_Pvsamv1", "cec_beta_oc");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_bifacial_ground_clearance_height_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_bifacial_ground_clearance_height", &result))
		make_access_error("SAM_Pvsamv1", "cec_bifacial_ground_clearance_height");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_bifacial_transmission_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_bifacial_transmission_factor", &result))
		make_access_error("SAM_Pvsamv1", "cec_bifacial_transmission_factor");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_bifaciality_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_bifaciality", &result))
		make_access_error("SAM_Pvsamv1", "cec_bifaciality");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_gamma_r_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_gamma_r", &result))
		make_access_error("SAM_Pvsamv1", "cec_gamma_r");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_gap_spacing_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_gap_spacing", &result))
		make_access_error("SAM_Pvsamv1", "cec_gap_spacing");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_heat_transfer_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_heat_transfer", &result))
		make_access_error("SAM_Pvsamv1", "cec_heat_transfer");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_height_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_height", &result))
		make_access_error("SAM_Pvsamv1", "cec_height");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_l_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_i_l_ref", &result))
		make_access_error("SAM_Pvsamv1", "cec_i_l_ref");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_mp_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_i_mp_ref", &result))
		make_access_error("SAM_Pvsamv1", "cec_i_mp_ref");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_o_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_i_o_ref", &result))
		make_access_error("SAM_Pvsamv1", "cec_i_o_ref");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_sc_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_i_sc_ref", &result))
		make_access_error("SAM_Pvsamv1", "cec_i_sc_ref");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_is_bifacial_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_is_bifacial", &result))
		make_access_error("SAM_Pvsamv1", "cec_is_bifacial");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_module_length_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_module_length", &result))
		make_access_error("SAM_Pvsamv1", "cec_module_length");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_module_width_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_module_width", &result))
		make_access_error("SAM_Pvsamv1", "cec_module_width");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_mounting_config_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_mounting_config", &result))
		make_access_error("SAM_Pvsamv1", "cec_mounting_config");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_mounting_orientation_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_mounting_orientation", &result))
		make_access_error("SAM_Pvsamv1", "cec_mounting_orientation");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_n_s_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_n_s", &result))
		make_access_error("SAM_Pvsamv1", "cec_n_s");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_r_s_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_r_s", &result))
		make_access_error("SAM_Pvsamv1", "cec_r_s");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_r_sh_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_r_sh_ref", &result))
		make_access_error("SAM_Pvsamv1", "cec_r_sh_ref");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_standoff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_standoff", &result))
		make_access_error("SAM_Pvsamv1", "cec_standoff");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_t_noct_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_t_noct", &result))
		make_access_error("SAM_Pvsamv1", "cec_t_noct");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_temp_corr_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_temp_corr_mode", &result))
		make_access_error("SAM_Pvsamv1", "cec_temp_corr_mode");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_transient_thermal_model_unit_mass_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_transient_thermal_model_unit_mass", &result))
		make_access_error("SAM_Pvsamv1", "cec_transient_thermal_model_unit_mass");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_v_mp_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_v_mp_ref", &result))
		make_access_error("SAM_Pvsamv1", "cec_v_mp_ref");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_v_oc_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cec_v_oc_ref", &result))
		make_access_error("SAM_Pvsamv1", "cec_v_oc_ref");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_aisc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "6par_aisc", &result))
		make_access_error("SAM_Pvsamv1", "6par_aisc");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "6par_area", &result))
		make_access_error("SAM_Pvsamv1", "6par_area");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_bifacial_ground_clearance_height_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "6par_bifacial_ground_clearance_height", &result))
		make_access_error("SAM_Pvsamv1", "6par_bifacial_ground_clearance_height");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_bifacial_transmission_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "6par_bifacial_transmission_factor", &result))
		make_access_error("SAM_Pvsamv1", "6par_bifacial_transmission_factor");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_bifaciality_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "6par_bifaciality", &result))
		make_access_error("SAM_Pvsamv1", "6par_bifaciality");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_bvoc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "6par_bvoc", &result))
		make_access_error("SAM_Pvsamv1", "6par_bvoc");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_celltech_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "6par_celltech", &result))
		make_access_error("SAM_Pvsamv1", "6par_celltech");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_gpmp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "6par_gpmp", &result))
		make_access_error("SAM_Pvsamv1", "6par_gpmp");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_imp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "6par_imp", &result))
		make_access_error("SAM_Pvsamv1", "6par_imp");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_is_bifacial_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "6par_is_bifacial", &result))
		make_access_error("SAM_Pvsamv1", "6par_is_bifacial");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_isc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "6par_isc", &result))
		make_access_error("SAM_Pvsamv1", "6par_isc");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_mounting_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "6par_mounting", &result))
		make_access_error("SAM_Pvsamv1", "6par_mounting");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_nser_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "6par_nser", &result))
		make_access_error("SAM_Pvsamv1", "6par_nser");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_standoff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "6par_standoff", &result))
		make_access_error("SAM_Pvsamv1", "6par_standoff");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_tnoct_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "6par_tnoct", &result))
		make_access_error("SAM_Pvsamv1", "6par_tnoct");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_transient_thermal_model_unit_mass_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "6par_transient_thermal_model_unit_mass", &result))
		make_access_error("SAM_Pvsamv1", "6par_transient_thermal_model_unit_mass");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_vmp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "6par_vmp", &result))
		make_access_error("SAM_Pvsamv1", "6par_vmp");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_voc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "6par_voc", &result))
		make_access_error("SAM_Pvsamv1", "6par_voc");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_a", &result))
		make_access_error("SAM_Pvsamv1", "snl_a");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_a0", &result))
		make_access_error("SAM_Pvsamv1", "snl_a0");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_a1", &result))
		make_access_error("SAM_Pvsamv1", "snl_a1");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_a2", &result))
		make_access_error("SAM_Pvsamv1", "snl_a2");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_a3", &result))
		make_access_error("SAM_Pvsamv1", "snl_a3");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_a4", &result))
		make_access_error("SAM_Pvsamv1", "snl_a4");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_aimp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_aimp", &result))
		make_access_error("SAM_Pvsamv1", "snl_aimp");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_aisc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_aisc", &result))
		make_access_error("SAM_Pvsamv1", "snl_aisc");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_area", &result))
		make_access_error("SAM_Pvsamv1", "snl_area");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_b", &result))
		make_access_error("SAM_Pvsamv1", "snl_b");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_b0", &result))
		make_access_error("SAM_Pvsamv1", "snl_b0");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_b1", &result))
		make_access_error("SAM_Pvsamv1", "snl_b1");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_b2", &result))
		make_access_error("SAM_Pvsamv1", "snl_b2");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_b3", &result))
		make_access_error("SAM_Pvsamv1", "snl_b3");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_b4", &result))
		make_access_error("SAM_Pvsamv1", "snl_b4");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_b5", &result))
		make_access_error("SAM_Pvsamv1", "snl_b5");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_bvmpo_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_bvmpo", &result))
		make_access_error("SAM_Pvsamv1", "snl_bvmpo");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_bvoco_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_bvoco", &result))
		make_access_error("SAM_Pvsamv1", "snl_bvoco");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_c0", &result))
		make_access_error("SAM_Pvsamv1", "snl_c0");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_c1", &result))
		make_access_error("SAM_Pvsamv1", "snl_c1");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_c2", &result))
		make_access_error("SAM_Pvsamv1", "snl_c2");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_c3", &result))
		make_access_error("SAM_Pvsamv1", "snl_c3");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_c4", &result))
		make_access_error("SAM_Pvsamv1", "snl_c4");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_c5", &result))
		make_access_error("SAM_Pvsamv1", "snl_c5");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c6_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_c6", &result))
		make_access_error("SAM_Pvsamv1", "snl_c6");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c7_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_c7", &result))
		make_access_error("SAM_Pvsamv1", "snl_c7");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_dtc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_dtc", &result))
		make_access_error("SAM_Pvsamv1", "snl_dtc");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_fd_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_fd", &result))
		make_access_error("SAM_Pvsamv1", "snl_fd");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_impo_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_impo", &result))
		make_access_error("SAM_Pvsamv1", "snl_impo");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_isco_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_isco", &result))
		make_access_error("SAM_Pvsamv1", "snl_isco");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_ixo_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_ixo", &result))
		make_access_error("SAM_Pvsamv1", "snl_ixo");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_ixxo_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_ixxo", &result))
		make_access_error("SAM_Pvsamv1", "snl_ixxo");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_mbvmp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_mbvmp", &result))
		make_access_error("SAM_Pvsamv1", "snl_mbvmp");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_mbvoc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_mbvoc", &result))
		make_access_error("SAM_Pvsamv1", "snl_mbvoc");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_module_structure_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_module_structure", &result))
		make_access_error("SAM_Pvsamv1", "snl_module_structure");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_n_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_n", &result))
		make_access_error("SAM_Pvsamv1", "snl_n");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_ref_a_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_ref_a", &result))
		make_access_error("SAM_Pvsamv1", "snl_ref_a");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_ref_b_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_ref_b", &result))
		make_access_error("SAM_Pvsamv1", "snl_ref_b");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_ref_dT_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_ref_dT", &result))
		make_access_error("SAM_Pvsamv1", "snl_ref_dT");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_series_cells_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_series_cells", &result))
		make_access_error("SAM_Pvsamv1", "snl_series_cells");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_transient_thermal_model_unit_mass_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_transient_thermal_model_unit_mass", &result))
		make_access_error("SAM_Pvsamv1", "snl_transient_thermal_model_unit_mass");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_vmpo_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_vmpo", &result))
		make_access_error("SAM_Pvsamv1", "snl_vmpo");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_voco_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "snl_voco", &result))
		make_access_error("SAM_Pvsamv1", "snl_voco");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_AMa0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sd11par_AMa0", &result))
		make_access_error("SAM_Pvsamv1", "sd11par_AMa0");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_AMa1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sd11par_AMa1", &result))
		make_access_error("SAM_Pvsamv1", "sd11par_AMa1");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_AMa2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sd11par_AMa2", &result))
		make_access_error("SAM_Pvsamv1", "sd11par_AMa2");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_AMa3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sd11par_AMa3", &result))
		make_access_error("SAM_Pvsamv1", "sd11par_AMa3");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_AMa4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sd11par_AMa4", &result))
		make_access_error("SAM_Pvsamv1", "sd11par_AMa4");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Egref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sd11par_Egref", &result))
		make_access_error("SAM_Pvsamv1", "sd11par_Egref");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Il_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sd11par_Il", &result))
		make_access_error("SAM_Pvsamv1", "sd11par_Il");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Imp0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sd11par_Imp0", &result))
		make_access_error("SAM_Pvsamv1", "sd11par_Imp0");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Io_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sd11par_Io", &result))
		make_access_error("SAM_Pvsamv1", "sd11par_Io");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Isc0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sd11par_Isc0", &result))
		make_access_error("SAM_Pvsamv1", "sd11par_Isc0");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Vmp0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sd11par_Vmp0", &result))
		make_access_error("SAM_Pvsamv1", "sd11par_Vmp0");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Voc0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sd11par_Voc0", &result))
		make_access_error("SAM_Pvsamv1", "sd11par_Voc0");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_alphaIsc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sd11par_alphaIsc", &result))
		make_access_error("SAM_Pvsamv1", "sd11par_alphaIsc");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sd11par_area", &result))
		make_access_error("SAM_Pvsamv1", "sd11par_area");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_c1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sd11par_c1", &result))
		make_access_error("SAM_Pvsamv1", "sd11par_c1");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_c2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sd11par_c2", &result))
		make_access_error("SAM_Pvsamv1", "sd11par_c2");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_c3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sd11par_c3", &result))
		make_access_error("SAM_Pvsamv1", "sd11par_c3");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_d1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sd11par_d1", &result))
		make_access_error("SAM_Pvsamv1", "sd11par_d1");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_d2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sd11par_d2", &result))
		make_access_error("SAM_Pvsamv1", "sd11par_d2");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_d3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sd11par_d3", &result))
		make_access_error("SAM_Pvsamv1", "sd11par_d3");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_glass_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sd11par_glass", &result))
		make_access_error("SAM_Pvsamv1", "sd11par_glass");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_mounting_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sd11par_mounting", &result))
		make_access_error("SAM_Pvsamv1", "sd11par_mounting");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_n_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sd11par_n", &result))
		make_access_error("SAM_Pvsamv1", "sd11par_n");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_nser_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sd11par_nser", &result))
		make_access_error("SAM_Pvsamv1", "sd11par_nser");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_standoff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sd11par_standoff", &result))
		make_access_error("SAM_Pvsamv1", "sd11par_standoff");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_tnoct_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sd11par_tnoct", &result))
		make_access_error("SAM_Pvsamv1", "sd11par_tnoct");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_AM_c_lp0", &result))
		make_access_error("SAM_Pvsamv1", "mlm_AM_c_lp0");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_AM_c_lp1", &result))
		make_access_error("SAM_Pvsamv1", "mlm_AM_c_lp1");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_AM_c_lp2", &result))
		make_access_error("SAM_Pvsamv1", "mlm_AM_c_lp2");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_AM_c_lp3", &result))
		make_access_error("SAM_Pvsamv1", "mlm_AM_c_lp3");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_AM_c_lp4", &result))
		make_access_error("SAM_Pvsamv1", "mlm_AM_c_lp4");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_AM_c_lp5", &result))
		make_access_error("SAM_Pvsamv1", "mlm_AM_c_lp5");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_sa0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_AM_c_sa0", &result))
		make_access_error("SAM_Pvsamv1", "mlm_AM_c_sa0");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_sa1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_AM_c_sa1", &result))
		make_access_error("SAM_Pvsamv1", "mlm_AM_c_sa1");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_sa2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_AM_c_sa2", &result))
		make_access_error("SAM_Pvsamv1", "mlm_AM_c_sa2");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_sa3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_AM_c_sa3", &result))
		make_access_error("SAM_Pvsamv1", "mlm_AM_c_sa3");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_sa4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_AM_c_sa4", &result))
		make_access_error("SAM_Pvsamv1", "mlm_AM_c_sa4");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_AM_mode", &result))
		make_access_error("SAM_Pvsamv1", "mlm_AM_mode");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_D2MuTau_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_D2MuTau", &result))
		make_access_error("SAM_Pvsamv1", "mlm_D2MuTau");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_E_g_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_E_g", &result))
		make_access_error("SAM_Pvsamv1", "mlm_E_g");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_as_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_IAM_c_as", &result))
		make_access_error("SAM_Pvsamv1", "mlm_IAM_c_as");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_cs_iamValue_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mlm_IAM_c_cs_iamValue", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "mlm_IAM_c_cs_iamValue");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_cs_incAngle_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mlm_IAM_c_cs_incAngle", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "mlm_IAM_c_cs_incAngle");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_IAM_c_sa0", &result))
		make_access_error("SAM_Pvsamv1", "mlm_IAM_c_sa0");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_IAM_c_sa1", &result))
		make_access_error("SAM_Pvsamv1", "mlm_IAM_c_sa1");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_IAM_c_sa2", &result))
		make_access_error("SAM_Pvsamv1", "mlm_IAM_c_sa2");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_IAM_c_sa3", &result))
		make_access_error("SAM_Pvsamv1", "mlm_IAM_c_sa3");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_IAM_c_sa4", &result))
		make_access_error("SAM_Pvsamv1", "mlm_IAM_c_sa4");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_IAM_c_sa5", &result))
		make_access_error("SAM_Pvsamv1", "mlm_IAM_c_sa5");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_IAM_mode", &result))
		make_access_error("SAM_Pvsamv1", "mlm_IAM_mode");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_I_mp_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_I_mp_ref", &result))
		make_access_error("SAM_Pvsamv1", "mlm_I_mp_ref");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_I_sc_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_I_sc_ref", &result))
		make_access_error("SAM_Pvsamv1", "mlm_I_sc_ref");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_Length_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_Length", &result))
		make_access_error("SAM_Pvsamv1", "mlm_Length");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_N_diodes_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_N_diodes", &result))
		make_access_error("SAM_Pvsamv1", "mlm_N_diodes");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_N_parallel_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_N_parallel", &result))
		make_access_error("SAM_Pvsamv1", "mlm_N_parallel");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_N_series_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_N_series", &result))
		make_access_error("SAM_Pvsamv1", "mlm_N_series");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_R_s_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_R_s", &result))
		make_access_error("SAM_Pvsamv1", "mlm_R_s");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_R_sh0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_R_sh0", &result))
		make_access_error("SAM_Pvsamv1", "mlm_R_sh0");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_R_shexp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_R_shexp", &result))
		make_access_error("SAM_Pvsamv1", "mlm_R_shexp");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_R_shref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_R_shref", &result))
		make_access_error("SAM_Pvsamv1", "mlm_R_shref");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_S_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_S_ref", &result))
		make_access_error("SAM_Pvsamv1", "mlm_S_ref");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_fa_U0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_T_c_fa_U0", &result))
		make_access_error("SAM_Pvsamv1", "mlm_T_c_fa_U0");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_fa_U1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_T_c_fa_U1", &result))
		make_access_error("SAM_Pvsamv1", "mlm_T_c_fa_U1");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_fa_alpha_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_T_c_fa_alpha", &result))
		make_access_error("SAM_Pvsamv1", "mlm_T_c_fa_alpha");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_no_mounting_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_T_c_no_mounting", &result))
		make_access_error("SAM_Pvsamv1", "mlm_T_c_no_mounting");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_no_standoff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_T_c_no_standoff", &result))
		make_access_error("SAM_Pvsamv1", "mlm_T_c_no_standoff");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_no_tnoct_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_T_c_no_tnoct", &result))
		make_access_error("SAM_Pvsamv1", "mlm_T_c_no_tnoct");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_T_mode", &result))
		make_access_error("SAM_Pvsamv1", "mlm_T_mode");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_T_ref", &result))
		make_access_error("SAM_Pvsamv1", "mlm_T_ref");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_V_mp_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_V_mp_ref", &result))
		make_access_error("SAM_Pvsamv1", "mlm_V_mp_ref");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_V_oc_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_V_oc_ref", &result))
		make_access_error("SAM_Pvsamv1", "mlm_V_oc_ref");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_Width_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_Width", &result))
		make_access_error("SAM_Pvsamv1", "mlm_Width");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_alpha_isc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_alpha_isc", &result))
		make_access_error("SAM_Pvsamv1", "mlm_alpha_isc");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_beta_voc_spec_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_beta_voc_spec", &result))
		make_access_error("SAM_Pvsamv1", "mlm_beta_voc_spec");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_groundRelfectionFraction_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_groundRelfectionFraction", &result))
		make_access_error("SAM_Pvsamv1", "mlm_groundRelfectionFraction");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_mu_n_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_mu_n", &result))
		make_access_error("SAM_Pvsamv1", "mlm_mu_n");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_n_0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mlm_n_0", &result))
		make_access_error("SAM_Pvsamv1", "mlm_n_0");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Inverter_inv_cec_cg_eff_cec_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_cec_cg_eff_cec", &result))
		make_access_error("SAM_Pvsamv1", "inv_cec_cg_eff_cec");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Inverter_inv_cec_cg_paco_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_cec_cg_paco", &result))
		make_access_error("SAM_Pvsamv1", "inv_cec_cg_paco");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Inverter_inv_ds_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_ds_eff", &result))
		make_access_error("SAM_Pvsamv1", "inv_ds_eff");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Inverter_inv_ds_paco_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_ds_paco", &result))
		make_access_error("SAM_Pvsamv1", "inv_ds_paco");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Inverter_inv_num_mppt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_num_mppt", &result))
		make_access_error("SAM_Pvsamv1", "inv_num_mppt");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Inverter_inv_pd_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_pd_eff", &result))
		make_access_error("SAM_Pvsamv1", "inv_pd_eff");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Inverter_inv_pd_paco_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_pd_paco", &result))
		make_access_error("SAM_Pvsamv1", "inv_pd_paco");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Inverter_inv_snl_eff_cec_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_snl_eff_cec", &result))
		make_access_error("SAM_Pvsamv1", "inv_snl_eff_cec");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Inverter_inv_snl_paco_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_snl_paco", &result))
		make_access_error("SAM_Pvsamv1", "inv_snl_paco");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Inverter_inverter_count_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inverter_count", &result))
		make_access_error("SAM_Pvsamv1", "inverter_count");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Inverter_inverter_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inverter_model", &result))
		make_access_error("SAM_Pvsamv1", "inverter_model");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Inverter_mppt_hi_inverter_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mppt_hi_inverter", &result))
		make_access_error("SAM_Pvsamv1", "mppt_hi_inverter");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Inverter_mppt_low_inverter_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mppt_low_inverter", &result))
		make_access_error("SAM_Pvsamv1", "mppt_low_inverter");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterCECDatabase_inv_snl_c0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_snl_c0", &result))
		make_access_error("SAM_Pvsamv1", "inv_snl_c0");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterCECDatabase_inv_snl_c1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_snl_c1", &result))
		make_access_error("SAM_Pvsamv1", "inv_snl_c1");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterCECDatabase_inv_snl_c2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_snl_c2", &result))
		make_access_error("SAM_Pvsamv1", "inv_snl_c2");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterCECDatabase_inv_snl_c3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_snl_c3", &result))
		make_access_error("SAM_Pvsamv1", "inv_snl_c3");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterCECDatabase_inv_snl_paco_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_snl_paco", &result))
		make_access_error("SAM_Pvsamv1", "inv_snl_paco");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterCECDatabase_inv_snl_pdco_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_snl_pdco", &result))
		make_access_error("SAM_Pvsamv1", "inv_snl_pdco");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterCECDatabase_inv_snl_pnt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_snl_pnt", &result))
		make_access_error("SAM_Pvsamv1", "inv_snl_pnt");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterCECDatabase_inv_snl_pso_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_snl_pso", &result))
		make_access_error("SAM_Pvsamv1", "inv_snl_pso");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterCECDatabase_inv_snl_vdcmax_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_snl_vdcmax", &result))
		make_access_error("SAM_Pvsamv1", "inv_snl_vdcmax");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterCECDatabase_inv_snl_vdco_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_snl_vdco", &result))
		make_access_error("SAM_Pvsamv1", "inv_snl_vdco");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_InverterCECDatabase_inv_tdc_cec_db_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "inv_tdc_cec_db", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "inv_tdc_cec_db");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_c0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_cec_cg_c0", &result))
		make_access_error("SAM_Pvsamv1", "inv_cec_cg_c0");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_c1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_cec_cg_c1", &result))
		make_access_error("SAM_Pvsamv1", "inv_cec_cg_c1");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_c2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_cec_cg_c2", &result))
		make_access_error("SAM_Pvsamv1", "inv_cec_cg_c2");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_c3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_cec_cg_c3", &result))
		make_access_error("SAM_Pvsamv1", "inv_cec_cg_c3");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_paco_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_cec_cg_paco", &result))
		make_access_error("SAM_Pvsamv1", "inv_cec_cg_paco");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_pdco_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_cec_cg_pdco", &result))
		make_access_error("SAM_Pvsamv1", "inv_cec_cg_pdco");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_pnt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_cec_cg_pnt", &result))
		make_access_error("SAM_Pvsamv1", "inv_cec_cg_pnt");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_psco_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_cec_cg_psco", &result))
		make_access_error("SAM_Pvsamv1", "inv_cec_cg_psco");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_vdcmax_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_cec_cg_vdcmax", &result))
		make_access_error("SAM_Pvsamv1", "inv_cec_cg_vdcmax");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_vdco_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_cec_cg_vdco", &result))
		make_access_error("SAM_Pvsamv1", "inv_cec_cg_vdco");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_tdc_cec_cg_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "inv_tdc_cec_cg", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "inv_tdc_cec_cg");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterDatasheet_inv_ds_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_ds_eff", &result))
		make_access_error("SAM_Pvsamv1", "inv_ds_eff");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterDatasheet_inv_ds_paco_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_ds_paco", &result))
		make_access_error("SAM_Pvsamv1", "inv_ds_paco");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterDatasheet_inv_ds_pnt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_ds_pnt", &result))
		make_access_error("SAM_Pvsamv1", "inv_ds_pnt");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterDatasheet_inv_ds_pso_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_ds_pso", &result))
		make_access_error("SAM_Pvsamv1", "inv_ds_pso");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterDatasheet_inv_ds_vdcmax_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_ds_vdcmax", &result))
		make_access_error("SAM_Pvsamv1", "inv_ds_vdcmax");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterDatasheet_inv_ds_vdco_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_ds_vdco", &result))
		make_access_error("SAM_Pvsamv1", "inv_ds_vdco");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_InverterDatasheet_inv_tdc_ds_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "inv_tdc_ds", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "inv_tdc_ds");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_efficiency_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "inv_pd_efficiency", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "inv_pd_efficiency");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_paco_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_pd_paco", &result))
		make_access_error("SAM_Pvsamv1", "inv_pd_paco");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_partload_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "inv_pd_partload", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "inv_pd_partload");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_pdco_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_pd_pdco", &result))
		make_access_error("SAM_Pvsamv1", "inv_pd_pdco");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_pnt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_pd_pnt", &result))
		make_access_error("SAM_Pvsamv1", "inv_pd_pnt");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_vdcmax_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_pd_vdcmax", &result))
		make_access_error("SAM_Pvsamv1", "inv_pd_vdcmax");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_vdco_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_pd_vdco", &result))
		make_access_error("SAM_Pvsamv1", "inv_pd_vdco");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_InverterPartLoadCurve_inv_tdc_plc_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "inv_tdc_plc", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "inv_tdc_plc");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_Aux_Loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ond_Aux_Loss", &result))
		make_access_error("SAM_Pvsamv1", "ond_Aux_Loss");
	});
	return result;
}



SAM_EXPORT const char* SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_CompPMax_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "ond_CompPMax");
	if (!result)
		make_access_error("SAM_Pvsamv1", "ond_CompPMax");
	});
	return result;
}



SAM_EXPORT const char* SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_CompVMax_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "ond_CompVMax");
	if (!result)
		make_access_error("SAM_Pvsamv1", "ond_CompVMax");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_IMaxAC_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ond_IMaxAC", &result))
		make_access_error("SAM_Pvsamv1", "ond_IMaxAC");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_IMaxDC_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ond_IMaxDC", &result))
		make_access_error("SAM_Pvsamv1", "ond_IMaxDC");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_INomAC_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ond_INomAC", &result))
		make_access_error("SAM_Pvsamv1", "ond_INomAC");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_INomDC_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ond_INomDC", &result))
		make_access_error("SAM_Pvsamv1", "ond_INomDC");
	});
	return result;
}



SAM_EXPORT const char* SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_ModeAffEnum_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "ond_ModeAffEnum");
	if (!result)
		make_access_error("SAM_Pvsamv1", "ond_ModeAffEnum");
	});
	return result;
}



SAM_EXPORT const char* SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_ModeOper_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "ond_ModeOper");
	if (!result)
		make_access_error("SAM_Pvsamv1", "ond_ModeOper");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_NbInputs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ond_NbInputs", &result))
		make_access_error("SAM_Pvsamv1", "ond_NbInputs");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_NbMPPT_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ond_NbMPPT", &result))
		make_access_error("SAM_Pvsamv1", "ond_NbMPPT");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_Night_Loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ond_Night_Loss", &result))
		make_access_error("SAM_Pvsamv1", "ond_Night_Loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PLim1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ond_PLim1", &result))
		make_access_error("SAM_Pvsamv1", "ond_PLim1");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PLimAbs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ond_PLimAbs", &result))
		make_access_error("SAM_Pvsamv1", "ond_PLimAbs");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PMaxDC_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ond_PMaxDC", &result))
		make_access_error("SAM_Pvsamv1", "ond_PMaxDC");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PMaxOUT_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ond_PMaxOUT", &result))
		make_access_error("SAM_Pvsamv1", "ond_PMaxOUT");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PNomConv_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ond_PNomConv", &result))
		make_access_error("SAM_Pvsamv1", "ond_PNomConv");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PNomDC_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ond_PNomDC", &result))
		make_access_error("SAM_Pvsamv1", "ond_PNomDC");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PSeuil_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ond_PSeuil", &result))
		make_access_error("SAM_Pvsamv1", "ond_PSeuil");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_TPLim1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ond_TPLim1", &result))
		make_access_error("SAM_Pvsamv1", "ond_TPLim1");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_TPLimAbs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ond_TPLimAbs", &result))
		make_access_error("SAM_Pvsamv1", "ond_TPLimAbs");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_TPMax_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ond_TPMax", &result))
		make_access_error("SAM_Pvsamv1", "ond_TPMax");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_TPNom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ond_TPNom", &result))
		make_access_error("SAM_Pvsamv1", "ond_TPNom");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_VAbsMax_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ond_VAbsMax", &result))
		make_access_error("SAM_Pvsamv1", "ond_VAbsMax");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_VMPPMax_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ond_VMPPMax", &result))
		make_access_error("SAM_Pvsamv1", "ond_VMPPMax");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_VMppMin_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ond_VMppMin", &result))
		make_access_error("SAM_Pvsamv1", "ond_VMppMin");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_VNomEff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ond_VNomEff", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "ond_VNomEff");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_VOutConv_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ond_VOutConv", &result))
		make_access_error("SAM_Pvsamv1", "ond_VOutConv");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_doAllowOverpower_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ond_doAllowOverpower", &result))
		make_access_error("SAM_Pvsamv1", "ond_doAllowOverpower");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_doUseTemperatureLimit_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ond_doUseTemperatureLimit", &result))
		make_access_error("SAM_Pvsamv1", "ond_doUseTemperatureLimit");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_effCurve_Pac_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ond_effCurve_Pac", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "ond_effCurve_Pac");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_effCurve_Pdc_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ond_effCurve_Pdc", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "ond_effCurve_Pdc");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_effCurve_elements_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ond_effCurve_elements", &result))
		make_access_error("SAM_Pvsamv1", "ond_effCurve_elements");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_effCurve_eta_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ond_effCurve_eta", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "ond_effCurve_eta");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_lossRAc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ond_lossRAc", &result))
		make_access_error("SAM_Pvsamv1", "ond_lossRAc");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_lossRDc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ond_lossRDc", &result))
		make_access_error("SAM_Pvsamv1", "ond_lossRDc");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_ac_dc_efficiency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_ac_dc_efficiency", &result))
		make_access_error("SAM_Pvsamv1", "batt_ac_dc_efficiency");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_ac_or_dc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_ac_or_dc", &result))
		make_access_error("SAM_Pvsamv1", "batt_ac_or_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_computed_bank_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_computed_bank_capacity", &result))
		make_access_error("SAM_Pvsamv1", "batt_computed_bank_capacity");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_computed_series_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_computed_series", &result))
		make_access_error("SAM_Pvsamv1", "batt_computed_series");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_computed_strings_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_computed_strings", &result))
		make_access_error("SAM_Pvsamv1", "batt_computed_strings");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_current_charge_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_current_charge_max", &result))
		make_access_error("SAM_Pvsamv1", "batt_current_charge_max");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_current_choice_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_current_choice", &result))
		make_access_error("SAM_Pvsamv1", "batt_current_choice");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_current_discharge_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_current_discharge_max", &result))
		make_access_error("SAM_Pvsamv1", "batt_current_discharge_max");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_dc_ac_efficiency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_dc_ac_efficiency", &result))
		make_access_error("SAM_Pvsamv1", "batt_dc_ac_efficiency");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_dc_dc_efficiency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_dc_dc_efficiency", &result))
		make_access_error("SAM_Pvsamv1", "batt_dc_dc_efficiency");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_inverter_efficiency_cutoff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_inverter_efficiency_cutoff", &result))
		make_access_error("SAM_Pvsamv1", "batt_inverter_efficiency_cutoff");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_loss_choice_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_loss_choice", &result))
		make_access_error("SAM_Pvsamv1", "batt_loss_choice");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_BatterySystem_batt_losses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_losses", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_losses");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_BatterySystem_batt_losses_charging_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_losses_charging", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_losses_charging");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_BatterySystem_batt_losses_discharging_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_losses_discharging", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_losses_discharging");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_BatterySystem_batt_losses_idle_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_losses_idle", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_losses_idle");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_mass_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_mass", &result))
		make_access_error("SAM_Pvsamv1", "batt_mass");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_meter_position_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_meter_position", &result))
		make_access_error("SAM_Pvsamv1", "batt_meter_position");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_power_charge_max_kwac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_power_charge_max_kwac", &result))
		make_access_error("SAM_Pvsamv1", "batt_power_charge_max_kwac");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_power_charge_max_kwdc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_power_charge_max_kwdc", &result))
		make_access_error("SAM_Pvsamv1", "batt_power_charge_max_kwdc");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_power_discharge_max_kwac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_power_discharge_max_kwac", &result))
		make_access_error("SAM_Pvsamv1", "batt_power_discharge_max_kwac");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_power_discharge_max_kwdc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_power_discharge_max_kwdc", &result))
		make_access_error("SAM_Pvsamv1", "batt_power_discharge_max_kwdc");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_replacement_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_replacement_capacity", &result))
		make_access_error("SAM_Pvsamv1", "batt_replacement_capacity");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_replacement_option_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_replacement_option", &result))
		make_access_error("SAM_Pvsamv1", "batt_replacement_option");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_BatterySystem_batt_replacement_schedule_percent_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_replacement_schedule_percent", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_replacement_schedule_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_surface_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_surface_area", &result))
		make_access_error("SAM_Pvsamv1", "batt_surface_area");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatterySystem_en_batt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_batt", &result))
		make_access_error("SAM_Pvsamv1", "en_batt");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_BatterySystem_om_replacement_cost1_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_replacement_cost1", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "om_replacement_cost1");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Load_crit_load_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "crit_load", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "crit_load");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Load_load_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "load", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "load");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Load_load_escalation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "load_escalation", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "load_escalation");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryCell_LeadAcid_q10_computed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LeadAcid_q10_computed", &result))
		make_access_error("SAM_Pvsamv1", "LeadAcid_q10_computed");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryCell_LeadAcid_q20_computed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LeadAcid_q20_computed", &result))
		make_access_error("SAM_Pvsamv1", "LeadAcid_q20_computed");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryCell_LeadAcid_qn_computed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LeadAcid_qn_computed", &result))
		make_access_error("SAM_Pvsamv1", "LeadAcid_qn_computed");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryCell_LeadAcid_tn_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LeadAcid_tn", &result))
		make_access_error("SAM_Pvsamv1", "LeadAcid_tn");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_C_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_C_rate", &result))
		make_access_error("SAM_Pvsamv1", "batt_C_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_Cp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_Cp", &result))
		make_access_error("SAM_Pvsamv1", "batt_Cp");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_Qexp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_Qexp", &result))
		make_access_error("SAM_Pvsamv1", "batt_Qexp");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_Qfull_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_Qfull", &result))
		make_access_error("SAM_Pvsamv1", "batt_Qfull");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_Qfull_flow_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_Qfull_flow", &result))
		make_access_error("SAM_Pvsamv1", "batt_Qfull_flow");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_Qnom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_Qnom", &result))
		make_access_error("SAM_Pvsamv1", "batt_Qnom");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_Vexp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_Vexp", &result))
		make_access_error("SAM_Pvsamv1", "batt_Vexp");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_Vfull_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_Vfull", &result))
		make_access_error("SAM_Pvsamv1", "batt_Vfull");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_Vnom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_Vnom", &result))
		make_access_error("SAM_Pvsamv1", "batt_Vnom");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_Vnom_default_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_Vnom_default", &result))
		make_access_error("SAM_Pvsamv1", "batt_Vnom_default");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_calendar_a_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_calendar_a", &result))
		make_access_error("SAM_Pvsamv1", "batt_calendar_a");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_calendar_b_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_calendar_b", &result))
		make_access_error("SAM_Pvsamv1", "batt_calendar_b");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_calendar_c_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_calendar_c", &result))
		make_access_error("SAM_Pvsamv1", "batt_calendar_c");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_calendar_choice_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_calendar_choice", &result))
		make_access_error("SAM_Pvsamv1", "batt_calendar_choice");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_BatteryCell_batt_calendar_lifetime_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "batt_calendar_lifetime_matrix", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_calendar_lifetime_matrix");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_calendar_q0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_calendar_q0", &result))
		make_access_error("SAM_Pvsamv1", "batt_calendar_q0");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_chem_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_chem", &result))
		make_access_error("SAM_Pvsamv1", "batt_chem");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_h_to_ambient_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_h_to_ambient", &result))
		make_access_error("SAM_Pvsamv1", "batt_h_to_ambient");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_initial_SOC_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_initial_SOC", &result))
		make_access_error("SAM_Pvsamv1", "batt_initial_SOC");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_BatteryCell_batt_lifetime_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "batt_lifetime_matrix", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_lifetime_matrix");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_maximum_SOC_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_maximum_SOC", &result))
		make_access_error("SAM_Pvsamv1", "batt_maximum_SOC");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_minimum_SOC_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_minimum_SOC", &result))
		make_access_error("SAM_Pvsamv1", "batt_minimum_SOC");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_minimum_modetime_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_minimum_modetime", &result))
		make_access_error("SAM_Pvsamv1", "batt_minimum_modetime");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_resistance_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_resistance", &result))
		make_access_error("SAM_Pvsamv1", "batt_resistance");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_BatteryCell_batt_room_temperature_celsius_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_room_temperature_celsius", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_room_temperature_celsius");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_voltage_choice_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_voltage_choice", &result))
		make_access_error("SAM_Pvsamv1", "batt_voltage_choice");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_BatteryCell_batt_voltage_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "batt_voltage_matrix", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_voltage_matrix");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_BatteryCell_cap_vs_temp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "cap_vs_temp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "cap_vs_temp");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryDispatch_batt_auto_gridcharge_max_daily_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_auto_gridcharge_max_daily", &result))
		make_access_error("SAM_Pvsamv1", "batt_auto_gridcharge_max_daily");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_BatteryDispatch_batt_custom_dispatch_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_custom_dispatch", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_custom_dispatch");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_BatteryDispatch_batt_cycle_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_cycle_cost", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_cycle_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryDispatch_batt_cycle_cost_choice_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_cycle_cost_choice", &result))
		make_access_error("SAM_Pvsamv1", "batt_cycle_cost_choice");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryDispatch_batt_dispatch_auto_can_charge_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_dispatch_auto_can_charge", &result))
		make_access_error("SAM_Pvsamv1", "batt_dispatch_auto_can_charge");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryDispatch_batt_dispatch_auto_can_clipcharge_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_dispatch_auto_can_clipcharge", &result))
		make_access_error("SAM_Pvsamv1", "batt_dispatch_auto_can_clipcharge");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryDispatch_batt_dispatch_auto_can_fuelcellcharge_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_dispatch_auto_can_fuelcellcharge", &result))
		make_access_error("SAM_Pvsamv1", "batt_dispatch_auto_can_fuelcellcharge");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryDispatch_batt_dispatch_auto_can_gridcharge_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_dispatch_auto_can_gridcharge", &result))
		make_access_error("SAM_Pvsamv1", "batt_dispatch_auto_can_gridcharge");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryDispatch_batt_dispatch_choice_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_dispatch_choice", &result))
		make_access_error("SAM_Pvsamv1", "batt_dispatch_choice");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryDispatch_batt_dispatch_update_frequency_hours_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_dispatch_update_frequency_hours", &result))
		make_access_error("SAM_Pvsamv1", "batt_dispatch_update_frequency_hours");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryDispatch_batt_look_ahead_hours_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_look_ahead_hours", &result))
		make_access_error("SAM_Pvsamv1", "batt_look_ahead_hours");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_BatteryDispatch_batt_pv_ac_forecast_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_pv_ac_forecast", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_pv_ac_forecast");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_BatteryDispatch_batt_pv_clipping_forecast_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_pv_clipping_forecast", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_pv_clipping_forecast");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_BatteryDispatch_batt_target_choice_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_target_choice", &result))
		make_access_error("SAM_Pvsamv1", "batt_target_choice");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_BatteryDispatch_batt_target_power_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_target_power", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_target_power");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_BatteryDispatch_batt_target_power_monthly_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_target_power_monthly", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_target_power_monthly");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_BatteryDispatch_dispatch_manual_charge_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_manual_charge", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "dispatch_manual_charge");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_BatteryDispatch_dispatch_manual_discharge_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_manual_discharge", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "dispatch_manual_discharge");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_BatteryDispatch_dispatch_manual_fuelcellcharge_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_manual_fuelcellcharge", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "dispatch_manual_fuelcellcharge");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_BatteryDispatch_dispatch_manual_gridcharge_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_manual_gridcharge", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "dispatch_manual_gridcharge");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_BatteryDispatch_dispatch_manual_percent_discharge_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_manual_percent_discharge", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "dispatch_manual_percent_discharge");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_BatteryDispatch_dispatch_manual_percent_gridcharge_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_manual_percent_gridcharge", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "dispatch_manual_percent_gridcharge");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_BatteryDispatch_dispatch_manual_sched_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "dispatch_manual_sched", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "dispatch_manual_sched");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_BatteryDispatch_dispatch_manual_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "dispatch_manual_sched_weekend", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "dispatch_manual_sched_weekend");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_FuelCell_fuelcell_power_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "fuelcell_power", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "fuelcell_power");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_PriceSignal_dispatch_factors_ts_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_factors_ts", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "dispatch_factors_ts");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_PriceSignal_dispatch_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "dispatch_sched_weekday", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "dispatch_sched_weekday");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_PriceSignal_dispatch_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "dispatch_sched_weekend", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "dispatch_sched_weekend");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_PriceSignal_dispatch_tod_factors_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_tod_factors", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "dispatch_tod_factors");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_PriceSignal_forecast_price_signal_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "forecast_price_signal_model", &result))
		make_access_error("SAM_Pvsamv1", "forecast_price_signal_model");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_PriceSignal_mp_ancserv1_revenue_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "mp_ancserv1_revenue", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "mp_ancserv1_revenue");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_PriceSignal_mp_ancserv2_revenue_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "mp_ancserv2_revenue", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "mp_ancserv2_revenue");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_PriceSignal_mp_ancserv3_revenue_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "mp_ancserv3_revenue", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "mp_ancserv3_revenue");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_PriceSignal_mp_ancserv4_revenue_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "mp_ancserv4_revenue", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "mp_ancserv4_revenue");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_PriceSignal_mp_enable_ancserv1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mp_enable_ancserv1", &result))
		make_access_error("SAM_Pvsamv1", "mp_enable_ancserv1");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_PriceSignal_mp_enable_ancserv2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mp_enable_ancserv2", &result))
		make_access_error("SAM_Pvsamv1", "mp_enable_ancserv2");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_PriceSignal_mp_enable_ancserv3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mp_enable_ancserv3", &result))
		make_access_error("SAM_Pvsamv1", "mp_enable_ancserv3");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_PriceSignal_mp_enable_ancserv4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mp_enable_ancserv4", &result))
		make_access_error("SAM_Pvsamv1", "mp_enable_ancserv4");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_PriceSignal_mp_enable_energy_market_revenue_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mp_enable_energy_market_revenue", &result))
		make_access_error("SAM_Pvsamv1", "mp_enable_energy_market_revenue");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_PriceSignal_mp_energy_market_revenue_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "mp_energy_market_revenue", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "mp_energy_market_revenue");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_PriceSignal_ppa_multiplier_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ppa_multiplier_model", &result))
		make_access_error("SAM_Pvsamv1", "ppa_multiplier_model");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_PriceSignal_ppa_price_input_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ppa_price_input", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "ppa_price_input");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_ElectricityRates_rate_escalation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rate_escalation", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "rate_escalation");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_ElectricityRates_ur_annual_min_charge_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_annual_min_charge", &result))
		make_access_error("SAM_Pvsamv1", "ur_annual_min_charge");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_ElectricityRates_ur_dc_enable_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_enable", &result))
		make_access_error("SAM_Pvsamv1", "ur_dc_enable");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_ElectricityRates_ur_dc_flat_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_dc_flat_mat", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "ur_dc_flat_mat");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_ElectricityRates_ur_dc_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_dc_sched_weekday", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "ur_dc_sched_weekday");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_ElectricityRates_ur_dc_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_dc_sched_weekend", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "ur_dc_sched_weekend");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_ElectricityRates_ur_dc_tou_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_dc_tou_mat", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "ur_dc_tou_mat");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_ElectricityRates_ur_ec_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_ec_sched_weekday", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "ur_ec_sched_weekday");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_ElectricityRates_ur_ec_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_ec_sched_weekend", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "ur_ec_sched_weekend");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_ElectricityRates_ur_ec_tou_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_ec_tou_mat", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "ur_ec_tou_mat");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_ElectricityRates_ur_en_ts_buy_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_en_ts_buy_rate", &result))
		make_access_error("SAM_Pvsamv1", "ur_en_ts_buy_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_ElectricityRates_ur_en_ts_sell_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_en_ts_sell_rate", &result))
		make_access_error("SAM_Pvsamv1", "ur_en_ts_sell_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_ElectricityRates_ur_metering_option_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_metering_option", &result))
		make_access_error("SAM_Pvsamv1", "ur_metering_option");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_ElectricityRates_ur_monthly_fixed_charge_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_monthly_fixed_charge", &result))
		make_access_error("SAM_Pvsamv1", "ur_monthly_fixed_charge");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_ElectricityRates_ur_monthly_min_charge_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_monthly_min_charge", &result))
		make_access_error("SAM_Pvsamv1", "ur_monthly_min_charge");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_ElectricityRates_ur_nm_credit_month_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_nm_credit_month", &result))
		make_access_error("SAM_Pvsamv1", "ur_nm_credit_month");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_ElectricityRates_ur_nm_credit_rollover_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_nm_credit_rollover", &result))
		make_access_error("SAM_Pvsamv1", "ur_nm_credit_rollover");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_ElectricityRates_ur_nm_yearend_sell_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_nm_yearend_sell_rate", &result))
		make_access_error("SAM_Pvsamv1", "ur_nm_yearend_sell_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_ElectricityRates_ur_sell_eq_buy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_sell_eq_buy", &result))
		make_access_error("SAM_Pvsamv1", "ur_sell_eq_buy");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_ElectricityRates_ur_ts_buy_rate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ur_ts_buy_rate", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "ur_ts_buy_rate");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_ElectricityRates_ur_ts_sell_rate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ur_ts_sell_rate", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "ur_ts_sell_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_ac_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ac_loss", &result))
		make_access_error("SAM_Pvsamv1", "ac_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_ac_transmission_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ac_transmission_loss", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "ac_transmission_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_ac_wiring_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ac_wiring_loss", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "ac_wiring_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_airmass_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "airmass", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "airmass");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_alb_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "alb", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "alb");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_ac_battery_loss_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_ac_battery_loss_percent", &result))
		make_access_error("SAM_Pvsamv1", "annual_ac_battery_loss_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_ac_gross_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_ac_gross", &result))
		make_access_error("SAM_Pvsamv1", "annual_ac_gross");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_ac_inv_clip_loss_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_ac_inv_clip_loss_percent", &result))
		make_access_error("SAM_Pvsamv1", "annual_ac_inv_clip_loss_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_ac_inv_eff_loss_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_ac_inv_eff_loss_percent", &result))
		make_access_error("SAM_Pvsamv1", "annual_ac_inv_eff_loss_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_ac_inv_pnt_loss_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_ac_inv_pnt_loss_percent", &result))
		make_access_error("SAM_Pvsamv1", "annual_ac_inv_pnt_loss_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_ac_inv_pso_loss_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_ac_inv_pso_loss_percent", &result))
		make_access_error("SAM_Pvsamv1", "annual_ac_inv_pso_loss_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_ac_lifetime_loss_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_ac_lifetime_loss_percent", &result))
		make_access_error("SAM_Pvsamv1", "annual_ac_lifetime_loss_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_ac_loss_ond_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_ac_loss_ond", &result))
		make_access_error("SAM_Pvsamv1", "annual_ac_loss_ond");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_ac_perf_adj_loss_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_ac_perf_adj_loss_percent", &result))
		make_access_error("SAM_Pvsamv1", "annual_ac_perf_adj_loss_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_ac_wiring_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_ac_wiring_loss", &result))
		make_access_error("SAM_Pvsamv1", "annual_ac_wiring_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_ac_wiring_loss_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_ac_wiring_loss_percent", &result))
		make_access_error("SAM_Pvsamv1", "annual_ac_wiring_loss_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_battery_loss_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_dc_battery_loss_percent", &result))
		make_access_error("SAM_Pvsamv1", "annual_dc_battery_loss_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_diodes_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_dc_diodes_loss", &result))
		make_access_error("SAM_Pvsamv1", "annual_dc_diodes_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_diodes_loss_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_dc_diodes_loss_percent", &result))
		make_access_error("SAM_Pvsamv1", "annual_dc_diodes_loss_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_gross_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_dc_gross", &result))
		make_access_error("SAM_Pvsamv1", "annual_dc_gross");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_inv_tdc_loss_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_dc_inv_tdc_loss_percent", &result))
		make_access_error("SAM_Pvsamv1", "annual_dc_inv_tdc_loss_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_invmppt_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_dc_invmppt_loss", &result))
		make_access_error("SAM_Pvsamv1", "annual_dc_invmppt_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_lifetime_loss_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_dc_lifetime_loss_percent", &result))
		make_access_error("SAM_Pvsamv1", "annual_dc_lifetime_loss_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_loss_ond_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_dc_loss_ond", &result))
		make_access_error("SAM_Pvsamv1", "annual_dc_loss_ond");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_mismatch_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_dc_mismatch_loss", &result))
		make_access_error("SAM_Pvsamv1", "annual_dc_mismatch_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_mismatch_loss_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_dc_mismatch_loss_percent", &result))
		make_access_error("SAM_Pvsamv1", "annual_dc_mismatch_loss_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_module_loss_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_dc_module_loss_percent", &result))
		make_access_error("SAM_Pvsamv1", "annual_dc_module_loss_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_mppt_clip_loss_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_dc_mppt_clip_loss_percent", &result))
		make_access_error("SAM_Pvsamv1", "annual_dc_mppt_clip_loss_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_nameplate_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_dc_nameplate_loss", &result))
		make_access_error("SAM_Pvsamv1", "annual_dc_nameplate_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_nameplate_loss_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_dc_nameplate_loss_percent", &result))
		make_access_error("SAM_Pvsamv1", "annual_dc_nameplate_loss_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_net_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_dc_net", &result))
		make_access_error("SAM_Pvsamv1", "annual_dc_net");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_nominal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_dc_nominal", &result))
		make_access_error("SAM_Pvsamv1", "annual_dc_nominal");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_optimizer_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_dc_optimizer_loss", &result))
		make_access_error("SAM_Pvsamv1", "annual_dc_optimizer_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_optimizer_loss_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_dc_optimizer_loss_percent", &result))
		make_access_error("SAM_Pvsamv1", "annual_dc_optimizer_loss_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_perf_adj_loss_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_dc_perf_adj_loss_percent", &result))
		make_access_error("SAM_Pvsamv1", "annual_dc_perf_adj_loss_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_snow_loss_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_dc_snow_loss_percent", &result))
		make_access_error("SAM_Pvsamv1", "annual_dc_snow_loss_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_tracking_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_dc_tracking_loss", &result))
		make_access_error("SAM_Pvsamv1", "annual_dc_tracking_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_tracking_loss_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_dc_tracking_loss_percent", &result))
		make_access_error("SAM_Pvsamv1", "annual_dc_tracking_loss_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_wiring_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_dc_wiring_loss", &result))
		make_access_error("SAM_Pvsamv1", "annual_dc_wiring_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_wiring_loss_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_dc_wiring_loss_percent", &result))
		make_access_error("SAM_Pvsamv1", "annual_dc_wiring_loss_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_Pvsamv1", "annual_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_annual_export_to_grid_energy_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "annual_export_to_grid_energy", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "annual_export_to_grid_energy");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_gh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_gh", &result))
		make_access_error("SAM_Pvsamv1", "annual_gh");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_annual_import_to_grid_energy_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "annual_import_to_grid_energy", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "annual_import_to_grid_energy");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_inv_cliploss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_inv_cliploss", &result))
		make_access_error("SAM_Pvsamv1", "annual_inv_cliploss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_inv_pntloss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_inv_pntloss", &result))
		make_access_error("SAM_Pvsamv1", "annual_inv_pntloss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_inv_psoloss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_inv_psoloss", &result))
		make_access_error("SAM_Pvsamv1", "annual_inv_psoloss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_inv_tdcloss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_inv_tdcloss", &result))
		make_access_error("SAM_Pvsamv1", "annual_inv_tdcloss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_poa_beam_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_poa_beam_eff", &result))
		make_access_error("SAM_Pvsamv1", "annual_poa_beam_eff");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_poa_beam_nom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_poa_beam_nom", &result))
		make_access_error("SAM_Pvsamv1", "annual_poa_beam_nom");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_poa_cover_loss_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_poa_cover_loss_percent", &result))
		make_access_error("SAM_Pvsamv1", "annual_poa_cover_loss_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_poa_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_poa_eff", &result))
		make_access_error("SAM_Pvsamv1", "annual_poa_eff");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_poa_front_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_poa_front", &result))
		make_access_error("SAM_Pvsamv1", "annual_poa_front");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_poa_nom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_poa_nom", &result))
		make_access_error("SAM_Pvsamv1", "annual_poa_nom");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_poa_rear_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_poa_rear", &result))
		make_access_error("SAM_Pvsamv1", "annual_poa_rear");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_poa_rear_gain_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_poa_rear_gain_percent", &result))
		make_access_error("SAM_Pvsamv1", "annual_poa_rear_gain_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_poa_shaded_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_poa_shaded", &result))
		make_access_error("SAM_Pvsamv1", "annual_poa_shaded");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_poa_shaded_soiled_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_poa_shaded_soiled", &result))
		make_access_error("SAM_Pvsamv1", "annual_poa_shaded_soiled");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_poa_shading_loss_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_poa_shading_loss_percent", &result))
		make_access_error("SAM_Pvsamv1", "annual_poa_shading_loss_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_poa_soiling_loss_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_poa_soiling_loss_percent", &result))
		make_access_error("SAM_Pvsamv1", "annual_poa_soiling_loss_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_snow_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_snow_loss", &result))
		make_access_error("SAM_Pvsamv1", "annual_snow_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray1_dc_diodes_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_subarray1_dc_diodes_loss", &result))
		make_access_error("SAM_Pvsamv1", "annual_subarray1_dc_diodes_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray1_dc_gross_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_subarray1_dc_gross", &result))
		make_access_error("SAM_Pvsamv1", "annual_subarray1_dc_gross");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray1_dc_mismatch_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_subarray1_dc_mismatch_loss", &result))
		make_access_error("SAM_Pvsamv1", "annual_subarray1_dc_mismatch_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray1_dc_nameplate_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_subarray1_dc_nameplate_loss", &result))
		make_access_error("SAM_Pvsamv1", "annual_subarray1_dc_nameplate_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray1_dc_tracking_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_subarray1_dc_tracking_loss", &result))
		make_access_error("SAM_Pvsamv1", "annual_subarray1_dc_tracking_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray1_dc_wiring_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_subarray1_dc_wiring_loss", &result))
		make_access_error("SAM_Pvsamv1", "annual_subarray1_dc_wiring_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray2_dc_diodes_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_subarray2_dc_diodes_loss", &result))
		make_access_error("SAM_Pvsamv1", "annual_subarray2_dc_diodes_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray2_dc_gross_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_subarray2_dc_gross", &result))
		make_access_error("SAM_Pvsamv1", "annual_subarray2_dc_gross");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray2_dc_mismatch_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_subarray2_dc_mismatch_loss", &result))
		make_access_error("SAM_Pvsamv1", "annual_subarray2_dc_mismatch_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray2_dc_nameplate_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_subarray2_dc_nameplate_loss", &result))
		make_access_error("SAM_Pvsamv1", "annual_subarray2_dc_nameplate_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray2_dc_tracking_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_subarray2_dc_tracking_loss", &result))
		make_access_error("SAM_Pvsamv1", "annual_subarray2_dc_tracking_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray2_dc_wiring_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_subarray2_dc_wiring_loss", &result))
		make_access_error("SAM_Pvsamv1", "annual_subarray2_dc_wiring_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray3_dc_diodes_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_subarray3_dc_diodes_loss", &result))
		make_access_error("SAM_Pvsamv1", "annual_subarray3_dc_diodes_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray3_dc_gross_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_subarray3_dc_gross", &result))
		make_access_error("SAM_Pvsamv1", "annual_subarray3_dc_gross");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray3_dc_mismatch_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_subarray3_dc_mismatch_loss", &result))
		make_access_error("SAM_Pvsamv1", "annual_subarray3_dc_mismatch_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray3_dc_nameplate_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_subarray3_dc_nameplate_loss", &result))
		make_access_error("SAM_Pvsamv1", "annual_subarray3_dc_nameplate_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray3_dc_tracking_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_subarray3_dc_tracking_loss", &result))
		make_access_error("SAM_Pvsamv1", "annual_subarray3_dc_tracking_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray3_dc_wiring_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_subarray3_dc_wiring_loss", &result))
		make_access_error("SAM_Pvsamv1", "annual_subarray3_dc_wiring_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray4_dc_diodes_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_subarray4_dc_diodes_loss", &result))
		make_access_error("SAM_Pvsamv1", "annual_subarray4_dc_diodes_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray4_dc_gross_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_subarray4_dc_gross", &result))
		make_access_error("SAM_Pvsamv1", "annual_subarray4_dc_gross");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray4_dc_mismatch_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_subarray4_dc_mismatch_loss", &result))
		make_access_error("SAM_Pvsamv1", "annual_subarray4_dc_mismatch_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray4_dc_nameplate_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_subarray4_dc_nameplate_loss", &result))
		make_access_error("SAM_Pvsamv1", "annual_subarray4_dc_nameplate_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray4_dc_tracking_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_subarray4_dc_tracking_loss", &result))
		make_access_error("SAM_Pvsamv1", "annual_subarray4_dc_tracking_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray4_dc_wiring_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_subarray4_dc_wiring_loss", &result))
		make_access_error("SAM_Pvsamv1", "annual_subarray4_dc_wiring_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_total_loss_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_total_loss_percent", &result))
		make_access_error("SAM_Pvsamv1", "annual_total_loss_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_transmission_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_transmission_loss", &result))
		make_access_error("SAM_Pvsamv1", "annual_transmission_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_transmission_loss_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_transmission_loss_percent", &result))
		make_access_error("SAM_Pvsamv1", "annual_transmission_loss_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_xfmr_loss_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_xfmr_loss_percent", &result))
		make_access_error("SAM_Pvsamv1", "annual_xfmr_loss_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_average_battery_conversion_efficiency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "average_battery_conversion_efficiency", &result))
		make_access_error("SAM_Pvsamv1", "average_battery_conversion_efficiency");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_average_battery_roundtrip_efficiency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "average_battery_roundtrip_efficiency", &result))
		make_access_error("SAM_Pvsamv1", "average_battery_roundtrip_efficiency");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_avg_critical_load_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "avg_critical_load", &result))
		make_access_error("SAM_Pvsamv1", "avg_critical_load");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_DOD_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_DOD", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_DOD");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_DOD_cycle_average_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_DOD_cycle_average", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_DOD_cycle_average");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_I_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_I", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_I");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_SOC_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_SOC", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_SOC");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_annual_charge_energy_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_annual_charge_energy", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_annual_charge_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_annual_charge_from_grid_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_annual_charge_from_grid", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_annual_charge_from_grid");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_annual_charge_from_system_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_annual_charge_from_system", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_annual_charge_from_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_annual_discharge_energy_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_annual_discharge_energy", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_annual_discharge_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_annual_energy_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_annual_energy_loss", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_annual_energy_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_annual_energy_system_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_annual_energy_system_loss", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_annual_energy_system_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_batt_bank_installed_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_bank_installed_capacity", &result))
		make_access_error("SAM_Pvsamv1", "batt_bank_installed_capacity");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_bank_replacement_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_bank_replacement", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_bank_replacement");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_capacity_percent_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_capacity_percent", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_capacity_percent");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_capacity_percent_calendar_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_capacity_percent_calendar", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_capacity_percent_calendar");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_capacity_percent_cycle_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_capacity_percent_cycle", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_capacity_percent_cycle");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_capacity_thermal_percent_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_capacity_thermal_percent", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_capacity_thermal_percent");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_conversion_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_conversion_loss", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_conversion_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_cost_to_cycle_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_cost_to_cycle", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_cost_to_cycle");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_cycles_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_cycles", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_cycles");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_dispatch_sched_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "batt_dispatch_sched", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_dispatch_sched");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_power_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_power", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_power");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_power_target_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_power_target", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_power_target");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_q0_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_q0", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_q0");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_q1_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_q1", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_q1");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_q2_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_q2", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_q2");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_qmax_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_qmax", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_qmax");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_qmaxI_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_qmaxI", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_qmaxI");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_qmax_thermal_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_qmax_thermal", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_qmax_thermal");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_revenue_charge_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_revenue_charge", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_revenue_charge");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_revenue_clipcharge_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_revenue_clipcharge", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_revenue_clipcharge");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_revenue_discharge_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_revenue_discharge", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_revenue_discharge");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_revenue_gridcharge_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_revenue_gridcharge", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_revenue_gridcharge");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_batt_system_charge_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_system_charge_percent", &result))
		make_access_error("SAM_Pvsamv1", "batt_system_charge_percent");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_system_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_system_loss", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_system_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_temperature_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_temperature", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_temperature");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_to_grid_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_to_grid", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_to_grid");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_to_load_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_to_load", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_to_load");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_voltage_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_voltage", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_voltage");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_voltage_cell_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_voltage_cell", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "batt_voltage_cell");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_Pvsamv1", "capacity_factor");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_capacity_factor_ac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor_ac", &result))
		make_access_error("SAM_Pvsamv1", "capacity_factor_ac");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_cdf_of_surviving_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cdf_of_surviving", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "cdf_of_surviving");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_dc_degrade_factor_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dc_degrade_factor", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "dc_degrade_factor");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_dc_invmppt_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dc_invmppt_loss", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "dc_invmppt_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_dc_net_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dc_net", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "dc_net");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_dc_snow_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dc_snow_loss", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "dc_snow_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_df_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "df", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "df");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_df_calc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "df_calc", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "df_calc");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_dn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dn", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "dn");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_dn_calc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dn_calc", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "dn_calc");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_fuelcell_to_batt_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "fuelcell_to_batt", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "fuelcell_to_batt");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "gen");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_gen_without_battery_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen_without_battery", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "gen_without_battery");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_gh_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gh", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "gh");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_gh_calc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gh_calc", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "gh_calc");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_grid_power_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "grid_power", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "grid_power");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_grid_power_target_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "grid_power_target", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "grid_power_target");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_grid_to_batt_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "grid_to_batt", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "grid_to_batt");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_grid_to_load_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "grid_to_load", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "grid_to_load");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_inv_cliploss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "inv_cliploss", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "inv_cliploss");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_inv_eff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "inv_eff", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "inv_eff");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_inv_pntloss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "inv_pntloss", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "inv_pntloss");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_inv_psoloss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "inv_psoloss", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "inv_psoloss");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_inv_tdcloss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "inv_tdcloss", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "inv_tdcloss");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_inv_total_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "inv_total_loss", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "inv_total_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_inverterMPPT1_DCVoltage_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "inverterMPPT1_DCVoltage", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "inverterMPPT1_DCVoltage");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_inverterMPPT2_DCVoltage_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "inverterMPPT2_DCVoltage", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "inverterMPPT2_DCVoltage");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_inverterMPPT3_DCVoltage_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "inverterMPPT3_DCVoltage", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "inverterMPPT3_DCVoltage");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_inverterMPPT4_DCVoltage_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "inverterMPPT4_DCVoltage", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "inverterMPPT4_DCVoltage");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "kwh_per_kw", &result))
		make_access_error("SAM_Pvsamv1", "kwh_per_kw");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_market_sell_rate_series_yr1_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "market_sell_rate_series_yr1", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "market_sell_rate_series_yr1");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_batt_to_grid_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_batt_to_grid", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "monthly_batt_to_grid");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_batt_to_load_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_batt_to_load", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "monthly_batt_to_load");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_dc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_dc", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "monthly_dc");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_energy_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_energy", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "monthly_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_grid_to_batt_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_grid_to_batt", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "monthly_grid_to_batt");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_grid_to_load_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_grid_to_load", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "monthly_grid_to_load");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_poa_beam_eff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_poa_beam_eff", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "monthly_poa_beam_eff");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_poa_beam_nom_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_poa_beam_nom", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "monthly_poa_beam_nom");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_poa_eff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_poa_eff", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "monthly_poa_eff");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_poa_front_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_poa_front", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "monthly_poa_front");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_poa_nom_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_poa_nom", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "monthly_poa_nom");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_poa_rear_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_poa_rear", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "monthly_poa_rear");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_snow_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_snow_loss", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "monthly_snow_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_system_to_batt_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_system_to_batt", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "monthly_system_to_batt");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_system_to_grid_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_system_to_grid", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "monthly_system_to_grid");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_system_to_load_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_system_to_load", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "monthly_system_to_load");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_nameplate_dc_rating_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nameplate_dc_rating", &result))
		make_access_error("SAM_Pvsamv1", "nameplate_dc_rating");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_outage_durations_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "outage_durations", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "outage_durations");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_pdf_of_surviving_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pdf_of_surviving", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "pdf_of_surviving");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_performance_ratio_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "performance_ratio", &result))
		make_access_error("SAM_Pvsamv1", "performance_ratio");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_poa_beam_eff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "poa_beam_eff", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "poa_beam_eff");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_poa_beam_nom_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "poa_beam_nom", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "poa_beam_nom");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_poa_eff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "poa_eff", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "poa_eff");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_poa_front_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "poa_front", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "poa_front");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_poa_nom_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "poa_nom", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "poa_nom");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_poa_rear_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "poa_rear", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "poa_rear");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_poa_shaded_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "poa_shaded", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "poa_shaded");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_poa_shaded_soiled_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "poa_shaded_soiled", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "poa_shaded_soiled");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_resilience_hrs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "resilience_hrs", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "resilience_hrs");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_resilience_hrs_avg_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "resilience_hrs_avg", &result))
		make_access_error("SAM_Pvsamv1", "resilience_hrs_avg");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_resilience_hrs_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "resilience_hrs_max", &result))
		make_access_error("SAM_Pvsamv1", "resilience_hrs_max");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_resilience_hrs_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "resilience_hrs_min", &result))
		make_access_error("SAM_Pvsamv1", "resilience_hrs_min");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_shadedb_subarray1_shade_frac_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "shadedb_subarray1_shade_frac", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "shadedb_subarray1_shade_frac");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_shadedb_subarray2_shade_frac_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "shadedb_subarray2_shade_frac", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "shadedb_subarray2_shade_frac");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_shadedb_subarray3_shade_frac_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "shadedb_subarray3_shade_frac", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "shadedb_subarray3_shade_frac");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_shadedb_subarray4_shade_frac_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "shadedb_subarray4_shade_frac", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "shadedb_subarray4_shade_frac");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_sixpar_Adj_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "6par_Adj", &result))
		make_access_error("SAM_Pvsamv1", "6par_Adj");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_sixpar_Il_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "6par_Il", &result))
		make_access_error("SAM_Pvsamv1", "6par_Il");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_sixpar_Io_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "6par_Io", &result))
		make_access_error("SAM_Pvsamv1", "6par_Io");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_sixpar_Rs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "6par_Rs", &result))
		make_access_error("SAM_Pvsamv1", "6par_Rs");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_sixpar_Rsh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "6par_Rsh", &result))
		make_access_error("SAM_Pvsamv1", "6par_Rsh");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_sixpar_a_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "6par_a", &result))
		make_access_error("SAM_Pvsamv1", "6par_a");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_snowdepth_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "snowdepth", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "snowdepth");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_sol_alt_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sol_alt", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "sol_alt");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_sol_azi_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sol_azi", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "sol_azi");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_sol_zen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sol_zen", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "sol_zen");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_aoi_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_aoi", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_aoi");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_aoi_modifier_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_aoi_modifier", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_aoi_modifier");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_axisrot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_axisrot", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_axisrot");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_beam_shading_factor_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_beam_shading_factor", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_beam_shading_factor");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_celltemp_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_celltemp", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_celltemp");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_celltempSS_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_celltempSS", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_celltempSS");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_dc_gross_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_dc_gross", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_dc_gross");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_dc_voltage_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_dc_voltage", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_dc_voltage");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_subarray1_dcloss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray1_dcloss", &result))
		make_access_error("SAM_Pvsamv1", "subarray1_dcloss");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_idealrot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_idealrot", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_idealrot");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_isc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_isc", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_isc");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_linear_derate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_linear_derate", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_linear_derate");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_modeff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_modeff", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_modeff");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_poa_eff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_poa_eff", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_poa_eff");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_poa_eff_beam_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_poa_eff_beam", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_poa_eff_beam");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_poa_eff_diff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_poa_eff_diff", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_poa_eff_diff");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_poa_front_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_poa_front", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_poa_front");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_poa_nom_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_poa_nom", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_poa_nom");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_poa_rear_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_poa_rear", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_poa_rear");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_poa_shaded_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_poa_shaded", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_poa_shaded");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_poa_shaded_soiled_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_poa_shaded_soiled", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_poa_shaded_soiled");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_snow_coverage_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_snow_coverage", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_snow_coverage");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_snow_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_snow_loss", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_snow_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_soiling_derate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_soiling_derate", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_soiling_derate");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_ss_derate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_ss_derate", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_ss_derate");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_ss_diffuse_derate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_ss_diffuse_derate", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_ss_diffuse_derate");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_ss_reflected_derate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_ss_reflected_derate", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_ss_reflected_derate");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_surf_azi_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_surf_azi", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_surf_azi");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_surf_tilt_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_surf_tilt", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_surf_tilt");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_voc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray1_voc", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray1_voc");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_aoi_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_aoi", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_aoi");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_aoi_modifier_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_aoi_modifier", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_aoi_modifier");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_axisrot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_axisrot", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_axisrot");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_beam_shading_factor_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_beam_shading_factor", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_beam_shading_factor");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_celltemp_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_celltemp", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_celltemp");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_celltempSS_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_celltempSS", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_celltempSS");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_dc_gross_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_dc_gross", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_dc_gross");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_dc_voltage_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_dc_voltage", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_dc_voltage");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_subarray2_dcloss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray2_dcloss", &result))
		make_access_error("SAM_Pvsamv1", "subarray2_dcloss");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_idealrot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_idealrot", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_idealrot");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_isc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_isc", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_isc");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_linear_derate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_linear_derate", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_linear_derate");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_modeff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_modeff", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_modeff");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_poa_eff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_poa_eff", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_poa_eff");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_poa_eff_beam_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_poa_eff_beam", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_poa_eff_beam");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_poa_eff_diff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_poa_eff_diff", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_poa_eff_diff");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_poa_front_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_poa_front", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_poa_front");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_poa_nom_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_poa_nom", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_poa_nom");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_poa_rear_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_poa_rear", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_poa_rear");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_poa_shaded_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_poa_shaded", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_poa_shaded");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_poa_shaded_soiled_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_poa_shaded_soiled", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_poa_shaded_soiled");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_snow_coverage_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_snow_coverage", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_snow_coverage");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_snow_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_snow_loss", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_snow_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_soiling_derate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_soiling_derate", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_soiling_derate");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_ss_derate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_ss_derate", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_ss_derate");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_ss_diffuse_derate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_ss_diffuse_derate", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_ss_diffuse_derate");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_ss_reflected_derate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_ss_reflected_derate", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_ss_reflected_derate");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_surf_azi_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_surf_azi", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_surf_azi");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_surf_tilt_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_surf_tilt", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_surf_tilt");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_voc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray2_voc", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray2_voc");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_aoi_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_aoi", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_aoi");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_aoi_modifier_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_aoi_modifier", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_aoi_modifier");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_axisrot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_axisrot", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_axisrot");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_beam_shading_factor_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_beam_shading_factor", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_beam_shading_factor");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_celltemp_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_celltemp", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_celltemp");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_celltempSS_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_celltempSS", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_celltempSS");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_dc_gross_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_dc_gross", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_dc_gross");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_dc_voltage_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_dc_voltage", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_dc_voltage");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_subarray3_dcloss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray3_dcloss", &result))
		make_access_error("SAM_Pvsamv1", "subarray3_dcloss");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_idealrot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_idealrot", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_idealrot");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_isc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_isc", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_isc");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_linear_derate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_linear_derate", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_linear_derate");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_modeff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_modeff", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_modeff");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_poa_eff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_poa_eff", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_poa_eff");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_poa_eff_beam_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_poa_eff_beam", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_poa_eff_beam");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_poa_eff_diff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_poa_eff_diff", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_poa_eff_diff");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_poa_front_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_poa_front", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_poa_front");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_poa_nom_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_poa_nom", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_poa_nom");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_poa_rear_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_poa_rear", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_poa_rear");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_poa_shaded_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_poa_shaded", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_poa_shaded");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_poa_shaded_soiled_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_poa_shaded_soiled", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_poa_shaded_soiled");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_snow_coverage_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_snow_coverage", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_snow_coverage");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_snow_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_snow_loss", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_snow_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_soiling_derate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_soiling_derate", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_soiling_derate");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_ss_derate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_ss_derate", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_ss_derate");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_ss_diffuse_derate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_ss_diffuse_derate", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_ss_diffuse_derate");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_ss_reflected_derate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_ss_reflected_derate", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_ss_reflected_derate");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_surf_azi_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_surf_azi", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_surf_azi");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_surf_tilt_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_surf_tilt", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_surf_tilt");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_voc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray3_voc", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray3_voc");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_aoi_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_aoi", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_aoi");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_aoi_modifier_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_aoi_modifier", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_aoi_modifier");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_axisrot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_axisrot", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_axisrot");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_beam_shading_factor_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_beam_shading_factor", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_beam_shading_factor");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_celltemp_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_celltemp", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_celltemp");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_celltempSS_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_celltempSS", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_celltempSS");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_dc_gross_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_dc_gross", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_dc_gross");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_dc_voltage_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_dc_voltage", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_dc_voltage");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_subarray4_dcloss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subarray4_dcloss", &result))
		make_access_error("SAM_Pvsamv1", "subarray4_dcloss");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_idealrot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_idealrot", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_idealrot");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_isc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_isc", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_isc");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_linear_derate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_linear_derate", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_linear_derate");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_modeff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_modeff", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_modeff");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_poa_eff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_poa_eff", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_poa_eff");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_poa_eff_beam_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_poa_eff_beam", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_poa_eff_beam");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_poa_eff_diff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_poa_eff_diff", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_poa_eff_diff");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_poa_front_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_poa_front", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_poa_front");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_poa_nom_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_poa_nom", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_poa_nom");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_poa_rear_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_poa_rear", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_poa_rear");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_poa_shaded_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_poa_shaded", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_poa_shaded");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_poa_shaded_soiled_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_poa_shaded_soiled", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_poa_shaded_soiled");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_snow_coverage_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_snow_coverage", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_snow_coverage");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_snow_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_snow_loss", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_snow_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_soiling_derate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_soiling_derate", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_soiling_derate");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_ss_derate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_ss_derate", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_ss_derate");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_ss_diffuse_derate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_ss_diffuse_derate", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_ss_diffuse_derate");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_ss_reflected_derate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_ss_reflected_derate", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_ss_reflected_derate");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_surf_azi_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_surf_azi", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_surf_azi");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_surf_tilt_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_surf_tilt", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_surf_tilt");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_voc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subarray4_voc", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "subarray4_voc");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_sunpos_hour_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sunpos_hour", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "sunpos_hour");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_sunup_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sunup", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "sunup");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_survival_function_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "survival_function", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "survival_function");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_system_to_batt_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "system_to_batt", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "system_to_batt");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_system_to_grid_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "system_to_grid", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "system_to_grid");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_system_to_load_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "system_to_load", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "system_to_load");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_tdry_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tdry", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "tdry");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_ts_shift_hours_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ts_shift_hours", &result))
		make_access_error("SAM_Pvsamv1", "ts_shift_hours");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_wfpoa_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wfpoa", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "wfpoa");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_wspd_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wspd", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "wspd");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_xfmr_ll_ts_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "xfmr_ll_ts", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "xfmr_ll_ts");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_xfmr_ll_year1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "xfmr_ll_year1", &result))
		make_access_error("SAM_Pvsamv1", "xfmr_ll_year1");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_xfmr_loss_ts_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "xfmr_loss_ts", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "xfmr_loss_ts");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_xfmr_loss_year1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "xfmr_loss_year1", &result))
		make_access_error("SAM_Pvsamv1", "xfmr_loss_year1");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsamv1_Outputs_xfmr_nll_ts_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "xfmr_nll_ts", length);
	if (!result)
		make_access_error("SAM_Pvsamv1", "xfmr_nll_ts");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsamv1_Outputs_xfmr_nll_year1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "xfmr_nll_year1", &result))
		make_access_error("SAM_Pvsamv1", "xfmr_nll_year1");
	});
	return result;
}




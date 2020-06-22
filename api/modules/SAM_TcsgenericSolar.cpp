#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_TcsgenericSolar.h"

SAM_EXPORT int SAM_TcsgenericSolar_execute(SAM_table data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("tcsgeneric_solar", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_TcsgenericSolar_Weather_azimuth_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "azimuth", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Weather_file_name_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "file_name", str);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Weather_tilt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tilt", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Weather_track_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "track_mode", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_GenericSolar_system_capacity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_capacity", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_TouTranslator_weekday_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "weekday_schedule", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_TouTranslator_weekend_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "weekend_schedule", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_OpticalTable_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "OpticalTable", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_PC_T_corr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "PC_T_corr", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_T_pcdes_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_pcdes", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_T_sfdes_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_sfdes", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_Wpar_prodD_coefs_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Wpar_prodD_coefs", arr, length);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_Wpar_prodQ_coefs_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Wpar_prodQ_coefs", arr, length);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_Wpar_prodT_coefs_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "Wpar_prodT_coefs", arr, length);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_diswos_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "diswos", arr, length);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_disws_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "disws", arr, length);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_etaQ_coefs_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "etaQ_coefs", arr, length);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_etaT_coefs_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "etaT_coefs", arr, length);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_eta_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_des", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_eta_lhv_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_lhv", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_eta_opt_gen_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_opt_gen", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_eta_opt_soil_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_opt_soil", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_exergy_table_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "exergy_table", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_f_Wpar_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_Wpar_fixed", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_f_Wpar_prod_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_Wpar_prod", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_f_charge_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_charge", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_f_disch_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_disch", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_f_etes_0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_etes_0", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_f_sfhl_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_sfhl_ref", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_f_startup_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_startup", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_f_teshl_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_teshl_ref", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_f_wmax_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_wmax", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_f_wmin_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_wmin", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_fdisp_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "fdisp", arr, length);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_hrs_tes_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hrs_tes", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_ibh_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibh", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_ibn_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibn", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_interp_arr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "interp_arr", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_irr_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "irr_des", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_istableunsorted_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "istableunsorted", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_itoth_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "itoth", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_latitude_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "latitude", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_longitude_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "longitude", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_ntod_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ntod", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_qdisp_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "qdisp", arr, length);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_qsf_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "qsf_des", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_rad_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rad_type", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_sfhlQ_coefs_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "sfhlQ_coefs", arr, length);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_sfhlT_coefs_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "sfhlT_coefs", arr, length);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_sfhlV_coefs_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "sfhlV_coefs", arr, length);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_solarm_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "solarm", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_storage_config_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "storage_config", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_tdb_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tdb", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_teshlT_coefs_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "teshlT_coefs", arr, length);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_teshlX_coefs_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "teshlX_coefs", arr, length);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_theta_dep_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "theta_dep", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_theta_stow_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "theta_stow", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_timezone_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "timezone", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_twb_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "twb", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_vwind_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "vwind", number);
	});
}

SAM_EXPORT void SAM_TcsgenericSolar_Type260_w_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "w_des", number);
	});
}

SAM_EXPORT double SAM_TcsgenericSolar_Weather_azimuth_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "azimuth", &result))
		make_access_error("SAM_TcsgenericSolar", "azimuth");
	});
	return result;
}



SAM_EXPORT const char* SAM_TcsgenericSolar_Weather_file_name_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "file_name");
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "file_name");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Weather_tilt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tilt", &result))
		make_access_error("SAM_TcsgenericSolar", "tilt");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Weather_track_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "track_mode", &result))
		make_access_error("SAM_TcsgenericSolar", "track_mode");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_GenericSolar_system_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_TcsgenericSolar", "system_capacity");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_TouTranslator_weekday_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "weekday_schedule", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "weekday_schedule");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_TouTranslator_weekend_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "weekend_schedule", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "weekend_schedule");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Type260_OpticalTable_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "OpticalTable", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "OpticalTable");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_PC_T_corr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "PC_T_corr", &result))
		make_access_error("SAM_TcsgenericSolar", "PC_T_corr");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_T_pcdes_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_pcdes", &result))
		make_access_error("SAM_TcsgenericSolar", "T_pcdes");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_T_sfdes_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_sfdes", &result))
		make_access_error("SAM_TcsgenericSolar", "T_sfdes");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Type260_Wpar_prodD_coefs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Wpar_prodD_coefs", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "Wpar_prodD_coefs");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Type260_Wpar_prodQ_coefs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Wpar_prodQ_coefs", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "Wpar_prodQ_coefs");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Type260_Wpar_prodT_coefs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Wpar_prodT_coefs", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "Wpar_prodT_coefs");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Type260_diswos_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "diswos", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "diswos");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Type260_disws_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disws", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "disws");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Type260_etaQ_coefs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "etaQ_coefs", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "etaQ_coefs");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Type260_etaT_coefs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "etaT_coefs", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "etaT_coefs");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_eta_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_des", &result))
		make_access_error("SAM_TcsgenericSolar", "eta_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_eta_lhv_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_lhv", &result))
		make_access_error("SAM_TcsgenericSolar", "eta_lhv");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_eta_opt_gen_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_opt_gen", &result))
		make_access_error("SAM_TcsgenericSolar", "eta_opt_gen");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_eta_opt_soil_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_opt_soil", &result))
		make_access_error("SAM_TcsgenericSolar", "eta_opt_soil");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Type260_exergy_table_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "exergy_table", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "exergy_table");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_f_Wpar_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_Wpar_fixed", &result))
		make_access_error("SAM_TcsgenericSolar", "f_Wpar_fixed");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_f_Wpar_prod_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_Wpar_prod", &result))
		make_access_error("SAM_TcsgenericSolar", "f_Wpar_prod");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_f_charge_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_charge", &result))
		make_access_error("SAM_TcsgenericSolar", "f_charge");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_f_disch_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_disch", &result))
		make_access_error("SAM_TcsgenericSolar", "f_disch");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_f_etes_0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_etes_0", &result))
		make_access_error("SAM_TcsgenericSolar", "f_etes_0");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_f_sfhl_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_sfhl_ref", &result))
		make_access_error("SAM_TcsgenericSolar", "f_sfhl_ref");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_f_startup_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_startup", &result))
		make_access_error("SAM_TcsgenericSolar", "f_startup");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_f_teshl_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_teshl_ref", &result))
		make_access_error("SAM_TcsgenericSolar", "f_teshl_ref");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_f_wmax_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_wmax", &result))
		make_access_error("SAM_TcsgenericSolar", "f_wmax");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_f_wmin_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_wmin", &result))
		make_access_error("SAM_TcsgenericSolar", "f_wmin");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Type260_fdisp_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "fdisp", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "fdisp");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_hrs_tes_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hrs_tes", &result))
		make_access_error("SAM_TcsgenericSolar", "hrs_tes");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_ibh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibh", &result))
		make_access_error("SAM_TcsgenericSolar", "ibh");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_ibn_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibn", &result))
		make_access_error("SAM_TcsgenericSolar", "ibn");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_interp_arr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "interp_arr", &result))
		make_access_error("SAM_TcsgenericSolar", "interp_arr");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_irr_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "irr_des", &result))
		make_access_error("SAM_TcsgenericSolar", "irr_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_istableunsorted_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "istableunsorted", &result))
		make_access_error("SAM_TcsgenericSolar", "istableunsorted");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_itoth_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itoth", &result))
		make_access_error("SAM_TcsgenericSolar", "itoth");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_latitude_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "latitude", &result))
		make_access_error("SAM_TcsgenericSolar", "latitude");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_longitude_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "longitude", &result))
		make_access_error("SAM_TcsgenericSolar", "longitude");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_ntod_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ntod", &result))
		make_access_error("SAM_TcsgenericSolar", "ntod");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Type260_qdisp_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "qdisp", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "qdisp");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_qsf_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "qsf_des", &result))
		make_access_error("SAM_TcsgenericSolar", "qsf_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_rad_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rad_type", &result))
		make_access_error("SAM_TcsgenericSolar", "rad_type");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Type260_sfhlQ_coefs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sfhlQ_coefs", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "sfhlQ_coefs");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Type260_sfhlT_coefs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sfhlT_coefs", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "sfhlT_coefs");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Type260_sfhlV_coefs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sfhlV_coefs", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "sfhlV_coefs");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_solarm_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "solarm", &result))
		make_access_error("SAM_TcsgenericSolar", "solarm");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_storage_config_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "storage_config", &result))
		make_access_error("SAM_TcsgenericSolar", "storage_config");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_tdb_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tdb", &result))
		make_access_error("SAM_TcsgenericSolar", "tdb");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Type260_teshlT_coefs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "teshlT_coefs", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "teshlT_coefs");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Type260_teshlX_coefs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "teshlX_coefs", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "teshlX_coefs");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_theta_dep_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "theta_dep", &result))
		make_access_error("SAM_TcsgenericSolar", "theta_dep");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_theta_stow_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "theta_stow", &result))
		make_access_error("SAM_TcsgenericSolar", "theta_stow");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_timezone_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "timezone", &result))
		make_access_error("SAM_TcsgenericSolar", "timezone");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_twb_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "twb", &result))
		make_access_error("SAM_TcsgenericSolar", "twb");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_vwind_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "vwind", &result))
		make_access_error("SAM_TcsgenericSolar", "vwind");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Type260_w_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "w_des", &result))
		make_access_error("SAM_TcsgenericSolar", "w_des");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_TcsgenericSolar", "annual_energy");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Outputs_annual_fuel_usage_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_fuel_usage", &result))
		make_access_error("SAM_TcsgenericSolar", "annual_fuel_usage");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Outputs_annual_q_dump_tot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_q_dump_tot", &result))
		make_access_error("SAM_TcsgenericSolar", "annual_q_dump_tot");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Outputs_annual_q_fossil_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_q_fossil", &result))
		make_access_error("SAM_TcsgenericSolar", "annual_q_fossil");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Outputs_annual_q_from_tes_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_q_from_tes", &result))
		make_access_error("SAM_TcsgenericSolar", "annual_q_from_tes");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Outputs_annual_q_hl_sf_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_q_hl_sf", &result))
		make_access_error("SAM_TcsgenericSolar", "annual_q_hl_sf");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Outputs_annual_q_hl_tes_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_q_hl_tes", &result))
		make_access_error("SAM_TcsgenericSolar", "annual_q_hl_tes");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Outputs_annual_q_sf_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_q_sf", &result))
		make_access_error("SAM_TcsgenericSolar", "annual_q_sf");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Outputs_annual_q_startup_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_q_startup", &result))
		make_access_error("SAM_TcsgenericSolar", "annual_q_startup");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Outputs_annual_q_to_pb_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_q_to_pb", &result))
		make_access_error("SAM_TcsgenericSolar", "annual_q_to_pb");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Outputs_annual_q_to_tes_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_q_to_tes", &result))
		make_access_error("SAM_TcsgenericSolar", "annual_q_to_tes");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Outputs_annual_w_gr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_w_gr", &result))
		make_access_error("SAM_TcsgenericSolar", "annual_w_gr");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_beam_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "beam", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "beam");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_TcsgenericSolar", "capacity_factor");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Outputs_conversion_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "conversion_factor", &result))
		make_access_error("SAM_TcsgenericSolar", "conversion_factor");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_diff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "diff", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "diff");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_e_in_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "e_in_tes", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "e_in_tes");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_enet_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "enet", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "enet");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_eta_cycle_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_cycle", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "eta_cycle");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_eta_opt_sf_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_opt_sf", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "eta_opt_sf");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_f_effpc_qtpb_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "f_effpc_qtpb", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "f_effpc_qtpb");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_f_effpc_tamb_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "f_effpc_tamb", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "f_effpc_tamb");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_f_sfhl_qdni_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "f_sfhl_qdni", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "f_sfhl_qdni");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_f_sfhl_tamb_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "f_sfhl_tamb", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "f_sfhl_tamb");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_f_sfhl_vwind_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "f_sfhl_vwind", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "f_sfhl_vwind");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "gen");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_global_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "global", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "global");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_hour_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hour", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "hour");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "kwh_per_kw", &result))
		make_access_error("SAM_TcsgenericSolar", "kwh_per_kw");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_month_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "month", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "month");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_monthly_energy_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_energy", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "monthly_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_monthly_q_dump_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_q_dump_tot", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "monthly_q_dump_tot");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_monthly_q_fossil_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_q_fossil", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "monthly_q_fossil");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_monthly_q_from_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_q_from_tes", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "monthly_q_from_tes");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_monthly_q_hl_sf_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_q_hl_sf", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "monthly_q_hl_sf");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_monthly_q_hl_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_q_hl_tes", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "monthly_q_hl_tes");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_monthly_q_sf_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_q_sf", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "monthly_q_sf");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_monthly_q_startup_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_q_startup", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "monthly_q_startup");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_monthly_q_to_pb_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_q_to_pb", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "monthly_q_to_pb");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_monthly_q_to_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_q_to_tes", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "monthly_q_to_tes");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_monthly_w_gr_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_w_gr", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "monthly_w_gr");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_pres_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pres", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "pres");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_q_dump_teschg_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dump_teschg", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "q_dump_teschg");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_q_dump_tesfull_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dump_tesfull", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "q_dump_tesfull");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_q_dump_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dump_tot", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "q_dump_tot");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_q_dump_umin_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dump_umin", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "q_dump_umin");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_q_fossil_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_fossil", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "q_fossil");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_q_from_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_from_tes", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "q_from_tes");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_q_gas_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_gas", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "q_gas");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_q_hl_sf_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_hl_sf", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "q_hl_sf");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_q_hl_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_hl_tes", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "q_hl_tes");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_q_inc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_inc", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "q_inc");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_q_sf_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_sf", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "q_sf");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_q_startup_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_startup", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "q_startup");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_q_to_pb_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_to_pb", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "q_to_pb");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_q_to_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_to_tes", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "q_to_tes");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_solazi_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solazi", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "solazi");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_solzen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solzen", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "solzen");
	});
	return result;
}



SAM_EXPORT double SAM_TcsgenericSolar_Outputs_system_heat_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_heat_rate", &result))
		make_access_error("SAM_TcsgenericSolar", "system_heat_rate");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_tdry_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tdry", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "tdry");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_twet_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "twet", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "twet");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_w_gr_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "w_gr", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "w_gr");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_w_gr_fossil_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "w_gr_fossil", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "w_gr_fossil");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_w_gr_solar_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "w_gr_solar", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "w_gr_solar");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_w_par_fixed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "w_par_fixed", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "w_par_fixed");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_w_par_offline_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "w_par_offline", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "w_par_offline");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_w_par_online_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "w_par_online", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "w_par_online");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_w_par_prod_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "w_par_prod", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "w_par_prod");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_w_par_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "w_par_tot", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "w_par_tot");
	});
	return result;
}



SAM_EXPORT double* SAM_TcsgenericSolar_Outputs_wspd_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wspd", length);
	if (!result)
		make_access_error("SAM_TcsgenericSolar", "wspd");
	});
	return result;
}




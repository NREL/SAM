#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_UiUdpcChecks.h"

SAM_EXPORT int SAM_UiUdpcChecks_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("ui_udpc_checks", data, verbosity, err);
}

SAM_EXPORT void SAM_UiUdpcChecks_UserDefinedPowerCycle_ud_ind_od_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ud_ind_od", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_UiUdpcChecks_Common_T_htf_des_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_htf_des_in", number);
	});
}

SAM_EXPORT double* SAM_UiUdpcChecks_UserDefinedPowerCycle_ud_ind_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ud_ind_od", nrows, ncols);
	if (!result)
		make_access_error("SAM_UiUdpcChecks", "ud_ind_od");
	});
	return result;
}



SAM_EXPORT double SAM_UiUdpcChecks_Common_T_htf_des_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_des_in", &result))
		make_access_error("SAM_UiUdpcChecks", "T_htf_des_in");
	});
	return result;
}



SAM_EXPORT double SAM_UiUdpcChecks_Outputs_Q_dot_HTF_ND_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Q_dot_HTF_ND_des", &result))
		make_access_error("SAM_UiUdpcChecks", "Q_dot_HTF_ND_des");
	});
	return result;
}



SAM_EXPORT double SAM_UiUdpcChecks_Outputs_T_amb_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_des", &result))
		make_access_error("SAM_UiUdpcChecks", "T_amb_des");
	});
	return result;
}



SAM_EXPORT double SAM_UiUdpcChecks_Outputs_T_amb_high_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_high", &result))
		make_access_error("SAM_UiUdpcChecks", "T_amb_high");
	});
	return result;
}



SAM_EXPORT double SAM_UiUdpcChecks_Outputs_T_amb_low_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_low", &result))
		make_access_error("SAM_UiUdpcChecks", "T_amb_low");
	});
	return result;
}



SAM_EXPORT double SAM_UiUdpcChecks_Outputs_T_htf_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_des", &result))
		make_access_error("SAM_UiUdpcChecks", "T_htf_des");
	});
	return result;
}



SAM_EXPORT double SAM_UiUdpcChecks_Outputs_T_htf_high_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_high", &result))
		make_access_error("SAM_UiUdpcChecks", "T_htf_high");
	});
	return result;
}



SAM_EXPORT double SAM_UiUdpcChecks_Outputs_T_htf_low_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_low", &result))
		make_access_error("SAM_UiUdpcChecks", "T_htf_low");
	});
	return result;
}



SAM_EXPORT double SAM_UiUdpcChecks_Outputs_W_dot_cooling_ND_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_cooling_ND_des", &result))
		make_access_error("SAM_UiUdpcChecks", "W_dot_cooling_ND_des");
	});
	return result;
}



SAM_EXPORT double SAM_UiUdpcChecks_Outputs_W_dot_gross_ND_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_gross_ND_des", &result))
		make_access_error("SAM_UiUdpcChecks", "W_dot_gross_ND_des");
	});
	return result;
}



SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_des", &result))
		make_access_error("SAM_UiUdpcChecks", "m_dot_des");
	});
	return result;
}



SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_high_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_high", &result))
		make_access_error("SAM_UiUdpcChecks", "m_dot_high");
	});
	return result;
}



SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_low_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_low", &result))
		make_access_error("SAM_UiUdpcChecks", "m_dot_low");
	});
	return result;
}



SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_water_ND_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_water_ND_des", &result))
		make_access_error("SAM_UiUdpcChecks", "m_dot_water_ND_des");
	});
	return result;
}



SAM_EXPORT double SAM_UiUdpcChecks_Outputs_n_T_amb_pars_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_T_amb_pars", &result))
		make_access_error("SAM_UiUdpcChecks", "n_T_amb_pars");
	});
	return result;
}



SAM_EXPORT double SAM_UiUdpcChecks_Outputs_n_T_htf_pars_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_T_htf_pars", &result))
		make_access_error("SAM_UiUdpcChecks", "n_T_htf_pars");
	});
	return result;
}



SAM_EXPORT double SAM_UiUdpcChecks_Outputs_n_m_dot_pars_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_m_dot_pars", &result))
		make_access_error("SAM_UiUdpcChecks", "n_m_dot_pars");
	});
	return result;
}




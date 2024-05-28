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

SAM_EXPORT void SAM_UiUdpcChecks_Common_T_htf_cold_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_htf_cold_des", number);
	});
}

SAM_EXPORT void SAM_UiUdpcChecks_Common_T_htf_des_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_htf_des_in", number);
	});
}

SAM_EXPORT void SAM_UiUdpcChecks_Common_cooler_tot_W_dot_fan_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cooler_tot_W_dot_fan", number);
	});
}

SAM_EXPORT void SAM_UiUdpcChecks_Common_is_calc_m_dot_vs_T_amb_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_calc_m_dot_vs_T_amb", number);
	});
}

SAM_EXPORT void SAM_UiUdpcChecks_SystemDesign_W_dot_net_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "W_dot_net_des", number);
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

SAM_EXPORT double SAM_UiUdpcChecks_Common_T_htf_cold_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_cold_des", &result))
		make_access_error("SAM_UiUdpcChecks", "T_htf_cold_des");
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

SAM_EXPORT double SAM_UiUdpcChecks_Common_cooler_tot_W_dot_fan_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cooler_tot_W_dot_fan", &result))
		make_access_error("SAM_UiUdpcChecks", "cooler_tot_W_dot_fan");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Common_is_calc_m_dot_vs_T_amb_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_calc_m_dot_vs_T_amb", &result))
		make_access_error("SAM_UiUdpcChecks", "is_calc_m_dot_vs_T_amb");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_SystemDesign_W_dot_net_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_net_des", &result))
		make_access_error("SAM_UiUdpcChecks", "W_dot_net_des");
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

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_T_amb_HT_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_HT", &result))
		make_access_error("SAM_UiUdpcChecks", "T_amb_HT");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_T_amb_LT_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_LT", &result))
		make_access_error("SAM_UiUdpcChecks", "T_amb_LT");
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

SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_T_amb_pars_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_amb_pars", length);
	if (!result)
		make_access_error("SAM_UiUdpcChecks", "T_amb_pars");
	});
	return result;
}

SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_T_amb_sweep_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_amb_sweep", length);
	if (!result)
		make_access_error("SAM_UiUdpcChecks", "T_amb_sweep");
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

SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_T_htf_pars_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_htf_pars", length);
	if (!result)
		make_access_error("SAM_UiUdpcChecks", "T_htf_pars");
	});
	return result;
}

SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_W_dot_ND_regr_vs_T_amb__T_HTF_low_level_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_ND_regr_vs_T_amb__T_HTF_low_level", length);
	if (!result)
		make_access_error("SAM_UiUdpcChecks", "W_dot_ND_regr_vs_T_amb__T_HTF_low_level");
	});
	return result;
}

SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_W_dot_ND_regr_vs_m_dot__T_amb_HT_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_ND_regr_vs_m_dot__T_amb_HT", length);
	if (!result)
		make_access_error("SAM_UiUdpcChecks", "W_dot_ND_regr_vs_m_dot__T_amb_HT");
	});
	return result;
}

SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_W_dot_ND_regr_vs_m_dot__T_amb_LT_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_ND_regr_vs_m_dot__T_amb_LT", length);
	if (!result)
		make_access_error("SAM_UiUdpcChecks", "W_dot_ND_regr_vs_m_dot__T_amb_LT");
	});
	return result;
}

SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_W_dot_ND_regr_vs_m_dot__T_amb_design_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_ND_regr_vs_m_dot__T_amb_design", length);
	if (!result)
		make_access_error("SAM_UiUdpcChecks", "W_dot_ND_regr_vs_m_dot__T_amb_design");
	});
	return result;
}

SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_W_dot_ND_regr_vs_m_dot__T_amb_high_level_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_ND_regr_vs_m_dot__T_amb_high_level", length);
	if (!result)
		make_access_error("SAM_UiUdpcChecks", "W_dot_ND_regr_vs_m_dot__T_amb_high_level");
	});
	return result;
}

SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_W_dot_ND_regr_vs_m_dot__T_amb_low_level_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_ND_regr_vs_m_dot__T_amb_low_level", length);
	if (!result)
		make_access_error("SAM_UiUdpcChecks", "W_dot_ND_regr_vs_m_dot__T_amb_low_level");
	});
	return result;
}

SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_W_dot_ND_vs_m_dot__T_amb_HT_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_ND_vs_m_dot__T_amb_HT", length);
	if (!result)
		make_access_error("SAM_UiUdpcChecks", "W_dot_ND_vs_m_dot__T_amb_HT");
	});
	return result;
}

SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_W_dot_ND_vs_m_dot__T_amb_LT_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_ND_vs_m_dot__T_amb_LT", length);
	if (!result)
		make_access_error("SAM_UiUdpcChecks", "W_dot_ND_vs_m_dot__T_amb_LT");
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

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_W_dot_htf_ND_max_at_T_amb_HT_regr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_htf_ND_max_at_T_amb_HT_regr", &result))
		make_access_error("SAM_UiUdpcChecks", "W_dot_htf_ND_max_at_T_amb_HT_regr");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_W_dot_htf_ND_max_at_T_amb_HT_rule0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_htf_ND_max_at_T_amb_HT_rule0", &result))
		make_access_error("SAM_UiUdpcChecks", "W_dot_htf_ND_max_at_T_amb_HT_rule0");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_W_dot_htf_ND_max_at_T_amb_LT_regr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_htf_ND_max_at_T_amb_LT_regr", &result))
		make_access_error("SAM_UiUdpcChecks", "W_dot_htf_ND_max_at_T_amb_LT_regr");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_W_dot_htf_ND_max_at_T_amb_LT_rule0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_htf_ND_max_at_T_amb_LT_rule0", &result))
		make_access_error("SAM_UiUdpcChecks", "W_dot_htf_ND_max_at_T_amb_LT_rule0");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_W_dot_htf_ND_max_at_T_amb_design_regr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_htf_ND_max_at_T_amb_design_regr", &result))
		make_access_error("SAM_UiUdpcChecks", "W_dot_htf_ND_max_at_T_amb_design_regr");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_W_dot_htf_ND_max_at_T_amb_design_rule0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_htf_ND_max_at_T_amb_design_rule0", &result))
		make_access_error("SAM_UiUdpcChecks", "W_dot_htf_ND_max_at_T_amb_design_rule0");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_W_dot_htf_ND_max_at_T_amb_high_level_regr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_htf_ND_max_at_T_amb_high_level_regr", &result))
		make_access_error("SAM_UiUdpcChecks", "W_dot_htf_ND_max_at_T_amb_high_level_regr");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_W_dot_htf_ND_max_at_T_amb_high_level_rule0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_htf_ND_max_at_T_amb_high_level_rule0", &result))
		make_access_error("SAM_UiUdpcChecks", "W_dot_htf_ND_max_at_T_amb_high_level_rule0");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_W_dot_htf_ND_max_at_T_amb_low_level_regr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_htf_ND_max_at_T_amb_low_level_regr", &result))
		make_access_error("SAM_UiUdpcChecks", "W_dot_htf_ND_max_at_T_amb_low_level_regr");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_W_dot_htf_ND_max_at_T_amb_low_level_rule0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_htf_ND_max_at_T_amb_low_level_rule0", &result))
		make_access_error("SAM_UiUdpcChecks", "W_dot_htf_ND_max_at_T_amb_low_level_rule0");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_eta_ND_max_at_T_amb_HT_regr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_ND_max_at_T_amb_HT_regr", &result))
		make_access_error("SAM_UiUdpcChecks", "eta_ND_max_at_T_amb_HT_regr");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_eta_ND_max_at_T_amb_HT_rule0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_ND_max_at_T_amb_HT_rule0", &result))
		make_access_error("SAM_UiUdpcChecks", "eta_ND_max_at_T_amb_HT_rule0");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_eta_ND_max_at_T_amb_LT_regr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_ND_max_at_T_amb_LT_regr", &result))
		make_access_error("SAM_UiUdpcChecks", "eta_ND_max_at_T_amb_LT_regr");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_eta_ND_max_at_T_amb_LT_rule0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_ND_max_at_T_amb_LT_rule0", &result))
		make_access_error("SAM_UiUdpcChecks", "eta_ND_max_at_T_amb_LT_rule0");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_eta_ND_max_at_T_amb_design_regr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_ND_max_at_T_amb_design_regr", &result))
		make_access_error("SAM_UiUdpcChecks", "eta_ND_max_at_T_amb_design_regr");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_eta_ND_max_at_T_amb_design_rule0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_ND_max_at_T_amb_design_rule0", &result))
		make_access_error("SAM_UiUdpcChecks", "eta_ND_max_at_T_amb_design_rule0");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_eta_ND_max_at_T_amb_high_level_regr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_ND_max_at_T_amb_high_level_regr", &result))
		make_access_error("SAM_UiUdpcChecks", "eta_ND_max_at_T_amb_high_level_regr");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_eta_ND_max_at_T_amb_high_level_rule0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_ND_max_at_T_amb_high_level_rule0", &result))
		make_access_error("SAM_UiUdpcChecks", "eta_ND_max_at_T_amb_high_level_rule0");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_eta_ND_max_at_T_amb_low_level_regr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_ND_max_at_T_amb_low_level_regr", &result))
		make_access_error("SAM_UiUdpcChecks", "eta_ND_max_at_T_amb_low_level_regr");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_eta_ND_max_at_T_amb_low_level_rule0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_ND_max_at_T_amb_low_level_rule0", &result))
		make_access_error("SAM_UiUdpcChecks", "eta_ND_max_at_T_amb_low_level_rule0");
	});
	return result;
}

SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_eta_ND_regr_vs_T_amb__T_HTF_low_level_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_ND_regr_vs_T_amb__T_HTF_low_level", length);
	if (!result)
		make_access_error("SAM_UiUdpcChecks", "eta_ND_regr_vs_T_amb__T_HTF_low_level");
	});
	return result;
}

SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_eta_ND_regr_vs_m_dot__T_amb_HT_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_ND_regr_vs_m_dot__T_amb_HT", length);
	if (!result)
		make_access_error("SAM_UiUdpcChecks", "eta_ND_regr_vs_m_dot__T_amb_HT");
	});
	return result;
}

SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_eta_ND_regr_vs_m_dot__T_amb_LT_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_ND_regr_vs_m_dot__T_amb_LT", length);
	if (!result)
		make_access_error("SAM_UiUdpcChecks", "eta_ND_regr_vs_m_dot__T_amb_LT");
	});
	return result;
}

SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_eta_ND_regr_vs_m_dot__T_amb_design_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_ND_regr_vs_m_dot__T_amb_design", length);
	if (!result)
		make_access_error("SAM_UiUdpcChecks", "eta_ND_regr_vs_m_dot__T_amb_design");
	});
	return result;
}

SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_eta_ND_regr_vs_m_dot__T_amb_high_level_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_ND_regr_vs_m_dot__T_amb_high_level", length);
	if (!result)
		make_access_error("SAM_UiUdpcChecks", "eta_ND_regr_vs_m_dot__T_amb_high_level");
	});
	return result;
}

SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_eta_ND_regr_vs_m_dot__T_amb_low_level_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_ND_regr_vs_m_dot__T_amb_low_level", length);
	if (!result)
		make_access_error("SAM_UiUdpcChecks", "eta_ND_regr_vs_m_dot__T_amb_low_level");
	});
	return result;
}

SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_eta_ND_vs_m_dot__T_amb_HT_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_ND_vs_m_dot__T_amb_HT", length);
	if (!result)
		make_access_error("SAM_UiUdpcChecks", "eta_ND_vs_m_dot__T_amb_HT");
	});
	return result;
}

SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_eta_ND_vs_m_dot__T_amb_LT_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_ND_vs_m_dot__T_amb_LT", length);
	if (!result)
		make_access_error("SAM_UiUdpcChecks", "eta_ND_vs_m_dot__T_amb_LT");
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

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_htf_ND_max_at_T_amb_HT_regr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htf_ND_max_at_T_amb_HT_regr", &result))
		make_access_error("SAM_UiUdpcChecks", "m_dot_htf_ND_max_at_T_amb_HT_regr");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_htf_ND_max_at_T_amb_HT_rule0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htf_ND_max_at_T_amb_HT_rule0", &result))
		make_access_error("SAM_UiUdpcChecks", "m_dot_htf_ND_max_at_T_amb_HT_rule0");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_htf_ND_max_at_T_amb_LT_regr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htf_ND_max_at_T_amb_LT_regr", &result))
		make_access_error("SAM_UiUdpcChecks", "m_dot_htf_ND_max_at_T_amb_LT_regr");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_htf_ND_max_at_T_amb_LT_rule0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htf_ND_max_at_T_amb_LT_rule0", &result))
		make_access_error("SAM_UiUdpcChecks", "m_dot_htf_ND_max_at_T_amb_LT_rule0");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_htf_ND_max_at_T_amb_design_regr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htf_ND_max_at_T_amb_design_regr", &result))
		make_access_error("SAM_UiUdpcChecks", "m_dot_htf_ND_max_at_T_amb_design_regr");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_htf_ND_max_at_T_amb_design_rule0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htf_ND_max_at_T_amb_design_rule0", &result))
		make_access_error("SAM_UiUdpcChecks", "m_dot_htf_ND_max_at_T_amb_design_rule0");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_htf_ND_max_at_T_amb_high_level_regr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htf_ND_max_at_T_amb_high_level_regr", &result))
		make_access_error("SAM_UiUdpcChecks", "m_dot_htf_ND_max_at_T_amb_high_level_regr");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_htf_ND_max_at_T_amb_high_level_rule0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htf_ND_max_at_T_amb_high_level_rule0", &result))
		make_access_error("SAM_UiUdpcChecks", "m_dot_htf_ND_max_at_T_amb_high_level_rule0");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_htf_ND_max_at_T_amb_low_level_regr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htf_ND_max_at_T_amb_low_level_regr", &result))
		make_access_error("SAM_UiUdpcChecks", "m_dot_htf_ND_max_at_T_amb_low_level_regr");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_htf_ND_max_at_T_amb_low_level_rule0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htf_ND_max_at_T_amb_low_level_rule0", &result))
		make_access_error("SAM_UiUdpcChecks", "m_dot_htf_ND_max_at_T_amb_low_level_rule0");
	});
	return result;
}

SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_m_dot_htf_ND_max_vs_T_amb_rule0_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_htf_ND_max_vs_T_amb_rule0", length);
	if (!result)
		make_access_error("SAM_UiUdpcChecks", "m_dot_htf_ND_max_vs_T_amb_rule0");
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

SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_m_dot_pars_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_pars", length);
	if (!result)
		make_access_error("SAM_UiUdpcChecks", "m_dot_pars");
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

SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_q_dot_ND_regr_vs_T_amb__T_HTF_low_level_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_ND_regr_vs_T_amb__T_HTF_low_level", length);
	if (!result)
		make_access_error("SAM_UiUdpcChecks", "q_dot_ND_regr_vs_T_amb__T_HTF_low_level");
	});
	return result;
}

SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_q_dot_ND_regr_vs_m_dot__T_amb_HT_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_ND_regr_vs_m_dot__T_amb_HT", length);
	if (!result)
		make_access_error("SAM_UiUdpcChecks", "q_dot_ND_regr_vs_m_dot__T_amb_HT");
	});
	return result;
}

SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_q_dot_ND_regr_vs_m_dot__T_amb_LT_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_ND_regr_vs_m_dot__T_amb_LT", length);
	if (!result)
		make_access_error("SAM_UiUdpcChecks", "q_dot_ND_regr_vs_m_dot__T_amb_LT");
	});
	return result;
}

SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_q_dot_ND_regr_vs_m_dot__T_amb_design_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_ND_regr_vs_m_dot__T_amb_design", length);
	if (!result)
		make_access_error("SAM_UiUdpcChecks", "q_dot_ND_regr_vs_m_dot__T_amb_design");
	});
	return result;
}

SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_q_dot_ND_regr_vs_m_dot__T_amb_high_level_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_ND_regr_vs_m_dot__T_amb_high_level", length);
	if (!result)
		make_access_error("SAM_UiUdpcChecks", "q_dot_ND_regr_vs_m_dot__T_amb_high_level");
	});
	return result;
}

SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_q_dot_ND_regr_vs_m_dot__T_amb_low_level_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_ND_regr_vs_m_dot__T_amb_low_level", length);
	if (!result)
		make_access_error("SAM_UiUdpcChecks", "q_dot_ND_regr_vs_m_dot__T_amb_low_level");
	});
	return result;
}

SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_q_dot_ND_vs_m_dot__T_amb_HT_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_ND_vs_m_dot__T_amb_HT", length);
	if (!result)
		make_access_error("SAM_UiUdpcChecks", "q_dot_ND_vs_m_dot__T_amb_HT");
	});
	return result;
}

SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_q_dot_ND_vs_m_dot__T_amb_LT_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_ND_vs_m_dot__T_amb_LT", length);
	if (!result)
		make_access_error("SAM_UiUdpcChecks", "q_dot_ND_vs_m_dot__T_amb_LT");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_q_dot_htf_ND_max_at_T_amb_HT_regr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_htf_ND_max_at_T_amb_HT_regr", &result))
		make_access_error("SAM_UiUdpcChecks", "q_dot_htf_ND_max_at_T_amb_HT_regr");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_q_dot_htf_ND_max_at_T_amb_HT_rule0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_htf_ND_max_at_T_amb_HT_rule0", &result))
		make_access_error("SAM_UiUdpcChecks", "q_dot_htf_ND_max_at_T_amb_HT_rule0");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_q_dot_htf_ND_max_at_T_amb_LT_regr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_htf_ND_max_at_T_amb_LT_regr", &result))
		make_access_error("SAM_UiUdpcChecks", "q_dot_htf_ND_max_at_T_amb_LT_regr");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_q_dot_htf_ND_max_at_T_amb_LT_rule0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_htf_ND_max_at_T_amb_LT_rule0", &result))
		make_access_error("SAM_UiUdpcChecks", "q_dot_htf_ND_max_at_T_amb_LT_rule0");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_q_dot_htf_ND_max_at_T_amb_design_regr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_htf_ND_max_at_T_amb_design_regr", &result))
		make_access_error("SAM_UiUdpcChecks", "q_dot_htf_ND_max_at_T_amb_design_regr");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_q_dot_htf_ND_max_at_T_amb_design_rule0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_htf_ND_max_at_T_amb_design_rule0", &result))
		make_access_error("SAM_UiUdpcChecks", "q_dot_htf_ND_max_at_T_amb_design_rule0");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_q_dot_htf_ND_max_at_T_amb_high_level_regr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_htf_ND_max_at_T_amb_high_level_regr", &result))
		make_access_error("SAM_UiUdpcChecks", "q_dot_htf_ND_max_at_T_amb_high_level_regr");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_q_dot_htf_ND_max_at_T_amb_high_level_rule0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_htf_ND_max_at_T_amb_high_level_rule0", &result))
		make_access_error("SAM_UiUdpcChecks", "q_dot_htf_ND_max_at_T_amb_high_level_rule0");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_q_dot_htf_ND_max_at_T_amb_low_level_regr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_htf_ND_max_at_T_amb_low_level_regr", &result))
		make_access_error("SAM_UiUdpcChecks", "q_dot_htf_ND_max_at_T_amb_low_level_regr");
	});
	return result;
}

SAM_EXPORT double SAM_UiUdpcChecks_Outputs_q_dot_htf_ND_max_at_T_amb_low_level_rule0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_htf_ND_max_at_T_amb_low_level_rule0", &result))
		make_access_error("SAM_UiUdpcChecks", "q_dot_htf_ND_max_at_T_amb_low_level_rule0");
	});
	return result;
}


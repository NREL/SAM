#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Sco2CspUdPcTables.h"

SAM_EXPORT int SAM_Sco2CspUdPcTables_execute(SAM_table data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("sco2_csp_ud_pc_tables", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_Sco2CspUdPcTables_SystemDesign_T_amb_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_amb_des", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_SystemDesign_T_htf_hot_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_htf_hot_des", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_SystemDesign_W_dot_net_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "W_dot_net_des", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_SystemDesign_dT_PHX_hot_approach_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dT_PHX_hot_approach", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_SystemDesign_dT_mc_approach_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dT_mc_approach", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_SystemDesign_design_method_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "design_method", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_SystemDesign_eta_thermal_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_thermal_des", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_SystemDesign_htf_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "htf", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_SystemDesign_htf_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "htf_props", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_SystemDesign_site_elevation_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "site_elevation", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_HTR_HP_deltaP_des_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HTR_HP_deltaP_des_in", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_HTR_LP_deltaP_des_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HTR_LP_deltaP_des_in", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_HTR_UA_des_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HTR_UA_des_in", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_HTR_design_code_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HTR_design_code", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_HTR_eff_des_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HTR_eff_des_in", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_HTR_min_dT_des_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HTR_min_dT_des_in", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_HT_recup_eff_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HT_recup_eff_max", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_LTR_HP_deltaP_des_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "LTR_HP_deltaP_des_in", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_LTR_LP_deltaP_des_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "LTR_LP_deltaP_des_in", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_LTR_UA_des_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "LTR_UA_des_in", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_LTR_design_code_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "LTR_design_code", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_LTR_eff_des_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "LTR_eff_des_in", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_LTR_min_dT_des_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "LTR_min_dT_des_in", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_LT_recup_eff_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "LT_recup_eff_max", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_UA_recup_tot_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "UA_recup_tot_des", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_cycle_config_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cycle_config", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_des_objective_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "des_objective", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_is_IP_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_IP_fixed", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_is_PR_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_PR_fixed", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_is_P_high_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_P_high_fixed", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_is_recomp_ok_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_recomp_ok", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_min_phx_deltaT_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "min_phx_deltaT", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_rel_tol_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rel_tol", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_CycleDesign_PHX_co2_deltaP_des_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "PHX_co2_deltaP_des_in", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_CycleDesign_P_high_limit_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_high_limit", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_CycleDesign_deltaP_counterHX_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "deltaP_counterHX_frac", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_CycleDesign_eta_isen_mc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_isen_mc", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_CycleDesign_eta_isen_pc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_isen_pc", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_CycleDesign_eta_isen_rc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_isen_rc", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_CycleDesign_eta_isen_t_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_isen_t", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_PHXDesign_dT_PHX_cold_approach_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dT_PHX_cold_approach", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_AirCoolerDesign_deltaP_cooler_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "deltaP_cooler_frac", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_AirCoolerDesign_fan_power_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fan_power_frac", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_AirCoolerDesign_is_design_air_cooler_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_design_air_cooler", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_Common_T_amb_high_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_amb_high", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_Common_T_amb_low_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_amb_low", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_Common_T_htf_hot_high_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_htf_hot_high", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_Common_T_htf_hot_low_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_htf_hot_low", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_Common_is_apply_default_htf_mins_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_apply_default_htf_mins", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_Common_is_generate_udpc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_generate_udpc", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_Common_m_dot_htf_ND_high_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "m_dot_htf_ND_high", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_Common_m_dot_htf_ND_low_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "m_dot_htf_ND_low", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_Common_n_T_amb_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_T_amb", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_Common_n_T_htf_hot_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_T_htf_hot", number);
	});
}

SAM_EXPORT void SAM_Sco2CspUdPcTables_Common_n_m_dot_htf_ND_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_m_dot_htf_ND", number);
	});
}

SAM_EXPORT double SAM_Sco2CspUdPcTables_SystemDesign_T_amb_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "T_amb_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_SystemDesign_T_htf_hot_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_hot_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "T_htf_hot_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_SystemDesign_W_dot_net_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_net_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "W_dot_net_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_SystemDesign_dT_PHX_hot_approach_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dT_PHX_hot_approach", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "dT_PHX_hot_approach");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_SystemDesign_dT_mc_approach_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dT_mc_approach", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "dT_mc_approach");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_SystemDesign_design_method_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "design_method", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "design_method");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_SystemDesign_eta_thermal_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_thermal_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "eta_thermal_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_SystemDesign_htf_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "htf", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "htf");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_SystemDesign_htf_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "htf_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "htf_props");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_SystemDesign_site_elevation_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "site_elevation", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "site_elevation");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_HTR_HP_deltaP_des_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTR_HP_deltaP_des_in", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "HTR_HP_deltaP_des_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_HTR_LP_deltaP_des_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTR_LP_deltaP_des_in", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "HTR_LP_deltaP_des_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_HTR_UA_des_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTR_UA_des_in", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "HTR_UA_des_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_HTR_design_code_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTR_design_code", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "HTR_design_code");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_HTR_eff_des_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTR_eff_des_in", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "HTR_eff_des_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_HTR_min_dT_des_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTR_min_dT_des_in", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "HTR_min_dT_des_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_HT_recup_eff_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HT_recup_eff_max", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "HT_recup_eff_max");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_LTR_HP_deltaP_des_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LTR_HP_deltaP_des_in", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "LTR_HP_deltaP_des_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_LTR_LP_deltaP_des_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LTR_LP_deltaP_des_in", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "LTR_LP_deltaP_des_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_LTR_UA_des_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LTR_UA_des_in", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "LTR_UA_des_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_LTR_design_code_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LTR_design_code", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "LTR_design_code");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_LTR_eff_des_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LTR_eff_des_in", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "LTR_eff_des_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_LTR_min_dT_des_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LTR_min_dT_des_in", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "LTR_min_dT_des_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_LT_recup_eff_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LT_recup_eff_max", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "LT_recup_eff_max");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_UA_recup_tot_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "UA_recup_tot_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "UA_recup_tot_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_cycle_config_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_config", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "cycle_config");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_des_objective_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "des_objective", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "des_objective");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_is_IP_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_IP_fixed", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "is_IP_fixed");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_is_PR_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_PR_fixed", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "is_PR_fixed");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_is_P_high_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_P_high_fixed", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "is_P_high_fixed");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_is_recomp_ok_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_recomp_ok", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "is_recomp_ok");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_min_phx_deltaT_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "min_phx_deltaT", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "min_phx_deltaT");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_rel_tol_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rel_tol", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "rel_tol");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_CycleDesign_PHX_co2_deltaP_des_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "PHX_co2_deltaP_des_in", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "PHX_co2_deltaP_des_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_CycleDesign_P_high_limit_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_high_limit", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "P_high_limit");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_CycleDesign_deltaP_counterHX_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "deltaP_counterHX_frac", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "deltaP_counterHX_frac");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_CycleDesign_eta_isen_mc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_isen_mc", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "eta_isen_mc");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_CycleDesign_eta_isen_pc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_isen_pc", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "eta_isen_pc");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_CycleDesign_eta_isen_rc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_isen_rc", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "eta_isen_rc");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_CycleDesign_eta_isen_t_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_isen_t", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "eta_isen_t");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_PHXDesign_dT_PHX_cold_approach_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dT_PHX_cold_approach", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "dT_PHX_cold_approach");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_AirCoolerDesign_deltaP_cooler_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "deltaP_cooler_frac", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "deltaP_cooler_frac");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_AirCoolerDesign_fan_power_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fan_power_frac", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "fan_power_frac");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_AirCoolerDesign_is_design_air_cooler_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_design_air_cooler", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "is_design_air_cooler");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Common_T_amb_high_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_high", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "T_amb_high");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Common_T_amb_low_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_low", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "T_amb_low");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Common_T_htf_hot_high_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_hot_high", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "T_htf_hot_high");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Common_T_htf_hot_low_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_hot_low", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "T_htf_hot_low");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Common_is_apply_default_htf_mins_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_apply_default_htf_mins", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "is_apply_default_htf_mins");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Common_is_generate_udpc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_generate_udpc", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "is_generate_udpc");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Common_m_dot_htf_ND_high_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htf_ND_high", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "m_dot_htf_ND_high");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Common_m_dot_htf_ND_low_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htf_ND_low", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "m_dot_htf_ND_low");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Common_n_T_amb_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_T_amb", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "n_T_amb");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Common_n_T_htf_hot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_T_htf_hot", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "n_T_htf_hot");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Common_n_m_dot_htf_ND_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_m_dot_htf_ND", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "n_m_dot_htf_ND");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_HTR_HP_T_in_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTR_HP_T_in_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "HTR_HP_T_in_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_HTR_HP_deltaP_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTR_HP_deltaP_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "HTR_HP_deltaP_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_HTR_LP_T_out_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTR_LP_T_out_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "HTR_LP_T_out_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_HTR_LP_deltaP_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTR_LP_deltaP_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "HTR_LP_deltaP_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_HTR_UA_assigned_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTR_UA_assigned", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "HTR_UA_assigned");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_HTR_UA_calculated_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTR_UA_calculated", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "HTR_UA_calculated");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_HTR_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTR_cost", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "HTR_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_HTR_min_dT_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTR_min_dT", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "HTR_min_dT");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_IP_cooler_P_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "IP_cooler_P_in", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "IP_cooler_P_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_IP_cooler_T_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "IP_cooler_T_in", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "IP_cooler_T_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_IP_cooler_UA_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "IP_cooler_UA", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "IP_cooler_UA");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_IP_cooler_W_dot_fan_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "IP_cooler_W_dot_fan", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "IP_cooler_W_dot_fan");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_IP_cooler_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "IP_cooler_cost", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "IP_cooler_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_IP_cooler_m_dot_co2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "IP_cooler_m_dot_co2", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "IP_cooler_m_dot_co2");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_IP_cooler_q_dot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "IP_cooler_q_dot", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "IP_cooler_q_dot");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_LP_cooler_P_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LP_cooler_P_in", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "LP_cooler_P_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_LP_cooler_T_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LP_cooler_T_in", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "LP_cooler_T_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_LP_cooler_UA_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LP_cooler_UA", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "LP_cooler_UA");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_LP_cooler_W_dot_fan_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LP_cooler_W_dot_fan", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "LP_cooler_W_dot_fan");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_LP_cooler_co2_deltaP_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LP_cooler_co2_deltaP_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "LP_cooler_co2_deltaP_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_LP_cooler_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LP_cooler_cost", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "LP_cooler_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_LP_cooler_in_isen_deltah_to_P_mc_out_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LP_cooler_in_isen_deltah_to_P_mc_out", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "LP_cooler_in_isen_deltah_to_P_mc_out");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_LP_cooler_m_dot_co2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LP_cooler_m_dot_co2", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "LP_cooler_m_dot_co2");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_LP_cooler_q_dot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LP_cooler_q_dot", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "LP_cooler_q_dot");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_LP_cooler_rho_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LP_cooler_rho_in", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "LP_cooler_rho_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_LTR_HP_T_out_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LTR_HP_T_out_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "LTR_HP_T_out_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_LTR_HP_deltaP_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LTR_HP_deltaP_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "LTR_HP_deltaP_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_LTR_LP_deltaP_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LTR_LP_deltaP_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "LTR_LP_deltaP_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_LTR_UA_assigned_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LTR_UA_assigned", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "LTR_UA_assigned");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_LTR_UA_calculated_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LTR_UA_calculated", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "LTR_UA_calculated");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_LTR_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LTR_cost", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "LTR_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_LTR_min_dT_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LTR_min_dT", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "LTR_min_dT");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_NTU_HTR_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "NTU_HTR", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "NTU_HTR");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_NTU_LTR_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "NTU_LTR", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "NTU_LTR");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_NTU_PHX_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "NTU_PHX", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "NTU_PHX");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_PHX_co2_deltaP_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "PHX_co2_deltaP_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "PHX_co2_deltaP_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_PHX_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "PHX_cost", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "PHX_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_P_co2_PHX_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_co2_PHX_in", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "P_co2_PHX_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_P_comp_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_comp_in", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "P_comp_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_P_comp_out_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_comp_out", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "P_comp_out");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_P_mc_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_mc_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "P_mc_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_P_pc_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_pc_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "P_pc_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_P_rc_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_rc_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "P_rc_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_P_state_points_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_state_points", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "P_state_points");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_P_t_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_t_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "P_t_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_T_HTR_HP_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_HTR_HP_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "T_HTR_HP_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_T_HTR_LP_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_HTR_LP_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "T_HTR_LP_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_T_LTR_HP_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_LTR_HP_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "T_LTR_HP_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_T_LTR_LP_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_LTR_LP_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "T_LTR_LP_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_T_PHX_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_PHX_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "T_PHX_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_T_amb_ind_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "T_amb_ind", nrows, ncols);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "T_amb_ind");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_T_co2_PHX_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_co2_PHX_in", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "T_co2_PHX_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_T_comp_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_comp_in", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "T_comp_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_T_htf_cold_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_cold_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "T_htf_cold_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_T_htf_ind_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "T_htf_ind", nrows, ncols);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "T_htf_ind");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_T_main_cooler_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_main_cooler_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "T_main_cooler_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_T_pre_cooler_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_pre_cooler_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "T_pre_cooler_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_T_state_points_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_state_points", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "T_state_points");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_T_turb_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_turb_in", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "T_turb_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_UA_PHX_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "UA_PHX", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "UA_PHX");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_c_tot_W_dot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_tot_W_dot", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "c_tot_W_dot");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_c_tot_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_tot_cost", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "c_tot_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_cooler_tot_UA_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cooler_tot_UA", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "cooler_tot_UA");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_cooler_tot_W_dot_fan_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cooler_tot_W_dot_fan", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "cooler_tot_W_dot_fan");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_cooler_tot_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cooler_tot_cost", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "cooler_tot_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_cycle_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_cost", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "cycle_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_cycle_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_spec_cost", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "cycle_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_cycle_spec_cost_thermal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_spec_cost_thermal", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "cycle_spec_cost_thermal");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_deltaT_HTF_PHX_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "deltaT_HTF_PHX", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "deltaT_HTF_PHX");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_eff_HTR_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eff_HTR", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "eff_HTR");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_eff_LTR_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eff_LTR", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "eff_LTR");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_eff_PHX_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eff_PHX", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "eff_PHX");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_eta_thermal_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_thermal_calc", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "eta_thermal_calc");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_h_mc_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "h_mc_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "h_mc_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_h_pc_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "h_pc_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "h_pc_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_h_rc_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "h_rc_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "h_rc_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_h_state_points_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "h_state_points", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "h_state_points");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_h_t_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "h_t_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "h_t_data");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_m_dot_co2_full_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_co2_full", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "m_dot_co2_full");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_m_dot_htf_ND_ind_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "m_dot_htf_ND_ind", nrows, ncols);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "m_dot_htf_ND_ind");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_m_dot_htf_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htf_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "m_dot_htf_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_mc_D_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mc_D", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "mc_D");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_N_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_N_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "mc_N_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_T_out_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_T_out", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "mc_T_out");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_W_dot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_W_dot", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "mc_W_dot");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_cost", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "mc_cost");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_mc_eta_stages_des_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mc_eta_stages_des", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "mc_eta_stages_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_ideal_spec_work_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_ideal_spec_work", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "mc_ideal_spec_work");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_m_dot_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_m_dot_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "mc_m_dot_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_n_stages_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_n_stages", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "mc_n_stages");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_phi_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_phi_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "mc_phi_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_phi_surge_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_phi_surge", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "mc_phi_surge");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_psi_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_psi_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "mc_psi_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_psi_max_at_N_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_psi_max_at_N_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "mc_psi_max_at_N_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_rho_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_rho_in", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "mc_rho_in");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_mc_tip_ratio_des_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mc_tip_ratio_des", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "mc_tip_ratio_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_pc_D_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pc_D", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "pc_D");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_N_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_N_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "pc_N_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_P_in_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_P_in_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "pc_P_in_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_T_in_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_T_in_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "pc_T_in_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_W_dot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_W_dot", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "pc_W_dot");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_cost", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "pc_cost");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_pc_eta_stages_des_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pc_eta_stages_des", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "pc_eta_stages_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_ideal_spec_work_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_ideal_spec_work_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "pc_ideal_spec_work_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_m_dot_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_m_dot_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "pc_m_dot_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_n_stages_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_n_stages", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "pc_n_stages");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_phi_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_phi_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "pc_phi_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_phi_surge_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_phi_surge", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "pc_phi_surge");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_rho_in_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_rho_in_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "pc_rho_in_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_pc_tip_ratio_des_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pc_tip_ratio_des", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "pc_tip_ratio_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_q_dot_HTR_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_HTR", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "q_dot_HTR");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_q_dot_LTR_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_LTR", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "q_dot_LTR");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_q_dot_PHX_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_PHX", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "q_dot_PHX");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_rc_D_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rc_D", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "rc_D");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_rc_N_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rc_N_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "rc_N_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_rc_P_in_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rc_P_in_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "rc_P_in_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_rc_P_out_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rc_P_out_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "rc_P_out_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_rc_T_in_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rc_T_in_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "rc_T_in_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_rc_T_out_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rc_T_out_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "rc_T_out_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_rc_W_dot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rc_W_dot", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "rc_W_dot");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_rc_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rc_cost", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "rc_cost");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_rc_eta_stages_des_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rc_eta_stages_des", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "rc_eta_stages_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_rc_m_dot_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rc_m_dot_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "rc_m_dot_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_rc_n_stages_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rc_n_stages", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "rc_n_stages");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_rc_phi_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rc_phi_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "rc_phi_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_rc_phi_surge_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rc_phi_surge", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "rc_phi_surge");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_rc_psi_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rc_psi_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "rc_psi_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_rc_psi_max_at_N_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rc_psi_max_at_N_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "rc_psi_max_at_N_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_rc_tip_ratio_des_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rc_tip_ratio_des", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "rc_tip_ratio_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_recomp_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "recomp_frac", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "recomp_frac");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_recup_LTR_UA_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "recup_LTR_UA_frac", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "recup_LTR_UA_frac");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_recup_total_UA_assigned_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "recup_total_UA_assigned", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "recup_total_UA_assigned");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_recup_total_UA_calculated_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "recup_total_UA_calculated", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "recup_total_UA_calculated");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_recup_total_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "recup_total_cost", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "recup_total_cost");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_s_HTR_HP_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "s_HTR_HP_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "s_HTR_HP_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_s_HTR_LP_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "s_HTR_LP_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "s_HTR_LP_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_s_LTR_HP_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "s_LTR_HP_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "s_LTR_HP_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_s_LTR_LP_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "s_LTR_LP_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "s_LTR_LP_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_s_PHX_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "s_PHX_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "s_PHX_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_s_main_cooler_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "s_main_cooler_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "s_main_cooler_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_s_pre_cooler_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "s_pre_cooler_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "s_pre_cooler_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_s_state_points_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "s_state_points", length);
	if (!result)
		make_access_error("SAM_Sco2CspUdPcTables", "s_state_points");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_t_D_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_D", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "t_D");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_t_N_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_N_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "t_N_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_t_P_in_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_P_in_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "t_P_in_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_t_P_out_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_P_out_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "t_P_out_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_t_T_out_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_T_out_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "t_T_out_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_t_W_dot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_W_dot", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "t_W_dot");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_t_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_cost", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "t_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_t_m_dot_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_m_dot_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "t_m_dot_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_t_nu_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_nu_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "t_nu_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_t_tip_ratio_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_tip_ratio_des", &result))
		make_access_error("SAM_Sco2CspUdPcTables", "t_tip_ratio_des");
	});
	return result;
}




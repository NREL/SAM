#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Utilityrate.h"

SAM_EXPORT SAM_Utilityrate SAM_Utilityrate_construct(const char* def, SAM_error* err){
	SAM_Utilityrate result = nullptr;
	translateExceptions(err, [&]{
		result = ssc_data_create();
	});
	return result;
}

SAM_EXPORT int SAM_Utilityrate_execute(SAM_Utilityrate data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("utilityrate", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_Utilityrate_destruct(SAM_Utilityrate system)
{
	ssc_data_free(system);
}

SAM_EXPORT void SAM_Utilityrate_Common_analysis_period_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "analysis_period", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_e_with_system_aset(SAM_Utilityrate ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "e_with_system", arr, length);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_e_without_system_aset(SAM_Utilityrate ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "e_without_system", arr, length);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_load_escalation_aset(SAM_Utilityrate ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "load_escalation", arr, length);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_p_with_system_aset(SAM_Utilityrate ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "p_with_system", arr, length);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_p_without_system_aset(SAM_Utilityrate ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "p_without_system", arr, length);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_rate_escalation_aset(SAM_Utilityrate ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "rate_escalation", arr, length);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_system_availability_aset(SAM_Utilityrate ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "system_availability", arr, length);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_system_degradation_aset(SAM_Utilityrate ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "system_degradation", arr, length);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_enable_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_enable", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_fixed_m1_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_fixed_m1", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_fixed_m10_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_fixed_m10", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_fixed_m11_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_fixed_m11", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_fixed_m12_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_fixed_m12", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_fixed_m2_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_fixed_m2", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_fixed_m3_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_fixed_m3", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_fixed_m4_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_fixed_m4", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_fixed_m5_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_fixed_m5", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_fixed_m6_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_fixed_m6", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_fixed_m7_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_fixed_m7", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_fixed_m8_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_fixed_m8", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_fixed_m9_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_fixed_m9", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_p1_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p1", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_p2_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p2", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_p3_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p3", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_p4_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p4", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_p5_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p5", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_p6_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p6", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_p7_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p7", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_p8_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p8", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_p9_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p9", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_sched_weekday_sset(SAM_Utilityrate ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "ur_dc_sched_weekday", str);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_sched_weekend_sset(SAM_Utilityrate ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "ur_dc_sched_weekend", str);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_flat_buy_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_flat_buy_rate", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_flat_sell_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_flat_sell_rate", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_monthly_fixed_charge_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_monthly_fixed_charge", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_sell_eq_buy_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_sell_eq_buy", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_enable_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tou_enable", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p1_buy_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tou_p1_buy_rate", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p1_sell_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tou_p1_sell_rate", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p2_buy_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tou_p2_buy_rate", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p2_sell_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tou_p2_sell_rate", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p3_buy_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tou_p3_buy_rate", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p3_sell_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tou_p3_sell_rate", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p4_buy_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tou_p4_buy_rate", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p4_sell_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tou_p4_sell_rate", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p5_buy_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tou_p5_buy_rate", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p5_sell_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tou_p5_sell_rate", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p6_buy_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tou_p6_buy_rate", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p6_sell_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tou_p6_sell_rate", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p7_buy_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tou_p7_buy_rate", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p7_sell_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tou_p7_sell_rate", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p8_buy_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tou_p8_buy_rate", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p8_sell_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tou_p8_sell_rate", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p9_buy_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tou_p9_buy_rate", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p9_sell_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tou_p9_sell_rate", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_sched_weekday_sset(SAM_Utilityrate ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "ur_tou_sched_weekday", str);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_sched_weekend_sset(SAM_Utilityrate ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "ur_tou_sched_weekend", str);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_enable_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_enable", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s1_energy_ub1_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s1_energy_ub1", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s1_energy_ub2_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s1_energy_ub2", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s1_energy_ub3_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s1_energy_ub3", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s1_energy_ub4_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s1_energy_ub4", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s1_energy_ub5_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s1_energy_ub5", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s1_energy_ub6_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s1_energy_ub6", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s1_rate1_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s1_rate1", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s1_rate2_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s1_rate2", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s1_rate3_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s1_rate3", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s1_rate4_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s1_rate4", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s1_rate5_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s1_rate5", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s1_rate6_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s1_rate6", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s2_energy_ub1_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s2_energy_ub1", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s2_energy_ub2_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s2_energy_ub2", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s2_energy_ub3_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s2_energy_ub3", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s2_energy_ub4_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s2_energy_ub4", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s2_energy_ub5_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s2_energy_ub5", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s2_energy_ub6_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s2_energy_ub6", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s2_rate1_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s2_rate1", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s2_rate2_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s2_rate2", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s2_rate3_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s2_rate3", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s2_rate4_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s2_rate4", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s2_rate5_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s2_rate5", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s2_rate6_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s2_rate6", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s3_energy_ub1_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s3_energy_ub1", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s3_energy_ub2_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s3_energy_ub2", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s3_energy_ub3_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s3_energy_ub3", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s3_energy_ub4_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s3_energy_ub4", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s3_energy_ub5_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s3_energy_ub5", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s3_energy_ub6_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s3_energy_ub6", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s3_rate1_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s3_rate1", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s3_rate2_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s3_rate2", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s3_rate3_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s3_rate3", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s3_rate4_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s3_rate4", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s3_rate5_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s3_rate5", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s3_rate6_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s3_rate6", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s4_energy_ub1_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s4_energy_ub1", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s4_energy_ub2_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s4_energy_ub2", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s4_energy_ub3_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s4_energy_ub3", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s4_energy_ub4_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s4_energy_ub4", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s4_energy_ub5_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s4_energy_ub5", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s4_energy_ub6_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s4_energy_ub6", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s4_rate1_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s4_rate1", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s4_rate2_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s4_rate2", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s4_rate3_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s4_rate3", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s4_rate4_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s4_rate4", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s4_rate5_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s4_rate5", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s4_rate6_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s4_rate6", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s5_energy_ub1_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s5_energy_ub1", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s5_energy_ub2_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s5_energy_ub2", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s5_energy_ub3_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s5_energy_ub3", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s5_energy_ub4_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s5_energy_ub4", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s5_energy_ub5_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s5_energy_ub5", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s5_energy_ub6_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s5_energy_ub6", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s5_rate1_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s5_rate1", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s5_rate2_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s5_rate2", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s5_rate3_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s5_rate3", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s5_rate4_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s5_rate4", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s5_rate5_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s5_rate5", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s5_rate6_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s5_rate6", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s6_energy_ub1_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s6_energy_ub1", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s6_energy_ub2_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s6_energy_ub2", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s6_energy_ub3_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s6_energy_ub3", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s6_energy_ub4_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s6_energy_ub4", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s6_energy_ub5_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s6_energy_ub5", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s6_energy_ub6_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s6_energy_ub6", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s6_rate1_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s6_rate1", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s6_rate2_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s6_rate2", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s6_rate3_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s6_rate3", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s6_rate4_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s6_rate4", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s6_rate5_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s6_rate5", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s6_rate6_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_s6_rate6", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_sched_m1_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_sched_m1", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_sched_m10_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_sched_m10", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_sched_m11_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_sched_m11", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_sched_m12_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_sched_m12", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_sched_m2_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_sched_m2", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_sched_m3_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_sched_m3", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_sched_m4_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_sched_m4", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_sched_m5_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_sched_m5", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_sched_m6_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_sched_m6", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_sched_m7_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_sched_m7", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_sched_m8_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_sched_m8", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_sched_m9_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_sched_m9", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_sell_mode_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_sell_mode", number);
	});
}

SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_sell_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_tr_sell_rate", number);
	});
}

SAM_EXPORT double SAM_Utilityrate_Common_analysis_period_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "analysis_period", &result))
		make_access_error("SAM_Utilityrate", "analysis_period");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Common_e_with_system_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "e_with_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "e_with_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Common_e_without_system_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "e_without_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "e_without_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Common_load_escalation_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "load_escalation", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "load_escalation");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Common_p_with_system_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "p_with_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "p_with_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Common_p_without_system_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "p_without_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "p_without_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Common_rate_escalation_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rate_escalation", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "rate_escalation");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Common_system_availability_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "system_availability", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "system_availability");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Common_system_degradation_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "system_degradation", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "system_degradation");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_enable_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_enable", &result))
		make_access_error("SAM_Utilityrate", "ur_dc_enable");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_fixed_m1_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_fixed_m1", &result))
		make_access_error("SAM_Utilityrate", "ur_dc_fixed_m1");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_fixed_m10_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_fixed_m10", &result))
		make_access_error("SAM_Utilityrate", "ur_dc_fixed_m10");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_fixed_m11_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_fixed_m11", &result))
		make_access_error("SAM_Utilityrate", "ur_dc_fixed_m11");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_fixed_m12_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_fixed_m12", &result))
		make_access_error("SAM_Utilityrate", "ur_dc_fixed_m12");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_fixed_m2_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_fixed_m2", &result))
		make_access_error("SAM_Utilityrate", "ur_dc_fixed_m2");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_fixed_m3_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_fixed_m3", &result))
		make_access_error("SAM_Utilityrate", "ur_dc_fixed_m3");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_fixed_m4_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_fixed_m4", &result))
		make_access_error("SAM_Utilityrate", "ur_dc_fixed_m4");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_fixed_m5_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_fixed_m5", &result))
		make_access_error("SAM_Utilityrate", "ur_dc_fixed_m5");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_fixed_m6_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_fixed_m6", &result))
		make_access_error("SAM_Utilityrate", "ur_dc_fixed_m6");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_fixed_m7_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_fixed_m7", &result))
		make_access_error("SAM_Utilityrate", "ur_dc_fixed_m7");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_fixed_m8_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_fixed_m8", &result))
		make_access_error("SAM_Utilityrate", "ur_dc_fixed_m8");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_fixed_m9_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_fixed_m9", &result))
		make_access_error("SAM_Utilityrate", "ur_dc_fixed_m9");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_p1_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p1", &result))
		make_access_error("SAM_Utilityrate", "ur_dc_p1");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_p2_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p2", &result))
		make_access_error("SAM_Utilityrate", "ur_dc_p2");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_p3_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p3", &result))
		make_access_error("SAM_Utilityrate", "ur_dc_p3");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_p4_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p4", &result))
		make_access_error("SAM_Utilityrate", "ur_dc_p4");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_p5_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p5", &result))
		make_access_error("SAM_Utilityrate", "ur_dc_p5");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_p6_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p6", &result))
		make_access_error("SAM_Utilityrate", "ur_dc_p6");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_p7_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p7", &result))
		make_access_error("SAM_Utilityrate", "ur_dc_p7");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_p8_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p8", &result))
		make_access_error("SAM_Utilityrate", "ur_dc_p8");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_p9_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p9", &result))
		make_access_error("SAM_Utilityrate", "ur_dc_p9");
	});
	return result;
}



SAM_EXPORT const char* SAM_Utilityrate_Common_ur_dc_sched_weekday_sget(SAM_Utilityrate ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "ur_dc_sched_weekday");
	if (!result)
		make_access_error("SAM_Utilityrate", "ur_dc_sched_weekday");
	});
	return result;
}



SAM_EXPORT const char* SAM_Utilityrate_Common_ur_dc_sched_weekend_sget(SAM_Utilityrate ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "ur_dc_sched_weekend");
	if (!result)
		make_access_error("SAM_Utilityrate", "ur_dc_sched_weekend");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_flat_buy_rate_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_flat_buy_rate", &result))
		make_access_error("SAM_Utilityrate", "ur_flat_buy_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_flat_sell_rate_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_flat_sell_rate", &result))
		make_access_error("SAM_Utilityrate", "ur_flat_sell_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_monthly_fixed_charge_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_monthly_fixed_charge", &result))
		make_access_error("SAM_Utilityrate", "ur_monthly_fixed_charge");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_sell_eq_buy_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_sell_eq_buy", &result))
		make_access_error("SAM_Utilityrate", "ur_sell_eq_buy");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_enable_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tou_enable", &result))
		make_access_error("SAM_Utilityrate", "ur_tou_enable");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p1_buy_rate_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tou_p1_buy_rate", &result))
		make_access_error("SAM_Utilityrate", "ur_tou_p1_buy_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p1_sell_rate_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tou_p1_sell_rate", &result))
		make_access_error("SAM_Utilityrate", "ur_tou_p1_sell_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p2_buy_rate_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tou_p2_buy_rate", &result))
		make_access_error("SAM_Utilityrate", "ur_tou_p2_buy_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p2_sell_rate_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tou_p2_sell_rate", &result))
		make_access_error("SAM_Utilityrate", "ur_tou_p2_sell_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p3_buy_rate_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tou_p3_buy_rate", &result))
		make_access_error("SAM_Utilityrate", "ur_tou_p3_buy_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p3_sell_rate_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tou_p3_sell_rate", &result))
		make_access_error("SAM_Utilityrate", "ur_tou_p3_sell_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p4_buy_rate_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tou_p4_buy_rate", &result))
		make_access_error("SAM_Utilityrate", "ur_tou_p4_buy_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p4_sell_rate_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tou_p4_sell_rate", &result))
		make_access_error("SAM_Utilityrate", "ur_tou_p4_sell_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p5_buy_rate_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tou_p5_buy_rate", &result))
		make_access_error("SAM_Utilityrate", "ur_tou_p5_buy_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p5_sell_rate_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tou_p5_sell_rate", &result))
		make_access_error("SAM_Utilityrate", "ur_tou_p5_sell_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p6_buy_rate_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tou_p6_buy_rate", &result))
		make_access_error("SAM_Utilityrate", "ur_tou_p6_buy_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p6_sell_rate_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tou_p6_sell_rate", &result))
		make_access_error("SAM_Utilityrate", "ur_tou_p6_sell_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p7_buy_rate_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tou_p7_buy_rate", &result))
		make_access_error("SAM_Utilityrate", "ur_tou_p7_buy_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p7_sell_rate_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tou_p7_sell_rate", &result))
		make_access_error("SAM_Utilityrate", "ur_tou_p7_sell_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p8_buy_rate_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tou_p8_buy_rate", &result))
		make_access_error("SAM_Utilityrate", "ur_tou_p8_buy_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p8_sell_rate_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tou_p8_sell_rate", &result))
		make_access_error("SAM_Utilityrate", "ur_tou_p8_sell_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p9_buy_rate_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tou_p9_buy_rate", &result))
		make_access_error("SAM_Utilityrate", "ur_tou_p9_buy_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p9_sell_rate_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tou_p9_sell_rate", &result))
		make_access_error("SAM_Utilityrate", "ur_tou_p9_sell_rate");
	});
	return result;
}



SAM_EXPORT const char* SAM_Utilityrate_Common_ur_tou_sched_weekday_sget(SAM_Utilityrate ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "ur_tou_sched_weekday");
	if (!result)
		make_access_error("SAM_Utilityrate", "ur_tou_sched_weekday");
	});
	return result;
}



SAM_EXPORT const char* SAM_Utilityrate_Common_ur_tou_sched_weekend_sget(SAM_Utilityrate ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "ur_tou_sched_weekend");
	if (!result)
		make_access_error("SAM_Utilityrate", "ur_tou_sched_weekend");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_enable_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_enable", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_enable");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s1_energy_ub1_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s1_energy_ub1", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s1_energy_ub1");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s1_energy_ub2_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s1_energy_ub2", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s1_energy_ub2");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s1_energy_ub3_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s1_energy_ub3", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s1_energy_ub3");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s1_energy_ub4_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s1_energy_ub4", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s1_energy_ub4");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s1_energy_ub5_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s1_energy_ub5", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s1_energy_ub5");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s1_energy_ub6_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s1_energy_ub6", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s1_energy_ub6");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s1_rate1_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s1_rate1", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s1_rate1");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s1_rate2_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s1_rate2", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s1_rate2");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s1_rate3_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s1_rate3", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s1_rate3");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s1_rate4_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s1_rate4", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s1_rate4");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s1_rate5_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s1_rate5", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s1_rate5");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s1_rate6_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s1_rate6", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s1_rate6");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s2_energy_ub1_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s2_energy_ub1", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s2_energy_ub1");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s2_energy_ub2_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s2_energy_ub2", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s2_energy_ub2");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s2_energy_ub3_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s2_energy_ub3", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s2_energy_ub3");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s2_energy_ub4_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s2_energy_ub4", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s2_energy_ub4");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s2_energy_ub5_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s2_energy_ub5", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s2_energy_ub5");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s2_energy_ub6_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s2_energy_ub6", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s2_energy_ub6");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s2_rate1_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s2_rate1", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s2_rate1");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s2_rate2_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s2_rate2", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s2_rate2");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s2_rate3_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s2_rate3", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s2_rate3");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s2_rate4_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s2_rate4", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s2_rate4");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s2_rate5_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s2_rate5", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s2_rate5");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s2_rate6_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s2_rate6", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s2_rate6");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s3_energy_ub1_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s3_energy_ub1", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s3_energy_ub1");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s3_energy_ub2_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s3_energy_ub2", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s3_energy_ub2");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s3_energy_ub3_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s3_energy_ub3", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s3_energy_ub3");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s3_energy_ub4_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s3_energy_ub4", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s3_energy_ub4");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s3_energy_ub5_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s3_energy_ub5", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s3_energy_ub5");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s3_energy_ub6_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s3_energy_ub6", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s3_energy_ub6");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s3_rate1_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s3_rate1", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s3_rate1");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s3_rate2_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s3_rate2", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s3_rate2");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s3_rate3_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s3_rate3", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s3_rate3");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s3_rate4_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s3_rate4", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s3_rate4");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s3_rate5_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s3_rate5", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s3_rate5");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s3_rate6_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s3_rate6", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s3_rate6");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s4_energy_ub1_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s4_energy_ub1", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s4_energy_ub1");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s4_energy_ub2_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s4_energy_ub2", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s4_energy_ub2");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s4_energy_ub3_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s4_energy_ub3", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s4_energy_ub3");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s4_energy_ub4_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s4_energy_ub4", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s4_energy_ub4");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s4_energy_ub5_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s4_energy_ub5", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s4_energy_ub5");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s4_energy_ub6_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s4_energy_ub6", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s4_energy_ub6");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s4_rate1_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s4_rate1", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s4_rate1");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s4_rate2_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s4_rate2", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s4_rate2");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s4_rate3_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s4_rate3", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s4_rate3");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s4_rate4_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s4_rate4", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s4_rate4");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s4_rate5_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s4_rate5", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s4_rate5");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s4_rate6_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s4_rate6", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s4_rate6");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s5_energy_ub1_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s5_energy_ub1", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s5_energy_ub1");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s5_energy_ub2_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s5_energy_ub2", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s5_energy_ub2");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s5_energy_ub3_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s5_energy_ub3", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s5_energy_ub3");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s5_energy_ub4_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s5_energy_ub4", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s5_energy_ub4");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s5_energy_ub5_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s5_energy_ub5", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s5_energy_ub5");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s5_energy_ub6_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s5_energy_ub6", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s5_energy_ub6");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s5_rate1_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s5_rate1", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s5_rate1");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s5_rate2_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s5_rate2", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s5_rate2");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s5_rate3_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s5_rate3", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s5_rate3");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s5_rate4_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s5_rate4", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s5_rate4");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s5_rate5_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s5_rate5", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s5_rate5");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s5_rate6_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s5_rate6", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s5_rate6");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s6_energy_ub1_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s6_energy_ub1", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s6_energy_ub1");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s6_energy_ub2_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s6_energy_ub2", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s6_energy_ub2");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s6_energy_ub3_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s6_energy_ub3", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s6_energy_ub3");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s6_energy_ub4_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s6_energy_ub4", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s6_energy_ub4");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s6_energy_ub5_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s6_energy_ub5", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s6_energy_ub5");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s6_energy_ub6_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s6_energy_ub6", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s6_energy_ub6");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s6_rate1_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s6_rate1", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s6_rate1");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s6_rate2_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s6_rate2", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s6_rate2");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s6_rate3_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s6_rate3", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s6_rate3");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s6_rate4_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s6_rate4", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s6_rate4");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s6_rate5_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s6_rate5", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s6_rate5");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s6_rate6_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_s6_rate6", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_s6_rate6");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_sched_m1_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_sched_m1", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_sched_m1");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_sched_m10_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_sched_m10", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_sched_m10");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_sched_m11_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_sched_m11", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_sched_m11");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_sched_m12_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_sched_m12", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_sched_m12");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_sched_m2_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_sched_m2", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_sched_m2");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_sched_m3_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_sched_m3", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_sched_m3");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_sched_m4_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_sched_m4", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_sched_m4");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_sched_m5_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_sched_m5", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_sched_m5");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_sched_m6_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_sched_m6", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_sched_m6");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_sched_m7_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_sched_m7", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_sched_m7");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_sched_m8_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_sched_m8", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_sched_m8");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_sched_m9_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_sched_m9", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_sched_m9");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_sell_mode_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_sell_mode", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_sell_mode");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_sell_rate_nget(SAM_Utilityrate ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_tr_sell_rate", &result))
		make_access_error("SAM_Utilityrate", "ur_tr_sell_rate");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_dc_fixed_apr_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_fixed_apr", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_dc_fixed_apr");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_dc_fixed_aug_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_fixed_aug", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_dc_fixed_aug");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_dc_fixed_dec_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_fixed_dec", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_dc_fixed_dec");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_dc_fixed_feb_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_fixed_feb", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_dc_fixed_feb");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_dc_fixed_jan_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_fixed_jan", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_dc_fixed_jan");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_dc_fixed_jul_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_fixed_jul", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_dc_fixed_jul");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_dc_fixed_jun_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_fixed_jun", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_dc_fixed_jun");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_dc_fixed_mar_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_fixed_mar", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_dc_fixed_mar");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_dc_fixed_may_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_fixed_may", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_dc_fixed_may");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_dc_fixed_nov_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_fixed_nov", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_dc_fixed_nov");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_dc_fixed_oct_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_fixed_oct", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_dc_fixed_oct");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_dc_fixed_sep_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_fixed_sep", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_dc_fixed_sep");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_dc_tou_apr_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_tou_apr", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_dc_tou_apr");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_dc_tou_aug_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_tou_aug", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_dc_tou_aug");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_dc_tou_dec_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_tou_dec", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_dc_tou_dec");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_dc_tou_feb_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_tou_feb", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_dc_tou_feb");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_dc_tou_jan_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_tou_jan", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_dc_tou_jan");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_dc_tou_jul_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_tou_jul", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_dc_tou_jul");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_dc_tou_jun_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_tou_jun", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_dc_tou_jun");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_dc_tou_mar_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_tou_mar", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_dc_tou_mar");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_dc_tou_may_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_tou_may", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_dc_tou_may");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_dc_tou_nov_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_tou_nov", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_dc_tou_nov");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_dc_tou_oct_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_tou_oct", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_dc_tou_oct");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_dc_tou_sep_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_tou_sep", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_dc_tou_sep");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_tr_apr_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_tr_apr", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_tr_apr");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_tr_aug_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_tr_aug", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_tr_aug");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_tr_dec_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_tr_dec", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_tr_dec");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_tr_feb_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_tr_feb", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_tr_feb");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_tr_jan_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_tr_jan", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_tr_jan");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_tr_jul_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_tr_jul", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_tr_jul");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_tr_jun_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_tr_jun", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_tr_jun");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_tr_mar_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_tr_mar", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_tr_mar");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_tr_may_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_tr_may", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_tr_may");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_tr_nov_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_tr_nov", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_tr_nov");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_tr_oct_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_tr_oct", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_tr_oct");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_charge_tr_sep_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_tr_sep", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "charge_tr_sep");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_elec_cost_with_system_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "elec_cost_with_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "elec_cost_with_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_elec_cost_without_system_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "elec_cost_without_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "elec_cost_without_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_energy_net_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "energy_net", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "energy_net");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_energy_value_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "energy_value", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "energy_value");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_revenue_with_system_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "revenue_with_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "revenue_with_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_revenue_without_system_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "revenue_without_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "revenue_without_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_year1_hourly_e_demand_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_e_demand", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "year1_hourly_e_demand");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_year1_hourly_e_grid_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_e_grid", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "year1_hourly_e_grid");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_year1_hourly_income_with_system_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_income_with_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "year1_hourly_income_with_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_year1_hourly_income_without_system_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_income_without_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "year1_hourly_income_without_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_year1_hourly_p_demand_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_p_demand", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "year1_hourly_p_demand");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_year1_hourly_p_grid_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_p_grid", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "year1_hourly_p_grid");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_year1_hourly_p_system_to_load_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_p_system_to_load", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "year1_hourly_p_system_to_load");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_year1_hourly_payment_with_system_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_payment_with_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "year1_hourly_payment_with_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_year1_hourly_payment_without_system_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_payment_without_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "year1_hourly_payment_without_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_year1_hourly_price_with_system_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_price_with_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "year1_hourly_price_with_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_year1_hourly_price_without_system_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_price_without_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "year1_hourly_price_without_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_year1_hourly_revenue_with_system_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_revenue_with_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "year1_hourly_revenue_with_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_year1_hourly_revenue_without_system_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_revenue_without_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "year1_hourly_revenue_without_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_year1_hourly_system_output_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_system_output", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "year1_hourly_system_output");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_year1_hourly_system_to_grid_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_system_to_grid", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "year1_hourly_system_to_grid");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_year1_hourly_system_to_load_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_system_to_load", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "year1_hourly_system_to_load");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_year1_monthly_dc_fixed_with_system_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_dc_fixed_with_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "year1_monthly_dc_fixed_with_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_year1_monthly_dc_fixed_without_system_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_dc_fixed_without_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "year1_monthly_dc_fixed_without_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_year1_monthly_dc_tou_with_system_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_dc_tou_with_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "year1_monthly_dc_tou_with_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_year1_monthly_dc_tou_without_system_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_dc_tou_without_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "year1_monthly_dc_tou_without_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_year1_monthly_tr_charge_with_system_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_tr_charge_with_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "year1_monthly_tr_charge_with_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_year1_monthly_tr_charge_without_system_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_tr_charge_without_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "year1_monthly_tr_charge_without_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_year1_monthly_tr_rate_with_system_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_tr_rate_with_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "year1_monthly_tr_rate_with_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate_Outputs_year1_monthly_tr_rate_without_system_aget(SAM_Utilityrate ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_tr_rate_without_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate", "year1_monthly_tr_rate_without_system");
	});
	return result;
}




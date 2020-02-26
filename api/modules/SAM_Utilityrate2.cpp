#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Utilityrate2.h"

SAM_EXPORT SAM_Utilityrate2 SAM_Utilityrate2_construct(const char* def, SAM_error* err){
	SAM_Utilityrate2 result = nullptr;
	translateExceptions(err, [&]{
		result = ssc_data_create();
	});
	return result;
}

SAM_EXPORT int SAM_Utilityrate2_execute(SAM_Utilityrate2 data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("utilityrate2", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_Utilityrate2_destruct(SAM_Utilityrate2 system)
{
	ssc_data_free(system);
}

SAM_EXPORT void SAM_Utilityrate2_Common_analysis_period_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "analysis_period", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_e_load_aset(SAM_Utilityrate2 ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "e_load", arr, length);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_hourly_gen_aset(SAM_Utilityrate2 ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "hourly_gen", arr, length);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_load_escalation_aset(SAM_Utilityrate2 ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "load_escalation", arr, length);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_p_load_aset(SAM_Utilityrate2 ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "p_load", arr, length);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_p_with_system_aset(SAM_Utilityrate2 ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "p_with_system", arr, length);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_rate_escalation_aset(SAM_Utilityrate2 ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "rate_escalation", arr, length);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_apr_t1_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_apr_t1_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_apr_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_apr_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_apr_t2_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_apr_t2_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_apr_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_apr_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_apr_t3_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_apr_t3_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_apr_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_apr_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_apr_t4_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_apr_t4_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_apr_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_apr_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_apr_t5_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_apr_t5_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_apr_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_apr_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_apr_t6_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_apr_t6_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_apr_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_apr_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_aug_t1_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_aug_t1_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_aug_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_aug_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_aug_t2_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_aug_t2_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_aug_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_aug_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_aug_t3_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_aug_t3_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_aug_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_aug_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_aug_t4_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_aug_t4_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_aug_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_aug_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_aug_t5_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_aug_t5_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_aug_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_aug_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_aug_t6_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_aug_t6_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_aug_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_aug_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_dec_t1_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_dec_t1_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_dec_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_dec_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_dec_t2_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_dec_t2_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_dec_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_dec_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_dec_t3_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_dec_t3_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_dec_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_dec_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_dec_t4_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_dec_t4_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_dec_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_dec_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_dec_t5_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_dec_t5_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_dec_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_dec_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_dec_t6_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_dec_t6_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_dec_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_dec_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_enable_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_enable", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_feb_t1_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_feb_t1_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_feb_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_feb_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_feb_t2_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_feb_t2_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_feb_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_feb_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_feb_t3_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_feb_t3_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_feb_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_feb_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_feb_t4_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_feb_t4_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_feb_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_feb_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_feb_t5_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_feb_t5_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_feb_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_feb_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_feb_t6_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_feb_t6_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_feb_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_feb_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jan_t1_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jan_t1_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jan_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jan_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jan_t2_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jan_t2_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jan_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jan_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jan_t3_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jan_t3_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jan_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jan_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jan_t4_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jan_t4_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jan_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jan_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jan_t5_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jan_t5_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jan_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jan_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jan_t6_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jan_t6_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jan_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jan_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jul_t1_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jul_t1_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jul_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jul_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jul_t2_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jul_t2_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jul_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jul_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jul_t3_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jul_t3_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jul_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jul_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jul_t4_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jul_t4_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jul_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jul_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jul_t5_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jul_t5_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jul_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jul_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jul_t6_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jul_t6_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jul_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jul_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jun_t1_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jun_t1_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jun_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jun_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jun_t2_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jun_t2_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jun_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jun_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jun_t3_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jun_t3_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jun_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jun_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jun_t4_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jun_t4_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jun_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jun_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jun_t5_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jun_t5_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jun_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jun_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jun_t6_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jun_t6_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_jun_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_jun_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_mar_t1_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_mar_t1_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_mar_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_mar_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_mar_t2_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_mar_t2_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_mar_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_mar_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_mar_t3_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_mar_t3_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_mar_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_mar_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_mar_t4_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_mar_t4_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_mar_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_mar_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_mar_t5_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_mar_t5_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_mar_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_mar_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_mar_t6_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_mar_t6_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_mar_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_mar_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_may_t1_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_may_t1_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_may_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_may_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_may_t2_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_may_t2_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_may_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_may_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_may_t3_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_may_t3_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_may_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_may_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_may_t4_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_may_t4_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_may_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_may_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_may_t5_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_may_t5_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_may_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_may_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_may_t6_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_may_t6_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_may_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_may_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_nov_t1_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_nov_t1_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_nov_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_nov_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_nov_t2_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_nov_t2_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_nov_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_nov_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_nov_t3_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_nov_t3_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_nov_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_nov_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_nov_t4_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_nov_t4_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_nov_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_nov_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_nov_t5_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_nov_t5_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_nov_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_nov_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_nov_t6_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_nov_t6_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_nov_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_nov_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_oct_t1_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_oct_t1_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_oct_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_oct_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_oct_t2_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_oct_t2_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_oct_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_oct_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_oct_t3_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_oct_t3_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_oct_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_oct_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_oct_t4_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_oct_t4_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_oct_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_oct_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_oct_t5_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_oct_t5_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_oct_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_oct_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_oct_t6_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_oct_t6_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_oct_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_oct_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p10_t1_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p10_t1_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p10_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p10_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p10_t2_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p10_t2_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p10_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p10_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p10_t3_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p10_t3_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p10_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p10_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p10_t4_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p10_t4_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p10_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p10_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p10_t5_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p10_t5_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p10_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p10_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p10_t6_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p10_t6_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p10_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p10_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p11_t1_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p11_t1_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p11_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p11_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p11_t2_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p11_t2_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p11_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p11_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p11_t3_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p11_t3_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p11_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p11_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p11_t4_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p11_t4_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p11_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p11_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p11_t5_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p11_t5_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p11_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p11_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p11_t6_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p11_t6_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p11_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p11_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p12_t1_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p12_t1_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p12_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p12_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p12_t2_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p12_t2_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p12_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p12_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p12_t3_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p12_t3_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p12_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p12_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p12_t4_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p12_t4_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p12_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p12_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p12_t5_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p12_t5_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p12_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p12_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p12_t6_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p12_t6_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p12_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p12_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p1_t1_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p1_t1_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p1_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p1_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p1_t2_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p1_t2_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p1_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p1_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p1_t3_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p1_t3_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p1_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p1_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p1_t4_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p1_t4_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p1_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p1_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p1_t5_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p1_t5_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p1_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p1_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p1_t6_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p1_t6_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p1_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p1_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p2_t1_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p2_t1_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p2_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p2_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p2_t2_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p2_t2_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p2_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p2_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p2_t3_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p2_t3_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p2_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p2_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p2_t4_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p2_t4_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p2_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p2_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p2_t5_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p2_t5_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p2_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p2_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p2_t6_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p2_t6_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p2_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p2_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p3_t1_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p3_t1_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p3_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p3_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p3_t2_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p3_t2_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p3_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p3_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p3_t3_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p3_t3_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p3_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p3_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p3_t4_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p3_t4_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p3_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p3_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p3_t5_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p3_t5_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p3_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p3_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p3_t6_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p3_t6_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p3_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p3_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p4_t1_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p4_t1_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p4_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p4_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p4_t2_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p4_t2_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p4_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p4_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p4_t3_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p4_t3_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p4_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p4_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p4_t4_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p4_t4_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p4_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p4_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p4_t5_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p4_t5_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p4_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p4_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p4_t6_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p4_t6_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p4_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p4_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p5_t1_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p5_t1_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p5_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p5_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p5_t2_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p5_t2_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p5_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p5_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p5_t3_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p5_t3_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p5_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p5_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p5_t4_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p5_t4_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p5_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p5_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p5_t5_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p5_t5_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p5_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p5_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p5_t6_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p5_t6_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p5_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p5_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p6_t1_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p6_t1_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p6_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p6_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p6_t2_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p6_t2_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p6_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p6_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p6_t3_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p6_t3_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p6_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p6_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p6_t4_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p6_t4_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p6_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p6_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p6_t5_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p6_t5_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p6_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p6_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p6_t6_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p6_t6_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p6_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p6_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p7_t1_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p7_t1_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p7_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p7_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p7_t2_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p7_t2_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p7_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p7_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p7_t3_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p7_t3_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p7_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p7_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p7_t4_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p7_t4_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p7_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p7_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p7_t5_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p7_t5_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p7_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p7_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p7_t6_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p7_t6_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p7_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p7_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p8_t1_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p8_t1_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p8_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p8_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p8_t2_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p8_t2_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p8_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p8_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p8_t3_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p8_t3_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p8_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p8_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p8_t4_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p8_t4_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p8_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p8_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p8_t5_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p8_t5_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p8_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p8_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p8_t6_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p8_t6_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p8_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p8_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p9_t1_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p9_t1_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p9_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p9_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p9_t2_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p9_t2_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p9_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p9_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p9_t3_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p9_t3_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p9_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p9_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p9_t4_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p9_t4_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p9_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p9_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p9_t5_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p9_t5_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p9_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p9_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p9_t6_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p9_t6_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_p9_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_p9_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_sched_weekday_mset(SAM_Utilityrate2 ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_dc_sched_weekday", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_sched_weekend_mset(SAM_Utilityrate2 ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_dc_sched_weekend", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_sep_t1_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_sep_t1_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_sep_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_sep_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_sep_t2_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_sep_t2_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_sep_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_sep_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_sep_t3_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_sep_t3_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_sep_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_sep_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_sep_t4_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_sep_t4_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_sep_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_sep_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_sep_t5_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_sep_t5_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_sep_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_sep_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_sep_t6_dc_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_sep_t6_dc", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_dc_sep_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_dc_sep_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_enable_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_enable", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p10_t1_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p10_t1_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p10_t1_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p10_t1_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p10_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p10_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p10_t2_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p10_t2_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p10_t2_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p10_t2_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p10_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p10_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p10_t3_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p10_t3_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p10_t3_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p10_t3_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p10_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p10_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p10_t4_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p10_t4_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p10_t4_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p10_t4_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p10_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p10_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p10_t5_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p10_t5_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p10_t5_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p10_t5_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p10_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p10_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p10_t6_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p10_t6_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p10_t6_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p10_t6_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p10_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p10_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p11_t1_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p11_t1_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p11_t1_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p11_t1_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p11_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p11_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p11_t2_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p11_t2_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p11_t2_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p11_t2_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p11_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p11_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p11_t3_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p11_t3_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p11_t3_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p11_t3_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p11_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p11_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p11_t4_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p11_t4_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p11_t4_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p11_t4_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p11_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p11_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p11_t5_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p11_t5_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p11_t5_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p11_t5_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p11_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p11_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p11_t6_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p11_t6_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p11_t6_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p11_t6_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p11_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p11_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p12_t1_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p12_t1_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p12_t1_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p12_t1_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p12_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p12_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p12_t2_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p12_t2_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p12_t2_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p12_t2_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p12_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p12_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p12_t3_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p12_t3_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p12_t3_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p12_t3_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p12_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p12_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p12_t4_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p12_t4_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p12_t4_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p12_t4_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p12_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p12_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p12_t5_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p12_t5_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p12_t5_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p12_t5_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p12_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p12_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p12_t6_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p12_t6_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p12_t6_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p12_t6_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p12_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p12_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p1_t1_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p1_t1_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p1_t1_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p1_t1_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p1_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p1_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p1_t2_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p1_t2_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p1_t2_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p1_t2_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p1_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p1_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p1_t3_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p1_t3_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p1_t3_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p1_t3_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p1_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p1_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p1_t4_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p1_t4_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p1_t4_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p1_t4_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p1_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p1_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p1_t5_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p1_t5_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p1_t5_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p1_t5_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p1_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p1_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p1_t6_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p1_t6_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p1_t6_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p1_t6_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p1_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p1_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p2_t1_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p2_t1_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p2_t1_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p2_t1_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p2_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p2_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p2_t2_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p2_t2_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p2_t2_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p2_t2_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p2_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p2_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p2_t3_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p2_t3_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p2_t3_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p2_t3_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p2_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p2_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p2_t4_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p2_t4_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p2_t4_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p2_t4_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p2_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p2_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p2_t5_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p2_t5_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p2_t5_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p2_t5_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p2_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p2_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p2_t6_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p2_t6_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p2_t6_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p2_t6_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p2_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p2_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p3_t1_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p3_t1_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p3_t1_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p3_t1_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p3_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p3_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p3_t2_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p3_t2_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p3_t2_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p3_t2_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p3_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p3_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p3_t3_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p3_t3_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p3_t3_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p3_t3_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p3_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p3_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p3_t4_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p3_t4_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p3_t4_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p3_t4_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p3_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p3_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p3_t5_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p3_t5_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p3_t5_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p3_t5_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p3_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p3_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p3_t6_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p3_t6_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p3_t6_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p3_t6_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p3_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p3_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p4_t1_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p4_t1_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p4_t1_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p4_t1_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p4_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p4_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p4_t2_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p4_t2_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p4_t2_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p4_t2_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p4_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p4_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p4_t3_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p4_t3_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p4_t3_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p4_t3_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p4_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p4_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p4_t4_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p4_t4_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p4_t4_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p4_t4_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p4_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p4_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p4_t5_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p4_t5_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p4_t5_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p4_t5_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p4_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p4_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p4_t6_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p4_t6_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p4_t6_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p4_t6_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p4_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p4_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p5_t1_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p5_t1_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p5_t1_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p5_t1_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p5_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p5_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p5_t2_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p5_t2_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p5_t2_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p5_t2_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p5_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p5_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p5_t3_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p5_t3_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p5_t3_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p5_t3_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p5_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p5_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p5_t4_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p5_t4_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p5_t4_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p5_t4_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p5_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p5_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p5_t5_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p5_t5_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p5_t5_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p5_t5_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p5_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p5_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p5_t6_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p5_t6_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p5_t6_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p5_t6_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p5_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p5_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p6_t1_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p6_t1_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p6_t1_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p6_t1_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p6_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p6_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p6_t2_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p6_t2_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p6_t2_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p6_t2_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p6_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p6_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p6_t3_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p6_t3_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p6_t3_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p6_t3_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p6_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p6_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p6_t4_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p6_t4_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p6_t4_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p6_t4_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p6_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p6_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p6_t5_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p6_t5_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p6_t5_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p6_t5_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p6_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p6_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p6_t6_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p6_t6_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p6_t6_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p6_t6_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p6_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p6_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p7_t1_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p7_t1_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p7_t1_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p7_t1_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p7_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p7_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p7_t2_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p7_t2_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p7_t2_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p7_t2_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p7_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p7_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p7_t3_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p7_t3_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p7_t3_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p7_t3_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p7_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p7_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p7_t4_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p7_t4_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p7_t4_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p7_t4_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p7_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p7_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p7_t5_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p7_t5_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p7_t5_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p7_t5_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p7_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p7_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p7_t6_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p7_t6_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p7_t6_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p7_t6_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p7_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p7_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p8_t1_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p8_t1_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p8_t1_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p8_t1_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p8_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p8_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p8_t2_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p8_t2_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p8_t2_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p8_t2_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p8_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p8_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p8_t3_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p8_t3_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p8_t3_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p8_t3_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p8_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p8_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p8_t4_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p8_t4_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p8_t4_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p8_t4_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p8_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p8_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p8_t5_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p8_t5_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p8_t5_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p8_t5_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p8_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p8_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p8_t6_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p8_t6_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p8_t6_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p8_t6_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p8_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p8_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p9_t1_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p9_t1_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p9_t1_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p9_t1_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p9_t1_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p9_t1_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p9_t2_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p9_t2_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p9_t2_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p9_t2_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p9_t2_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p9_t2_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p9_t3_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p9_t3_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p9_t3_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p9_t3_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p9_t3_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p9_t3_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p9_t4_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p9_t4_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p9_t4_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p9_t4_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p9_t4_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p9_t4_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p9_t5_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p9_t5_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p9_t5_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p9_t5_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p9_t5_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p9_t5_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p9_t6_br_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p9_t6_br", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p9_t6_sr_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p9_t6_sr", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_p9_t6_ub_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_ec_p9_t6_ub", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_sched_weekday_mset(SAM_Utilityrate2 ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_ec_sched_weekday", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_ec_sched_weekend_mset(SAM_Utilityrate2 ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_ec_sched_weekend", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_enable_net_metering_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_enable_net_metering", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_flat_buy_rate_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_flat_buy_rate", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_flat_sell_rate_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_flat_sell_rate", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_monthly_fixed_charge_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_monthly_fixed_charge", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_Common_ur_nm_yearend_sell_rate_nset(SAM_Utilityrate2 ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_nm_yearend_sell_rate", number);
	});
}

SAM_EXPORT void SAM_Utilityrate2_AnnualOutput_degradation_aset(SAM_Utilityrate2 ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "degradation", arr, length);
	});
}

SAM_EXPORT double SAM_Utilityrate2_Common_analysis_period_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "analysis_period", &result))
		make_access_error("SAM_Utilityrate2", "analysis_period");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Common_e_load_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "e_load", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "e_load");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Common_hourly_gen_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hourly_gen", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "hourly_gen");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Common_load_escalation_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "load_escalation", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "load_escalation");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Common_p_load_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "p_load", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "p_load");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Common_p_with_system_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "p_with_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "p_with_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Common_rate_escalation_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rate_escalation", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "rate_escalation");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_apr_t1_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_apr_t1_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_apr_t1_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_apr_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_apr_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_apr_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_apr_t2_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_apr_t2_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_apr_t2_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_apr_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_apr_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_apr_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_apr_t3_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_apr_t3_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_apr_t3_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_apr_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_apr_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_apr_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_apr_t4_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_apr_t4_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_apr_t4_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_apr_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_apr_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_apr_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_apr_t5_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_apr_t5_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_apr_t5_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_apr_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_apr_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_apr_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_apr_t6_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_apr_t6_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_apr_t6_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_apr_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_apr_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_apr_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_aug_t1_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_aug_t1_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_aug_t1_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_aug_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_aug_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_aug_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_aug_t2_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_aug_t2_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_aug_t2_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_aug_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_aug_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_aug_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_aug_t3_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_aug_t3_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_aug_t3_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_aug_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_aug_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_aug_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_aug_t4_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_aug_t4_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_aug_t4_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_aug_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_aug_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_aug_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_aug_t5_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_aug_t5_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_aug_t5_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_aug_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_aug_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_aug_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_aug_t6_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_aug_t6_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_aug_t6_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_aug_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_aug_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_aug_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_dec_t1_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_dec_t1_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_dec_t1_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_dec_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_dec_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_dec_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_dec_t2_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_dec_t2_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_dec_t2_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_dec_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_dec_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_dec_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_dec_t3_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_dec_t3_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_dec_t3_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_dec_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_dec_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_dec_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_dec_t4_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_dec_t4_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_dec_t4_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_dec_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_dec_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_dec_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_dec_t5_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_dec_t5_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_dec_t5_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_dec_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_dec_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_dec_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_dec_t6_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_dec_t6_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_dec_t6_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_dec_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_dec_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_dec_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_enable_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_enable", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_enable");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_feb_t1_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_feb_t1_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_feb_t1_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_feb_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_feb_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_feb_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_feb_t2_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_feb_t2_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_feb_t2_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_feb_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_feb_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_feb_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_feb_t3_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_feb_t3_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_feb_t3_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_feb_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_feb_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_feb_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_feb_t4_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_feb_t4_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_feb_t4_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_feb_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_feb_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_feb_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_feb_t5_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_feb_t5_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_feb_t5_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_feb_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_feb_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_feb_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_feb_t6_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_feb_t6_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_feb_t6_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_feb_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_feb_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_feb_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jan_t1_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jan_t1_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jan_t1_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jan_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jan_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jan_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jan_t2_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jan_t2_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jan_t2_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jan_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jan_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jan_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jan_t3_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jan_t3_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jan_t3_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jan_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jan_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jan_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jan_t4_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jan_t4_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jan_t4_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jan_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jan_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jan_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jan_t5_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jan_t5_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jan_t5_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jan_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jan_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jan_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jan_t6_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jan_t6_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jan_t6_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jan_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jan_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jan_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jul_t1_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jul_t1_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jul_t1_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jul_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jul_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jul_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jul_t2_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jul_t2_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jul_t2_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jul_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jul_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jul_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jul_t3_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jul_t3_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jul_t3_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jul_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jul_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jul_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jul_t4_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jul_t4_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jul_t4_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jul_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jul_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jul_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jul_t5_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jul_t5_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jul_t5_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jul_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jul_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jul_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jul_t6_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jul_t6_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jul_t6_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jul_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jul_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jul_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jun_t1_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jun_t1_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jun_t1_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jun_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jun_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jun_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jun_t2_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jun_t2_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jun_t2_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jun_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jun_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jun_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jun_t3_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jun_t3_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jun_t3_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jun_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jun_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jun_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jun_t4_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jun_t4_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jun_t4_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jun_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jun_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jun_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jun_t5_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jun_t5_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jun_t5_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jun_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jun_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jun_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jun_t6_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jun_t6_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jun_t6_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_jun_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_jun_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_jun_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_mar_t1_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_mar_t1_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_mar_t1_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_mar_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_mar_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_mar_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_mar_t2_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_mar_t2_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_mar_t2_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_mar_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_mar_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_mar_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_mar_t3_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_mar_t3_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_mar_t3_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_mar_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_mar_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_mar_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_mar_t4_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_mar_t4_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_mar_t4_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_mar_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_mar_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_mar_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_mar_t5_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_mar_t5_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_mar_t5_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_mar_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_mar_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_mar_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_mar_t6_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_mar_t6_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_mar_t6_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_mar_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_mar_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_mar_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_may_t1_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_may_t1_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_may_t1_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_may_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_may_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_may_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_may_t2_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_may_t2_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_may_t2_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_may_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_may_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_may_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_may_t3_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_may_t3_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_may_t3_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_may_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_may_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_may_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_may_t4_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_may_t4_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_may_t4_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_may_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_may_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_may_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_may_t5_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_may_t5_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_may_t5_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_may_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_may_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_may_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_may_t6_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_may_t6_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_may_t6_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_may_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_may_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_may_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_nov_t1_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_nov_t1_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_nov_t1_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_nov_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_nov_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_nov_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_nov_t2_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_nov_t2_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_nov_t2_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_nov_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_nov_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_nov_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_nov_t3_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_nov_t3_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_nov_t3_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_nov_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_nov_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_nov_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_nov_t4_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_nov_t4_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_nov_t4_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_nov_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_nov_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_nov_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_nov_t5_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_nov_t5_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_nov_t5_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_nov_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_nov_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_nov_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_nov_t6_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_nov_t6_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_nov_t6_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_nov_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_nov_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_nov_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_oct_t1_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_oct_t1_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_oct_t1_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_oct_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_oct_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_oct_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_oct_t2_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_oct_t2_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_oct_t2_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_oct_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_oct_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_oct_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_oct_t3_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_oct_t3_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_oct_t3_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_oct_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_oct_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_oct_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_oct_t4_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_oct_t4_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_oct_t4_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_oct_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_oct_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_oct_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_oct_t5_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_oct_t5_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_oct_t5_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_oct_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_oct_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_oct_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_oct_t6_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_oct_t6_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_oct_t6_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_oct_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_oct_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_oct_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p10_t1_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p10_t1_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p10_t1_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p10_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p10_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p10_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p10_t2_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p10_t2_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p10_t2_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p10_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p10_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p10_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p10_t3_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p10_t3_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p10_t3_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p10_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p10_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p10_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p10_t4_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p10_t4_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p10_t4_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p10_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p10_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p10_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p10_t5_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p10_t5_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p10_t5_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p10_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p10_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p10_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p10_t6_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p10_t6_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p10_t6_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p10_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p10_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p10_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p11_t1_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p11_t1_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p11_t1_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p11_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p11_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p11_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p11_t2_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p11_t2_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p11_t2_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p11_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p11_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p11_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p11_t3_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p11_t3_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p11_t3_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p11_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p11_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p11_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p11_t4_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p11_t4_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p11_t4_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p11_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p11_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p11_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p11_t5_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p11_t5_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p11_t5_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p11_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p11_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p11_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p11_t6_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p11_t6_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p11_t6_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p11_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p11_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p11_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p12_t1_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p12_t1_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p12_t1_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p12_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p12_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p12_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p12_t2_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p12_t2_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p12_t2_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p12_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p12_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p12_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p12_t3_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p12_t3_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p12_t3_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p12_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p12_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p12_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p12_t4_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p12_t4_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p12_t4_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p12_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p12_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p12_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p12_t5_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p12_t5_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p12_t5_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p12_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p12_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p12_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p12_t6_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p12_t6_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p12_t6_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p12_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p12_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p12_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p1_t1_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p1_t1_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p1_t1_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p1_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p1_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p1_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p1_t2_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p1_t2_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p1_t2_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p1_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p1_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p1_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p1_t3_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p1_t3_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p1_t3_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p1_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p1_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p1_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p1_t4_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p1_t4_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p1_t4_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p1_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p1_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p1_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p1_t5_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p1_t5_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p1_t5_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p1_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p1_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p1_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p1_t6_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p1_t6_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p1_t6_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p1_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p1_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p1_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p2_t1_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p2_t1_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p2_t1_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p2_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p2_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p2_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p2_t2_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p2_t2_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p2_t2_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p2_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p2_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p2_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p2_t3_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p2_t3_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p2_t3_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p2_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p2_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p2_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p2_t4_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p2_t4_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p2_t4_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p2_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p2_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p2_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p2_t5_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p2_t5_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p2_t5_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p2_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p2_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p2_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p2_t6_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p2_t6_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p2_t6_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p2_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p2_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p2_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p3_t1_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p3_t1_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p3_t1_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p3_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p3_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p3_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p3_t2_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p3_t2_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p3_t2_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p3_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p3_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p3_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p3_t3_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p3_t3_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p3_t3_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p3_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p3_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p3_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p3_t4_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p3_t4_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p3_t4_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p3_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p3_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p3_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p3_t5_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p3_t5_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p3_t5_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p3_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p3_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p3_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p3_t6_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p3_t6_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p3_t6_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p3_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p3_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p3_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p4_t1_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p4_t1_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p4_t1_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p4_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p4_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p4_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p4_t2_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p4_t2_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p4_t2_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p4_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p4_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p4_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p4_t3_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p4_t3_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p4_t3_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p4_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p4_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p4_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p4_t4_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p4_t4_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p4_t4_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p4_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p4_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p4_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p4_t5_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p4_t5_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p4_t5_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p4_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p4_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p4_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p4_t6_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p4_t6_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p4_t6_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p4_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p4_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p4_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p5_t1_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p5_t1_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p5_t1_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p5_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p5_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p5_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p5_t2_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p5_t2_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p5_t2_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p5_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p5_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p5_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p5_t3_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p5_t3_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p5_t3_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p5_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p5_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p5_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p5_t4_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p5_t4_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p5_t4_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p5_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p5_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p5_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p5_t5_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p5_t5_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p5_t5_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p5_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p5_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p5_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p5_t6_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p5_t6_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p5_t6_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p5_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p5_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p5_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p6_t1_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p6_t1_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p6_t1_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p6_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p6_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p6_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p6_t2_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p6_t2_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p6_t2_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p6_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p6_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p6_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p6_t3_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p6_t3_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p6_t3_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p6_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p6_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p6_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p6_t4_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p6_t4_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p6_t4_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p6_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p6_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p6_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p6_t5_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p6_t5_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p6_t5_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p6_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p6_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p6_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p6_t6_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p6_t6_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p6_t6_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p6_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p6_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p6_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p7_t1_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p7_t1_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p7_t1_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p7_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p7_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p7_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p7_t2_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p7_t2_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p7_t2_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p7_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p7_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p7_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p7_t3_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p7_t3_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p7_t3_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p7_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p7_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p7_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p7_t4_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p7_t4_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p7_t4_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p7_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p7_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p7_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p7_t5_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p7_t5_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p7_t5_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p7_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p7_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p7_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p7_t6_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p7_t6_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p7_t6_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p7_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p7_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p7_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p8_t1_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p8_t1_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p8_t1_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p8_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p8_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p8_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p8_t2_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p8_t2_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p8_t2_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p8_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p8_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p8_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p8_t3_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p8_t3_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p8_t3_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p8_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p8_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p8_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p8_t4_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p8_t4_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p8_t4_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p8_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p8_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p8_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p8_t5_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p8_t5_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p8_t5_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p8_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p8_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p8_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p8_t6_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p8_t6_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p8_t6_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p8_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p8_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p8_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p9_t1_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p9_t1_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p9_t1_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p9_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p9_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p9_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p9_t2_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p9_t2_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p9_t2_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p9_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p9_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p9_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p9_t3_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p9_t3_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p9_t3_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p9_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p9_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p9_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p9_t4_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p9_t4_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p9_t4_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p9_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p9_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p9_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p9_t5_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p9_t5_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p9_t5_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p9_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p9_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p9_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p9_t6_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p9_t6_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p9_t6_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_p9_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_p9_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_p9_t6_ub");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Common_ur_dc_sched_weekday_mget(SAM_Utilityrate2 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_dc_sched_weekday", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate2", "ur_dc_sched_weekday");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Common_ur_dc_sched_weekend_mget(SAM_Utilityrate2 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_dc_sched_weekend", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate2", "ur_dc_sched_weekend");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_sep_t1_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_sep_t1_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_sep_t1_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_sep_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_sep_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_sep_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_sep_t2_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_sep_t2_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_sep_t2_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_sep_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_sep_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_sep_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_sep_t3_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_sep_t3_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_sep_t3_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_sep_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_sep_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_sep_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_sep_t4_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_sep_t4_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_sep_t4_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_sep_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_sep_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_sep_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_sep_t5_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_sep_t5_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_sep_t5_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_sep_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_sep_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_sep_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_sep_t6_dc_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_sep_t6_dc", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_sep_t6_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_dc_sep_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_dc_sep_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_dc_sep_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_enable_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_enable", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_enable");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p10_t1_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p10_t1_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p10_t1_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p10_t1_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p10_t1_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p10_t1_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p10_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p10_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p10_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p10_t2_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p10_t2_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p10_t2_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p10_t2_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p10_t2_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p10_t2_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p10_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p10_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p10_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p10_t3_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p10_t3_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p10_t3_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p10_t3_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p10_t3_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p10_t3_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p10_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p10_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p10_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p10_t4_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p10_t4_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p10_t4_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p10_t4_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p10_t4_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p10_t4_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p10_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p10_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p10_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p10_t5_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p10_t5_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p10_t5_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p10_t5_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p10_t5_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p10_t5_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p10_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p10_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p10_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p10_t6_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p10_t6_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p10_t6_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p10_t6_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p10_t6_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p10_t6_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p10_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p10_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p10_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p11_t1_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p11_t1_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p11_t1_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p11_t1_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p11_t1_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p11_t1_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p11_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p11_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p11_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p11_t2_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p11_t2_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p11_t2_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p11_t2_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p11_t2_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p11_t2_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p11_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p11_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p11_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p11_t3_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p11_t3_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p11_t3_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p11_t3_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p11_t3_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p11_t3_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p11_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p11_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p11_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p11_t4_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p11_t4_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p11_t4_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p11_t4_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p11_t4_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p11_t4_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p11_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p11_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p11_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p11_t5_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p11_t5_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p11_t5_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p11_t5_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p11_t5_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p11_t5_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p11_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p11_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p11_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p11_t6_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p11_t6_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p11_t6_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p11_t6_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p11_t6_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p11_t6_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p11_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p11_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p11_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p12_t1_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p12_t1_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p12_t1_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p12_t1_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p12_t1_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p12_t1_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p12_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p12_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p12_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p12_t2_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p12_t2_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p12_t2_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p12_t2_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p12_t2_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p12_t2_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p12_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p12_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p12_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p12_t3_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p12_t3_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p12_t3_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p12_t3_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p12_t3_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p12_t3_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p12_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p12_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p12_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p12_t4_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p12_t4_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p12_t4_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p12_t4_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p12_t4_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p12_t4_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p12_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p12_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p12_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p12_t5_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p12_t5_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p12_t5_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p12_t5_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p12_t5_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p12_t5_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p12_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p12_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p12_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p12_t6_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p12_t6_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p12_t6_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p12_t6_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p12_t6_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p12_t6_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p12_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p12_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p12_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p1_t1_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p1_t1_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p1_t1_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p1_t1_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p1_t1_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p1_t1_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p1_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p1_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p1_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p1_t2_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p1_t2_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p1_t2_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p1_t2_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p1_t2_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p1_t2_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p1_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p1_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p1_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p1_t3_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p1_t3_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p1_t3_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p1_t3_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p1_t3_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p1_t3_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p1_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p1_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p1_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p1_t4_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p1_t4_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p1_t4_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p1_t4_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p1_t4_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p1_t4_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p1_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p1_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p1_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p1_t5_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p1_t5_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p1_t5_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p1_t5_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p1_t5_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p1_t5_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p1_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p1_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p1_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p1_t6_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p1_t6_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p1_t6_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p1_t6_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p1_t6_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p1_t6_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p1_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p1_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p1_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p2_t1_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p2_t1_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p2_t1_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p2_t1_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p2_t1_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p2_t1_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p2_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p2_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p2_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p2_t2_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p2_t2_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p2_t2_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p2_t2_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p2_t2_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p2_t2_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p2_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p2_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p2_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p2_t3_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p2_t3_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p2_t3_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p2_t3_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p2_t3_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p2_t3_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p2_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p2_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p2_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p2_t4_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p2_t4_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p2_t4_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p2_t4_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p2_t4_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p2_t4_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p2_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p2_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p2_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p2_t5_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p2_t5_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p2_t5_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p2_t5_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p2_t5_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p2_t5_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p2_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p2_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p2_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p2_t6_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p2_t6_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p2_t6_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p2_t6_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p2_t6_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p2_t6_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p2_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p2_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p2_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p3_t1_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p3_t1_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p3_t1_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p3_t1_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p3_t1_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p3_t1_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p3_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p3_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p3_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p3_t2_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p3_t2_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p3_t2_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p3_t2_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p3_t2_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p3_t2_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p3_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p3_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p3_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p3_t3_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p3_t3_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p3_t3_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p3_t3_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p3_t3_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p3_t3_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p3_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p3_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p3_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p3_t4_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p3_t4_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p3_t4_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p3_t4_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p3_t4_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p3_t4_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p3_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p3_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p3_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p3_t5_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p3_t5_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p3_t5_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p3_t5_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p3_t5_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p3_t5_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p3_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p3_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p3_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p3_t6_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p3_t6_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p3_t6_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p3_t6_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p3_t6_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p3_t6_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p3_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p3_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p3_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p4_t1_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p4_t1_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p4_t1_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p4_t1_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p4_t1_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p4_t1_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p4_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p4_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p4_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p4_t2_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p4_t2_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p4_t2_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p4_t2_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p4_t2_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p4_t2_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p4_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p4_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p4_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p4_t3_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p4_t3_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p4_t3_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p4_t3_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p4_t3_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p4_t3_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p4_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p4_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p4_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p4_t4_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p4_t4_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p4_t4_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p4_t4_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p4_t4_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p4_t4_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p4_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p4_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p4_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p4_t5_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p4_t5_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p4_t5_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p4_t5_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p4_t5_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p4_t5_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p4_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p4_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p4_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p4_t6_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p4_t6_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p4_t6_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p4_t6_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p4_t6_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p4_t6_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p4_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p4_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p4_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p5_t1_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p5_t1_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p5_t1_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p5_t1_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p5_t1_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p5_t1_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p5_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p5_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p5_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p5_t2_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p5_t2_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p5_t2_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p5_t2_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p5_t2_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p5_t2_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p5_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p5_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p5_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p5_t3_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p5_t3_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p5_t3_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p5_t3_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p5_t3_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p5_t3_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p5_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p5_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p5_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p5_t4_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p5_t4_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p5_t4_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p5_t4_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p5_t4_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p5_t4_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p5_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p5_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p5_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p5_t5_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p5_t5_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p5_t5_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p5_t5_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p5_t5_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p5_t5_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p5_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p5_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p5_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p5_t6_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p5_t6_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p5_t6_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p5_t6_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p5_t6_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p5_t6_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p5_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p5_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p5_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p6_t1_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p6_t1_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p6_t1_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p6_t1_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p6_t1_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p6_t1_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p6_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p6_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p6_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p6_t2_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p6_t2_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p6_t2_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p6_t2_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p6_t2_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p6_t2_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p6_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p6_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p6_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p6_t3_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p6_t3_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p6_t3_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p6_t3_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p6_t3_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p6_t3_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p6_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p6_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p6_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p6_t4_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p6_t4_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p6_t4_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p6_t4_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p6_t4_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p6_t4_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p6_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p6_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p6_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p6_t5_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p6_t5_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p6_t5_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p6_t5_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p6_t5_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p6_t5_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p6_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p6_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p6_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p6_t6_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p6_t6_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p6_t6_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p6_t6_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p6_t6_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p6_t6_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p6_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p6_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p6_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p7_t1_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p7_t1_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p7_t1_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p7_t1_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p7_t1_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p7_t1_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p7_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p7_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p7_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p7_t2_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p7_t2_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p7_t2_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p7_t2_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p7_t2_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p7_t2_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p7_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p7_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p7_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p7_t3_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p7_t3_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p7_t3_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p7_t3_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p7_t3_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p7_t3_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p7_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p7_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p7_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p7_t4_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p7_t4_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p7_t4_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p7_t4_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p7_t4_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p7_t4_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p7_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p7_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p7_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p7_t5_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p7_t5_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p7_t5_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p7_t5_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p7_t5_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p7_t5_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p7_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p7_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p7_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p7_t6_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p7_t6_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p7_t6_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p7_t6_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p7_t6_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p7_t6_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p7_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p7_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p7_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p8_t1_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p8_t1_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p8_t1_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p8_t1_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p8_t1_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p8_t1_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p8_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p8_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p8_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p8_t2_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p8_t2_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p8_t2_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p8_t2_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p8_t2_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p8_t2_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p8_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p8_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p8_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p8_t3_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p8_t3_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p8_t3_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p8_t3_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p8_t3_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p8_t3_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p8_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p8_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p8_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p8_t4_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p8_t4_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p8_t4_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p8_t4_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p8_t4_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p8_t4_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p8_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p8_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p8_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p8_t5_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p8_t5_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p8_t5_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p8_t5_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p8_t5_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p8_t5_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p8_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p8_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p8_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p8_t6_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p8_t6_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p8_t6_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p8_t6_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p8_t6_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p8_t6_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p8_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p8_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p8_t6_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p9_t1_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p9_t1_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p9_t1_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p9_t1_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p9_t1_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p9_t1_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p9_t1_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p9_t1_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p9_t1_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p9_t2_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p9_t2_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p9_t2_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p9_t2_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p9_t2_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p9_t2_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p9_t2_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p9_t2_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p9_t2_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p9_t3_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p9_t3_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p9_t3_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p9_t3_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p9_t3_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p9_t3_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p9_t3_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p9_t3_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p9_t3_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p9_t4_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p9_t4_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p9_t4_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p9_t4_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p9_t4_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p9_t4_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p9_t4_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p9_t4_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p9_t4_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p9_t5_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p9_t5_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p9_t5_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p9_t5_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p9_t5_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p9_t5_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p9_t5_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p9_t5_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p9_t5_ub");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p9_t6_br_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p9_t6_br", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p9_t6_br");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p9_t6_sr_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p9_t6_sr", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p9_t6_sr");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_ec_p9_t6_ub_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_ec_p9_t6_ub", &result))
		make_access_error("SAM_Utilityrate2", "ur_ec_p9_t6_ub");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Common_ur_ec_sched_weekday_mget(SAM_Utilityrate2 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_ec_sched_weekday", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate2", "ur_ec_sched_weekday");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Common_ur_ec_sched_weekend_mget(SAM_Utilityrate2 ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_ec_sched_weekend", nrows, ncols);
	if (!result)
		make_access_error("SAM_Utilityrate2", "ur_ec_sched_weekend");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_enable_net_metering_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_enable_net_metering", &result))
		make_access_error("SAM_Utilityrate2", "ur_enable_net_metering");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_flat_buy_rate_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_flat_buy_rate", &result))
		make_access_error("SAM_Utilityrate2", "ur_flat_buy_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_flat_sell_rate_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_flat_sell_rate", &result))
		make_access_error("SAM_Utilityrate2", "ur_flat_sell_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_monthly_fixed_charge_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_monthly_fixed_charge", &result))
		make_access_error("SAM_Utilityrate2", "ur_monthly_fixed_charge");
	});
	return result;
}



SAM_EXPORT double SAM_Utilityrate2_Common_ur_nm_yearend_sell_rate_nget(SAM_Utilityrate2 ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_nm_yearend_sell_rate", &result))
		make_access_error("SAM_Utilityrate2", "ur_nm_yearend_sell_rate");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_AnnualOutput_degradation_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "degradation", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "degradation");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_annual_energy_value_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "annual_energy_value", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "annual_energy_value");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_dc_fixed_apr_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_fixed_apr", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_dc_fixed_apr");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_dc_fixed_aug_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_fixed_aug", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_dc_fixed_aug");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_dc_fixed_dec_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_fixed_dec", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_dc_fixed_dec");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_dc_fixed_feb_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_fixed_feb", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_dc_fixed_feb");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_dc_fixed_jan_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_fixed_jan", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_dc_fixed_jan");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_dc_fixed_jul_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_fixed_jul", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_dc_fixed_jul");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_dc_fixed_jun_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_fixed_jun", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_dc_fixed_jun");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_dc_fixed_mar_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_fixed_mar", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_dc_fixed_mar");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_dc_fixed_may_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_fixed_may", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_dc_fixed_may");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_dc_fixed_nov_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_fixed_nov", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_dc_fixed_nov");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_dc_fixed_oct_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_fixed_oct", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_dc_fixed_oct");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_dc_fixed_sep_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_fixed_sep", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_dc_fixed_sep");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_dc_tou_apr_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_tou_apr", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_dc_tou_apr");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_dc_tou_aug_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_tou_aug", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_dc_tou_aug");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_dc_tou_dec_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_tou_dec", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_dc_tou_dec");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_dc_tou_feb_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_tou_feb", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_dc_tou_feb");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_dc_tou_jan_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_tou_jan", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_dc_tou_jan");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_dc_tou_jul_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_tou_jul", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_dc_tou_jul");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_dc_tou_jun_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_tou_jun", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_dc_tou_jun");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_dc_tou_mar_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_tou_mar", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_dc_tou_mar");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_dc_tou_may_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_tou_may", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_dc_tou_may");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_dc_tou_nov_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_tou_nov", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_dc_tou_nov");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_dc_tou_oct_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_tou_oct", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_dc_tou_oct");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_dc_tou_sep_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_dc_tou_sep", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_dc_tou_sep");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_ec_apr_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_ec_apr", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_ec_apr");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_ec_aug_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_ec_aug", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_ec_aug");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_ec_dec_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_ec_dec", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_ec_dec");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_ec_feb_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_ec_feb", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_ec_feb");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_ec_jan_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_ec_jan", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_ec_jan");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_ec_jul_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_ec_jul", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_ec_jul");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_ec_jun_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_ec_jun", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_ec_jun");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_ec_mar_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_ec_mar", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_ec_mar");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_ec_may_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_ec_may", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_ec_may");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_ec_nov_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_ec_nov", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_ec_nov");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_ec_oct_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_ec_oct", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_ec_oct");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_charge_ec_sep_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "charge_ec_sep", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "charge_ec_sep");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_elec_cost_with_system_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "elec_cost_with_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "elec_cost_with_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_elec_cost_without_system_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "elec_cost_without_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "elec_cost_without_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_year1_hourly_dc_tou_schedule_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_dc_tou_schedule", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "year1_hourly_dc_tou_schedule");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_year1_hourly_dc_with_system_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_dc_with_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "year1_hourly_dc_with_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_year1_hourly_dc_without_system_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_dc_without_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "year1_hourly_dc_without_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_year1_hourly_e_tofromgrid_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_e_tofromgrid", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "year1_hourly_e_tofromgrid");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_year1_hourly_ec_tou_schedule_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_ec_tou_schedule", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "year1_hourly_ec_tou_schedule");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_year1_hourly_load_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_load", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "year1_hourly_load");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_year1_hourly_p_system_to_load_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_p_system_to_load", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "year1_hourly_p_system_to_load");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_year1_hourly_p_tofromgrid_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_p_tofromgrid", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "year1_hourly_p_tofromgrid");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_year1_hourly_salespurchases_with_system_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_salespurchases_with_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "year1_hourly_salespurchases_with_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_year1_hourly_salespurchases_without_system_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_salespurchases_without_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "year1_hourly_salespurchases_without_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_year1_monthly_cumulative_excess_generation_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_cumulative_excess_generation", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "year1_monthly_cumulative_excess_generation");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_year1_monthly_dc_fixed_with_system_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_dc_fixed_with_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "year1_monthly_dc_fixed_with_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_year1_monthly_dc_fixed_without_system_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_dc_fixed_without_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "year1_monthly_dc_fixed_without_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_year1_monthly_dc_tou_with_system_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_dc_tou_with_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "year1_monthly_dc_tou_with_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_year1_monthly_dc_tou_without_system_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_dc_tou_without_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "year1_monthly_dc_tou_without_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_year1_monthly_ec_charge_with_system_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_ec_charge_with_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "year1_monthly_ec_charge_with_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_year1_monthly_ec_charge_without_system_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_ec_charge_without_system", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "year1_monthly_ec_charge_without_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_year1_monthly_electricity_to_grid_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_electricity_to_grid", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "year1_monthly_electricity_to_grid");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_year1_monthly_load_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_load", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "year1_monthly_load");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_year1_monthly_salespurchases_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_salespurchases", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "year1_monthly_salespurchases");
	});
	return result;
}



SAM_EXPORT double* SAM_Utilityrate2_Outputs_year1_monthly_salespurchases_wo_sys_aget(SAM_Utilityrate2 ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_salespurchases_wo_sys", length);
	if (!result)
		make_access_error("SAM_Utilityrate2", "year1_monthly_salespurchases_wo_sys");
	});
	return result;
}




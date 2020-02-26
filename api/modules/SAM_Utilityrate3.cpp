#include
<string>
#include
<utility>
#include
<vector>
#include
<memory>
#include
<iostream>

#include
<ssc/sscapi.h>

#include
"SAM_api.h"
#include
"ErrorHandler.h"
#include
"SAM_Utilityrate3.h"

SAM_EXPORT SAM_Utilityrate3 SAM_Utilityrate3_construct(const char* def, SAM_error* err){
SAM_Utilityrate3 result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_create();
});
return result;
}

SAM_EXPORT int SAM_Utilityrate3_execute(SAM_Utilityrate3 data, int verbosity, SAM_error* err){
int n_err = 0;
translateExceptions(err,[&]{
n_err += SAM_module_exec("utilityrate3", data, verbosity, err);
});
return n_err;
}


SAM_EXPORT void SAM_Utilityrate3_destruct(SAM_Utilityrate3 system)
{
ssc_data_free(system);
}

SAM_EXPORT void SAM_Utilityrate3_Common_analysis_period_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "analysis_period", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_load_escalation_aset(SAM_Utilityrate3 ptr, double* arr, int length, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_array(ptr, "load_escalation", arr, length);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_rate_escalation_aset(SAM_Utilityrate3 ptr, double* arr, int length, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_array(ptr, "rate_escalation", arr, length);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_system_use_lifetime_output_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "system_use_lifetime_output", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_annual_min_charge_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_annual_min_charge", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_apr_t1_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_apr_t1_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_apr_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_apr_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_apr_t2_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_apr_t2_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_apr_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_apr_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_apr_t3_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_apr_t3_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_apr_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_apr_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_apr_t4_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_apr_t4_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_apr_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_apr_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_apr_t5_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_apr_t5_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_apr_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_apr_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_apr_t6_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_apr_t6_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_apr_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_apr_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_aug_t1_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_aug_t1_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_aug_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_aug_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_aug_t2_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_aug_t2_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_aug_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_aug_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_aug_t3_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_aug_t3_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_aug_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_aug_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_aug_t4_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_aug_t4_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_aug_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_aug_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_aug_t5_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_aug_t5_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_aug_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_aug_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_aug_t6_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_aug_t6_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_aug_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_aug_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_dec_t1_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_dec_t1_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_dec_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_dec_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_dec_t2_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_dec_t2_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_dec_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_dec_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_dec_t3_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_dec_t3_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_dec_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_dec_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_dec_t4_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_dec_t4_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_dec_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_dec_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_dec_t5_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_dec_t5_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_dec_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_dec_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_dec_t6_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_dec_t6_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_dec_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_dec_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_enable_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_enable", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_feb_t1_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_feb_t1_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_feb_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_feb_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_feb_t2_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_feb_t2_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_feb_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_feb_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_feb_t3_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_feb_t3_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_feb_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_feb_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_feb_t4_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_feb_t4_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_feb_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_feb_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_feb_t5_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_feb_t5_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_feb_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_feb_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_feb_t6_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_feb_t6_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_feb_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_feb_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jan_t1_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jan_t1_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jan_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jan_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jan_t2_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jan_t2_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jan_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jan_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jan_t3_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jan_t3_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jan_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jan_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jan_t4_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jan_t4_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jan_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jan_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jan_t5_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jan_t5_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jan_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jan_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jan_t6_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jan_t6_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jan_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jan_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jul_t1_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jul_t1_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jul_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jul_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jul_t2_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jul_t2_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jul_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jul_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jul_t3_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jul_t3_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jul_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jul_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jul_t4_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jul_t4_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jul_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jul_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jul_t5_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jul_t5_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jul_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jul_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jul_t6_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jul_t6_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jul_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jul_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jun_t1_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jun_t1_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jun_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jun_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jun_t2_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jun_t2_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jun_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jun_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jun_t3_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jun_t3_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jun_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jun_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jun_t4_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jun_t4_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jun_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jun_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jun_t5_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jun_t5_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jun_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jun_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jun_t6_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jun_t6_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_jun_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_jun_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_mar_t1_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_mar_t1_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_mar_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_mar_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_mar_t2_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_mar_t2_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_mar_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_mar_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_mar_t3_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_mar_t3_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_mar_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_mar_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_mar_t4_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_mar_t4_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_mar_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_mar_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_mar_t5_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_mar_t5_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_mar_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_mar_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_mar_t6_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_mar_t6_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_mar_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_mar_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_may_t1_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_may_t1_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_may_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_may_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_may_t2_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_may_t2_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_may_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_may_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_may_t3_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_may_t3_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_may_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_may_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_may_t4_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_may_t4_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_may_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_may_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_may_t5_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_may_t5_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_may_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_may_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_may_t6_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_may_t6_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_may_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_may_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_nov_t1_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_nov_t1_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_nov_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_nov_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_nov_t2_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_nov_t2_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_nov_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_nov_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_nov_t3_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_nov_t3_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_nov_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_nov_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_nov_t4_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_nov_t4_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_nov_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_nov_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_nov_t5_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_nov_t5_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_nov_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_nov_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_nov_t6_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_nov_t6_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_nov_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_nov_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_oct_t1_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_oct_t1_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_oct_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_oct_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_oct_t2_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_oct_t2_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_oct_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_oct_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_oct_t3_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_oct_t3_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_oct_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_oct_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_oct_t4_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_oct_t4_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_oct_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_oct_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_oct_t5_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_oct_t5_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_oct_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_oct_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_oct_t6_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_oct_t6_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_oct_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_oct_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p10_t1_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p10_t1_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p10_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p10_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p10_t2_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p10_t2_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p10_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p10_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p10_t3_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p10_t3_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p10_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p10_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p10_t4_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p10_t4_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p10_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p10_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p10_t5_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p10_t5_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p10_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p10_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p10_t6_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p10_t6_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p10_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p10_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p11_t1_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p11_t1_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p11_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p11_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p11_t2_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p11_t2_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p11_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p11_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p11_t3_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p11_t3_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p11_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p11_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p11_t4_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p11_t4_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p11_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p11_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p11_t5_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p11_t5_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p11_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p11_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p11_t6_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p11_t6_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p11_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p11_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p12_t1_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p12_t1_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p12_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p12_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p12_t2_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p12_t2_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p12_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p12_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p12_t3_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p12_t3_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p12_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p12_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p12_t4_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p12_t4_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p12_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p12_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p12_t5_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p12_t5_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p12_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p12_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p12_t6_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p12_t6_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p12_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p12_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p1_t1_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p1_t1_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p1_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p1_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p1_t2_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p1_t2_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p1_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p1_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p1_t3_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p1_t3_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p1_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p1_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p1_t4_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p1_t4_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p1_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p1_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p1_t5_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p1_t5_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p1_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p1_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p1_t6_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p1_t6_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p1_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p1_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p2_t1_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p2_t1_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p2_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p2_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p2_t2_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p2_t2_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p2_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p2_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p2_t3_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p2_t3_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p2_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p2_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p2_t4_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p2_t4_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p2_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p2_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p2_t5_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p2_t5_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p2_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p2_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p2_t6_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p2_t6_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p2_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p2_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p3_t1_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p3_t1_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p3_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p3_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p3_t2_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p3_t2_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p3_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p3_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p3_t3_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p3_t3_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p3_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p3_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p3_t4_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p3_t4_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p3_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p3_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p3_t5_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p3_t5_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p3_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p3_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p3_t6_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p3_t6_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p3_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p3_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p4_t1_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p4_t1_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p4_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p4_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p4_t2_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p4_t2_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p4_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p4_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p4_t3_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p4_t3_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p4_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p4_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p4_t4_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p4_t4_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p4_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p4_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p4_t5_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p4_t5_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p4_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p4_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p4_t6_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p4_t6_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p4_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p4_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p5_t1_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p5_t1_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p5_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p5_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p5_t2_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p5_t2_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p5_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p5_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p5_t3_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p5_t3_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p5_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p5_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p5_t4_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p5_t4_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p5_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p5_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p5_t5_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p5_t5_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p5_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p5_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p5_t6_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p5_t6_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p5_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p5_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p6_t1_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p6_t1_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p6_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p6_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p6_t2_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p6_t2_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p6_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p6_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p6_t3_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p6_t3_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p6_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p6_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p6_t4_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p6_t4_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p6_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p6_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p6_t5_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p6_t5_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p6_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p6_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p6_t6_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p6_t6_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p6_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p6_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p7_t1_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p7_t1_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p7_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p7_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p7_t2_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p7_t2_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p7_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p7_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p7_t3_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p7_t3_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p7_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p7_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p7_t4_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p7_t4_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p7_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p7_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p7_t5_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p7_t5_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p7_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p7_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p7_t6_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p7_t6_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p7_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p7_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p8_t1_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p8_t1_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p8_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p8_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p8_t2_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p8_t2_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p8_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p8_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p8_t3_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p8_t3_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p8_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p8_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p8_t4_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p8_t4_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p8_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p8_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p8_t5_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p8_t5_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p8_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p8_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p8_t6_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p8_t6_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p8_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p8_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p9_t1_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p9_t1_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p9_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p9_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p9_t2_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p9_t2_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p9_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p9_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p9_t3_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p9_t3_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p9_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p9_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p9_t4_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p9_t4_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p9_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p9_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p9_t5_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p9_t5_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p9_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p9_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p9_t6_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p9_t6_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_p9_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_p9_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_sched_weekday_mset(SAM_Utilityrate3 ptr, double* mat, int nrows, int ncols, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_matrix(ptr, "ur_dc_sched_weekday", mat, nrows, ncols);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_sched_weekend_mset(SAM_Utilityrate3 ptr, double* mat, int nrows, int ncols, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_matrix(ptr, "ur_dc_sched_weekend", mat, nrows, ncols);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_sep_t1_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_sep_t1_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_sep_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_sep_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_sep_t2_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_sep_t2_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_sep_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_sep_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_sep_t3_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_sep_t3_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_sep_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_sep_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_sep_t4_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_sep_t4_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_sep_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_sep_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_sep_t5_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_sep_t5_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_sep_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_sep_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_sep_t6_dc_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_sep_t6_dc", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_dc_sep_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_dc_sep_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_enable_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_enable", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p10_t1_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p10_t1_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p10_t1_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p10_t1_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p10_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p10_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p10_t2_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p10_t2_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p10_t2_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p10_t2_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p10_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p10_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p10_t3_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p10_t3_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p10_t3_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p10_t3_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p10_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p10_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p10_t4_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p10_t4_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p10_t4_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p10_t4_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p10_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p10_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p10_t5_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p10_t5_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p10_t5_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p10_t5_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p10_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p10_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p10_t6_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p10_t6_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p10_t6_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p10_t6_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p10_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p10_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p11_t1_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p11_t1_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p11_t1_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p11_t1_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p11_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p11_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p11_t2_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p11_t2_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p11_t2_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p11_t2_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p11_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p11_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p11_t3_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p11_t3_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p11_t3_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p11_t3_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p11_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p11_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p11_t4_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p11_t4_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p11_t4_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p11_t4_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p11_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p11_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p11_t5_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p11_t5_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p11_t5_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p11_t5_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p11_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p11_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p11_t6_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p11_t6_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p11_t6_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p11_t6_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p11_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p11_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p12_t1_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p12_t1_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p12_t1_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p12_t1_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p12_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p12_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p12_t2_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p12_t2_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p12_t2_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p12_t2_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p12_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p12_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p12_t3_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p12_t3_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p12_t3_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p12_t3_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p12_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p12_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p12_t4_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p12_t4_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p12_t4_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p12_t4_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p12_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p12_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p12_t5_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p12_t5_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p12_t5_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p12_t5_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p12_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p12_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p12_t6_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p12_t6_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p12_t6_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p12_t6_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p12_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p12_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p1_t1_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p1_t1_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p1_t1_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p1_t1_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p1_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p1_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p1_t2_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p1_t2_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p1_t2_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p1_t2_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p1_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p1_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p1_t3_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p1_t3_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p1_t3_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p1_t3_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p1_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p1_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p1_t4_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p1_t4_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p1_t4_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p1_t4_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p1_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p1_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p1_t5_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p1_t5_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p1_t5_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p1_t5_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p1_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p1_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p1_t6_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p1_t6_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p1_t6_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p1_t6_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p1_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p1_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p2_t1_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p2_t1_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p2_t1_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p2_t1_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p2_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p2_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p2_t2_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p2_t2_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p2_t2_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p2_t2_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p2_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p2_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p2_t3_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p2_t3_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p2_t3_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p2_t3_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p2_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p2_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p2_t4_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p2_t4_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p2_t4_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p2_t4_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p2_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p2_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p2_t5_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p2_t5_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p2_t5_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p2_t5_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p2_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p2_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p2_t6_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p2_t6_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p2_t6_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p2_t6_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p2_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p2_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p3_t1_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p3_t1_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p3_t1_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p3_t1_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p3_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p3_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p3_t2_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p3_t2_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p3_t2_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p3_t2_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p3_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p3_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p3_t3_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p3_t3_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p3_t3_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p3_t3_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p3_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p3_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p3_t4_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p3_t4_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p3_t4_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p3_t4_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p3_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p3_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p3_t5_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p3_t5_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p3_t5_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p3_t5_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p3_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p3_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p3_t6_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p3_t6_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p3_t6_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p3_t6_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p3_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p3_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p4_t1_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p4_t1_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p4_t1_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p4_t1_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p4_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p4_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p4_t2_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p4_t2_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p4_t2_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p4_t2_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p4_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p4_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p4_t3_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p4_t3_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p4_t3_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p4_t3_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p4_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p4_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p4_t4_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p4_t4_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p4_t4_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p4_t4_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p4_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p4_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p4_t5_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p4_t5_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p4_t5_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p4_t5_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p4_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p4_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p4_t6_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p4_t6_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p4_t6_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p4_t6_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p4_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p4_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p5_t1_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p5_t1_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p5_t1_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p5_t1_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p5_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p5_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p5_t2_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p5_t2_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p5_t2_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p5_t2_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p5_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p5_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p5_t3_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p5_t3_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p5_t3_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p5_t3_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p5_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p5_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p5_t4_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p5_t4_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p5_t4_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p5_t4_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p5_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p5_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p5_t5_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p5_t5_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p5_t5_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p5_t5_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p5_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p5_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p5_t6_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p5_t6_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p5_t6_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p5_t6_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p5_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p5_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p6_t1_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p6_t1_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p6_t1_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p6_t1_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p6_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p6_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p6_t2_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p6_t2_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p6_t2_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p6_t2_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p6_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p6_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p6_t3_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p6_t3_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p6_t3_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p6_t3_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p6_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p6_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p6_t4_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p6_t4_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p6_t4_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p6_t4_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p6_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p6_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p6_t5_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p6_t5_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p6_t5_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p6_t5_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p6_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p6_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p6_t6_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p6_t6_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p6_t6_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p6_t6_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p6_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p6_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p7_t1_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p7_t1_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p7_t1_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p7_t1_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p7_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p7_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p7_t2_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p7_t2_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p7_t2_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p7_t2_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p7_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p7_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p7_t3_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p7_t3_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p7_t3_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p7_t3_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p7_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p7_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p7_t4_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p7_t4_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p7_t4_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p7_t4_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p7_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p7_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p7_t5_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p7_t5_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p7_t5_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p7_t5_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p7_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p7_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p7_t6_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p7_t6_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p7_t6_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p7_t6_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p7_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p7_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p8_t1_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p8_t1_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p8_t1_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p8_t1_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p8_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p8_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p8_t2_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p8_t2_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p8_t2_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p8_t2_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p8_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p8_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p8_t3_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p8_t3_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p8_t3_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p8_t3_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p8_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p8_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p8_t4_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p8_t4_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p8_t4_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p8_t4_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p8_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p8_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p8_t5_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p8_t5_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p8_t5_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p8_t5_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p8_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p8_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p8_t6_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p8_t6_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p8_t6_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p8_t6_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p8_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p8_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p9_t1_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p9_t1_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p9_t1_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p9_t1_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p9_t1_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p9_t1_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p9_t2_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p9_t2_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p9_t2_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p9_t2_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p9_t2_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p9_t2_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p9_t3_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p9_t3_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p9_t3_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p9_t3_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p9_t3_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p9_t3_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p9_t4_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p9_t4_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p9_t4_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p9_t4_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p9_t4_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p9_t4_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p9_t5_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p9_t5_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p9_t5_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p9_t5_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p9_t5_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p9_t5_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p9_t6_br_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p9_t6_br", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p9_t6_sr_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p9_t6_sr", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_p9_t6_ub_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_ec_p9_t6_ub", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_sched_weekday_mset(SAM_Utilityrate3 ptr, double* mat, int nrows, int ncols, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_matrix(ptr, "ur_ec_sched_weekday", mat, nrows, ncols);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_ec_sched_weekend_mset(SAM_Utilityrate3 ptr, double* mat, int nrows, int ncols, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_matrix(ptr, "ur_ec_sched_weekend", mat, nrows, ncols);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_enable_net_metering_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_enable_net_metering", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_excess_monthly_energy_or_dollars_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_excess_monthly_energy_or_dollars", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_flat_buy_rate_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_flat_buy_rate", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_flat_sell_rate_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_flat_sell_rate", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_monthly_fixed_charge_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_monthly_fixed_charge", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_monthly_min_charge_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_monthly_min_charge", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_Common_ur_nm_yearend_sell_rate_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "ur_nm_yearend_sell_rate", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_TimeSeries_gen_aset(SAM_Utilityrate3 ptr, double* arr, int length, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_array(ptr, "gen", arr, length);
});
}

SAM_EXPORT void SAM_Utilityrate3_TimeSeries_load_aset(SAM_Utilityrate3 ptr, double* arr, int length, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_array(ptr, "load", arr, length);
});
}

SAM_EXPORT void SAM_Utilityrate3_Financials_inflation_rate_nset(SAM_Utilityrate3 ptr, double number, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_number(ptr, "inflation_rate", number);
});
}

SAM_EXPORT void SAM_Utilityrate3_AnnualOutput_degradation_aset(SAM_Utilityrate3 ptr, double* arr, int length, SAM_error *err){
translateExceptions(err,[&]{
ssc_data_set_array(ptr, "degradation", arr, length);
});
}

SAM_EXPORT double SAM_Utilityrate3_Common_analysis_period_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "analysis_period", &result))
make_access_error("SAM_Utilityrate3", "analysis_period");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Common_load_escalation_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "load_escalation", length);
if (!result)
make_access_error("SAM_Utilityrate3", "load_escalation");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Common_rate_escalation_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "rate_escalation", length);
if (!result)
make_access_error("SAM_Utilityrate3", "rate_escalation");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_system_use_lifetime_output_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "system_use_lifetime_output", &result))
make_access_error("SAM_Utilityrate3", "system_use_lifetime_output");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_annual_min_charge_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_annual_min_charge", &result))
make_access_error("SAM_Utilityrate3", "ur_annual_min_charge");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_apr_t1_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_apr_t1_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_apr_t1_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_apr_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_apr_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_apr_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_apr_t2_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_apr_t2_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_apr_t2_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_apr_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_apr_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_apr_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_apr_t3_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_apr_t3_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_apr_t3_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_apr_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_apr_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_apr_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_apr_t4_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_apr_t4_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_apr_t4_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_apr_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_apr_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_apr_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_apr_t5_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_apr_t5_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_apr_t5_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_apr_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_apr_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_apr_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_apr_t6_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_apr_t6_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_apr_t6_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_apr_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_apr_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_apr_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_aug_t1_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_aug_t1_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_aug_t1_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_aug_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_aug_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_aug_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_aug_t2_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_aug_t2_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_aug_t2_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_aug_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_aug_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_aug_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_aug_t3_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_aug_t3_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_aug_t3_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_aug_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_aug_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_aug_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_aug_t4_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_aug_t4_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_aug_t4_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_aug_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_aug_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_aug_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_aug_t5_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_aug_t5_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_aug_t5_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_aug_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_aug_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_aug_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_aug_t6_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_aug_t6_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_aug_t6_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_aug_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_aug_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_aug_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_dec_t1_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_dec_t1_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_dec_t1_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_dec_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_dec_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_dec_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_dec_t2_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_dec_t2_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_dec_t2_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_dec_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_dec_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_dec_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_dec_t3_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_dec_t3_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_dec_t3_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_dec_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_dec_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_dec_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_dec_t4_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_dec_t4_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_dec_t4_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_dec_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_dec_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_dec_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_dec_t5_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_dec_t5_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_dec_t5_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_dec_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_dec_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_dec_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_dec_t6_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_dec_t6_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_dec_t6_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_dec_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_dec_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_dec_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_enable_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_enable", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_enable");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_feb_t1_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_feb_t1_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_feb_t1_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_feb_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_feb_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_feb_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_feb_t2_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_feb_t2_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_feb_t2_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_feb_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_feb_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_feb_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_feb_t3_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_feb_t3_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_feb_t3_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_feb_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_feb_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_feb_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_feb_t4_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_feb_t4_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_feb_t4_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_feb_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_feb_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_feb_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_feb_t5_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_feb_t5_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_feb_t5_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_feb_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_feb_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_feb_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_feb_t6_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_feb_t6_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_feb_t6_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_feb_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_feb_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_feb_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jan_t1_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jan_t1_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jan_t1_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jan_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jan_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jan_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jan_t2_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jan_t2_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jan_t2_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jan_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jan_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jan_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jan_t3_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jan_t3_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jan_t3_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jan_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jan_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jan_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jan_t4_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jan_t4_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jan_t4_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jan_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jan_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jan_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jan_t5_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jan_t5_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jan_t5_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jan_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jan_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jan_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jan_t6_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jan_t6_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jan_t6_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jan_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jan_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jan_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jul_t1_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jul_t1_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jul_t1_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jul_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jul_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jul_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jul_t2_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jul_t2_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jul_t2_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jul_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jul_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jul_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jul_t3_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jul_t3_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jul_t3_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jul_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jul_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jul_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jul_t4_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jul_t4_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jul_t4_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jul_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jul_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jul_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jul_t5_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jul_t5_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jul_t5_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jul_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jul_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jul_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jul_t6_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jul_t6_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jul_t6_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jul_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jul_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jul_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jun_t1_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jun_t1_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jun_t1_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jun_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jun_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jun_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jun_t2_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jun_t2_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jun_t2_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jun_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jun_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jun_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jun_t3_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jun_t3_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jun_t3_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jun_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jun_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jun_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jun_t4_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jun_t4_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jun_t4_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jun_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jun_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jun_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jun_t5_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jun_t5_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jun_t5_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jun_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jun_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jun_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jun_t6_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jun_t6_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jun_t6_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_jun_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_jun_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_jun_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_mar_t1_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_mar_t1_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_mar_t1_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_mar_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_mar_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_mar_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_mar_t2_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_mar_t2_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_mar_t2_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_mar_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_mar_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_mar_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_mar_t3_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_mar_t3_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_mar_t3_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_mar_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_mar_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_mar_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_mar_t4_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_mar_t4_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_mar_t4_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_mar_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_mar_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_mar_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_mar_t5_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_mar_t5_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_mar_t5_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_mar_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_mar_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_mar_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_mar_t6_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_mar_t6_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_mar_t6_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_mar_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_mar_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_mar_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_may_t1_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_may_t1_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_may_t1_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_may_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_may_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_may_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_may_t2_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_may_t2_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_may_t2_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_may_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_may_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_may_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_may_t3_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_may_t3_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_may_t3_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_may_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_may_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_may_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_may_t4_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_may_t4_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_may_t4_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_may_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_may_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_may_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_may_t5_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_may_t5_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_may_t5_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_may_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_may_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_may_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_may_t6_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_may_t6_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_may_t6_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_may_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_may_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_may_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_nov_t1_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_nov_t1_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_nov_t1_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_nov_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_nov_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_nov_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_nov_t2_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_nov_t2_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_nov_t2_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_nov_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_nov_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_nov_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_nov_t3_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_nov_t3_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_nov_t3_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_nov_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_nov_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_nov_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_nov_t4_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_nov_t4_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_nov_t4_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_nov_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_nov_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_nov_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_nov_t5_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_nov_t5_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_nov_t5_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_nov_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_nov_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_nov_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_nov_t6_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_nov_t6_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_nov_t6_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_nov_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_nov_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_nov_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_oct_t1_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_oct_t1_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_oct_t1_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_oct_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_oct_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_oct_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_oct_t2_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_oct_t2_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_oct_t2_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_oct_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_oct_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_oct_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_oct_t3_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_oct_t3_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_oct_t3_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_oct_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_oct_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_oct_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_oct_t4_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_oct_t4_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_oct_t4_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_oct_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_oct_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_oct_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_oct_t5_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_oct_t5_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_oct_t5_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_oct_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_oct_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_oct_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_oct_t6_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_oct_t6_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_oct_t6_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_oct_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_oct_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_oct_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p10_t1_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p10_t1_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p10_t1_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p10_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p10_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p10_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p10_t2_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p10_t2_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p10_t2_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p10_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p10_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p10_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p10_t3_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p10_t3_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p10_t3_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p10_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p10_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p10_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p10_t4_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p10_t4_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p10_t4_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p10_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p10_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p10_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p10_t5_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p10_t5_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p10_t5_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p10_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p10_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p10_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p10_t6_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p10_t6_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p10_t6_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p10_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p10_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p10_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p11_t1_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p11_t1_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p11_t1_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p11_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p11_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p11_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p11_t2_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p11_t2_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p11_t2_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p11_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p11_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p11_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p11_t3_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p11_t3_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p11_t3_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p11_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p11_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p11_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p11_t4_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p11_t4_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p11_t4_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p11_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p11_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p11_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p11_t5_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p11_t5_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p11_t5_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p11_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p11_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p11_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p11_t6_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p11_t6_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p11_t6_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p11_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p11_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p11_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p12_t1_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p12_t1_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p12_t1_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p12_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p12_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p12_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p12_t2_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p12_t2_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p12_t2_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p12_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p12_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p12_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p12_t3_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p12_t3_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p12_t3_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p12_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p12_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p12_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p12_t4_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p12_t4_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p12_t4_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p12_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p12_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p12_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p12_t5_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p12_t5_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p12_t5_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p12_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p12_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p12_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p12_t6_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p12_t6_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p12_t6_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p12_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p12_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p12_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p1_t1_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p1_t1_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p1_t1_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p1_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p1_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p1_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p1_t2_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p1_t2_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p1_t2_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p1_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p1_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p1_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p1_t3_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p1_t3_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p1_t3_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p1_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p1_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p1_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p1_t4_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p1_t4_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p1_t4_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p1_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p1_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p1_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p1_t5_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p1_t5_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p1_t5_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p1_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p1_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p1_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p1_t6_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p1_t6_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p1_t6_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p1_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p1_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p1_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p2_t1_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p2_t1_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p2_t1_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p2_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p2_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p2_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p2_t2_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p2_t2_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p2_t2_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p2_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p2_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p2_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p2_t3_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p2_t3_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p2_t3_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p2_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p2_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p2_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p2_t4_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p2_t4_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p2_t4_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p2_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p2_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p2_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p2_t5_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p2_t5_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p2_t5_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p2_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p2_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p2_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p2_t6_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p2_t6_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p2_t6_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p2_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p2_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p2_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p3_t1_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p3_t1_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p3_t1_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p3_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p3_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p3_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p3_t2_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p3_t2_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p3_t2_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p3_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p3_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p3_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p3_t3_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p3_t3_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p3_t3_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p3_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p3_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p3_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p3_t4_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p3_t4_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p3_t4_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p3_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p3_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p3_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p3_t5_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p3_t5_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p3_t5_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p3_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p3_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p3_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p3_t6_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p3_t6_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p3_t6_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p3_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p3_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p3_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p4_t1_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p4_t1_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p4_t1_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p4_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p4_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p4_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p4_t2_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p4_t2_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p4_t2_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p4_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p4_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p4_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p4_t3_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p4_t3_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p4_t3_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p4_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p4_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p4_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p4_t4_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p4_t4_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p4_t4_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p4_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p4_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p4_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p4_t5_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p4_t5_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p4_t5_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p4_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p4_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p4_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p4_t6_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p4_t6_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p4_t6_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p4_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p4_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p4_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p5_t1_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p5_t1_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p5_t1_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p5_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p5_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p5_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p5_t2_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p5_t2_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p5_t2_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p5_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p5_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p5_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p5_t3_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p5_t3_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p5_t3_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p5_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p5_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p5_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p5_t4_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p5_t4_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p5_t4_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p5_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p5_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p5_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p5_t5_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p5_t5_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p5_t5_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p5_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p5_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p5_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p5_t6_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p5_t6_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p5_t6_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p5_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p5_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p5_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p6_t1_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p6_t1_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p6_t1_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p6_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p6_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p6_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p6_t2_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p6_t2_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p6_t2_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p6_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p6_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p6_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p6_t3_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p6_t3_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p6_t3_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p6_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p6_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p6_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p6_t4_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p6_t4_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p6_t4_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p6_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p6_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p6_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p6_t5_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p6_t5_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p6_t5_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p6_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p6_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p6_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p6_t6_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p6_t6_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p6_t6_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p6_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p6_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p6_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p7_t1_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p7_t1_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p7_t1_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p7_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p7_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p7_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p7_t2_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p7_t2_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p7_t2_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p7_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p7_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p7_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p7_t3_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p7_t3_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p7_t3_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p7_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p7_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p7_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p7_t4_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p7_t4_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p7_t4_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p7_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p7_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p7_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p7_t5_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p7_t5_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p7_t5_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p7_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p7_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p7_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p7_t6_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p7_t6_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p7_t6_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p7_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p7_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p7_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p8_t1_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p8_t1_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p8_t1_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p8_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p8_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p8_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p8_t2_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p8_t2_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p8_t2_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p8_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p8_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p8_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p8_t3_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p8_t3_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p8_t3_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p8_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p8_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p8_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p8_t4_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p8_t4_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p8_t4_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p8_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p8_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p8_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p8_t5_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p8_t5_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p8_t5_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p8_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p8_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p8_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p8_t6_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p8_t6_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p8_t6_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p8_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p8_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p8_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p9_t1_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p9_t1_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p9_t1_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p9_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p9_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p9_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p9_t2_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p9_t2_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p9_t2_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p9_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p9_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p9_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p9_t3_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p9_t3_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p9_t3_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p9_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p9_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p9_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p9_t4_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p9_t4_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p9_t4_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p9_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p9_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p9_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p9_t5_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p9_t5_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p9_t5_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p9_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p9_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p9_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p9_t6_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p9_t6_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p9_t6_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_p9_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_p9_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_p9_t6_ub");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Common_ur_dc_sched_weekday_mget(SAM_Utilityrate3 ptr, int* nrows, int* ncols, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_matrix(ptr, "ur_dc_sched_weekday", nrows, ncols);
if (!result)
make_access_error("SAM_Utilityrate3", "ur_dc_sched_weekday");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Common_ur_dc_sched_weekend_mget(SAM_Utilityrate3 ptr, int* nrows, int* ncols, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_matrix(ptr, "ur_dc_sched_weekend", nrows, ncols);
if (!result)
make_access_error("SAM_Utilityrate3", "ur_dc_sched_weekend");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_sep_t1_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_sep_t1_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_sep_t1_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_sep_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_sep_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_sep_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_sep_t2_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_sep_t2_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_sep_t2_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_sep_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_sep_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_sep_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_sep_t3_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_sep_t3_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_sep_t3_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_sep_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_sep_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_sep_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_sep_t4_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_sep_t4_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_sep_t4_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_sep_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_sep_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_sep_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_sep_t5_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_sep_t5_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_sep_t5_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_sep_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_sep_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_sep_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_sep_t6_dc_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_sep_t6_dc", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_sep_t6_dc");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_dc_sep_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_dc_sep_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_dc_sep_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_enable_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_enable", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_enable");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p10_t1_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p10_t1_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p10_t1_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p10_t1_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p10_t1_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p10_t1_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p10_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p10_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p10_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p10_t2_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p10_t2_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p10_t2_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p10_t2_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p10_t2_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p10_t2_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p10_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p10_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p10_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p10_t3_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p10_t3_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p10_t3_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p10_t3_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p10_t3_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p10_t3_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p10_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p10_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p10_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p10_t4_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p10_t4_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p10_t4_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p10_t4_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p10_t4_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p10_t4_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p10_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p10_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p10_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p10_t5_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p10_t5_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p10_t5_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p10_t5_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p10_t5_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p10_t5_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p10_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p10_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p10_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p10_t6_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p10_t6_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p10_t6_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p10_t6_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p10_t6_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p10_t6_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p10_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p10_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p10_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p11_t1_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p11_t1_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p11_t1_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p11_t1_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p11_t1_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p11_t1_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p11_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p11_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p11_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p11_t2_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p11_t2_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p11_t2_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p11_t2_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p11_t2_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p11_t2_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p11_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p11_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p11_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p11_t3_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p11_t3_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p11_t3_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p11_t3_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p11_t3_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p11_t3_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p11_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p11_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p11_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p11_t4_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p11_t4_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p11_t4_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p11_t4_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p11_t4_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p11_t4_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p11_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p11_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p11_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p11_t5_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p11_t5_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p11_t5_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p11_t5_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p11_t5_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p11_t5_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p11_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p11_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p11_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p11_t6_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p11_t6_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p11_t6_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p11_t6_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p11_t6_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p11_t6_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p11_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p11_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p11_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p12_t1_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p12_t1_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p12_t1_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p12_t1_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p12_t1_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p12_t1_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p12_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p12_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p12_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p12_t2_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p12_t2_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p12_t2_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p12_t2_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p12_t2_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p12_t2_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p12_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p12_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p12_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p12_t3_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p12_t3_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p12_t3_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p12_t3_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p12_t3_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p12_t3_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p12_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p12_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p12_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p12_t4_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p12_t4_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p12_t4_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p12_t4_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p12_t4_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p12_t4_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p12_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p12_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p12_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p12_t5_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p12_t5_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p12_t5_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p12_t5_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p12_t5_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p12_t5_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p12_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p12_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p12_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p12_t6_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p12_t6_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p12_t6_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p12_t6_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p12_t6_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p12_t6_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p12_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p12_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p12_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p1_t1_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p1_t1_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p1_t1_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p1_t1_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p1_t1_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p1_t1_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p1_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p1_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p1_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p1_t2_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p1_t2_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p1_t2_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p1_t2_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p1_t2_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p1_t2_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p1_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p1_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p1_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p1_t3_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p1_t3_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p1_t3_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p1_t3_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p1_t3_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p1_t3_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p1_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p1_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p1_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p1_t4_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p1_t4_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p1_t4_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p1_t4_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p1_t4_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p1_t4_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p1_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p1_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p1_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p1_t5_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p1_t5_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p1_t5_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p1_t5_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p1_t5_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p1_t5_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p1_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p1_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p1_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p1_t6_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p1_t6_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p1_t6_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p1_t6_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p1_t6_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p1_t6_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p1_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p1_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p1_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p2_t1_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p2_t1_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p2_t1_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p2_t1_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p2_t1_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p2_t1_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p2_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p2_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p2_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p2_t2_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p2_t2_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p2_t2_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p2_t2_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p2_t2_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p2_t2_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p2_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p2_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p2_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p2_t3_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p2_t3_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p2_t3_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p2_t3_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p2_t3_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p2_t3_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p2_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p2_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p2_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p2_t4_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p2_t4_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p2_t4_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p2_t4_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p2_t4_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p2_t4_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p2_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p2_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p2_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p2_t5_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p2_t5_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p2_t5_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p2_t5_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p2_t5_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p2_t5_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p2_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p2_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p2_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p2_t6_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p2_t6_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p2_t6_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p2_t6_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p2_t6_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p2_t6_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p2_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p2_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p2_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p3_t1_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p3_t1_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p3_t1_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p3_t1_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p3_t1_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p3_t1_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p3_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p3_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p3_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p3_t2_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p3_t2_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p3_t2_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p3_t2_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p3_t2_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p3_t2_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p3_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p3_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p3_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p3_t3_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p3_t3_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p3_t3_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p3_t3_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p3_t3_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p3_t3_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p3_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p3_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p3_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p3_t4_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p3_t4_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p3_t4_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p3_t4_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p3_t4_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p3_t4_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p3_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p3_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p3_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p3_t5_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p3_t5_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p3_t5_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p3_t5_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p3_t5_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p3_t5_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p3_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p3_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p3_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p3_t6_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p3_t6_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p3_t6_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p3_t6_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p3_t6_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p3_t6_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p3_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p3_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p3_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p4_t1_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p4_t1_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p4_t1_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p4_t1_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p4_t1_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p4_t1_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p4_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p4_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p4_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p4_t2_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p4_t2_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p4_t2_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p4_t2_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p4_t2_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p4_t2_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p4_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p4_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p4_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p4_t3_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p4_t3_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p4_t3_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p4_t3_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p4_t3_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p4_t3_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p4_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p4_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p4_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p4_t4_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p4_t4_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p4_t4_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p4_t4_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p4_t4_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p4_t4_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p4_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p4_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p4_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p4_t5_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p4_t5_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p4_t5_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p4_t5_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p4_t5_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p4_t5_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p4_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p4_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p4_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p4_t6_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p4_t6_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p4_t6_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p4_t6_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p4_t6_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p4_t6_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p4_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p4_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p4_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p5_t1_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p5_t1_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p5_t1_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p5_t1_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p5_t1_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p5_t1_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p5_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p5_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p5_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p5_t2_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p5_t2_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p5_t2_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p5_t2_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p5_t2_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p5_t2_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p5_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p5_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p5_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p5_t3_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p5_t3_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p5_t3_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p5_t3_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p5_t3_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p5_t3_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p5_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p5_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p5_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p5_t4_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p5_t4_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p5_t4_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p5_t4_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p5_t4_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p5_t4_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p5_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p5_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p5_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p5_t5_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p5_t5_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p5_t5_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p5_t5_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p5_t5_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p5_t5_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p5_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p5_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p5_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p5_t6_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p5_t6_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p5_t6_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p5_t6_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p5_t6_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p5_t6_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p5_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p5_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p5_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p6_t1_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p6_t1_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p6_t1_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p6_t1_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p6_t1_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p6_t1_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p6_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p6_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p6_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p6_t2_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p6_t2_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p6_t2_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p6_t2_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p6_t2_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p6_t2_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p6_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p6_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p6_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p6_t3_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p6_t3_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p6_t3_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p6_t3_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p6_t3_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p6_t3_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p6_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p6_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p6_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p6_t4_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p6_t4_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p6_t4_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p6_t4_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p6_t4_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p6_t4_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p6_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p6_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p6_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p6_t5_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p6_t5_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p6_t5_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p6_t5_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p6_t5_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p6_t5_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p6_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p6_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p6_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p6_t6_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p6_t6_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p6_t6_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p6_t6_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p6_t6_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p6_t6_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p6_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p6_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p6_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p7_t1_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p7_t1_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p7_t1_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p7_t1_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p7_t1_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p7_t1_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p7_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p7_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p7_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p7_t2_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p7_t2_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p7_t2_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p7_t2_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p7_t2_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p7_t2_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p7_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p7_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p7_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p7_t3_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p7_t3_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p7_t3_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p7_t3_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p7_t3_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p7_t3_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p7_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p7_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p7_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p7_t4_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p7_t4_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p7_t4_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p7_t4_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p7_t4_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p7_t4_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p7_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p7_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p7_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p7_t5_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p7_t5_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p7_t5_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p7_t5_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p7_t5_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p7_t5_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p7_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p7_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p7_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p7_t6_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p7_t6_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p7_t6_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p7_t6_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p7_t6_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p7_t6_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p7_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p7_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p7_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p8_t1_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p8_t1_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p8_t1_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p8_t1_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p8_t1_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p8_t1_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p8_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p8_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p8_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p8_t2_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p8_t2_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p8_t2_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p8_t2_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p8_t2_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p8_t2_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p8_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p8_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p8_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p8_t3_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p8_t3_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p8_t3_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p8_t3_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p8_t3_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p8_t3_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p8_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p8_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p8_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p8_t4_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p8_t4_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p8_t4_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p8_t4_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p8_t4_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p8_t4_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p8_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p8_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p8_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p8_t5_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p8_t5_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p8_t5_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p8_t5_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p8_t5_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p8_t5_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p8_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p8_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p8_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p8_t6_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p8_t6_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p8_t6_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p8_t6_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p8_t6_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p8_t6_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p8_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p8_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p8_t6_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p9_t1_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p9_t1_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p9_t1_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p9_t1_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p9_t1_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p9_t1_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p9_t1_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p9_t1_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p9_t1_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p9_t2_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p9_t2_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p9_t2_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p9_t2_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p9_t2_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p9_t2_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p9_t2_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p9_t2_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p9_t2_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p9_t3_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p9_t3_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p9_t3_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p9_t3_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p9_t3_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p9_t3_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p9_t3_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p9_t3_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p9_t3_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p9_t4_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p9_t4_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p9_t4_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p9_t4_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p9_t4_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p9_t4_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p9_t4_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p9_t4_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p9_t4_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p9_t5_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p9_t5_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p9_t5_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p9_t5_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p9_t5_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p9_t5_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p9_t5_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p9_t5_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p9_t5_ub");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p9_t6_br_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p9_t6_br", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p9_t6_br");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p9_t6_sr_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p9_t6_sr", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p9_t6_sr");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_ec_p9_t6_ub_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_ec_p9_t6_ub", &result))
make_access_error("SAM_Utilityrate3", "ur_ec_p9_t6_ub");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Common_ur_ec_sched_weekday_mget(SAM_Utilityrate3 ptr, int* nrows, int* ncols, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_matrix(ptr, "ur_ec_sched_weekday", nrows, ncols);
if (!result)
make_access_error("SAM_Utilityrate3", "ur_ec_sched_weekday");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Common_ur_ec_sched_weekend_mget(SAM_Utilityrate3 ptr, int* nrows, int* ncols, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_matrix(ptr, "ur_ec_sched_weekend", nrows, ncols);
if (!result)
make_access_error("SAM_Utilityrate3", "ur_ec_sched_weekend");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_enable_net_metering_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_enable_net_metering", &result))
make_access_error("SAM_Utilityrate3", "ur_enable_net_metering");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_excess_monthly_energy_or_dollars_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_excess_monthly_energy_or_dollars", &result))
make_access_error("SAM_Utilityrate3", "ur_excess_monthly_energy_or_dollars");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_flat_buy_rate_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_flat_buy_rate", &result))
make_access_error("SAM_Utilityrate3", "ur_flat_buy_rate");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_flat_sell_rate_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_flat_sell_rate", &result))
make_access_error("SAM_Utilityrate3", "ur_flat_sell_rate");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_monthly_fixed_charge_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_monthly_fixed_charge", &result))
make_access_error("SAM_Utilityrate3", "ur_monthly_fixed_charge");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_monthly_min_charge_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_monthly_min_charge", &result))
make_access_error("SAM_Utilityrate3", "ur_monthly_min_charge");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Common_ur_nm_yearend_sell_rate_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "ur_nm_yearend_sell_rate", &result))
make_access_error("SAM_Utilityrate3", "ur_nm_yearend_sell_rate");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_TimeSeries_gen_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "gen", length);
if (!result)
make_access_error("SAM_Utilityrate3", "gen");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_TimeSeries_load_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "load", length);
if (!result)
make_access_error("SAM_Utilityrate3", "load");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Financials_inflation_rate_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "inflation_rate", &result))
make_access_error("SAM_Utilityrate3", "inflation_rate");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_AnnualOutput_degradation_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "degradation", length);
if (!result)
make_access_error("SAM_Utilityrate3", "degradation");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_annual_electric_load_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "annual_electric_load", length);
if (!result)
make_access_error("SAM_Utilityrate3", "annual_electric_load");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_annual_energy_value_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "annual_energy_value", length);
if (!result)
make_access_error("SAM_Utilityrate3", "annual_energy_value");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_dc_fixed_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_dc_fixed", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_dc_fixed");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_dc_fixed_apr_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_dc_fixed_apr", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_dc_fixed_apr");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_dc_fixed_aug_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_dc_fixed_aug", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_dc_fixed_aug");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_dc_fixed_dec_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_dc_fixed_dec", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_dc_fixed_dec");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_dc_fixed_feb_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_dc_fixed_feb", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_dc_fixed_feb");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_dc_fixed_jan_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_dc_fixed_jan", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_dc_fixed_jan");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_dc_fixed_jul_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_dc_fixed_jul", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_dc_fixed_jul");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_dc_fixed_jun_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_dc_fixed_jun", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_dc_fixed_jun");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_dc_fixed_mar_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_dc_fixed_mar", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_dc_fixed_mar");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_dc_fixed_may_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_dc_fixed_may", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_dc_fixed_may");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_dc_fixed_nov_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_dc_fixed_nov", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_dc_fixed_nov");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_dc_fixed_oct_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_dc_fixed_oct", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_dc_fixed_oct");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_dc_fixed_sep_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_dc_fixed_sep", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_dc_fixed_sep");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_dc_tou_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_dc_tou", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_dc_tou");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_dc_tou_apr_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_dc_tou_apr", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_dc_tou_apr");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_dc_tou_aug_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_dc_tou_aug", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_dc_tou_aug");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_dc_tou_dec_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_dc_tou_dec", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_dc_tou_dec");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_dc_tou_feb_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_dc_tou_feb", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_dc_tou_feb");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_dc_tou_jan_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_dc_tou_jan", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_dc_tou_jan");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_dc_tou_jul_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_dc_tou_jul", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_dc_tou_jul");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_dc_tou_jun_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_dc_tou_jun", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_dc_tou_jun");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_dc_tou_mar_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_dc_tou_mar", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_dc_tou_mar");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_dc_tou_may_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_dc_tou_may", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_dc_tou_may");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_dc_tou_nov_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_dc_tou_nov", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_dc_tou_nov");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_dc_tou_oct_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_dc_tou_oct", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_dc_tou_oct");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_dc_tou_sep_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_dc_tou_sep", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_dc_tou_sep");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_apr_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_apr", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_apr");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_apr_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_apr_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_apr_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_apr_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_apr_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_apr_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_apr_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_apr_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_apr_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_apr_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_apr_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_apr_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_apr_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_apr_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_apr_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_apr_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_apr_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_apr_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_apr_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_apr_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_apr_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_apr_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_apr_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_apr_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_apr_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_apr_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_apr_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_apr_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_apr_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_apr_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_apr_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_apr_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_apr_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_apr_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_apr_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_apr_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_aug_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_aug", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_aug");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_aug_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_aug_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_aug_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_aug_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_aug_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_aug_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_aug_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_aug_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_aug_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_aug_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_aug_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_aug_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_aug_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_aug_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_aug_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_aug_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_aug_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_aug_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_aug_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_aug_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_aug_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_aug_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_aug_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_aug_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_aug_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_aug_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_aug_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_aug_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_aug_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_aug_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_aug_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_aug_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_aug_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_aug_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_aug_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_aug_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_dec_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_dec", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_dec");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_dec_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_dec_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_dec_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_dec_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_dec_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_dec_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_dec_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_dec_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_dec_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_dec_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_dec_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_dec_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_dec_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_dec_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_dec_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_dec_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_dec_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_dec_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_dec_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_dec_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_dec_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_dec_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_dec_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_dec_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_dec_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_dec_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_dec_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_dec_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_dec_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_dec_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_dec_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_dec_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_dec_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_dec_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_dec_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_dec_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_feb_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_feb", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_feb");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_feb_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_feb_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_feb_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_feb_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_feb_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_feb_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_feb_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_feb_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_feb_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_feb_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_feb_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_feb_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_feb_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_feb_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_feb_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_feb_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_feb_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_feb_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_feb_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_feb_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_feb_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_feb_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_feb_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_feb_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_feb_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_feb_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_feb_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_feb_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_feb_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_feb_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_feb_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_feb_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_feb_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_feb_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_feb_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_feb_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_flat_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_flat", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_flat");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_flat_apr_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_flat_apr", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_flat_apr");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_flat_aug_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_flat_aug", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_flat_aug");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_flat_dec_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_flat_dec", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_flat_dec");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_flat_feb_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_flat_feb", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_flat_feb");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_flat_jan_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_flat_jan", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_flat_jan");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_flat_jul_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_flat_jul", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_flat_jul");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_flat_jun_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_flat_jun", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_flat_jun");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_flat_mar_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_flat_mar", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_flat_mar");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_flat_may_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_flat_may", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_flat_may");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_flat_nov_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_flat_nov", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_flat_nov");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_flat_oct_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_flat_oct", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_flat_oct");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_flat_sep_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_flat_sep", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_flat_sep");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jan_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jan", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jan");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jan_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jan_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jan_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jan_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jan_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jan_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jan_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jan_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jan_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jan_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jan_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jan_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jan_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jan_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jan_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jan_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jan_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jan_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jan_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jan_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jan_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jan_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jan_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jan_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jan_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jan_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jan_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jan_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jan_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jan_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jan_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jan_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jan_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jan_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jan_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jan_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jul_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jul", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jul");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jul_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jul_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jul_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jul_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jul_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jul_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jul_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jul_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jul_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jul_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jul_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jul_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jul_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jul_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jul_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jul_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jul_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jul_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jul_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jul_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jul_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jul_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jul_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jul_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jul_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jul_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jul_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jul_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jul_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jul_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jul_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jul_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jul_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jul_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jul_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jul_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jun_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jun", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jun");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jun_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jun_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jun_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jun_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jun_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jun_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jun_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jun_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jun_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jun_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jun_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jun_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jun_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jun_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jun_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jun_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jun_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jun_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jun_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jun_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jun_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jun_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jun_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jun_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jun_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jun_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jun_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jun_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jun_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jun_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jun_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jun_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jun_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_jun_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_jun_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_jun_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_mar_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_mar", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_mar");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_mar_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_mar_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_mar_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_mar_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_mar_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_mar_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_mar_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_mar_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_mar_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_mar_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_mar_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_mar_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_mar_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_mar_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_mar_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_mar_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_mar_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_mar_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_mar_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_mar_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_mar_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_mar_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_mar_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_mar_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_mar_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_mar_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_mar_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_mar_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_mar_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_mar_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_mar_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_mar_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_mar_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_mar_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_mar_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_mar_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_may_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_may", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_may");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_may_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_may_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_may_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_may_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_may_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_may_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_may_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_may_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_may_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_may_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_may_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_may_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_may_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_may_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_may_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_may_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_may_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_may_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_may_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_may_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_may_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_may_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_may_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_may_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_may_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_may_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_may_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_may_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_may_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_may_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_may_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_may_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_may_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_may_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_may_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_may_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_nov_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_nov", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_nov");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_nov_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_nov_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_nov_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_nov_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_nov_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_nov_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_nov_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_nov_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_nov_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_nov_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_nov_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_nov_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_nov_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_nov_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_nov_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_nov_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_nov_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_nov_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_nov_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_nov_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_nov_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_nov_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_nov_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_nov_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_nov_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_nov_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_nov_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_nov_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_nov_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_nov_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_nov_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_nov_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_nov_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_nov_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_nov_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_nov_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_oct_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_oct", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_oct");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_oct_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_oct_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_oct_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_oct_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_oct_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_oct_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_oct_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_oct_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_oct_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_oct_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_oct_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_oct_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_oct_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_oct_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_oct_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_oct_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_oct_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_oct_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_oct_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_oct_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_oct_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_oct_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_oct_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_oct_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_oct_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_oct_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_oct_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_oct_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_oct_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_oct_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_oct_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_oct_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_oct_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_oct_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_oct_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_oct_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_sep_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_sep", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_sep");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_sep_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_sep_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_sep_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_sep_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_sep_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_sep_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_sep_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_sep_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_sep_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_sep_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_sep_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_sep_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_sep_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_sep_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_sep_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_sep_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_sep_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_sep_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_sep_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_sep_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_sep_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_sep_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_sep_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_sep_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_sep_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_sep_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_sep_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_sep_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_sep_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_sep_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_sep_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_sep_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_sep_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_ec_sep_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_ec_sep_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_ec_sep_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_fixed_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_fixed", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_fixed");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_fixed_apr_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_fixed_apr", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_fixed_apr");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_fixed_aug_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_fixed_aug", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_fixed_aug");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_fixed_dec_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_fixed_dec", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_fixed_dec");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_fixed_feb_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_fixed_feb", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_fixed_feb");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_fixed_jan_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_fixed_jan", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_fixed_jan");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_fixed_jul_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_fixed_jul", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_fixed_jul");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_fixed_jun_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_fixed_jun", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_fixed_jun");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_fixed_mar_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_fixed_mar", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_fixed_mar");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_fixed_may_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_fixed_may", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_fixed_may");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_fixed_nov_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_fixed_nov", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_fixed_nov");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_fixed_oct_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_fixed_oct", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_fixed_oct");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_fixed_sep_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_fixed_sep", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_fixed_sep");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_minimum_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_minimum", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_minimum");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_minimum_apr_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_minimum_apr", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_minimum_apr");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_minimum_aug_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_minimum_aug", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_minimum_aug");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_minimum_dec_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_minimum_dec", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_minimum_dec");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_minimum_feb_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_minimum_feb", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_minimum_feb");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_minimum_jan_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_minimum_jan", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_minimum_jan");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_minimum_jul_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_minimum_jul", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_minimum_jul");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_minimum_jun_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_minimum_jun", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_minimum_jun");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_minimum_mar_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_minimum_mar", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_minimum_mar");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_minimum_may_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_minimum_may", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_minimum_may");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_minimum_nov_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_minimum_nov", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_minimum_nov");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_minimum_oct_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_minimum_oct", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_minimum_oct");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_w_sys_minimum_sep_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_w_sys_minimum_sep", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_w_sys_minimum_sep");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_dc_fixed_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_dc_fixed", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_dc_fixed");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_dc_fixed_apr_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_dc_fixed_apr", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_dc_fixed_apr");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_dc_fixed_aug_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_dc_fixed_aug", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_dc_fixed_aug");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_dc_fixed_dec_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_dc_fixed_dec", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_dc_fixed_dec");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_dc_fixed_feb_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_dc_fixed_feb", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_dc_fixed_feb");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_dc_fixed_jan_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_dc_fixed_jan", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_dc_fixed_jan");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_dc_fixed_jul_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_dc_fixed_jul", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_dc_fixed_jul");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_dc_fixed_jun_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_dc_fixed_jun", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_dc_fixed_jun");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_dc_fixed_mar_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_dc_fixed_mar", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_dc_fixed_mar");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_dc_fixed_may_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_dc_fixed_may", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_dc_fixed_may");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_dc_fixed_nov_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_dc_fixed_nov", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_dc_fixed_nov");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_dc_fixed_oct_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_dc_fixed_oct", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_dc_fixed_oct");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_dc_fixed_sep_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_dc_fixed_sep", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_dc_fixed_sep");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_dc_tou_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_dc_tou", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_dc_tou");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_dc_tou_apr_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_dc_tou_apr", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_dc_tou_apr");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_dc_tou_aug_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_dc_tou_aug", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_dc_tou_aug");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_dc_tou_dec_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_dc_tou_dec", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_dc_tou_dec");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_dc_tou_feb_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_dc_tou_feb", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_dc_tou_feb");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_dc_tou_jan_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_dc_tou_jan", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_dc_tou_jan");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_dc_tou_jul_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_dc_tou_jul", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_dc_tou_jul");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_dc_tou_jun_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_dc_tou_jun", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_dc_tou_jun");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_dc_tou_mar_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_dc_tou_mar", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_dc_tou_mar");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_dc_tou_may_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_dc_tou_may", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_dc_tou_may");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_dc_tou_nov_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_dc_tou_nov", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_dc_tou_nov");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_dc_tou_oct_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_dc_tou_oct", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_dc_tou_oct");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_dc_tou_sep_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_dc_tou_sep", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_dc_tou_sep");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_apr_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_apr", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_apr");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_apr_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_apr_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_apr_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_apr_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_apr_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_apr_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_apr_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_apr_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_apr_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_apr_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_apr_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_apr_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_apr_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_apr_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_apr_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_apr_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_apr_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_apr_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_apr_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_apr_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_apr_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_apr_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_apr_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_apr_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_apr_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_apr_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_apr_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_apr_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_apr_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_apr_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_apr_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_apr_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_apr_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_apr_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_apr_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_apr_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_aug_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_aug", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_aug");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_aug_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_aug_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_aug_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_aug_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_aug_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_aug_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_aug_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_aug_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_aug_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_aug_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_aug_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_aug_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_aug_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_aug_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_aug_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_aug_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_aug_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_aug_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_aug_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_aug_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_aug_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_aug_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_aug_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_aug_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_aug_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_aug_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_aug_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_aug_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_aug_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_aug_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_aug_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_aug_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_aug_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_aug_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_aug_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_aug_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_dec_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_dec", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_dec");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_dec_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_dec_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_dec_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_dec_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_dec_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_dec_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_dec_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_dec_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_dec_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_dec_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_dec_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_dec_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_dec_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_dec_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_dec_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_dec_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_dec_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_dec_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_dec_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_dec_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_dec_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_dec_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_dec_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_dec_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_dec_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_dec_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_dec_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_dec_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_dec_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_dec_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_dec_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_dec_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_dec_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_dec_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_dec_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_dec_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_feb_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_feb", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_feb");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_feb_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_feb_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_feb_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_feb_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_feb_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_feb_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_feb_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_feb_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_feb_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_feb_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_feb_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_feb_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_feb_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_feb_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_feb_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_feb_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_feb_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_feb_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_feb_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_feb_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_feb_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_feb_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_feb_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_feb_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_feb_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_feb_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_feb_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_feb_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_feb_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_feb_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_feb_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_feb_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_feb_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_feb_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_feb_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_feb_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_flat_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_flat", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_flat");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_flat_apr_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_flat_apr", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_flat_apr");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_flat_aug_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_flat_aug", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_flat_aug");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_flat_dec_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_flat_dec", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_flat_dec");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_flat_feb_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_flat_feb", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_flat_feb");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_flat_jan_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_flat_jan", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_flat_jan");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_flat_jul_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_flat_jul", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_flat_jul");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_flat_jun_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_flat_jun", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_flat_jun");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_flat_mar_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_flat_mar", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_flat_mar");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_flat_may_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_flat_may", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_flat_may");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_flat_nov_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_flat_nov", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_flat_nov");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_flat_oct_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_flat_oct", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_flat_oct");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_flat_sep_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_flat_sep", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_flat_sep");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jan_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jan", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jan");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jan_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jan_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jan_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jan_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jan_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jan_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jan_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jan_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jan_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jan_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jan_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jan_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jan_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jan_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jan_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jan_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jan_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jan_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jan_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jan_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jan_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jan_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jan_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jan_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jan_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jan_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jan_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jan_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jan_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jan_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jan_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jan_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jan_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jan_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jan_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jan_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jul_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jul", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jul");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jul_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jul_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jul_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jul_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jul_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jul_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jul_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jul_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jul_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jul_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jul_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jul_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jul_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jul_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jul_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jul_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jul_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jul_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jul_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jul_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jul_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jul_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jul_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jul_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jul_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jul_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jul_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jul_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jul_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jul_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jul_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jul_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jul_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jul_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jul_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jul_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jun_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jun", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jun");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jun_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jun_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jun_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jun_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jun_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jun_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jun_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jun_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jun_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jun_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jun_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jun_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jun_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jun_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jun_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jun_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jun_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jun_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jun_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jun_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jun_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jun_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jun_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jun_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jun_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jun_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jun_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jun_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jun_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jun_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jun_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jun_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jun_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_jun_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_jun_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_jun_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_mar_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_mar", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_mar");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_mar_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_mar_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_mar_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_mar_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_mar_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_mar_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_mar_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_mar_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_mar_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_mar_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_mar_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_mar_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_mar_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_mar_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_mar_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_mar_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_mar_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_mar_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_mar_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_mar_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_mar_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_mar_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_mar_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_mar_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_mar_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_mar_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_mar_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_mar_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_mar_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_mar_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_mar_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_mar_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_mar_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_mar_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_mar_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_mar_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_may_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_may", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_may");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_may_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_may_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_may_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_may_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_may_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_may_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_may_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_may_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_may_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_may_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_may_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_may_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_may_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_may_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_may_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_may_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_may_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_may_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_may_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_may_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_may_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_may_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_may_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_may_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_may_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_may_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_may_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_may_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_may_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_may_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_may_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_may_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_may_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_may_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_may_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_may_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_nov_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_nov", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_nov");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_nov_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_nov_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_nov_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_nov_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_nov_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_nov_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_nov_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_nov_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_nov_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_nov_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_nov_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_nov_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_nov_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_nov_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_nov_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_nov_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_nov_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_nov_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_nov_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_nov_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_nov_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_nov_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_nov_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_nov_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_nov_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_nov_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_nov_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_nov_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_nov_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_nov_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_nov_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_nov_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_nov_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_nov_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_nov_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_nov_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_oct_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_oct", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_oct");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_oct_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_oct_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_oct_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_oct_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_oct_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_oct_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_oct_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_oct_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_oct_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_oct_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_oct_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_oct_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_oct_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_oct_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_oct_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_oct_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_oct_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_oct_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_oct_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_oct_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_oct_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_oct_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_oct_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_oct_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_oct_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_oct_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_oct_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_oct_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_oct_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_oct_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_oct_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_oct_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_oct_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_oct_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_oct_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_oct_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_sep_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_sep", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_sep");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_sep_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_sep_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_sep_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_sep_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_sep_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_sep_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_sep_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_sep_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_sep_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_sep_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_sep_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_sep_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_sep_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_sep_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_sep_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_sep_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_sep_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_sep_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_sep_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_sep_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_sep_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_sep_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_sep_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_sep_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_sep_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_sep_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_sep_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_sep_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_sep_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_sep_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_sep_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_sep_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_sep_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_ec_sep_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_ec_sep_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_ec_sep_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_fixed_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_fixed", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_fixed");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_fixed_apr_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_fixed_apr", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_fixed_apr");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_fixed_aug_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_fixed_aug", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_fixed_aug");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_fixed_dec_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_fixed_dec", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_fixed_dec");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_fixed_feb_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_fixed_feb", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_fixed_feb");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_fixed_jan_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_fixed_jan", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_fixed_jan");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_fixed_jul_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_fixed_jul", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_fixed_jul");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_fixed_jun_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_fixed_jun", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_fixed_jun");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_fixed_mar_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_fixed_mar", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_fixed_mar");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_fixed_may_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_fixed_may", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_fixed_may");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_fixed_nov_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_fixed_nov", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_fixed_nov");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_fixed_oct_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_fixed_oct", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_fixed_oct");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_fixed_sep_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_fixed_sep", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_fixed_sep");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_minimum_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_minimum", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_minimum");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_minimum_apr_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_minimum_apr", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_minimum_apr");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_minimum_aug_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_minimum_aug", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_minimum_aug");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_minimum_dec_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_minimum_dec", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_minimum_dec");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_minimum_feb_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_minimum_feb", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_minimum_feb");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_minimum_jan_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_minimum_jan", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_minimum_jan");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_minimum_jul_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_minimum_jul", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_minimum_jul");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_minimum_jun_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_minimum_jun", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_minimum_jun");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_minimum_mar_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_minimum_mar", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_minimum_mar");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_minimum_may_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_minimum_may", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_minimum_may");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_minimum_nov_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_minimum_nov", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_minimum_nov");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_minimum_oct_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_minimum_oct", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_minimum_oct");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_charge_wo_sys_minimum_sep_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "charge_wo_sys_minimum_sep", length);
if (!result)
make_access_error("SAM_Utilityrate3", "charge_wo_sys_minimum_sep");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_elec_cost_with_system_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "elec_cost_with_system", length);
if (!result)
make_access_error("SAM_Utilityrate3", "elec_cost_with_system");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Outputs_elec_cost_with_system_year1_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "elec_cost_with_system_year1", &result))
make_access_error("SAM_Utilityrate3", "elec_cost_with_system_year1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_elec_cost_without_system_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "elec_cost_without_system", length);
if (!result)
make_access_error("SAM_Utilityrate3", "elec_cost_without_system");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Outputs_elec_cost_without_system_year1_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "elec_cost_without_system_year1", &result))
make_access_error("SAM_Utilityrate3", "elec_cost_without_system_year1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_apr_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_apr_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_apr_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_apr_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_apr_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_apr_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_apr_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_apr_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_apr_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_apr_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_apr_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_apr_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_apr_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_apr_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_apr_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_apr_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_apr_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_apr_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_apr_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_apr_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_apr_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_apr_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_apr_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_apr_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_apr_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_apr_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_apr_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_apr_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_apr_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_apr_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_apr_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_apr_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_apr_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_apr_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_apr_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_apr_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_aug_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_aug_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_aug_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_aug_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_aug_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_aug_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_aug_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_aug_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_aug_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_aug_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_aug_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_aug_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_aug_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_aug_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_aug_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_aug_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_aug_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_aug_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_aug_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_aug_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_aug_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_aug_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_aug_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_aug_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_aug_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_aug_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_aug_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_aug_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_aug_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_aug_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_aug_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_aug_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_aug_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_aug_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_aug_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_aug_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_dec_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_dec_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_dec_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_dec_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_dec_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_dec_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_dec_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_dec_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_dec_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_dec_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_dec_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_dec_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_dec_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_dec_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_dec_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_dec_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_dec_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_dec_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_dec_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_dec_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_dec_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_dec_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_dec_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_dec_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_dec_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_dec_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_dec_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_dec_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_dec_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_dec_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_dec_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_dec_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_dec_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_dec_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_dec_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_dec_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_feb_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_feb_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_feb_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_feb_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_feb_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_feb_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_feb_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_feb_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_feb_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_feb_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_feb_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_feb_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_feb_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_feb_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_feb_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_feb_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_feb_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_feb_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_feb_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_feb_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_feb_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_feb_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_feb_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_feb_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_feb_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_feb_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_feb_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_feb_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_feb_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_feb_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_feb_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_feb_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_feb_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_feb_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_feb_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_feb_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jan_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jan_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jan_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jan_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jan_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jan_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jan_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jan_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jan_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jan_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jan_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jan_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jan_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jan_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jan_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jan_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jan_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jan_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jan_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jan_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jan_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jan_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jan_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jan_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jan_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jan_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jan_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jan_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jan_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jan_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jan_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jan_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jan_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jan_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jan_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jan_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jul_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jul_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jul_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jul_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jul_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jul_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jul_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jul_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jul_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jul_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jul_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jul_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jul_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jul_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jul_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jul_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jul_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jul_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jul_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jul_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jul_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jul_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jul_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jul_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jul_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jul_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jul_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jul_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jul_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jul_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jul_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jul_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jul_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jul_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jul_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jul_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jun_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jun_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jun_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jun_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jun_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jun_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jun_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jun_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jun_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jun_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jun_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jun_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jun_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jun_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jun_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jun_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jun_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jun_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jun_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jun_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jun_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jun_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jun_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jun_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jun_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jun_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jun_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jun_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jun_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jun_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jun_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jun_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jun_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_jun_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_jun_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_jun_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_mar_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_mar_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_mar_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_mar_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_mar_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_mar_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_mar_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_mar_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_mar_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_mar_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_mar_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_mar_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_mar_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_mar_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_mar_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_mar_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_mar_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_mar_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_mar_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_mar_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_mar_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_mar_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_mar_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_mar_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_mar_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_mar_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_mar_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_mar_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_mar_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_mar_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_mar_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_mar_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_mar_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_mar_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_mar_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_mar_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_may_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_may_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_may_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_may_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_may_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_may_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_may_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_may_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_may_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_may_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_may_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_may_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_may_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_may_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_may_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_may_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_may_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_may_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_may_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_may_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_may_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_may_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_may_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_may_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_may_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_may_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_may_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_may_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_may_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_may_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_may_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_may_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_may_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_may_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_may_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_may_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_nov_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_nov_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_nov_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_nov_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_nov_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_nov_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_nov_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_nov_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_nov_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_nov_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_nov_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_nov_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_nov_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_nov_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_nov_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_nov_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_nov_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_nov_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_nov_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_nov_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_nov_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_nov_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_nov_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_nov_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_nov_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_nov_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_nov_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_nov_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_nov_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_nov_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_nov_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_nov_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_nov_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_nov_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_nov_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_nov_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_oct_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_oct_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_oct_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_oct_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_oct_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_oct_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_oct_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_oct_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_oct_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_oct_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_oct_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_oct_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_oct_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_oct_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_oct_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_oct_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_oct_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_oct_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_oct_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_oct_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_oct_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_oct_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_oct_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_oct_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_oct_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_oct_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_oct_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_oct_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_oct_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_oct_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_oct_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_oct_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_oct_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_oct_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_oct_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_oct_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_sep_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_sep_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_sep_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_sep_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_sep_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_sep_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_sep_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_sep_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_sep_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_sep_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_sep_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_sep_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_sep_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_sep_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_sep_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_sep_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_sep_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_sep_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_sep_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_sep_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_sep_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_sep_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_sep_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_sep_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_sep_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_sep_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_sep_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_sep_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_sep_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_sep_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_sep_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_sep_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_sep_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_w_sys_ec_sep_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_w_sys_ec_sep_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_w_sys_ec_sep_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_apr_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_apr_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_apr_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_apr_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_apr_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_apr_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_apr_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_apr_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_apr_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_apr_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_apr_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_apr_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_apr_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_apr_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_apr_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_apr_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_apr_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_apr_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_apr_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_apr_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_apr_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_apr_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_apr_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_apr_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_apr_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_apr_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_apr_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_apr_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_apr_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_apr_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_apr_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_apr_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_apr_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_apr_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_apr_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_apr_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_aug_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_aug_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_aug_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_aug_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_aug_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_aug_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_aug_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_aug_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_aug_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_aug_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_aug_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_aug_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_aug_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_aug_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_aug_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_aug_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_aug_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_aug_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_aug_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_aug_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_aug_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_aug_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_aug_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_aug_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_aug_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_aug_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_aug_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_aug_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_aug_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_aug_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_aug_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_aug_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_aug_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_aug_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_aug_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_aug_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_dec_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_dec_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_dec_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_dec_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_dec_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_dec_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_dec_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_dec_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_dec_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_dec_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_dec_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_dec_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_dec_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_dec_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_dec_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_dec_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_dec_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_dec_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_dec_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_dec_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_dec_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_dec_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_dec_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_dec_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_dec_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_dec_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_dec_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_dec_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_dec_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_dec_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_dec_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_dec_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_dec_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_dec_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_dec_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_dec_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_feb_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_feb_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_feb_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_feb_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_feb_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_feb_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_feb_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_feb_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_feb_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_feb_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_feb_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_feb_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_feb_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_feb_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_feb_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_feb_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_feb_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_feb_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_feb_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_feb_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_feb_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_feb_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_feb_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_feb_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_feb_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_feb_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_feb_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_feb_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_feb_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_feb_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_feb_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_feb_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_feb_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_feb_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_feb_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_feb_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jan_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jan_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jan_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jan_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jan_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jan_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jan_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jan_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jan_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jan_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jan_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jan_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jan_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jan_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jan_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jan_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jan_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jan_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jan_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jan_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jan_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jan_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jan_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jan_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jan_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jan_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jan_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jan_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jan_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jan_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jan_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jan_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jan_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jan_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jan_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jan_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jul_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jul_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jul_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jul_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jul_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jul_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jul_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jul_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jul_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jul_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jul_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jul_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jul_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jul_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jul_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jul_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jul_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jul_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jul_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jul_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jul_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jul_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jul_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jul_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jul_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jul_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jul_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jul_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jul_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jul_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jul_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jul_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jul_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jul_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jul_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jul_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jun_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jun_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jun_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jun_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jun_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jun_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jun_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jun_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jun_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jun_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jun_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jun_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jun_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jun_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jun_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jun_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jun_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jun_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jun_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jun_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jun_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jun_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jun_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jun_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jun_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jun_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jun_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jun_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jun_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jun_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jun_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jun_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jun_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_jun_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_jun_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_jun_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_mar_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_mar_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_mar_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_mar_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_mar_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_mar_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_mar_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_mar_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_mar_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_mar_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_mar_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_mar_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_mar_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_mar_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_mar_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_mar_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_mar_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_mar_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_mar_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_mar_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_mar_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_mar_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_mar_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_mar_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_mar_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_mar_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_mar_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_mar_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_mar_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_mar_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_mar_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_mar_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_mar_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_mar_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_mar_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_mar_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_may_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_may_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_may_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_may_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_may_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_may_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_may_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_may_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_may_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_may_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_may_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_may_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_may_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_may_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_may_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_may_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_may_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_may_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_may_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_may_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_may_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_may_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_may_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_may_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_may_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_may_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_may_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_may_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_may_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_may_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_may_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_may_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_may_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_may_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_may_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_may_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_nov_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_nov_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_nov_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_nov_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_nov_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_nov_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_nov_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_nov_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_nov_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_nov_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_nov_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_nov_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_nov_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_nov_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_nov_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_nov_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_nov_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_nov_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_nov_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_nov_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_nov_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_nov_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_nov_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_nov_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_nov_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_nov_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_nov_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_nov_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_nov_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_nov_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_nov_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_nov_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_nov_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_nov_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_nov_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_nov_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_oct_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_oct_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_oct_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_oct_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_oct_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_oct_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_oct_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_oct_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_oct_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_oct_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_oct_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_oct_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_oct_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_oct_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_oct_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_oct_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_oct_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_oct_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_oct_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_oct_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_oct_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_oct_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_oct_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_oct_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_oct_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_oct_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_oct_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_oct_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_oct_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_oct_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_oct_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_oct_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_oct_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_oct_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_oct_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_oct_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_sep_p1_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_sep_p1", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_sep_p1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_sep_p10_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_sep_p10", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_sep_p10");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_sep_p11_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_sep_p11", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_sep_p11");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_sep_p12_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_sep_p12", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_sep_p12");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_sep_p2_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_sep_p2", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_sep_p2");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_sep_p3_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_sep_p3", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_sep_p3");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_sep_p4_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_sep_p4", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_sep_p4");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_sep_p5_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_sep_p5", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_sep_p5");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_sep_p6_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_sep_p6", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_sep_p6");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_sep_p7_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_sep_p7", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_sep_p7");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_sep_p8_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_sep_p8", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_sep_p8");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_energy_wo_sys_ec_sep_p9_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "energy_wo_sys_ec_sep_p9", length);
if (!result)
make_access_error("SAM_Utilityrate3", "energy_wo_sys_ec_sep_p9");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_lifetime_load_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "lifetime_load", length);
if (!result)
make_access_error("SAM_Utilityrate3", "lifetime_load");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Outputs_savings_year1_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "savings_year1", &result))
make_access_error("SAM_Utilityrate3", "savings_year1");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_utility_bill_w_sys_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "utility_bill_w_sys", length);
if (!result)
make_access_error("SAM_Utilityrate3", "utility_bill_w_sys");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_utility_bill_w_sys_apr_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "utility_bill_w_sys_apr", length);
if (!result)
make_access_error("SAM_Utilityrate3", "utility_bill_w_sys_apr");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_utility_bill_w_sys_aug_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "utility_bill_w_sys_aug", length);
if (!result)
make_access_error("SAM_Utilityrate3", "utility_bill_w_sys_aug");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_utility_bill_w_sys_dec_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "utility_bill_w_sys_dec", length);
if (!result)
make_access_error("SAM_Utilityrate3", "utility_bill_w_sys_dec");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_utility_bill_w_sys_feb_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "utility_bill_w_sys_feb", length);
if (!result)
make_access_error("SAM_Utilityrate3", "utility_bill_w_sys_feb");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_utility_bill_w_sys_jan_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "utility_bill_w_sys_jan", length);
if (!result)
make_access_error("SAM_Utilityrate3", "utility_bill_w_sys_jan");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_utility_bill_w_sys_jul_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "utility_bill_w_sys_jul", length);
if (!result)
make_access_error("SAM_Utilityrate3", "utility_bill_w_sys_jul");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_utility_bill_w_sys_jun_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "utility_bill_w_sys_jun", length);
if (!result)
make_access_error("SAM_Utilityrate3", "utility_bill_w_sys_jun");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_utility_bill_w_sys_mar_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "utility_bill_w_sys_mar", length);
if (!result)
make_access_error("SAM_Utilityrate3", "utility_bill_w_sys_mar");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_utility_bill_w_sys_may_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "utility_bill_w_sys_may", length);
if (!result)
make_access_error("SAM_Utilityrate3", "utility_bill_w_sys_may");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_utility_bill_w_sys_nov_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "utility_bill_w_sys_nov", length);
if (!result)
make_access_error("SAM_Utilityrate3", "utility_bill_w_sys_nov");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_utility_bill_w_sys_oct_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "utility_bill_w_sys_oct", length);
if (!result)
make_access_error("SAM_Utilityrate3", "utility_bill_w_sys_oct");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_utility_bill_w_sys_sep_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "utility_bill_w_sys_sep", length);
if (!result)
make_access_error("SAM_Utilityrate3", "utility_bill_w_sys_sep");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_utility_bill_wo_sys_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "utility_bill_wo_sys", length);
if (!result)
make_access_error("SAM_Utilityrate3", "utility_bill_wo_sys");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_utility_bill_wo_sys_apr_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "utility_bill_wo_sys_apr", length);
if (!result)
make_access_error("SAM_Utilityrate3", "utility_bill_wo_sys_apr");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_utility_bill_wo_sys_aug_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "utility_bill_wo_sys_aug", length);
if (!result)
make_access_error("SAM_Utilityrate3", "utility_bill_wo_sys_aug");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_utility_bill_wo_sys_dec_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "utility_bill_wo_sys_dec", length);
if (!result)
make_access_error("SAM_Utilityrate3", "utility_bill_wo_sys_dec");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_utility_bill_wo_sys_feb_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "utility_bill_wo_sys_feb", length);
if (!result)
make_access_error("SAM_Utilityrate3", "utility_bill_wo_sys_feb");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_utility_bill_wo_sys_jan_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "utility_bill_wo_sys_jan", length);
if (!result)
make_access_error("SAM_Utilityrate3", "utility_bill_wo_sys_jan");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_utility_bill_wo_sys_jul_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "utility_bill_wo_sys_jul", length);
if (!result)
make_access_error("SAM_Utilityrate3", "utility_bill_wo_sys_jul");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_utility_bill_wo_sys_jun_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "utility_bill_wo_sys_jun", length);
if (!result)
make_access_error("SAM_Utilityrate3", "utility_bill_wo_sys_jun");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_utility_bill_wo_sys_mar_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "utility_bill_wo_sys_mar", length);
if (!result)
make_access_error("SAM_Utilityrate3", "utility_bill_wo_sys_mar");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_utility_bill_wo_sys_may_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "utility_bill_wo_sys_may", length);
if (!result)
make_access_error("SAM_Utilityrate3", "utility_bill_wo_sys_may");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_utility_bill_wo_sys_nov_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "utility_bill_wo_sys_nov", length);
if (!result)
make_access_error("SAM_Utilityrate3", "utility_bill_wo_sys_nov");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_utility_bill_wo_sys_oct_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "utility_bill_wo_sys_oct", length);
if (!result)
make_access_error("SAM_Utilityrate3", "utility_bill_wo_sys_oct");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_utility_bill_wo_sys_sep_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "utility_bill_wo_sys_sep", length);
if (!result)
make_access_error("SAM_Utilityrate3", "utility_bill_wo_sys_sep");
});
return result;
}



SAM_EXPORT double SAM_Utilityrate3_Outputs_year1_electric_load_nget(SAM_Utilityrate3 ptr, SAM_error *err){
double result;
translateExceptions(err,[&]{
if (!ssc_data_get_number(ptr, "year1_electric_load", &result))
make_access_error("SAM_Utilityrate3", "year1_electric_load");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_hourly_dc_peak_per_period_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_hourly_dc_peak_per_period", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_hourly_dc_peak_per_period");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_hourly_dc_tou_schedule_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_hourly_dc_tou_schedule", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_hourly_dc_tou_schedule");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_hourly_dc_with_system_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_hourly_dc_with_system", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_hourly_dc_with_system");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_hourly_dc_without_system_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_hourly_dc_without_system", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_hourly_dc_without_system");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_hourly_e_tofromgrid_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_hourly_e_tofromgrid", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_hourly_e_tofromgrid");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_hourly_ec_tou_schedule_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_hourly_ec_tou_schedule", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_hourly_ec_tou_schedule");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_hourly_ec_with_system_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_hourly_ec_with_system", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_hourly_ec_with_system");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_hourly_ec_without_system_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_hourly_ec_without_system", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_hourly_ec_without_system");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_hourly_load_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_hourly_load", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_hourly_load");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_hourly_p_system_to_load_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_hourly_p_system_to_load", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_hourly_p_system_to_load");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_hourly_p_tofromgrid_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_hourly_p_tofromgrid", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_hourly_p_tofromgrid");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_hourly_salespurchases_with_system_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_hourly_salespurchases_with_system", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_hourly_salespurchases_with_system");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_hourly_salespurchases_without_system_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_hourly_salespurchases_without_system", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_hourly_salespurchases_without_system");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_monthly_cumulative_excess_dollars_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_monthly_cumulative_excess_dollars", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_monthly_cumulative_excess_dollars");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_monthly_cumulative_excess_generation_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_monthly_cumulative_excess_generation", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_monthly_cumulative_excess_generation");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_monthly_dc_fixed_with_system_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_monthly_dc_fixed_with_system", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_monthly_dc_fixed_with_system");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_monthly_dc_fixed_without_system_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_monthly_dc_fixed_without_system", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_monthly_dc_fixed_without_system");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_monthly_dc_tou_with_system_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_monthly_dc_tou_with_system", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_monthly_dc_tou_with_system");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_monthly_dc_tou_without_system_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_monthly_dc_tou_without_system", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_monthly_dc_tou_without_system");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_monthly_ec_charge_flat_with_system_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_monthly_ec_charge_flat_with_system", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_monthly_ec_charge_flat_with_system");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_monthly_ec_charge_flat_without_system_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_monthly_ec_charge_flat_without_system", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_monthly_ec_charge_flat_without_system");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_monthly_ec_charge_with_system_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_monthly_ec_charge_with_system", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_monthly_ec_charge_with_system");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_monthly_ec_charge_without_system_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_monthly_ec_charge_without_system", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_monthly_ec_charge_without_system");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_monthly_electricity_to_grid_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_monthly_electricity_to_grid", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_monthly_electricity_to_grid");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_monthly_fixed_with_system_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_monthly_fixed_with_system", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_monthly_fixed_with_system");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_monthly_fixed_without_system_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_monthly_fixed_without_system", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_monthly_fixed_without_system");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_monthly_load_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_monthly_load", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_monthly_load");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_monthly_minimum_with_system_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_monthly_minimum_with_system", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_monthly_minimum_with_system");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_monthly_minimum_without_system_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_monthly_minimum_without_system", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_monthly_minimum_without_system");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_monthly_utility_bill_w_sys_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_monthly_utility_bill_w_sys", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_monthly_utility_bill_w_sys");
});
return result;
}



SAM_EXPORT double* SAM_Utilityrate3_Outputs_year1_monthly_utility_bill_wo_sys_aget(SAM_Utilityrate3 ptr, int* length, SAM_error *err){
double* result = nullptr;
translateExceptions(err,[&]{
result = ssc_data_get_array(ptr, "year1_monthly_utility_bill_wo_sys", length);
if (!result)
make_access_error("SAM_Utilityrate3", "year1_monthly_utility_bill_wo_sys");
});
return result;
}




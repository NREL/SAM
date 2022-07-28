#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Pvsandiainv.h"

SAM_EXPORT int SAM_Pvsandiainv_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("pvsandiainv", data, verbosity, err);
}

SAM_EXPORT void SAM_Pvsandiainv_SandiaInverterModel_c0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c0", number);
	});
}

SAM_EXPORT void SAM_Pvsandiainv_SandiaInverterModel_c1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c1", number);
	});
}

SAM_EXPORT void SAM_Pvsandiainv_SandiaInverterModel_c2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c2", number);
	});
}

SAM_EXPORT void SAM_Pvsandiainv_SandiaInverterModel_c3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c3", number);
	});
}

SAM_EXPORT void SAM_Pvsandiainv_SandiaInverterModel_dc_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dc", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsandiainv_SandiaInverterModel_dc_voltage_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dc_voltage", arr, length);
	});
}

SAM_EXPORT void SAM_Pvsandiainv_SandiaInverterModel_paco_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "paco", number);
	});
}

SAM_EXPORT void SAM_Pvsandiainv_SandiaInverterModel_pdco_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pdco", number);
	});
}

SAM_EXPORT void SAM_Pvsandiainv_SandiaInverterModel_pntare_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pntare", number);
	});
}

SAM_EXPORT void SAM_Pvsandiainv_SandiaInverterModel_pso_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pso", number);
	});
}

SAM_EXPORT void SAM_Pvsandiainv_SandiaInverterModel_vdco_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "vdco", number);
	});
}

SAM_EXPORT double SAM_Pvsandiainv_SandiaInverterModel_c0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c0", &result))
		make_access_error("SAM_Pvsandiainv", "c0");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsandiainv_SandiaInverterModel_c1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c1", &result))
		make_access_error("SAM_Pvsandiainv", "c1");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsandiainv_SandiaInverterModel_c2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c2", &result))
		make_access_error("SAM_Pvsandiainv", "c2");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsandiainv_SandiaInverterModel_c3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c3", &result))
		make_access_error("SAM_Pvsandiainv", "c3");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsandiainv_SandiaInverterModel_dc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dc", length);
	if (!result)
		make_access_error("SAM_Pvsandiainv", "dc");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsandiainv_SandiaInverterModel_dc_voltage_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dc_voltage", length);
	if (!result)
		make_access_error("SAM_Pvsandiainv", "dc_voltage");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsandiainv_SandiaInverterModel_paco_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "paco", &result))
		make_access_error("SAM_Pvsandiainv", "paco");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsandiainv_SandiaInverterModel_pdco_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pdco", &result))
		make_access_error("SAM_Pvsandiainv", "pdco");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsandiainv_SandiaInverterModel_pntare_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pntare", &result))
		make_access_error("SAM_Pvsandiainv", "pntare");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsandiainv_SandiaInverterModel_pso_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pso", &result))
		make_access_error("SAM_Pvsandiainv", "pso");
	});
	return result;
}



SAM_EXPORT double SAM_Pvsandiainv_SandiaInverterModel_vdco_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "vdco", &result))
		make_access_error("SAM_Pvsandiainv", "vdco");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsandiainv_Outputs_ac_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ac", length);
	if (!result)
		make_access_error("SAM_Pvsandiainv", "ac");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsandiainv_Outputs_acpar_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "acpar", length);
	if (!result)
		make_access_error("SAM_Pvsandiainv", "acpar");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsandiainv_Outputs_cliploss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cliploss", length);
	if (!result)
		make_access_error("SAM_Pvsandiainv", "cliploss");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsandiainv_Outputs_eff_inv_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eff_inv", length);
	if (!result)
		make_access_error("SAM_Pvsandiainv", "eff_inv");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsandiainv_Outputs_ntloss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ntloss", length);
	if (!result)
		make_access_error("SAM_Pvsandiainv", "ntloss");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsandiainv_Outputs_plr_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "plr", length);
	if (!result)
		make_access_error("SAM_Pvsandiainv", "plr");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvsandiainv_Outputs_soloss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "soloss", length);
	if (!result)
		make_access_error("SAM_Pvsandiainv", "soloss");
	});
	return result;
}




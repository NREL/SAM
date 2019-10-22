#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_MhkTidal.h"

SAM_EXPORT SAM_MhkTidal SAM_MhkTidal_construct(const char* def, SAM_error* err){
	SAM_MhkTidal result = nullptr;
	translateExceptions(err, [&]{
		result = ssc_data_create();
	});
	return result;
}

SAM_EXPORT int SAM_MhkTidal_execute(SAM_MhkTidal data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("mhk_tidal", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_MhkTidal_destruct(SAM_MhkTidal system)
{
	ssc_data_free(system);
}

SAM_EXPORT void SAM_MhkTidal_MHKTidal_annual_energy_loss_nset(SAM_MhkTidal ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "annual_energy_loss", number);
	});
}

SAM_EXPORT void SAM_MhkTidal_MHKTidal_number_devices_nset(SAM_MhkTidal ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "number_devices", number);
	});
}

SAM_EXPORT void SAM_MhkTidal_MHKTidal_tidal_power_curve_mset(SAM_MhkTidal ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "tidal_power_curve", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_MhkTidal_MHKTidal_tidal_resource_mset(SAM_MhkTidal ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "tidal_resource", mat, nrows, ncols);
	});
}

SAM_EXPORT double SAM_MhkTidal_MHKTidal_annual_energy_loss_nget(SAM_MhkTidal ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy_loss", &result))
		make_access_error("SAM_MhkTidal", "annual_energy_loss");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_MHKTidal_number_devices_nget(SAM_MhkTidal ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "number_devices", &result))
		make_access_error("SAM_MhkTidal", "number_devices");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkTidal_MHKTidal_tidal_power_curve_mget(SAM_MhkTidal ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "tidal_power_curve", nrows, ncols);
	if (!result)
		make_access_error("SAM_MhkTidal", "tidal_power_curve");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkTidal_MHKTidal_tidal_resource_mget(SAM_MhkTidal ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "tidal_resource", nrows, ncols);
	if (!result)
		make_access_error("SAM_MhkTidal", "tidal_resource");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkTidal_Outputs_annual_cumulative_energy_distribution_aget(SAM_MhkTidal ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "annual_cumulative_energy_distribution", length);
	if (!result)
		make_access_error("SAM_MhkTidal", "annual_cumulative_energy_distribution");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_annual_energy_nget(SAM_MhkTidal ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_MhkTidal", "annual_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkTidal_Outputs_annual_energy_distribution_aget(SAM_MhkTidal ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "annual_energy_distribution", length);
	if (!result)
		make_access_error("SAM_MhkTidal", "annual_energy_distribution");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_capacity_factor_nget(SAM_MhkTidal ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_MhkTidal", "capacity_factor");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_device_average_power_nget(SAM_MhkTidal ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "device_average_power", &result))
		make_access_error("SAM_MhkTidal", "device_average_power");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_device_rated_capacity_nget(SAM_MhkTidal ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "device_rated_capacity", &result))
		make_access_error("SAM_MhkTidal", "device_rated_capacity");
	});
	return result;
}




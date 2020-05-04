#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_MhkWave.h"

SAM_EXPORT int SAM_MhkWave_execute(SAM_table data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("mhk_wave", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_MhkWave_MHKWave_device_rated_power_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "device_rated_power", number);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_loss_additional_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "loss_additional", number);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_loss_array_spacing_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "loss_array_spacing", number);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_loss_downtime_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "loss_downtime", number);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_loss_resource_overprediction_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "loss_resource_overprediction", number);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_loss_transmission_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "loss_transmission", number);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_number_devices_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "number_devices", number);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_system_capacity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_capacity", number);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_wave_power_matrix_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "wave_power_matrix", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_wave_resource_matrix_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "wave_resource_matrix", mat, nrows, ncols);
	});
}

SAM_EXPORT double SAM_MhkWave_MHKWave_device_rated_power_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "device_rated_power", &result))
		make_access_error("SAM_MhkWave", "device_rated_power");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_MHKWave_loss_additional_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loss_additional", &result))
		make_access_error("SAM_MhkWave", "loss_additional");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_MHKWave_loss_array_spacing_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loss_array_spacing", &result))
		make_access_error("SAM_MhkWave", "loss_array_spacing");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_MHKWave_loss_downtime_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loss_downtime", &result))
		make_access_error("SAM_MhkWave", "loss_downtime");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_MHKWave_loss_resource_overprediction_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loss_resource_overprediction", &result))
		make_access_error("SAM_MhkWave", "loss_resource_overprediction");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_MHKWave_loss_transmission_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loss_transmission", &result))
		make_access_error("SAM_MhkWave", "loss_transmission");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_MHKWave_number_devices_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "number_devices", &result))
		make_access_error("SAM_MhkWave", "number_devices");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_MHKWave_system_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_MhkWave", "system_capacity");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkWave_MHKWave_wave_power_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "wave_power_matrix", nrows, ncols);
	if (!result)
		make_access_error("SAM_MhkWave", "wave_power_matrix");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkWave_MHKWave_wave_resource_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "wave_resource_matrix", nrows, ncols);
	if (!result)
		make_access_error("SAM_MhkWave", "wave_resource_matrix");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_MhkWave", "annual_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkWave_Outputs_annual_energy_distribution_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "annual_energy_distribution", nrows, ncols);
	if (!result)
		make_access_error("SAM_MhkWave", "annual_energy_distribution");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_MhkWave", "capacity_factor");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_device_average_power_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "device_average_power", &result))
		make_access_error("SAM_MhkWave", "device_average_power");
	});
	return result;
}




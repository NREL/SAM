#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Hcpv.h"

SAM_EXPORT SAM_Hcpv SAM_Hcpv_construct(const char *def, SAM_error *err) {
    SAM_Hcpv result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_create();
    });
    return result;
}

SAM_EXPORT int SAM_Hcpv_execute(SAM_Hcpv data, int verbosity, SAM_error *err) {
    int n_err = 0;
    translateExceptions(err, [&] {
        n_err += SAM_module_exec("hcpv", data, verbosity, err);
    });
    return n_err;
}


SAM_EXPORT void SAM_Hcpv_destruct(SAM_Hcpv system) {
    ssc_data_free(system);
}

SAM_EXPORT void SAM_Hcpv_SolarResourceData_file_name_sset(SAM_Hcpv ptr, const char *str, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_string(ptr, "file_name", str);
    });
}

SAM_EXPORT void SAM_Hcpv_PVWatts_system_capacity_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "system_capacity", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVModule_module_a_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "module_a", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVModule_module_a0_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "module_a0", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVModule_module_a1_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "module_a1", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVModule_module_a2_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "module_a2", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVModule_module_a3_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "module_a3", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVModule_module_a4_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "module_a4", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVModule_module_alignment_error_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "module_alignment_error", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVModule_module_b_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "module_b", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVModule_module_cell_area_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "module_cell_area", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVModule_module_concentration_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "module_concentration", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVModule_module_dT_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "module_dT", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVModule_module_flutter_loss_coeff_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "module_flutter_loss_coeff", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVModule_module_mjeff_aset(SAM_Hcpv ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "module_mjeff", arr, length);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVModule_module_ncells_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "module_ncells", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVModule_module_optical_error_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "module_optical_error", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVModule_module_rad_aset(SAM_Hcpv ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "module_rad", arr, length);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVModule_module_reference_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "module_reference", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVModule_module_temp_coeff_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "module_temp_coeff", number);
    });
}

SAM_EXPORT void SAM_Hcpv_InverterCECDatabase_inv_snl_c0_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "inv_snl_c0", number);
    });
}

SAM_EXPORT void SAM_Hcpv_InverterCECDatabase_inv_snl_c1_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "inv_snl_c1", number);
    });
}

SAM_EXPORT void SAM_Hcpv_InverterCECDatabase_inv_snl_c2_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "inv_snl_c2", number);
    });
}

SAM_EXPORT void SAM_Hcpv_InverterCECDatabase_inv_snl_c3_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "inv_snl_c3", number);
    });
}

SAM_EXPORT void SAM_Hcpv_InverterCECDatabase_inv_snl_paco_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "inv_snl_paco", number);
    });
}

SAM_EXPORT void SAM_Hcpv_InverterCECDatabase_inv_snl_pdco_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "inv_snl_pdco", number);
    });
}

SAM_EXPORT void SAM_Hcpv_InverterCECDatabase_inv_snl_pnt_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "inv_snl_pnt", number);
    });
}

SAM_EXPORT void SAM_Hcpv_InverterCECDatabase_inv_snl_pso_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "inv_snl_pso", number);
    });
}

SAM_EXPORT void SAM_Hcpv_InverterCECDatabase_inv_snl_vdcmax_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "inv_snl_vdcmax", number);
    });
}

SAM_EXPORT void SAM_Hcpv_InverterCECDatabase_inv_snl_vdco_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "inv_snl_vdco", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVArray_array_ac_wiring_loss_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "array_ac_wiring_loss", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVArray_array_dc_mismatch_loss_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "array_dc_mismatch_loss", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVArray_array_dc_wiring_loss_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "array_dc_wiring_loss", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVArray_array_diode_conn_loss_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "array_diode_conn_loss", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVArray_array_enable_azalt_sf_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "array_enable_azalt_sf", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVArray_array_modules_per_tracker_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "array_modules_per_tracker", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVArray_array_monthly_soiling_aset(SAM_Hcpv ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "array_monthly_soiling", arr, length);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVArray_array_num_inverters_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "array_num_inverters", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVArray_array_num_trackers_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "array_num_trackers", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVArray_array_rlim_az_max_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "array_rlim_az_max", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVArray_array_rlim_az_min_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "array_rlim_az_min", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVArray_array_rlim_el_max_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "array_rlim_el_max", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVArray_array_rlim_el_min_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "array_rlim_el_min", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVArray_array_tracker_power_fraction_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "array_tracker_power_fraction", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVArray_array_tracking_error_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "array_tracking_error", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVArray_array_wind_stow_speed_nset(SAM_Hcpv ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "array_wind_stow_speed", number);
    });
}

SAM_EXPORT void SAM_Hcpv_HCPVArray_azaltsf_mset(SAM_Hcpv ptr, double *mat, int nrows, int ncols, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "azaltsf", mat, nrows, ncols);
    });
}

SAM_EXPORT const char *SAM_Hcpv_SolarResourceData_file_name_sget(SAM_Hcpv ptr, SAM_error *err) {
    const char *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_string(ptr, "file_name");
        if (!result)
            make_access_error("SAM_Hcpv", "file_name");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_PVWatts_system_capacity_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "system_capacity", &result))
            make_access_error("SAM_Hcpv", "system_capacity");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVModule_module_a_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "module_a", &result))
            make_access_error("SAM_Hcpv", "module_a");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVModule_module_a0_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "module_a0", &result))
            make_access_error("SAM_Hcpv", "module_a0");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVModule_module_a1_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "module_a1", &result))
            make_access_error("SAM_Hcpv", "module_a1");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVModule_module_a2_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "module_a2", &result))
            make_access_error("SAM_Hcpv", "module_a2");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVModule_module_a3_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "module_a3", &result))
            make_access_error("SAM_Hcpv", "module_a3");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVModule_module_a4_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "module_a4", &result))
            make_access_error("SAM_Hcpv", "module_a4");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVModule_module_alignment_error_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "module_alignment_error", &result))
            make_access_error("SAM_Hcpv", "module_alignment_error");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVModule_module_b_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "module_b", &result))
            make_access_error("SAM_Hcpv", "module_b");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVModule_module_cell_area_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "module_cell_area", &result))
            make_access_error("SAM_Hcpv", "module_cell_area");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVModule_module_concentration_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "module_concentration", &result))
            make_access_error("SAM_Hcpv", "module_concentration");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVModule_module_dT_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "module_dT", &result))
            make_access_error("SAM_Hcpv", "module_dT");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVModule_module_flutter_loss_coeff_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "module_flutter_loss_coeff", &result))
            make_access_error("SAM_Hcpv", "module_flutter_loss_coeff");
    });
    return result;
}


SAM_EXPORT double *SAM_Hcpv_HCPVModule_module_mjeff_aget(SAM_Hcpv ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "module_mjeff", length);
        if (!result)
            make_access_error("SAM_Hcpv", "module_mjeff");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVModule_module_ncells_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "module_ncells", &result))
            make_access_error("SAM_Hcpv", "module_ncells");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVModule_module_optical_error_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "module_optical_error", &result))
            make_access_error("SAM_Hcpv", "module_optical_error");
    });
    return result;
}


SAM_EXPORT double *SAM_Hcpv_HCPVModule_module_rad_aget(SAM_Hcpv ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "module_rad", length);
        if (!result)
            make_access_error("SAM_Hcpv", "module_rad");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVModule_module_reference_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "module_reference", &result))
            make_access_error("SAM_Hcpv", "module_reference");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVModule_module_temp_coeff_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "module_temp_coeff", &result))
            make_access_error("SAM_Hcpv", "module_temp_coeff");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_InverterCECDatabase_inv_snl_c0_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "inv_snl_c0", &result))
            make_access_error("SAM_Hcpv", "inv_snl_c0");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_InverterCECDatabase_inv_snl_c1_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "inv_snl_c1", &result))
            make_access_error("SAM_Hcpv", "inv_snl_c1");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_InverterCECDatabase_inv_snl_c2_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "inv_snl_c2", &result))
            make_access_error("SAM_Hcpv", "inv_snl_c2");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_InverterCECDatabase_inv_snl_c3_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "inv_snl_c3", &result))
            make_access_error("SAM_Hcpv", "inv_snl_c3");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_InverterCECDatabase_inv_snl_paco_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "inv_snl_paco", &result))
            make_access_error("SAM_Hcpv", "inv_snl_paco");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_InverterCECDatabase_inv_snl_pdco_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "inv_snl_pdco", &result))
            make_access_error("SAM_Hcpv", "inv_snl_pdco");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_InverterCECDatabase_inv_snl_pnt_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "inv_snl_pnt", &result))
            make_access_error("SAM_Hcpv", "inv_snl_pnt");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_InverterCECDatabase_inv_snl_pso_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "inv_snl_pso", &result))
            make_access_error("SAM_Hcpv", "inv_snl_pso");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_InverterCECDatabase_inv_snl_vdcmax_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "inv_snl_vdcmax", &result))
            make_access_error("SAM_Hcpv", "inv_snl_vdcmax");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_InverterCECDatabase_inv_snl_vdco_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "inv_snl_vdco", &result))
            make_access_error("SAM_Hcpv", "inv_snl_vdco");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVArray_array_ac_wiring_loss_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "array_ac_wiring_loss", &result))
            make_access_error("SAM_Hcpv", "array_ac_wiring_loss");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVArray_array_dc_mismatch_loss_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "array_dc_mismatch_loss", &result))
            make_access_error("SAM_Hcpv", "array_dc_mismatch_loss");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVArray_array_dc_wiring_loss_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "array_dc_wiring_loss", &result))
            make_access_error("SAM_Hcpv", "array_dc_wiring_loss");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVArray_array_diode_conn_loss_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "array_diode_conn_loss", &result))
            make_access_error("SAM_Hcpv", "array_diode_conn_loss");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVArray_array_enable_azalt_sf_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "array_enable_azalt_sf", &result))
            make_access_error("SAM_Hcpv", "array_enable_azalt_sf");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVArray_array_modules_per_tracker_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "array_modules_per_tracker", &result))
            make_access_error("SAM_Hcpv", "array_modules_per_tracker");
    });
    return result;
}


SAM_EXPORT double *SAM_Hcpv_HCPVArray_array_monthly_soiling_aget(SAM_Hcpv ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "array_monthly_soiling", length);
        if (!result)
            make_access_error("SAM_Hcpv", "array_monthly_soiling");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVArray_array_num_inverters_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "array_num_inverters", &result))
            make_access_error("SAM_Hcpv", "array_num_inverters");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVArray_array_num_trackers_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "array_num_trackers", &result))
            make_access_error("SAM_Hcpv", "array_num_trackers");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVArray_array_rlim_az_max_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "array_rlim_az_max", &result))
            make_access_error("SAM_Hcpv", "array_rlim_az_max");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVArray_array_rlim_az_min_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "array_rlim_az_min", &result))
            make_access_error("SAM_Hcpv", "array_rlim_az_min");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVArray_array_rlim_el_max_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "array_rlim_el_max", &result))
            make_access_error("SAM_Hcpv", "array_rlim_el_max");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVArray_array_rlim_el_min_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "array_rlim_el_min", &result))
            make_access_error("SAM_Hcpv", "array_rlim_el_min");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVArray_array_tracker_power_fraction_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "array_tracker_power_fraction", &result))
            make_access_error("SAM_Hcpv", "array_tracker_power_fraction");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVArray_array_tracking_error_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "array_tracking_error", &result))
            make_access_error("SAM_Hcpv", "array_tracking_error");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_HCPVArray_array_wind_stow_speed_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "array_wind_stow_speed", &result))
            make_access_error("SAM_Hcpv", "array_wind_stow_speed");
    });
    return result;
}


SAM_EXPORT double *SAM_Hcpv_HCPVArray_azaltsf_mget(SAM_Hcpv ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "azaltsf", nrows, ncols);
        if (!result)
            make_access_error("SAM_Hcpv", "azaltsf");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_Outputs_ac_loss_tracker_kwh_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "ac_loss_tracker_kwh", &result))
            make_access_error("SAM_Hcpv", "ac_loss_tracker_kwh");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_Outputs_annual_ac_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "annual_ac", &result))
            make_access_error("SAM_Hcpv", "annual_ac");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_Outputs_annual_beam_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "annual_beam", &result))
            make_access_error("SAM_Hcpv", "annual_beam");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_Outputs_annual_dc_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "annual_dc", &result))
            make_access_error("SAM_Hcpv", "annual_dc");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_Outputs_annual_dc_net_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "annual_dc_net", &result))
            make_access_error("SAM_Hcpv", "annual_dc_net");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_Outputs_annual_energy_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "annual_energy", &result))
            make_access_error("SAM_Hcpv", "annual_energy");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_Outputs_annual_input_radiation_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "annual_input_radiation", &result))
            make_access_error("SAM_Hcpv", "annual_input_radiation");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_Outputs_capacity_factor_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "capacity_factor", &result))
            make_access_error("SAM_Hcpv", "capacity_factor");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_Outputs_dc_loss_stowing_kwh_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "dc_loss_stowing_kwh", &result))
            make_access_error("SAM_Hcpv", "dc_loss_stowing_kwh");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_Outputs_dc_nominal_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "dc_nominal", &result))
            make_access_error("SAM_Hcpv", "dc_nominal");
    });
    return result;
}


SAM_EXPORT double *SAM_Hcpv_Outputs_gen_aget(SAM_Hcpv ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "gen", length);
        if (!result)
            make_access_error("SAM_Hcpv", "gen");
    });
    return result;
}


SAM_EXPORT double *SAM_Hcpv_Outputs_hourly_ac_aget(SAM_Hcpv ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "hourly_ac", length);
        if (!result)
            make_access_error("SAM_Hcpv", "hourly_ac");
    });
    return result;
}


SAM_EXPORT double *SAM_Hcpv_Outputs_hourly_airmass_aget(SAM_Hcpv ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "hourly_airmass", length);
        if (!result)
            make_access_error("SAM_Hcpv", "hourly_airmass");
    });
    return result;
}


SAM_EXPORT double *SAM_Hcpv_Outputs_hourly_beam_aget(SAM_Hcpv ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "hourly_beam", length);
        if (!result)
            make_access_error("SAM_Hcpv", "hourly_beam");
    });
    return result;
}


SAM_EXPORT double *SAM_Hcpv_Outputs_hourly_celleff_aget(SAM_Hcpv ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "hourly_celleff", length);
        if (!result)
            make_access_error("SAM_Hcpv", "hourly_celleff");
    });
    return result;
}


SAM_EXPORT double *SAM_Hcpv_Outputs_hourly_dc_aget(SAM_Hcpv ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "hourly_dc", length);
        if (!result)
            make_access_error("SAM_Hcpv", "hourly_dc");
    });
    return result;
}


SAM_EXPORT double *SAM_Hcpv_Outputs_hourly_dc_net_aget(SAM_Hcpv ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "hourly_dc_net", length);
        if (!result)
            make_access_error("SAM_Hcpv", "hourly_dc_net");
    });
    return result;
}


SAM_EXPORT double *SAM_Hcpv_Outputs_hourly_input_radiation_aget(SAM_Hcpv ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "hourly_input_radiation", length);
        if (!result)
            make_access_error("SAM_Hcpv", "hourly_input_radiation");
    });
    return result;
}


SAM_EXPORT double *SAM_Hcpv_Outputs_hourly_modeff_aget(SAM_Hcpv ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "hourly_modeff", length);
        if (!result)
            make_access_error("SAM_Hcpv", "hourly_modeff");
    });
    return result;
}


SAM_EXPORT double *SAM_Hcpv_Outputs_hourly_poa_aget(SAM_Hcpv ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "hourly_poa", length);
        if (!result)
            make_access_error("SAM_Hcpv", "hourly_poa");
    });
    return result;
}


SAM_EXPORT double *SAM_Hcpv_Outputs_hourly_sazi_aget(SAM_Hcpv ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "hourly_sazi", length);
        if (!result)
            make_access_error("SAM_Hcpv", "hourly_sazi");
    });
    return result;
}


SAM_EXPORT double *SAM_Hcpv_Outputs_hourly_shading_derate_aget(SAM_Hcpv ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "hourly_shading_derate", length);
        if (!result)
            make_access_error("SAM_Hcpv", "hourly_shading_derate");
    });
    return result;
}


SAM_EXPORT double *SAM_Hcpv_Outputs_hourly_solazi_aget(SAM_Hcpv ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "hourly_solazi", length);
        if (!result)
            make_access_error("SAM_Hcpv", "hourly_solazi");
    });
    return result;
}


SAM_EXPORT double *SAM_Hcpv_Outputs_hourly_solzen_aget(SAM_Hcpv ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "hourly_solzen", length);
        if (!result)
            make_access_error("SAM_Hcpv", "hourly_solzen");
    });
    return result;
}


SAM_EXPORT double *SAM_Hcpv_Outputs_hourly_stilt_aget(SAM_Hcpv ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "hourly_stilt", length);
        if (!result)
            make_access_error("SAM_Hcpv", "hourly_stilt");
    });
    return result;
}


SAM_EXPORT double *SAM_Hcpv_Outputs_hourly_sunup_aget(SAM_Hcpv ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "hourly_sunup", length);
        if (!result)
            make_access_error("SAM_Hcpv", "hourly_sunup");
    });
    return result;
}


SAM_EXPORT double *SAM_Hcpv_Outputs_hourly_tcell_aget(SAM_Hcpv ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "hourly_tcell", length);
        if (!result)
            make_access_error("SAM_Hcpv", "hourly_tcell");
    });
    return result;
}


SAM_EXPORT double *SAM_Hcpv_Outputs_hourly_tdry_aget(SAM_Hcpv ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "hourly_tdry", length);
        if (!result)
            make_access_error("SAM_Hcpv", "hourly_tdry");
    });
    return result;
}


SAM_EXPORT double *SAM_Hcpv_Outputs_hourly_tmod_aget(SAM_Hcpv ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "hourly_tmod", length);
        if (!result)
            make_access_error("SAM_Hcpv", "hourly_tmod");
    });
    return result;
}


SAM_EXPORT double *SAM_Hcpv_Outputs_hourly_windspd_aget(SAM_Hcpv ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "hourly_windspd", length);
        if (!result)
            make_access_error("SAM_Hcpv", "hourly_windspd");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_Outputs_kwh_per_kw_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "kwh_per_kw", &result))
            make_access_error("SAM_Hcpv", "kwh_per_kw");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_Outputs_modeff_ref_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "modeff_ref", &result))
            make_access_error("SAM_Hcpv", "modeff_ref");
    });
    return result;
}


SAM_EXPORT double *SAM_Hcpv_Outputs_monthly_beam_aget(SAM_Hcpv ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "monthly_beam", length);
        if (!result)
            make_access_error("SAM_Hcpv", "monthly_beam");
    });
    return result;
}


SAM_EXPORT double *SAM_Hcpv_Outputs_monthly_dc_net_aget(SAM_Hcpv ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "monthly_dc_net", length);
        if (!result)
            make_access_error("SAM_Hcpv", "monthly_dc_net");
    });
    return result;
}


SAM_EXPORT double *SAM_Hcpv_Outputs_monthly_energy_aget(SAM_Hcpv ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "monthly_energy", length);
        if (!result)
            make_access_error("SAM_Hcpv", "monthly_energy");
    });
    return result;
}


SAM_EXPORT double *SAM_Hcpv_Outputs_monthly_input_radiation_aget(SAM_Hcpv ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "monthly_input_radiation", length);
        if (!result)
            make_access_error("SAM_Hcpv", "monthly_input_radiation");
    });
    return result;
}


SAM_EXPORT double SAM_Hcpv_Outputs_tracker_nameplate_watts_nget(SAM_Hcpv ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "tracker_nameplate_watts", &result))
            make_access_error("SAM_Hcpv", "tracker_nameplate_watts");
    });
    return result;
}




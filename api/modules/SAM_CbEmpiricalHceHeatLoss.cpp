#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_CbEmpiricalHceHeatLoss.h"

SAM_EXPORT SAM_CbEmpiricalHceHeatLoss SAM_CbEmpiricalHceHeatLoss_construct(const char *def, SAM_error *err) {
    SAM_CbEmpiricalHceHeatLoss result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_create();
    });
    return result;
}

SAM_EXPORT int SAM_CbEmpiricalHceHeatLoss_execute(SAM_CbEmpiricalHceHeatLoss data, int verbosity, SAM_error *err) {
    int n_err = 0;
    translateExceptions(err, [&] {
        n_err += SAM_module_exec("cb_empirical_hce_heat_loss", data, verbosity, err);
    });
    return n_err;
}


SAM_EXPORT void SAM_CbEmpiricalHceHeatLoss_destruct(SAM_CbEmpiricalHceHeatLoss system) {
    ssc_data_free(system);
}

SAM_EXPORT void
SAM_CbEmpiricalHceHeatLoss_Hce_HCEFrac_aset(SAM_CbEmpiricalHceHeatLoss ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "HCEFrac", arr, length);
    });
}

SAM_EXPORT void
SAM_CbEmpiricalHceHeatLoss_Hce_HCE_A0_aset(SAM_CbEmpiricalHceHeatLoss ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "HCE_A0", arr, length);
    });
}

SAM_EXPORT void
SAM_CbEmpiricalHceHeatLoss_Hce_HCE_A1_aset(SAM_CbEmpiricalHceHeatLoss ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "HCE_A1", arr, length);
    });
}

SAM_EXPORT void
SAM_CbEmpiricalHceHeatLoss_Hce_HCE_A2_aset(SAM_CbEmpiricalHceHeatLoss ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "HCE_A2", arr, length);
    });
}

SAM_EXPORT void
SAM_CbEmpiricalHceHeatLoss_Hce_HCE_A3_aset(SAM_CbEmpiricalHceHeatLoss ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "HCE_A3", arr, length);
    });
}

SAM_EXPORT void
SAM_CbEmpiricalHceHeatLoss_Hce_HCE_A4_aset(SAM_CbEmpiricalHceHeatLoss ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "HCE_A4", arr, length);
    });
}

SAM_EXPORT void
SAM_CbEmpiricalHceHeatLoss_Hce_HCE_A5_aset(SAM_CbEmpiricalHceHeatLoss ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "HCE_A5", arr, length);
    });
}

SAM_EXPORT void
SAM_CbEmpiricalHceHeatLoss_Hce_HCE_A6_aset(SAM_CbEmpiricalHceHeatLoss ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "HCE_A6", arr, length);
    });
}

SAM_EXPORT void
SAM_CbEmpiricalHceHeatLoss_Hce_PerfFac_aset(SAM_CbEmpiricalHceHeatLoss ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "PerfFac", arr, length);
    });
}

SAM_EXPORT void SAM_CbEmpiricalHceHeatLoss_Hce_RefMirrAper_aset(SAM_CbEmpiricalHceHeatLoss ptr, double *arr, int length,
                                                                SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "RefMirrAper", arr, length);
    });
}

SAM_EXPORT void
SAM_CbEmpiricalHceHeatLoss_Hce_SfInTempD_nset(SAM_CbEmpiricalHceHeatLoss ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "SfInTempD", number);
    });
}

SAM_EXPORT void
SAM_CbEmpiricalHceHeatLoss_Hce_SfOutTempD_nset(SAM_CbEmpiricalHceHeatLoss ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "SfOutTempD", number);
    });
}

SAM_EXPORT void
SAM_CbEmpiricalHceHeatLoss_Hce_ui_reference_ambient_temperature_nset(SAM_CbEmpiricalHceHeatLoss ptr, double number,
                                                                     SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "ui_reference_ambient_temperature", number);
    });
}

SAM_EXPORT void
SAM_CbEmpiricalHceHeatLoss_Hce_ui_reference_direct_normal_irradiance_nset(SAM_CbEmpiricalHceHeatLoss ptr, double number,
                                                                          SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "ui_reference_direct_normal_irradiance", number);
    });
}

SAM_EXPORT void
SAM_CbEmpiricalHceHeatLoss_Hce_ui_reference_wind_speed_nset(SAM_CbEmpiricalHceHeatLoss ptr, double number,
                                                            SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "ui_reference_wind_speed", number);
    });
}

SAM_EXPORT double *
SAM_CbEmpiricalHceHeatLoss_Hce_HCEFrac_aget(SAM_CbEmpiricalHceHeatLoss ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "HCEFrac", length);
        if (!result)
            make_access_error("SAM_CbEmpiricalHceHeatLoss", "HCEFrac");
    });
    return result;
}


SAM_EXPORT double *
SAM_CbEmpiricalHceHeatLoss_Hce_HCE_A0_aget(SAM_CbEmpiricalHceHeatLoss ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "HCE_A0", length);
        if (!result)
            make_access_error("SAM_CbEmpiricalHceHeatLoss", "HCE_A0");
    });
    return result;
}


SAM_EXPORT double *
SAM_CbEmpiricalHceHeatLoss_Hce_HCE_A1_aget(SAM_CbEmpiricalHceHeatLoss ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "HCE_A1", length);
        if (!result)
            make_access_error("SAM_CbEmpiricalHceHeatLoss", "HCE_A1");
    });
    return result;
}


SAM_EXPORT double *
SAM_CbEmpiricalHceHeatLoss_Hce_HCE_A2_aget(SAM_CbEmpiricalHceHeatLoss ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "HCE_A2", length);
        if (!result)
            make_access_error("SAM_CbEmpiricalHceHeatLoss", "HCE_A2");
    });
    return result;
}


SAM_EXPORT double *
SAM_CbEmpiricalHceHeatLoss_Hce_HCE_A3_aget(SAM_CbEmpiricalHceHeatLoss ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "HCE_A3", length);
        if (!result)
            make_access_error("SAM_CbEmpiricalHceHeatLoss", "HCE_A3");
    });
    return result;
}


SAM_EXPORT double *
SAM_CbEmpiricalHceHeatLoss_Hce_HCE_A4_aget(SAM_CbEmpiricalHceHeatLoss ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "HCE_A4", length);
        if (!result)
            make_access_error("SAM_CbEmpiricalHceHeatLoss", "HCE_A4");
    });
    return result;
}


SAM_EXPORT double *
SAM_CbEmpiricalHceHeatLoss_Hce_HCE_A5_aget(SAM_CbEmpiricalHceHeatLoss ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "HCE_A5", length);
        if (!result)
            make_access_error("SAM_CbEmpiricalHceHeatLoss", "HCE_A5");
    });
    return result;
}


SAM_EXPORT double *
SAM_CbEmpiricalHceHeatLoss_Hce_HCE_A6_aget(SAM_CbEmpiricalHceHeatLoss ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "HCE_A6", length);
        if (!result)
            make_access_error("SAM_CbEmpiricalHceHeatLoss", "HCE_A6");
    });
    return result;
}


SAM_EXPORT double *
SAM_CbEmpiricalHceHeatLoss_Hce_PerfFac_aget(SAM_CbEmpiricalHceHeatLoss ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "PerfFac", length);
        if (!result)
            make_access_error("SAM_CbEmpiricalHceHeatLoss", "PerfFac");
    });
    return result;
}


SAM_EXPORT double *
SAM_CbEmpiricalHceHeatLoss_Hce_RefMirrAper_aget(SAM_CbEmpiricalHceHeatLoss ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "RefMirrAper", length);
        if (!result)
            make_access_error("SAM_CbEmpiricalHceHeatLoss", "RefMirrAper");
    });
    return result;
}


SAM_EXPORT double SAM_CbEmpiricalHceHeatLoss_Hce_SfInTempD_nget(SAM_CbEmpiricalHceHeatLoss ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "SfInTempD", &result))
            make_access_error("SAM_CbEmpiricalHceHeatLoss", "SfInTempD");
    });
    return result;
}


SAM_EXPORT double SAM_CbEmpiricalHceHeatLoss_Hce_SfOutTempD_nget(SAM_CbEmpiricalHceHeatLoss ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "SfOutTempD", &result))
            make_access_error("SAM_CbEmpiricalHceHeatLoss", "SfOutTempD");
    });
    return result;
}


SAM_EXPORT double
SAM_CbEmpiricalHceHeatLoss_Hce_ui_reference_ambient_temperature_nget(SAM_CbEmpiricalHceHeatLoss ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "ui_reference_ambient_temperature", &result))
            make_access_error("SAM_CbEmpiricalHceHeatLoss", "ui_reference_ambient_temperature");
    });
    return result;
}


SAM_EXPORT double
SAM_CbEmpiricalHceHeatLoss_Hce_ui_reference_direct_normal_irradiance_nget(SAM_CbEmpiricalHceHeatLoss ptr,
                                                                          SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "ui_reference_direct_normal_irradiance", &result))
            make_access_error("SAM_CbEmpiricalHceHeatLoss", "ui_reference_direct_normal_irradiance");
    });
    return result;
}


SAM_EXPORT double
SAM_CbEmpiricalHceHeatLoss_Hce_ui_reference_wind_speed_nget(SAM_CbEmpiricalHceHeatLoss ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "ui_reference_wind_speed", &result))
            make_access_error("SAM_CbEmpiricalHceHeatLoss", "ui_reference_wind_speed");
    });
    return result;
}


SAM_EXPORT double *
SAM_CbEmpiricalHceHeatLoss_Outputs_HL_aget(SAM_CbEmpiricalHceHeatLoss ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "HL", length);
        if (!result)
            make_access_error("SAM_CbEmpiricalHceHeatLoss", "HL");
    });
    return result;
}


SAM_EXPORT double SAM_CbEmpiricalHceHeatLoss_Outputs_HL_weighted_nget(SAM_CbEmpiricalHceHeatLoss ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "HL_weighted", &result))
            make_access_error("SAM_CbEmpiricalHceHeatLoss", "HL_weighted");
    });
    return result;
}


SAM_EXPORT double
SAM_CbEmpiricalHceHeatLoss_Outputs_HL_weighted_m2_nget(SAM_CbEmpiricalHceHeatLoss ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "HL_weighted_m2", &result))
            make_access_error("SAM_CbEmpiricalHceHeatLoss", "HL_weighted_m2");
    });
    return result;
}




#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_IsccDesignPoint.h"

SAM_EXPORT SAM_IsccDesignPoint SAM_IsccDesignPoint_construct(const char *def, SAM_error *err) {
    SAM_IsccDesignPoint result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_create();
    });
    return result;
}

SAM_EXPORT int SAM_IsccDesignPoint_execute(SAM_IsccDesignPoint data, int verbosity, SAM_error *err) {
    int n_err = 0;
    translateExceptions(err, [&] {
        n_err += SAM_module_exec("iscc_design_point", data, verbosity, err);
    });
    return n_err;
}


SAM_EXPORT void SAM_IsccDesignPoint_destruct(SAM_IsccDesignPoint system) {
    ssc_data_free(system);
}

SAM_EXPORT void SAM_IsccDesignPoint_Common_HTF_code_nset(SAM_IsccDesignPoint ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "HTF_code", number);
    });
}

SAM_EXPORT void SAM_IsccDesignPoint_Common_elev_nset(SAM_IsccDesignPoint ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "elev", number);
    });
}

SAM_EXPORT void
SAM_IsccDesignPoint_Common_field_fl_props_mset(SAM_IsccDesignPoint ptr, double *mat, int nrows, int ncols,
                                               SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "field_fl_props", mat, nrows, ncols);
    });
}

SAM_EXPORT void SAM_IsccDesignPoint_Common_ngcc_model_nset(SAM_IsccDesignPoint ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "ngcc_model", number);
    });
}

SAM_EXPORT void
SAM_IsccDesignPoint_Common_pinch_point_cold_nset(SAM_IsccDesignPoint ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "pinch_point_cold", number);
    });
}

SAM_EXPORT void
SAM_IsccDesignPoint_Common_pinch_point_hot_nset(SAM_IsccDesignPoint ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "pinch_point_hot", number);
    });
}

SAM_EXPORT void SAM_IsccDesignPoint_Common_q_pb_design_nset(SAM_IsccDesignPoint ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "q_pb_design", number);
    });
}

SAM_EXPORT double SAM_IsccDesignPoint_Common_HTF_code_nget(SAM_IsccDesignPoint ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "HTF_code", &result))
            make_access_error("SAM_IsccDesignPoint", "HTF_code");
    });
    return result;
}


SAM_EXPORT double SAM_IsccDesignPoint_Common_elev_nget(SAM_IsccDesignPoint ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "elev", &result))
            make_access_error("SAM_IsccDesignPoint", "elev");
    });
    return result;
}


SAM_EXPORT double *
SAM_IsccDesignPoint_Common_field_fl_props_mget(SAM_IsccDesignPoint ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "field_fl_props", nrows, ncols);
        if (!result)
            make_access_error("SAM_IsccDesignPoint", "field_fl_props");
    });
    return result;
}


SAM_EXPORT double SAM_IsccDesignPoint_Common_ngcc_model_nget(SAM_IsccDesignPoint ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "ngcc_model", &result))
            make_access_error("SAM_IsccDesignPoint", "ngcc_model");
    });
    return result;
}


SAM_EXPORT double SAM_IsccDesignPoint_Common_pinch_point_cold_nget(SAM_IsccDesignPoint ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "pinch_point_cold", &result))
            make_access_error("SAM_IsccDesignPoint", "pinch_point_cold");
    });
    return result;
}


SAM_EXPORT double SAM_IsccDesignPoint_Common_pinch_point_hot_nget(SAM_IsccDesignPoint ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "pinch_point_hot", &result))
            make_access_error("SAM_IsccDesignPoint", "pinch_point_hot");
    });
    return result;
}


SAM_EXPORT double SAM_IsccDesignPoint_Common_q_pb_design_nget(SAM_IsccDesignPoint ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "q_pb_design", &result))
            make_access_error("SAM_IsccDesignPoint", "q_pb_design");
    });
    return result;
}


SAM_EXPORT double SAM_IsccDesignPoint_Outputs_T_htf_cold_nget(SAM_IsccDesignPoint ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "T_htf_cold", &result))
            make_access_error("SAM_IsccDesignPoint", "T_htf_cold");
    });
    return result;
}


SAM_EXPORT double SAM_IsccDesignPoint_Outputs_T_st_inject_nget(SAM_IsccDesignPoint ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "T_st_inject", &result))
            make_access_error("SAM_IsccDesignPoint", "T_st_inject");
    });
    return result;
}


SAM_EXPORT double SAM_IsccDesignPoint_Outputs_W_dot_fossil_nget(SAM_IsccDesignPoint ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "W_dot_fossil", &result))
            make_access_error("SAM_IsccDesignPoint", "W_dot_fossil");
    });
    return result;
}


SAM_EXPORT double SAM_IsccDesignPoint_Outputs_W_dot_solar_nget(SAM_IsccDesignPoint ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "W_dot_solar", &result))
            make_access_error("SAM_IsccDesignPoint", "W_dot_solar");
    });
    return result;
}


SAM_EXPORT double SAM_IsccDesignPoint_Outputs_q_solar_max_nget(SAM_IsccDesignPoint ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "q_solar_max", &result))
            make_access_error("SAM_IsccDesignPoint", "q_solar_max");
    });
    return result;
}




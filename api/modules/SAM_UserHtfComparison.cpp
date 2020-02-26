#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_UserHtfComparison.h"

SAM_EXPORT SAM_UserHtfComparison SAM_UserHtfComparison_construct(const char *def, SAM_error *err) {
    SAM_UserHtfComparison result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_create();
    });
    return result;
}

SAM_EXPORT int SAM_UserHtfComparison_execute(SAM_UserHtfComparison data, int verbosity, SAM_error *err) {
    int n_err = 0;
    translateExceptions(err, [&] {
        n_err += SAM_module_exec("user_htf_comparison", data, verbosity, err);
    });
    return n_err;
}


SAM_EXPORT void SAM_UserHtfComparison_destruct(SAM_UserHtfComparison system) {
    ssc_data_free(system);
}

SAM_EXPORT void SAM_UserHtfComparison_Common_HTF_code1_nset(SAM_UserHtfComparison ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "HTF_code1", number);
    });
}

SAM_EXPORT void SAM_UserHtfComparison_Common_HTF_code2_nset(SAM_UserHtfComparison ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "HTF_code2", number);
    });
}

SAM_EXPORT void
SAM_UserHtfComparison_Common_fl_props1_mset(SAM_UserHtfComparison ptr, double *mat, int nrows, int ncols,
                                            SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "fl_props1", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_UserHtfComparison_Common_fl_props2_mset(SAM_UserHtfComparison ptr, double *mat, int nrows, int ncols,
                                            SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "fl_props2", mat, nrows, ncols);
    });
}

SAM_EXPORT double SAM_UserHtfComparison_Common_HTF_code1_nget(SAM_UserHtfComparison ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "HTF_code1", &result))
            make_access_error("SAM_UserHtfComparison", "HTF_code1");
    });
    return result;
}


SAM_EXPORT double SAM_UserHtfComparison_Common_HTF_code2_nget(SAM_UserHtfComparison ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "HTF_code2", &result))
            make_access_error("SAM_UserHtfComparison", "HTF_code2");
    });
    return result;
}


SAM_EXPORT double *
SAM_UserHtfComparison_Common_fl_props1_mget(SAM_UserHtfComparison ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "fl_props1", nrows, ncols);
        if (!result)
            make_access_error("SAM_UserHtfComparison", "fl_props1");
    });
    return result;
}


SAM_EXPORT double *
SAM_UserHtfComparison_Common_fl_props2_mget(SAM_UserHtfComparison ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "fl_props2", nrows, ncols);
        if (!result)
            make_access_error("SAM_UserHtfComparison", "fl_props2");
    });
    return result;
}


SAM_EXPORT double SAM_UserHtfComparison_Outputs_are_equal_nget(SAM_UserHtfComparison ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "are_equal", &result))
            make_access_error("SAM_UserHtfComparison", "are_equal");
    });
    return result;
}




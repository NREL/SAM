#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Iec61853par.h"

SAM_EXPORT SAM_Iec61853par SAM_Iec61853par_construct(const char *def, SAM_error *err) {
    SAM_Iec61853par result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_create();
    });
    return result;
}

SAM_EXPORT int SAM_Iec61853par_execute(SAM_Iec61853par data, int verbosity, SAM_error *err) {
    int n_err = 0;
    translateExceptions(err, [&] {
        n_err += SAM_module_exec("iec61853par", data, verbosity, err);
    });
    return n_err;
}


SAM_EXPORT void SAM_Iec61853par_destruct(SAM_Iec61853par system) {
    ssc_data_free(system);
}

SAM_EXPORT void
SAM_Iec61853par_IEC61853_input_mset(SAM_Iec61853par ptr, double *mat, int nrows, int ncols, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "input", mat, nrows, ncols);
    });
}

SAM_EXPORT void SAM_Iec61853par_IEC61853_nser_nset(SAM_Iec61853par ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "nser", number);
    });
}

SAM_EXPORT void SAM_Iec61853par_IEC61853_type_nset(SAM_Iec61853par ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "type", number);
    });
}

SAM_EXPORT void SAM_Iec61853par_IEC61853_verbose_nset(SAM_Iec61853par ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "verbose", number);
    });
}

SAM_EXPORT double *SAM_Iec61853par_IEC61853_input_mget(SAM_Iec61853par ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "input", nrows, ncols);
        if (!result)
            make_access_error("SAM_Iec61853par", "input");
    });
    return result;
}


SAM_EXPORT double SAM_Iec61853par_IEC61853_nser_nget(SAM_Iec61853par ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "nser", &result))
            make_access_error("SAM_Iec61853par", "nser");
    });
    return result;
}


SAM_EXPORT double SAM_Iec61853par_IEC61853_type_nget(SAM_Iec61853par ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "type", &result))
            make_access_error("SAM_Iec61853par", "type");
    });
    return result;
}


SAM_EXPORT double SAM_Iec61853par_IEC61853_verbose_nget(SAM_Iec61853par ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "verbose", &result))
            make_access_error("SAM_Iec61853par", "verbose");
    });
    return result;
}


SAM_EXPORT double SAM_Iec61853par_Outputs_C1_nget(SAM_Iec61853par ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "C1", &result))
            make_access_error("SAM_Iec61853par", "C1");
    });
    return result;
}


SAM_EXPORT double SAM_Iec61853par_Outputs_C2_nget(SAM_Iec61853par ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "C2", &result))
            make_access_error("SAM_Iec61853par", "C2");
    });
    return result;
}


SAM_EXPORT double SAM_Iec61853par_Outputs_C3_nget(SAM_Iec61853par ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "C3", &result))
            make_access_error("SAM_Iec61853par", "C3");
    });
    return result;
}


SAM_EXPORT double SAM_Iec61853par_Outputs_D1_nget(SAM_Iec61853par ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "D1", &result))
            make_access_error("SAM_Iec61853par", "D1");
    });
    return result;
}


SAM_EXPORT double SAM_Iec61853par_Outputs_D2_nget(SAM_Iec61853par ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "D2", &result))
            make_access_error("SAM_Iec61853par", "D2");
    });
    return result;
}


SAM_EXPORT double SAM_Iec61853par_Outputs_D3_nget(SAM_Iec61853par ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "D3", &result))
            make_access_error("SAM_Iec61853par", "D3");
    });
    return result;
}


SAM_EXPORT double SAM_Iec61853par_Outputs_Egref_nget(SAM_Iec61853par ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "Egref", &result))
            make_access_error("SAM_Iec61853par", "Egref");
    });
    return result;
}


SAM_EXPORT double SAM_Iec61853par_Outputs_Il_nget(SAM_Iec61853par ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "Il", &result))
            make_access_error("SAM_Iec61853par", "Il");
    });
    return result;
}


SAM_EXPORT double SAM_Iec61853par_Outputs_Io_nget(SAM_Iec61853par ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "Io", &result))
            make_access_error("SAM_Iec61853par", "Io");
    });
    return result;
}


SAM_EXPORT double SAM_Iec61853par_Outputs_alphaIsc_nget(SAM_Iec61853par ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "alphaIsc", &result))
            make_access_error("SAM_Iec61853par", "alphaIsc");
    });
    return result;
}


SAM_EXPORT double SAM_Iec61853par_Outputs_betaVoc_nget(SAM_Iec61853par ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "betaVoc", &result))
            make_access_error("SAM_Iec61853par", "betaVoc");
    });
    return result;
}


SAM_EXPORT double SAM_Iec61853par_Outputs_gammaPmp_nget(SAM_Iec61853par ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "gammaPmp", &result))
            make_access_error("SAM_Iec61853par", "gammaPmp");
    });
    return result;
}


SAM_EXPORT double SAM_Iec61853par_Outputs_n_nget(SAM_Iec61853par ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "n", &result))
            make_access_error("SAM_Iec61853par", "n");
    });
    return result;
}




#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_6parsolve.h"

SAM_EXPORT SAM_6parsolve SAM_6parsolve_construct(const char *def, SAM_error *err) {
    SAM_6parsolve result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_create();
    });
    return result;
}

SAM_EXPORT int SAM_6parsolve_execute(SAM_6parsolve data, int verbosity, SAM_error *err) {
    int n_err = 0;
    translateExceptions(err, [&] {
        n_err += SAM_module_exec("6parsolve", data, verbosity, err);
    });
    return n_err;
}


SAM_EXPORT void SAM_6parsolve_destruct(SAM_6parsolve system) {
    ssc_data_free(system);
}

SAM_EXPORT void SAM_6parsolve_SixParameterSolver_Imp_nset(SAM_6parsolve ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "Imp", number);
    });
}

SAM_EXPORT void SAM_6parsolve_SixParameterSolver_Isc_nset(SAM_6parsolve ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "Isc", number);
    });
}

SAM_EXPORT void SAM_6parsolve_SixParameterSolver_Nser_nset(SAM_6parsolve ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "Nser", number);
    });
}

SAM_EXPORT void SAM_6parsolve_SixParameterSolver_Tref_nset(SAM_6parsolve ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "Tref", number);
    });
}

SAM_EXPORT void SAM_6parsolve_SixParameterSolver_Vmp_nset(SAM_6parsolve ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "Vmp", number);
    });
}

SAM_EXPORT void SAM_6parsolve_SixParameterSolver_Voc_nset(SAM_6parsolve ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "Voc", number);
    });
}

SAM_EXPORT void SAM_6parsolve_SixParameterSolver_alpha_isc_nset(SAM_6parsolve ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "alpha_isc", number);
    });
}

SAM_EXPORT void SAM_6parsolve_SixParameterSolver_beta_voc_nset(SAM_6parsolve ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "beta_voc", number);
    });
}

SAM_EXPORT void SAM_6parsolve_SixParameterSolver_celltype_sset(SAM_6parsolve ptr, const char *str, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_string(ptr, "celltype", str);
    });
}

SAM_EXPORT void SAM_6parsolve_SixParameterSolver_gamma_pmp_nset(SAM_6parsolve ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "gamma_pmp", number);
    });
}

SAM_EXPORT double SAM_6parsolve_SixParameterSolver_Imp_nget(SAM_6parsolve ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "Imp", &result))
            make_access_error("SAM_6parsolve", "Imp");
    });
    return result;
}


SAM_EXPORT double SAM_6parsolve_SixParameterSolver_Isc_nget(SAM_6parsolve ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "Isc", &result))
            make_access_error("SAM_6parsolve", "Isc");
    });
    return result;
}


SAM_EXPORT double SAM_6parsolve_SixParameterSolver_Nser_nget(SAM_6parsolve ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "Nser", &result))
            make_access_error("SAM_6parsolve", "Nser");
    });
    return result;
}


SAM_EXPORT double SAM_6parsolve_SixParameterSolver_Tref_nget(SAM_6parsolve ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "Tref", &result))
            make_access_error("SAM_6parsolve", "Tref");
    });
    return result;
}


SAM_EXPORT double SAM_6parsolve_SixParameterSolver_Vmp_nget(SAM_6parsolve ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "Vmp", &result))
            make_access_error("SAM_6parsolve", "Vmp");
    });
    return result;
}


SAM_EXPORT double SAM_6parsolve_SixParameterSolver_Voc_nget(SAM_6parsolve ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "Voc", &result))
            make_access_error("SAM_6parsolve", "Voc");
    });
    return result;
}


SAM_EXPORT double SAM_6parsolve_SixParameterSolver_alpha_isc_nget(SAM_6parsolve ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "alpha_isc", &result))
            make_access_error("SAM_6parsolve", "alpha_isc");
    });
    return result;
}


SAM_EXPORT double SAM_6parsolve_SixParameterSolver_beta_voc_nget(SAM_6parsolve ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "beta_voc", &result))
            make_access_error("SAM_6parsolve", "beta_voc");
    });
    return result;
}


SAM_EXPORT const char *SAM_6parsolve_SixParameterSolver_celltype_sget(SAM_6parsolve ptr, SAM_error *err) {
    const char *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_string(ptr, "celltype");
        if (!result)
            make_access_error("SAM_6parsolve", "celltype");
    });
    return result;
}


SAM_EXPORT double SAM_6parsolve_SixParameterSolver_gamma_pmp_nget(SAM_6parsolve ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "gamma_pmp", &result))
            make_access_error("SAM_6parsolve", "gamma_pmp");
    });
    return result;
}


SAM_EXPORT double SAM_6parsolve_Outputs_Adj_nget(SAM_6parsolve ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "Adj", &result))
            make_access_error("SAM_6parsolve", "Adj");
    });
    return result;
}


SAM_EXPORT double SAM_6parsolve_Outputs_Il_nget(SAM_6parsolve ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "Il", &result))
            make_access_error("SAM_6parsolve", "Il");
    });
    return result;
}


SAM_EXPORT double SAM_6parsolve_Outputs_Io_nget(SAM_6parsolve ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "Io", &result))
            make_access_error("SAM_6parsolve", "Io");
    });
    return result;
}


SAM_EXPORT double SAM_6parsolve_Outputs_Rs_nget(SAM_6parsolve ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "Rs", &result))
            make_access_error("SAM_6parsolve", "Rs");
    });
    return result;
}


SAM_EXPORT double SAM_6parsolve_Outputs_Rsh_nget(SAM_6parsolve ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "Rsh", &result))
            make_access_error("SAM_6parsolve", "Rsh");
    });
    return result;
}


SAM_EXPORT double SAM_6parsolve_Outputs_a_nget(SAM_6parsolve ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "a", &result))
            make_access_error("SAM_6parsolve", "a");
    });
    return result;
}




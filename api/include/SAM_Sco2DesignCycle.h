#ifndef SAM_SCO2DESIGNCYCLE_H_
#define SAM_SCO2DESIGNCYCLE_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

//
// Sco2DesignCycle Technology Model
//

/**
 * Create a Sco2DesignCycle variable table.
 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
 * @param[in,out] err: a pointer to an error object
 */

SAM_EXPORT typedef void *SAM_Sco2DesignCycle;

SAM_EXPORT SAM_Sco2DesignCycle SAM_Sco2DesignCycle_construct(const char *def, SAM_error *err);

/// verbosity level 0 or 1. Returns 1 on success
SAM_EXPORT int SAM_Sco2DesignCycle_execute(SAM_Sco2DesignCycle data, int verbosity, SAM_error *err);

SAM_EXPORT void SAM_Sco2DesignCycle_destruct(SAM_Sco2DesignCycle system);


//
// SCO2PowerCycle parameters
//

/**
 * Set I_N_t_des: Design turbine speed, negative links to comp. [rpm]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Sco2DesignCycle_SCO2PowerCycle_I_N_t_des_nset(SAM_Sco2DesignCycle ptr, double number, SAM_error *err);

/**
 * Set I_P_high_limit: High pressure limit in cycle [MPa]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Sco2DesignCycle_SCO2PowerCycle_I_P_high_limit_nset(SAM_Sco2DesignCycle ptr, double number, SAM_error *err);

/**
 * Set I_T_mc_in_des: Main compressor inlet temp at design [C]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Sco2DesignCycle_SCO2PowerCycle_I_T_mc_in_des_nset(SAM_Sco2DesignCycle ptr, double number, SAM_error *err);

/**
 * Set I_T_t_in_des: Turbine inlet temp at design [C]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Sco2DesignCycle_SCO2PowerCycle_I_T_t_in_des_nset(SAM_Sco2DesignCycle ptr, double number, SAM_error *err);

/**
 * Set I_UA_total_des: Total UA allocatable to recuperators [kW/K]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Sco2DesignCycle_SCO2PowerCycle_I_UA_total_des_nset(SAM_Sco2DesignCycle ptr, double number, SAM_error *err);

/**
 * Set I_W_dot_net_des: Design cycle power output [MW]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Sco2DesignCycle_SCO2PowerCycle_I_W_dot_net_des_nset(SAM_Sco2DesignCycle ptr, double number, SAM_error *err);

/**
 * Set I_eta_mc: Design main compressor isentropic efficiency [-]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Sco2DesignCycle_SCO2PowerCycle_I_eta_mc_nset(SAM_Sco2DesignCycle ptr, double number, SAM_error *err);

/**
 * Set I_eta_rc: Design re-compressor isentropic efficiency [-]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Sco2DesignCycle_SCO2PowerCycle_I_eta_rc_nset(SAM_Sco2DesignCycle ptr, double number, SAM_error *err);

/**
 * Set I_eta_t: Design turbine isentropic efficiency [-]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Sco2DesignCycle_SCO2PowerCycle_I_eta_t_nset(SAM_Sco2DesignCycle ptr, double number, SAM_error *err);

/**
 * Set I_opt_tol: Convergence tolerance - optimization calcs [-]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Sco2DesignCycle_SCO2PowerCycle_I_opt_tol_nset(SAM_Sco2DesignCycle ptr, double number, SAM_error *err);

/**
 * Set I_tol: Convergence tolerance for performance calcs [-]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Sco2DesignCycle_SCO2PowerCycle_I_tol_nset(SAM_Sco2DesignCycle ptr, double number, SAM_error *err);


/**
 * SCO2PowerCycle Getters
 */

SAM_EXPORT double SAM_Sco2DesignCycle_SCO2PowerCycle_I_N_t_des_nget(SAM_Sco2DesignCycle ptr, SAM_error *err);

SAM_EXPORT double SAM_Sco2DesignCycle_SCO2PowerCycle_I_P_high_limit_nget(SAM_Sco2DesignCycle ptr, SAM_error *err);

SAM_EXPORT double SAM_Sco2DesignCycle_SCO2PowerCycle_I_T_mc_in_des_nget(SAM_Sco2DesignCycle ptr, SAM_error *err);

SAM_EXPORT double SAM_Sco2DesignCycle_SCO2PowerCycle_I_T_t_in_des_nget(SAM_Sco2DesignCycle ptr, SAM_error *err);

SAM_EXPORT double SAM_Sco2DesignCycle_SCO2PowerCycle_I_UA_total_des_nget(SAM_Sco2DesignCycle ptr, SAM_error *err);

SAM_EXPORT double SAM_Sco2DesignCycle_SCO2PowerCycle_I_W_dot_net_des_nget(SAM_Sco2DesignCycle ptr, SAM_error *err);

SAM_EXPORT double SAM_Sco2DesignCycle_SCO2PowerCycle_I_eta_mc_nget(SAM_Sco2DesignCycle ptr, SAM_error *err);

SAM_EXPORT double SAM_Sco2DesignCycle_SCO2PowerCycle_I_eta_rc_nget(SAM_Sco2DesignCycle ptr, SAM_error *err);

SAM_EXPORT double SAM_Sco2DesignCycle_SCO2PowerCycle_I_eta_t_nget(SAM_Sco2DesignCycle ptr, SAM_error *err);

SAM_EXPORT double SAM_Sco2DesignCycle_SCO2PowerCycle_I_opt_tol_nget(SAM_Sco2DesignCycle ptr, SAM_error *err);

SAM_EXPORT double SAM_Sco2DesignCycle_SCO2PowerCycle_I_tol_nget(SAM_Sco2DesignCycle ptr, SAM_error *err);


/**
 * Outputs Getters
 */

SAM_EXPORT double SAM_Sco2DesignCycle_Outputs_O_LT_frac_des_nget(SAM_Sco2DesignCycle ptr, SAM_error *err);

SAM_EXPORT double SAM_Sco2DesignCycle_Outputs_O_N_mc_des_nget(SAM_Sco2DesignCycle ptr, SAM_error *err);

SAM_EXPORT double SAM_Sco2DesignCycle_Outputs_O_PR_mc_des_nget(SAM_Sco2DesignCycle ptr, SAM_error *err);

SAM_EXPORT double SAM_Sco2DesignCycle_Outputs_O_P_mc_out_des_nget(SAM_Sco2DesignCycle ptr, SAM_error *err);

SAM_EXPORT double *SAM_Sco2DesignCycle_Outputs_O_T_array_des_aget(SAM_Sco2DesignCycle ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Sco2DesignCycle_Outputs_O_eta_thermal_des_nget(SAM_Sco2DesignCycle ptr, SAM_error *err);

SAM_EXPORT double SAM_Sco2DesignCycle_Outputs_O_m_dot_PHX_nget(SAM_Sco2DesignCycle ptr, SAM_error *err);

SAM_EXPORT double SAM_Sco2DesignCycle_Outputs_O_recomp_frac_des_nget(SAM_Sco2DesignCycle ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif

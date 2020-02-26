#ifndef SAM_DSGFLUXPREPROCESS_H_
#define SAM_DSGFLUXPREPROCESS_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

//
// DsgFluxPreprocess Technology Model
//

/**
 * Create a DsgFluxPreprocess variable table.
 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
 * @param[in,out] err: a pointer to an error object
 */

SAM_EXPORT typedef void *SAM_DsgFluxPreprocess;

SAM_EXPORT SAM_DsgFluxPreprocess SAM_DsgFluxPreprocess_construct(const char *def, SAM_error *err);

/// verbosity level 0 or 1. Returns 1 on success
SAM_EXPORT int SAM_DsgFluxPreprocess_execute(SAM_DsgFluxPreprocess data, int verbosity, SAM_error *err);

SAM_EXPORT void SAM_DsgFluxPreprocess_destruct(SAM_DsgFluxPreprocess system);


//
// Common parameters
//

/**
 * Set CT: Cooling type
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_DsgFluxPreprocess_Common_CT_nset(SAM_DsgFluxPreprocess ptr, double number, SAM_error *err);

/**
 * Set P_HP_in: HP Turbine inlet pressure [bar]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_DsgFluxPreprocess_Common_P_HP_in_nset(SAM_DsgFluxPreprocess ptr, double number, SAM_error *err);

/**
 * Set P_HP_out: HP Turbine outlet pressure [bar]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_DsgFluxPreprocess_Common_P_HP_out_nset(SAM_DsgFluxPreprocess ptr, double number, SAM_error *err);

/**
 * Set P_cycle_des: Cycle power output at design [MW]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_DsgFluxPreprocess_Common_P_cycle_des_nset(SAM_DsgFluxPreprocess ptr, double number, SAM_error *err);

/**
 * Set Q_rec_des: Receiver thermal power at des. [MW]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_DsgFluxPreprocess_Common_Q_rec_des_nset(SAM_DsgFluxPreprocess ptr, double number, SAM_error *err);

/**
 * Set T_ITD_des: T_cond - T_db [C]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_DsgFluxPreprocess_Common_T_ITD_des_nset(SAM_DsgFluxPreprocess ptr, double number, SAM_error *err);

/**
 * Set T_amb_des: Ambient (wb) temp at design [C]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_DsgFluxPreprocess_Common_T_amb_des_nset(SAM_DsgFluxPreprocess ptr, double number, SAM_error *err);

/**
 * Set T_approach: dT cold cooling water - T_wb [C]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_DsgFluxPreprocess_Common_T_approach_nset(SAM_DsgFluxPreprocess ptr, double number, SAM_error *err);

/**
 * Set T_rh_out_ref: Reheater outlet temperature [C]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_DsgFluxPreprocess_Common_T_rh_out_ref_nset(SAM_DsgFluxPreprocess ptr, double number, SAM_error *err);

/**
 * Set T_sh_out_ref: Superheater outlet temperature [C]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_DsgFluxPreprocess_Common_T_sh_out_ref_nset(SAM_DsgFluxPreprocess ptr, double number, SAM_error *err);

/**
 * Set b_q_loss_flux: Boiler heat loss flux [kW/m2]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_DsgFluxPreprocess_Common_b_q_loss_flux_nset(SAM_DsgFluxPreprocess ptr, double number, SAM_error *err);

/**
 * Set dT_cooling_ref: dT of cooling water [C]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_DsgFluxPreprocess_Common_dT_cooling_ref_nset(SAM_DsgFluxPreprocess ptr, double number, SAM_error *err);

/**
 * Set eta_cycle_des: Cycle thermal efficiency at des.
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_DsgFluxPreprocess_Common_eta_cycle_des_nset(SAM_DsgFluxPreprocess ptr, double number, SAM_error *err);

/**
 * Set max_flux_b: Max allow. boiler flux [kW/m2]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_DsgFluxPreprocess_Common_max_flux_b_nset(SAM_DsgFluxPreprocess ptr, double number, SAM_error *err);

/**
 * Set max_flux_rh: Max allow. reheater flux [kW/m2]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_DsgFluxPreprocess_Common_max_flux_rh_nset(SAM_DsgFluxPreprocess ptr, double number, SAM_error *err);

/**
 * Set max_flux_sh: Max allow. superheater flux [kW/m2]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_DsgFluxPreprocess_Common_max_flux_sh_nset(SAM_DsgFluxPreprocess ptr, double number, SAM_error *err);

/**
 * Set rh_frac_ref: Mdot fraction to reheat at design
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_DsgFluxPreprocess_Common_rh_frac_ref_nset(SAM_DsgFluxPreprocess ptr, double number, SAM_error *err);

/**
 * Set rh_q_loss_flux: Reheater heat loss flux [kW/m2]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_DsgFluxPreprocess_Common_rh_q_loss_flux_nset(SAM_DsgFluxPreprocess ptr, double number, SAM_error *err);

/**
 * Set sh_q_loss_flux: Superheater heat loss flux [kW/m2]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_DsgFluxPreprocess_Common_sh_q_loss_flux_nset(SAM_DsgFluxPreprocess ptr, double number, SAM_error *err);


/**
 * Common Getters
 */

SAM_EXPORT double SAM_DsgFluxPreprocess_Common_CT_nget(SAM_DsgFluxPreprocess ptr, SAM_error *err);

SAM_EXPORT double SAM_DsgFluxPreprocess_Common_P_HP_in_nget(SAM_DsgFluxPreprocess ptr, SAM_error *err);

SAM_EXPORT double SAM_DsgFluxPreprocess_Common_P_HP_out_nget(SAM_DsgFluxPreprocess ptr, SAM_error *err);

SAM_EXPORT double SAM_DsgFluxPreprocess_Common_P_cycle_des_nget(SAM_DsgFluxPreprocess ptr, SAM_error *err);

SAM_EXPORT double SAM_DsgFluxPreprocess_Common_Q_rec_des_nget(SAM_DsgFluxPreprocess ptr, SAM_error *err);

SAM_EXPORT double SAM_DsgFluxPreprocess_Common_T_ITD_des_nget(SAM_DsgFluxPreprocess ptr, SAM_error *err);

SAM_EXPORT double SAM_DsgFluxPreprocess_Common_T_amb_des_nget(SAM_DsgFluxPreprocess ptr, SAM_error *err);

SAM_EXPORT double SAM_DsgFluxPreprocess_Common_T_approach_nget(SAM_DsgFluxPreprocess ptr, SAM_error *err);

SAM_EXPORT double SAM_DsgFluxPreprocess_Common_T_rh_out_ref_nget(SAM_DsgFluxPreprocess ptr, SAM_error *err);

SAM_EXPORT double SAM_DsgFluxPreprocess_Common_T_sh_out_ref_nget(SAM_DsgFluxPreprocess ptr, SAM_error *err);

SAM_EXPORT double SAM_DsgFluxPreprocess_Common_b_q_loss_flux_nget(SAM_DsgFluxPreprocess ptr, SAM_error *err);

SAM_EXPORT double SAM_DsgFluxPreprocess_Common_dT_cooling_ref_nget(SAM_DsgFluxPreprocess ptr, SAM_error *err);

SAM_EXPORT double SAM_DsgFluxPreprocess_Common_eta_cycle_des_nget(SAM_DsgFluxPreprocess ptr, SAM_error *err);

SAM_EXPORT double SAM_DsgFluxPreprocess_Common_max_flux_b_nget(SAM_DsgFluxPreprocess ptr, SAM_error *err);

SAM_EXPORT double SAM_DsgFluxPreprocess_Common_max_flux_rh_nget(SAM_DsgFluxPreprocess ptr, SAM_error *err);

SAM_EXPORT double SAM_DsgFluxPreprocess_Common_max_flux_sh_nget(SAM_DsgFluxPreprocess ptr, SAM_error *err);

SAM_EXPORT double SAM_DsgFluxPreprocess_Common_rh_frac_ref_nget(SAM_DsgFluxPreprocess ptr, SAM_error *err);

SAM_EXPORT double SAM_DsgFluxPreprocess_Common_rh_q_loss_flux_nget(SAM_DsgFluxPreprocess ptr, SAM_error *err);

SAM_EXPORT double SAM_DsgFluxPreprocess_Common_sh_q_loss_flux_nget(SAM_DsgFluxPreprocess ptr, SAM_error *err);


/**
 * Outputs Getters
 */

SAM_EXPORT double SAM_DsgFluxPreprocess_Outputs_f_b_nget(SAM_DsgFluxPreprocess ptr, SAM_error *err);

SAM_EXPORT double SAM_DsgFluxPreprocess_Outputs_f_rh_nget(SAM_DsgFluxPreprocess ptr, SAM_error *err);

SAM_EXPORT double SAM_DsgFluxPreprocess_Outputs_f_sh_nget(SAM_DsgFluxPreprocess ptr, SAM_error *err);

SAM_EXPORT double SAM_DsgFluxPreprocess_Outputs_max_flux_nget(SAM_DsgFluxPreprocess ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif

#ifndef SAM_CBMSPTSYSTEMCOSTS_H_
#define SAM_CBMSPTSYSTEMCOSTS_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

//
// CbMsptSystemCosts Technology Model
//

/**
 * Create a CbMsptSystemCosts variable table.
 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
 * @param[in,out] err: a pointer to an error object
 */

SAM_EXPORT typedef void *SAM_CbMsptSystemCosts;

SAM_EXPORT SAM_CbMsptSystemCosts SAM_CbMsptSystemCosts_construct(const char *def, SAM_error *err);

/// verbosity level 0 or 1. Returns 1 on success
SAM_EXPORT int SAM_CbMsptSystemCosts_execute(SAM_CbMsptSystemCosts data, int verbosity, SAM_error *err);

SAM_EXPORT void SAM_CbMsptSystemCosts_destruct(SAM_CbMsptSystemCosts system);


//
// Heliostat parameters
//

/**
 * Set A_sf: Total reflective solar field area [m2]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_CbMsptSystemCosts_Heliostat_A_sf_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err);


//
// SystemCosts parameters
//

/**
 * Set bop_spec_cost: BOP specific cost [$/kWe]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_bop_spec_cost_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err);

/**
 * Set contingency_rate: Contingency for cost overrun [%]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_contingency_rate_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err);

/**
 * Set csp.pt.cost.epc.fixed: EPC fixed [$]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_epc_fixed_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err);

/**
 * Set csp.pt.cost.epc.per_acre: EPC cost per acre [$/acre]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_epc_per_acre_nset(SAM_CbMsptSystemCosts ptr, double number,
                                                                SAM_error *err);

/**
 * Set csp.pt.cost.epc.per_watt: EPC cost per watt [$/W]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_epc_per_watt_nset(SAM_CbMsptSystemCosts ptr, double number,
                                                                SAM_error *err);

/**
 * Set csp.pt.cost.epc.percent: EPC cost percent of direct [%]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_epc_percent_nset(SAM_CbMsptSystemCosts ptr, double number,
                                                                               SAM_error *err);

/**
 * Set csp.pt.cost.fixed_sf: Heliostat field cost fixed [$]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_fixed_sf_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err);

/**
 * Set csp.pt.cost.plm.fixed: PLM fixed [$]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_plm_fixed_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err);

/**
 * Set csp.pt.cost.plm.per_acre: PLM cost per acre [$/acre]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_plm_per_acre_nset(SAM_CbMsptSystemCosts ptr, double number,
                                                                SAM_error *err);

/**
 * Set csp.pt.cost.plm.per_watt: PLM cost per watt [$/W]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_plm_per_watt_nset(SAM_CbMsptSystemCosts ptr, double number,
                                                                SAM_error *err);

/**
 * Set csp.pt.cost.plm.percent: PLM cost percent of direct [%]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_plm_percent_nset(SAM_CbMsptSystemCosts ptr, double number,
                                                                               SAM_error *err);

/**
 * Set csp.pt.cost.power_block_per_kwe: Power cycle specific cost [$/kWe]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_power_block_per_kwe_nset(SAM_CbMsptSystemCosts ptr, double number,
                                                                       SAM_error *err);

/**
 * Set csp.pt.cost.total_land_area: Total land area [acre]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_total_land_area_nset(SAM_CbMsptSystemCosts ptr, double number,
                                                                   SAM_error *err);

/**
 * Set fossil_spec_cost: Fossil system specific cost [$/kWe]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_fossil_spec_cost_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err);

/**
 * Set heliostat_spec_cost: Heliostat field cost [$/m2]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_heliostat_spec_cost_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err);

/**
 * Set rec_cost_exp: Receiver cost scaling exponent
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_rec_cost_exp_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err);

/**
 * Set rec_ref_area: Receiver reference area for cost scale
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_rec_ref_area_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err);

/**
 * Set rec_ref_cost: Receiver reference cost [$]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_rec_ref_cost_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err);

/**
 * Set sales_tax_frac: Percent of cost to which sales tax applies [%]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_sales_tax_frac_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err);

/**
 * Set sales_tax_rate: Sales tax rate [%]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_sales_tax_rate_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err);

/**
 * Set site_spec_cost: Site improvement cost [$/m2]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_site_spec_cost_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err);

/**
 * Set tes_spec_cost: Thermal energy storage cost [$/kWht]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_tes_spec_cost_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err);

/**
 * Set tower_exp: Tower cost scaling exponent
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_tower_exp_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err);

/**
 * Set tower_fixed_cost: Tower fixed cost [$]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_tower_fixed_cost_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err);


//
// Receiver parameters
//

/**
 * Set H_rec: The height of the receiver [m]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_CbMsptSystemCosts_Receiver_H_rec_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err);

/**
 * Set csp.pt.cost.receiver.area: Receiver area [m2]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_CbMsptSystemCosts_Receiver_csp_pt_cost_receiver_area_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err);

/**
 * Set h_tower: Tower height [m]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_CbMsptSystemCosts_Receiver_h_tower_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err);

/**
 * Set helio_height: Heliostat height [m]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_CbMsptSystemCosts_Receiver_helio_height_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err);


//
// TES parameters
//

/**
 * Set csp.pt.cost.storage_mwht: Storage capacity [MWt-hr]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_CbMsptSystemCosts_TES_csp_pt_cost_storage_mwht_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err);


//
// SystemDesign parameters
//

/**
 * Set P_ref: Reference output electric power at design condition [MWe]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_CbMsptSystemCosts_SystemDesign_P_ref_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err);

/**
 * Set system_capacity: Nameplate capacity [MWe]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemDesign_system_capacity_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err);


/**
 * Heliostat Getters
 */

SAM_EXPORT double SAM_CbMsptSystemCosts_Heliostat_A_sf_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);


/**
 * SystemCosts Getters
 */

SAM_EXPORT double SAM_CbMsptSystemCosts_SystemCosts_bop_spec_cost_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double SAM_CbMsptSystemCosts_SystemCosts_contingency_rate_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_epc_fixed_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_epc_per_acre_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_epc_per_watt_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_epc_percent_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_fixed_sf_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_plm_fixed_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_plm_per_acre_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_plm_per_watt_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_plm_percent_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_power_block_per_kwe_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_total_land_area_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double SAM_CbMsptSystemCosts_SystemCosts_fossil_spec_cost_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double SAM_CbMsptSystemCosts_SystemCosts_heliostat_spec_cost_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double SAM_CbMsptSystemCosts_SystemCosts_rec_cost_exp_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double SAM_CbMsptSystemCosts_SystemCosts_rec_ref_area_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double SAM_CbMsptSystemCosts_SystemCosts_rec_ref_cost_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double SAM_CbMsptSystemCosts_SystemCosts_sales_tax_frac_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double SAM_CbMsptSystemCosts_SystemCosts_sales_tax_rate_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double SAM_CbMsptSystemCosts_SystemCosts_site_spec_cost_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double SAM_CbMsptSystemCosts_SystemCosts_tes_spec_cost_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double SAM_CbMsptSystemCosts_SystemCosts_tower_exp_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double SAM_CbMsptSystemCosts_SystemCosts_tower_fixed_cost_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);


/**
 * Receiver Getters
 */

SAM_EXPORT double SAM_CbMsptSystemCosts_Receiver_H_rec_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double
SAM_CbMsptSystemCosts_Receiver_csp_pt_cost_receiver_area_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double SAM_CbMsptSystemCosts_Receiver_h_tower_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double SAM_CbMsptSystemCosts_Receiver_helio_height_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);


/**
 * TES Getters
 */

SAM_EXPORT double SAM_CbMsptSystemCosts_TES_csp_pt_cost_storage_mwht_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);


/**
 * SystemDesign Getters
 */

SAM_EXPORT double SAM_CbMsptSystemCosts_SystemDesign_P_ref_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double SAM_CbMsptSystemCosts_SystemDesign_system_capacity_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);


/**
 * Outputs Getters
 */

SAM_EXPORT double SAM_CbMsptSystemCosts_Outputs_csp_pt_cost_bop_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double SAM_CbMsptSystemCosts_Outputs_csp_pt_cost_contingency_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double SAM_CbMsptSystemCosts_Outputs_csp_pt_cost_epc_total_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double SAM_CbMsptSystemCosts_Outputs_csp_pt_cost_fossil_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double SAM_CbMsptSystemCosts_Outputs_csp_pt_cost_heliostats_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double
SAM_CbMsptSystemCosts_Outputs_csp_pt_cost_installed_per_capacity_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double SAM_CbMsptSystemCosts_Outputs_csp_pt_cost_plm_total_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double SAM_CbMsptSystemCosts_Outputs_csp_pt_cost_power_block_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double SAM_CbMsptSystemCosts_Outputs_csp_pt_cost_receiver_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double
SAM_CbMsptSystemCosts_Outputs_csp_pt_cost_sales_tax_total_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double
SAM_CbMsptSystemCosts_Outputs_csp_pt_cost_site_improvements_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double SAM_CbMsptSystemCosts_Outputs_csp_pt_cost_storage_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double SAM_CbMsptSystemCosts_Outputs_csp_pt_cost_tower_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double SAM_CbMsptSystemCosts_Outputs_total_direct_cost_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double SAM_CbMsptSystemCosts_Outputs_total_indirect_cost_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double SAM_CbMsptSystemCosts_Outputs_total_installed_cost_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

SAM_EXPORT double SAM_CbMsptSystemCosts_Outputs_ui_direct_subtotal_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif

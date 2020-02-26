#ifndef SAM_UTILITYRATE_H_
#define SAM_UTILITYRATE_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

//
// Utilityrate Technology Model
//

/**
 * Create a Utilityrate variable table.
 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
 * @param[in,out] err: a pointer to an error object
 */

SAM_EXPORT typedef void *SAM_Utilityrate;

SAM_EXPORT SAM_Utilityrate SAM_Utilityrate_construct(const char *def, SAM_error *err);

/// verbosity level 0 or 1. Returns 1 on success
SAM_EXPORT int SAM_Utilityrate_execute(SAM_Utilityrate data, int verbosity, SAM_error *err);

SAM_EXPORT void SAM_Utilityrate_destruct(SAM_Utilityrate system);


//
// Common parameters
//

/**
 * Set analysis_period: Number of years in analysis [years]
 * options: None
 * constraints: INTEGER,POSITIVE
 * required if: *
 */
SAM_EXPORT void SAM_Utilityrate_Common_analysis_period_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set e_with_system: Energy at grid with system [kWh]
 * options: None
 * constraints: LENGTH=8760
 * required if: *
 */
SAM_EXPORT void SAM_Utilityrate_Common_e_with_system_aset(SAM_Utilityrate ptr, double *arr, int length, SAM_error *err);

/**
 * Set e_without_system: Energy at grid without system (load only) [kWh]
 * options: None
 * constraints: LENGTH=8760
 * required if: ?
 */
SAM_EXPORT void
SAM_Utilityrate_Common_e_without_system_aset(SAM_Utilityrate ptr, double *arr, int length, SAM_error *err);

/**
 * Set load_escalation: Annual load escalation [%/year]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Utilityrate_Common_load_escalation_aset(SAM_Utilityrate ptr, double *arr, int length, SAM_error *err);

/**
 * Set p_with_system: Max power at grid with system [kW]
 * options: None
 * constraints: LENGTH=8760
 * required if: ?
 */
SAM_EXPORT void SAM_Utilityrate_Common_p_with_system_aset(SAM_Utilityrate ptr, double *arr, int length, SAM_error *err);

/**
 * Set p_without_system: Max power at grid without system (load only) [kW]
 * options: None
 * constraints: LENGTH=8760
 * required if: ?
 */
SAM_EXPORT void
SAM_Utilityrate_Common_p_without_system_aset(SAM_Utilityrate ptr, double *arr, int length, SAM_error *err);

/**
 * Set rate_escalation: Annual utility rate escalation [%/year]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Utilityrate_Common_rate_escalation_aset(SAM_Utilityrate ptr, double *arr, int length, SAM_error *err);

/**
 * Set system_availability: Annual availability of system [%/year]
 * options: None
 * constraints: None
 * required if: ?=100
 */
SAM_EXPORT void
SAM_Utilityrate_Common_system_availability_aset(SAM_Utilityrate ptr, double *arr, int length, SAM_error *err);

/**
 * Set system_degradation: Annual degradation of system [%/year]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Utilityrate_Common_system_degradation_aset(SAM_Utilityrate ptr, double *arr, int length, SAM_error *err);

/**
 * Set ur_dc_enable: Enable demand charges [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_enable_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_dc_fixed_m1: DC fixed rate January [$/kW,pk]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_fixed_m1_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_dc_fixed_m10: DC fixed rate October [$/kW,pk]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_fixed_m10_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_dc_fixed_m11: DC fixed rate November [$/kW,pk]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_fixed_m11_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_dc_fixed_m12: DC fixed rate December [$/kW,pk]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_fixed_m12_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_dc_fixed_m2: DC fixed rate February [$/kW,pk]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_fixed_m2_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_dc_fixed_m3: DC fixed rate March [$/kW,pk]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_fixed_m3_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_dc_fixed_m4: DC fixed rate April [$/kW,pk]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_fixed_m4_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_dc_fixed_m5: DC fixed rate May [$/kW,pk]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_fixed_m5_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_dc_fixed_m6: DC fixed rate June [$/kW,pk]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_fixed_m6_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_dc_fixed_m7: DC fixed rate July [$/kW,pk]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_fixed_m7_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_dc_fixed_m8: DC fixed rate August [$/kW,pk]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_fixed_m8_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_dc_fixed_m9: DC fixed rate September [$/kW,pk]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_fixed_m9_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_dc_p1: DC TOU rate period 1 [$/kW,pk]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_p1_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_dc_p2: DC TOU rate period 2 [$/kW,pk]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_p2_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_dc_p3: DC TOU rate period 3 [$/kW,pk]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_p3_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_dc_p4: DC TOU rate period 4 [$/kW,pk]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_p4_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_dc_p5: DC TOU rate period 5 [$/kW,pk]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_p5_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_dc_p6: DC TOU rate period 6 [$/kW,pk]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_p6_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_dc_p7: DC TOU rate period 7 [$/kW,pk]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_p7_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_dc_p8: DC TOU rate period 8 [$/kW,pk]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_p8_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_dc_p9: DC TOU rate period 9 [$/kW,pk]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_p9_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_dc_sched_weekday: DC TOU weekday schedule
 * options: 288 digits 0-9, 24x12
 * constraints: TOUSCHED
 * required if: ur_dc_enable=1
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_sched_weekday_sset(SAM_Utilityrate ptr, const char *str, SAM_error *err);

/**
 * Set ur_dc_sched_weekend: DC TOU weekend schedule
 * options: 288 digits 0-9, 24x12
 * constraints: TOUSCHED
 * required if: ur_dc_enable=1
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_dc_sched_weekend_sset(SAM_Utilityrate ptr, const char *str, SAM_error *err);

/**
 * Set ur_flat_buy_rate: Flat rate (buy) [$/kWh]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_flat_buy_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_flat_sell_rate: Flat rate (sell) [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_flat_sell_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_monthly_fixed_charge: Monthly fixed charge [$]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_monthly_fixed_charge_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_sell_eq_buy: Force sell rate equal to buy [0/1]
 * options: Enforce net metering
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_sell_eq_buy_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tou_enable: Enable time-of-use rates [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_enable_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tou_p1_buy_rate: TOU period 1 rate (buy) [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p1_buy_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tou_p1_sell_rate: TOU period 1 rate (sell) [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p1_sell_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tou_p2_buy_rate: TOU period 2 rate (buy) [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p2_buy_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tou_p2_sell_rate: TOU period 2 rate (sell) [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p2_sell_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tou_p3_buy_rate: TOU period 3 rate (buy) [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p3_buy_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tou_p3_sell_rate: TOU period 3 rate (sell) [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p3_sell_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tou_p4_buy_rate: TOU period 4 rate (buy) [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p4_buy_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tou_p4_sell_rate: TOU period 4 rate (sell) [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p4_sell_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tou_p5_buy_rate: TOU period 5 rate (buy) [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p5_buy_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tou_p5_sell_rate: TOU period 5 rate (sell) [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p5_sell_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tou_p6_buy_rate: TOU period 6 rate (buy) [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p6_buy_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tou_p6_sell_rate: TOU period 6 rate (sell) [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p6_sell_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tou_p7_buy_rate: TOU period 7 rate (buy) [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p7_buy_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tou_p7_sell_rate: TOU period 7 rate (sell) [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p7_sell_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tou_p8_buy_rate: TOU period 8 rate (buy) [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p8_buy_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tou_p8_sell_rate: TOU period 8 rate (sell) [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p8_sell_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tou_p9_buy_rate: TOU period 9 rate (buy) [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p9_buy_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tou_p9_sell_rate: TOU period 9 rate (sell) [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_p9_sell_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tou_sched_weekday: TOU weekday schedule
 * options: 288 digits 0-9, 24x12
 * constraints: TOUSCHED
 * required if: ur_tou_enable=1
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_sched_weekday_sset(SAM_Utilityrate ptr, const char *str, SAM_error *err);

/**
 * Set ur_tou_sched_weekend: TOU weekend schedule
 * options: 288 digits 0-9, 24x12
 * constraints: TOUSCHED
 * required if: ur_tou_enable=1
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tou_sched_weekend_sset(SAM_Utilityrate ptr, const char *str, SAM_error *err);

/**
 * Set ur_tr_enable: Enable tiered rates [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_enable_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s1_energy_ub1: Tiered struct. 1 Energy UB 1 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s1_energy_ub1_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s1_energy_ub2: Tiered struct. 1 Energy UB 2 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s1_energy_ub2_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s1_energy_ub3: Tiered struct. 1 Energy UB 3 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s1_energy_ub3_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s1_energy_ub4: Tiered struct. 1 Energy UB 4 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s1_energy_ub4_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s1_energy_ub5: Tiered struct. 1 Energy UB 5 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s1_energy_ub5_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s1_energy_ub6: Tiered struct. 1 Energy UB 6 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s1_energy_ub6_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s1_rate1: Tiered struct. 1 Rate 1 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s1_rate1_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s1_rate2: Tiered struct. 1 Rate 2 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s1_rate2_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s1_rate3: Tiered struct. 1 Rate 3 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s1_rate3_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s1_rate4: Tiered struct. 1 Rate 4 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s1_rate4_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s1_rate5: Tiered struct. 1 Rate 5 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s1_rate5_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s1_rate6: Tiered struct. 1 Rate 6 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s1_rate6_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s2_energy_ub1: Tiered struct. 2 Energy UB 1 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s2_energy_ub1_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s2_energy_ub2: Tiered struct. 2 Energy UB 2 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s2_energy_ub2_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s2_energy_ub3: Tiered struct. 2 Energy UB 3 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s2_energy_ub3_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s2_energy_ub4: Tiered struct. 2 Energy UB 4 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s2_energy_ub4_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s2_energy_ub5: Tiered struct. 2 Energy UB 5 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s2_energy_ub5_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s2_energy_ub6: Tiered struct. 2 Energy UB 6 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s2_energy_ub6_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s2_rate1: Tiered struct. 2 Rate 1 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s2_rate1_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s2_rate2: Tiered struct. 2 Rate 2 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s2_rate2_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s2_rate3: Tiered struct. 2 Rate 3 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s2_rate3_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s2_rate4: Tiered struct. 2 Rate 4 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s2_rate4_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s2_rate5: Tiered struct. 2 Rate 5 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s2_rate5_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s2_rate6: Tiered struct. 2 Rate 6 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s2_rate6_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s3_energy_ub1: Tiered struct. 3 Energy UB 1 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s3_energy_ub1_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s3_energy_ub2: Tiered struct. 3 Energy UB 2 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s3_energy_ub2_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s3_energy_ub3: Tiered struct. 3 Energy UB 3 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s3_energy_ub3_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s3_energy_ub4: Tiered struct. 3 Energy UB 4 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s3_energy_ub4_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s3_energy_ub5: Tiered struct. 3 Energy UB 5 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s3_energy_ub5_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s3_energy_ub6: Tiered struct. 3 Energy UB 6 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s3_energy_ub6_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s3_rate1: Tiered struct. 3 Rate 1 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s3_rate1_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s3_rate2: Tiered struct. 3 Rate 2 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s3_rate2_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s3_rate3: Tiered struct. 3 Rate 3 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s3_rate3_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s3_rate4: Tiered struct. 3 Rate 4 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s3_rate4_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s3_rate5: Tiered struct. 3 Rate 5 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s3_rate5_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s3_rate6: Tiered struct. 3 Rate 6 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s3_rate6_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s4_energy_ub1: Tiered struct. 4 Energy UB 1 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s4_energy_ub1_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s4_energy_ub2: Tiered struct. 4 Energy UB 2 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s4_energy_ub2_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s4_energy_ub3: Tiered struct. 4 Energy UB 3 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s4_energy_ub3_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s4_energy_ub4: Tiered struct. 4 Energy UB 4 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s4_energy_ub4_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s4_energy_ub5: Tiered struct. 4 Energy UB 5 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s4_energy_ub5_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s4_energy_ub6: Tiered struct. 4 Energy UB 6 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s4_energy_ub6_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s4_rate1: Tiered struct. 4 Rate 1 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s4_rate1_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s4_rate2: Tiered struct. 4 Rate 2 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s4_rate2_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s4_rate3: Tiered struct. 4 Rate 3 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s4_rate3_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s4_rate4: Tiered struct. 4 Rate 4 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s4_rate4_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s4_rate5: Tiered struct. 4 Rate 5 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s4_rate5_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s4_rate6: Tiered struct. 4 Rate 6 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s4_rate6_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s5_energy_ub1: Tiered struct. 5 Energy UB 1 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s5_energy_ub1_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s5_energy_ub2: Tiered struct. 5 Energy UB 2 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s5_energy_ub2_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s5_energy_ub3: Tiered struct. 5 Energy UB 3 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s5_energy_ub3_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s5_energy_ub4: Tiered struct. 5 Energy UB 4 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s5_energy_ub4_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s5_energy_ub5: Tiered struct. 5 Energy UB 5 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s5_energy_ub5_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s5_energy_ub6: Tiered struct. 5 Energy UB 6 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s5_energy_ub6_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s5_rate1: Tiered struct. 5 Rate 1 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s5_rate1_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s5_rate2: Tiered struct. 5 Rate 2 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s5_rate2_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s5_rate3: Tiered struct. 5 Rate 3 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s5_rate3_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s5_rate4: Tiered struct. 5 Rate 4 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s5_rate4_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s5_rate5: Tiered struct. 5 Rate 5 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s5_rate5_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s5_rate6: Tiered struct. 5 Rate 6 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s5_rate6_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s6_energy_ub1: Tiered struct. 6 Energy UB 1 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s6_energy_ub1_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s6_energy_ub2: Tiered struct. 6 Energy UB 2 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s6_energy_ub2_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s6_energy_ub3: Tiered struct. 6 Energy UB 3 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s6_energy_ub3_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s6_energy_ub4: Tiered struct. 6 Energy UB 4 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s6_energy_ub4_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s6_energy_ub5: Tiered struct. 6 Energy UB 5 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s6_energy_ub5_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s6_energy_ub6: Tiered struct. 6 Energy UB 6 [kWh]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s6_energy_ub6_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s6_rate1: Tiered struct. 6 Rate 1 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s6_rate1_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s6_rate2: Tiered struct. 6 Rate 2 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s6_rate2_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s6_rate3: Tiered struct. 6 Rate 3 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s6_rate3_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s6_rate4: Tiered struct. 6 Rate 4 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s6_rate4_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s6_rate5: Tiered struct. 6 Rate 5 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s6_rate5_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_s6_rate6: Tiered struct. 6 Rate 6 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_s6_rate6_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_sched_m1: Tiered structure for January [0-5]
 * options: tiered structure #
 * constraints: INTEGER,MIN=0,MAX=5
 * required if: ?=0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_sched_m1_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_sched_m10: Tiered structure for October [0-5]
 * options: tiered structure #
 * constraints: INTEGER,MIN=0,MAX=5
 * required if: ?=0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_sched_m10_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_sched_m11: Tiered structure for November [0-5]
 * options: tiered structure #
 * constraints: INTEGER,MIN=0,MAX=5
 * required if: ?=0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_sched_m11_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_sched_m12: Tiered structure for December [0-5]
 * options: tiered structure #
 * constraints: INTEGER,MIN=0,MAX=5
 * required if: ?=0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_sched_m12_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_sched_m2: Tiered structure for February [0-5]
 * options: tiered structure #
 * constraints: INTEGER,MIN=0,MAX=5
 * required if: ?=0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_sched_m2_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_sched_m3: Tiered structure for March [0-5]
 * options: tiered structure #
 * constraints: INTEGER,MIN=0,MAX=5
 * required if: ?=0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_sched_m3_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_sched_m4: Tiered structure for April [0-5]
 * options: tiered structure #
 * constraints: INTEGER,MIN=0,MAX=5
 * required if: ?=0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_sched_m4_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_sched_m5: Tiered structure for May [0-5]
 * options: tiered structure #
 * constraints: INTEGER,MIN=0,MAX=5
 * required if: ?=0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_sched_m5_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_sched_m6: Tiered structure for June [0-5]
 * options: tiered structure #
 * constraints: INTEGER,MIN=0,MAX=5
 * required if: ?=0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_sched_m6_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_sched_m7: Tiered structure for July [0-5]
 * options: tiered structure #
 * constraints: INTEGER,MIN=0,MAX=5
 * required if: ?=0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_sched_m7_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_sched_m8: Tiered structure for August [0-5]
 * options: tiered structure #
 * constraints: INTEGER,MIN=0,MAX=5
 * required if: ?=0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_sched_m8_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_sched_m9: Tiered structure for September [0-5]
 * options: tiered structure #
 * constraints: INTEGER,MIN=0,MAX=5
 * required if: ?=0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_sched_m9_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_sell_mode: Tiered rate sell mode [0,1,2]
 * options: 0=specified,1=tier1,2=lowest
 * constraints: INTEGER,MIN=0,MAX=2
 * required if: ?=1
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_sell_mode_nset(SAM_Utilityrate ptr, double number, SAM_error *err);

/**
 * Set ur_tr_sell_rate: Specified tiered sell rate [$/kW]
 * options: None
 * constraints: None
 * required if: ur_tr_sell_mode=0
 */
SAM_EXPORT void SAM_Utilityrate_Common_ur_tr_sell_rate_nset(SAM_Utilityrate ptr, double number, SAM_error *err);


/**
 * Common Getters
 */

SAM_EXPORT double SAM_Utilityrate_Common_analysis_period_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Common_e_with_system_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Common_e_without_system_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Common_load_escalation_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Common_p_with_system_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Common_p_without_system_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Common_rate_escalation_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Common_system_availability_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Common_system_degradation_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_enable_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_fixed_m1_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_fixed_m10_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_fixed_m11_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_fixed_m12_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_fixed_m2_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_fixed_m3_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_fixed_m4_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_fixed_m5_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_fixed_m6_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_fixed_m7_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_fixed_m8_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_fixed_m9_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_p1_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_p2_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_p3_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_p4_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_p5_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_p6_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_p7_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_p8_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_dc_p9_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT const char *SAM_Utilityrate_Common_ur_dc_sched_weekday_sget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT const char *SAM_Utilityrate_Common_ur_dc_sched_weekend_sget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_flat_buy_rate_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_flat_sell_rate_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_monthly_fixed_charge_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_sell_eq_buy_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_enable_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p1_buy_rate_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p1_sell_rate_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p2_buy_rate_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p2_sell_rate_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p3_buy_rate_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p3_sell_rate_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p4_buy_rate_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p4_sell_rate_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p5_buy_rate_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p5_sell_rate_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p6_buy_rate_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p6_sell_rate_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p7_buy_rate_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p7_sell_rate_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p8_buy_rate_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p8_sell_rate_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p9_buy_rate_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tou_p9_sell_rate_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT const char *SAM_Utilityrate_Common_ur_tou_sched_weekday_sget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT const char *SAM_Utilityrate_Common_ur_tou_sched_weekend_sget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_enable_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s1_energy_ub1_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s1_energy_ub2_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s1_energy_ub3_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s1_energy_ub4_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s1_energy_ub5_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s1_energy_ub6_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s1_rate1_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s1_rate2_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s1_rate3_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s1_rate4_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s1_rate5_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s1_rate6_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s2_energy_ub1_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s2_energy_ub2_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s2_energy_ub3_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s2_energy_ub4_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s2_energy_ub5_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s2_energy_ub6_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s2_rate1_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s2_rate2_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s2_rate3_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s2_rate4_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s2_rate5_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s2_rate6_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s3_energy_ub1_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s3_energy_ub2_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s3_energy_ub3_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s3_energy_ub4_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s3_energy_ub5_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s3_energy_ub6_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s3_rate1_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s3_rate2_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s3_rate3_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s3_rate4_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s3_rate5_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s3_rate6_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s4_energy_ub1_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s4_energy_ub2_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s4_energy_ub3_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s4_energy_ub4_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s4_energy_ub5_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s4_energy_ub6_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s4_rate1_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s4_rate2_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s4_rate3_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s4_rate4_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s4_rate5_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s4_rate6_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s5_energy_ub1_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s5_energy_ub2_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s5_energy_ub3_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s5_energy_ub4_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s5_energy_ub5_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s5_energy_ub6_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s5_rate1_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s5_rate2_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s5_rate3_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s5_rate4_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s5_rate5_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s5_rate6_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s6_energy_ub1_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s6_energy_ub2_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s6_energy_ub3_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s6_energy_ub4_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s6_energy_ub5_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s6_energy_ub6_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s6_rate1_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s6_rate2_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s6_rate3_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s6_rate4_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s6_rate5_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_s6_rate6_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_sched_m1_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_sched_m10_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_sched_m11_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_sched_m12_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_sched_m2_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_sched_m3_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_sched_m4_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_sched_m5_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_sched_m6_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_sched_m7_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_sched_m8_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_sched_m9_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_sell_mode_nget(SAM_Utilityrate ptr, SAM_error *err);

SAM_EXPORT double SAM_Utilityrate_Common_ur_tr_sell_rate_nget(SAM_Utilityrate ptr, SAM_error *err);


/**
 * Outputs Getters
 */

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_dc_fixed_apr_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_dc_fixed_aug_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_dc_fixed_dec_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_dc_fixed_feb_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_dc_fixed_jan_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_dc_fixed_jul_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_dc_fixed_jun_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_dc_fixed_mar_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_dc_fixed_may_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_dc_fixed_nov_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_dc_fixed_oct_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_dc_fixed_sep_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_dc_tou_apr_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_dc_tou_aug_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_dc_tou_dec_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_dc_tou_feb_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_dc_tou_jan_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_dc_tou_jul_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_dc_tou_jun_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_dc_tou_mar_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_dc_tou_may_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_dc_tou_nov_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_dc_tou_oct_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_dc_tou_sep_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_tr_apr_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_tr_aug_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_tr_dec_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_tr_feb_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_tr_jan_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_tr_jul_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_tr_jun_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_tr_mar_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_tr_may_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_tr_nov_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_tr_oct_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_charge_tr_sep_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_elec_cost_with_system_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Utilityrate_Outputs_elec_cost_without_system_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_energy_net_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_energy_value_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_revenue_with_system_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Utilityrate_Outputs_revenue_without_system_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_year1_hourly_e_demand_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_year1_hourly_e_grid_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Utilityrate_Outputs_year1_hourly_income_with_system_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Utilityrate_Outputs_year1_hourly_income_without_system_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_year1_hourly_p_demand_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Utilityrate_Outputs_year1_hourly_p_grid_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Utilityrate_Outputs_year1_hourly_p_system_to_load_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Utilityrate_Outputs_year1_hourly_payment_with_system_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Utilityrate_Outputs_year1_hourly_payment_without_system_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Utilityrate_Outputs_year1_hourly_price_with_system_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Utilityrate_Outputs_year1_hourly_price_without_system_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Utilityrate_Outputs_year1_hourly_revenue_with_system_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Utilityrate_Outputs_year1_hourly_revenue_without_system_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Utilityrate_Outputs_year1_hourly_system_output_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Utilityrate_Outputs_year1_hourly_system_to_grid_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Utilityrate_Outputs_year1_hourly_system_to_load_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Utilityrate_Outputs_year1_monthly_dc_fixed_with_system_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Utilityrate_Outputs_year1_monthly_dc_fixed_without_system_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Utilityrate_Outputs_year1_monthly_dc_tou_with_system_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Utilityrate_Outputs_year1_monthly_dc_tou_without_system_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Utilityrate_Outputs_year1_monthly_tr_charge_with_system_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Utilityrate_Outputs_year1_monthly_tr_charge_without_system_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Utilityrate_Outputs_year1_monthly_tr_rate_with_system_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Utilityrate_Outputs_year1_monthly_tr_rate_without_system_aget(SAM_Utilityrate ptr, int *length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif

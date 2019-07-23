#ifndef SAM_BATTERY_FUNCTIONS_H_
#define SAM_BATTERY_FUNCTIONS_H_

#include "Battery-data.h"

#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	/** 
	 * Create a BatteryModel variable table for a BatteryNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Battery_BatteryModel SAM_Battery_BatteryModel_create(const char* def, SAM_error* err);


	/**
	 * Set LeadAcid_q10: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryModel_LeadAcid_q10_set(SAM_Battery_BatteryModel ptr, float number, SAM_error* err);

	/**
	 * Set LeadAcid_q20: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryModel_LeadAcid_q20_set(SAM_Battery_BatteryModel ptr, float number, SAM_error* err);

	/**
	 * Set LeadAcid_qn: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryModel_LeadAcid_qn_set(SAM_Battery_BatteryModel ptr, float number, SAM_error* err);

	/**
	 * Set batt_type: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryModel_batt_type_set(SAM_Battery_BatteryModel ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Battery_BatteryModel_LeadAcid_q10_get(SAM_Battery_BatteryModel ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryModel_LeadAcid_q20_get(SAM_Battery_BatteryModel ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryModel_LeadAcid_qn_get(SAM_Battery_BatteryModel ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryModel_batt_type_get(SAM_Battery_BatteryModel ptr, SAM_error* err);



	/** 
	 * Create a BatteryBankSizing variable table for a BatteryNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Battery_BatteryBankSizing SAM_Battery_BatteryBankSizing_create(const char* def, SAM_error* err);


	/**
	 * Set batt_C_rate_max_charge_input: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryBankSizing_batt_C_rate_max_charge_input_set(SAM_Battery_BatteryBankSizing ptr, float number, SAM_error* err);

	/**
	 * Set batt_C_rate_max_discharge_input: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryBankSizing_batt_C_rate_max_discharge_input_set(SAM_Battery_BatteryBankSizing ptr, float number, SAM_error* err);

	/**
	 * Set batt_bank_ncells_serial: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryBankSizing_batt_bank_ncells_serial_set(SAM_Battery_BatteryBankSizing ptr, float number, SAM_error* err);

	/**
	 * Set batt_bank_nseries_stacks: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryBankSizing_batt_bank_nseries_stacks_set(SAM_Battery_BatteryBankSizing ptr, float number, SAM_error* err);

	/**
	 * Set batt_bank_nstrings: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryBankSizing_batt_bank_nstrings_set(SAM_Battery_BatteryBankSizing ptr, float number, SAM_error* err);

	/**
	 * Set batt_bank_power: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryBankSizing_batt_bank_power_set(SAM_Battery_BatteryBankSizing ptr, float number, SAM_error* err);

	/**
	 * Set batt_bank_power_dc_ac: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryBankSizing_batt_bank_power_dc_ac_set(SAM_Battery_BatteryBankSizing ptr, float number, SAM_error* err);

	/**
	 * Set batt_bank_size: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryBankSizing_batt_bank_size_set(SAM_Battery_BatteryBankSizing ptr, float number, SAM_error* err);

	/**
	 * Set batt_bank_size_dc_ac: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryBankSizing_batt_bank_size_dc_ac_set(SAM_Battery_BatteryBankSizing ptr, float number, SAM_error* err);

	/**
	 * Set batt_bank_size_specify: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryBankSizing_batt_bank_size_specify_set(SAM_Battery_BatteryBankSizing ptr, float number, SAM_error* err);

	/**
	 * Set batt_size_choice: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryBankSizing_batt_size_choice_set(SAM_Battery_BatteryBankSizing ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Battery_BatteryBankSizing_batt_C_rate_max_charge_input_get(SAM_Battery_BatteryBankSizing ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryBankSizing_batt_C_rate_max_discharge_input_get(SAM_Battery_BatteryBankSizing ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryBankSizing_batt_bank_ncells_serial_get(SAM_Battery_BatteryBankSizing ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryBankSizing_batt_bank_nseries_stacks_get(SAM_Battery_BatteryBankSizing ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryBankSizing_batt_bank_nstrings_get(SAM_Battery_BatteryBankSizing ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryBankSizing_batt_bank_power_get(SAM_Battery_BatteryBankSizing ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryBankSizing_batt_bank_power_dc_ac_get(SAM_Battery_BatteryBankSizing ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryBankSizing_batt_bank_size_get(SAM_Battery_BatteryBankSizing ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryBankSizing_batt_bank_size_dc_ac_get(SAM_Battery_BatteryBankSizing ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryBankSizing_batt_bank_size_specify_get(SAM_Battery_BatteryBankSizing ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryBankSizing_batt_size_choice_get(SAM_Battery_BatteryBankSizing ptr, SAM_error* err);



	/** 
	 * Create a BatteryCurrentAndCapacity variable table for a BatteryNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Battery_BatteryCurrentAndCapacity SAM_Battery_BatteryCurrentAndCapacity_create(const char* def, SAM_error* err);


	/**
	 * Set batt_Qfull: Fully charged cell capacity
	 * type: numeric
	 * units: Ah
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCurrentAndCapacity_batt_Qfull_set(SAM_Battery_BatteryCurrentAndCapacity ptr, float number, SAM_error* err);

	/**
	 * Set batt_cell_current_charge_max: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCurrentAndCapacity_batt_cell_current_charge_max_set(SAM_Battery_BatteryCurrentAndCapacity ptr, float number, SAM_error* err);

	/**
	 * Set batt_cell_current_discharge_max: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCurrentAndCapacity_batt_cell_current_discharge_max_set(SAM_Battery_BatteryCurrentAndCapacity ptr, float number, SAM_error* err);

	/**
	 * Set batt_cell_power_charge_max: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCurrentAndCapacity_batt_cell_power_charge_max_set(SAM_Battery_BatteryCurrentAndCapacity ptr, float number, SAM_error* err);

	/**
	 * Set batt_cell_power_discharge_max: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCurrentAndCapacity_batt_cell_power_discharge_max_set(SAM_Battery_BatteryCurrentAndCapacity ptr, float number, SAM_error* err);

	/**
	 * Set batt_current_choice: Limit cells by current or power
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCurrentAndCapacity_batt_current_choice_set(SAM_Battery_BatteryCurrentAndCapacity ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Battery_BatteryCurrentAndCapacity_batt_Qfull_get(SAM_Battery_BatteryCurrentAndCapacity ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryCurrentAndCapacity_batt_cell_current_charge_max_get(SAM_Battery_BatteryCurrentAndCapacity ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryCurrentAndCapacity_batt_cell_current_discharge_max_get(SAM_Battery_BatteryCurrentAndCapacity ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryCurrentAndCapacity_batt_cell_power_charge_max_get(SAM_Battery_BatteryCurrentAndCapacity ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryCurrentAndCapacity_batt_cell_power_discharge_max_get(SAM_Battery_BatteryCurrentAndCapacity ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryCurrentAndCapacity_batt_current_choice_get(SAM_Battery_BatteryCurrentAndCapacity ptr, SAM_error* err);



	/** 
	 * Create a BatteryConfiguration variable table for a BatteryNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Battery_BatteryConfiguration SAM_Battery_BatteryConfiguration_create(const char* def, SAM_error* err);


	/**
	 * Set batt_ac_or_dc: Battery interconnection (AC or DC)
	 * type: numeric
	 * units: None
	 * options: 0=DC_Connected,1=AC_Connected
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryConfiguration_batt_ac_or_dc_set(SAM_Battery_BatteryConfiguration ptr, float number, SAM_error* err);

	/**
	 * Set batt_dc_ac_efficiency: Battery DC to AC efficiency
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryConfiguration_batt_dc_ac_efficiency_set(SAM_Battery_BatteryConfiguration ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Battery_BatteryConfiguration_batt_ac_or_dc_get(SAM_Battery_BatteryConfiguration ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryConfiguration_batt_dc_ac_efficiency_get(SAM_Battery_BatteryConfiguration ptr, SAM_error* err);



	/** 
	 * Create a BatteryDispatchBehindTheMeter variable table for a BatteryNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Battery_BatteryDispatchBehindTheMeter SAM_Battery_BatteryDispatchBehindTheMeter_create(const char* def, SAM_error* err);


	/**
	 * Set batt_dispatch_choice: Battery dispatch algorithm
	 * type: numeric
	 * units: 0/1/2/3/4
	 * options: If behind the meter: 0=PeakShavingLookAhead,1=PeakShavingLookBehind,2=InputGridTarget,3=InputBatteryPower,4=ManualDispatch, if front of meter: 0=AutomatedLookAhead,1=AutomatedLookBehind,2=AutomatedInputForecast,3=InputBatteryPower,4=ManualDispatch
	 * constraints: None
	 * required if: en_batt=1
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatchBehindTheMeter_batt_dispatch_choice_set(SAM_Battery_BatteryDispatchBehindTheMeter ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Battery_BatteryDispatchBehindTheMeter_batt_dispatch_choice_get(SAM_Battery_BatteryDispatchBehindTheMeter ptr, SAM_error* err);



	/** 
	 * Create a BatteryDispatchCommon variable table for a BatteryNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Battery_BatteryDispatchCommon SAM_Battery_BatteryDispatchCommon_create(const char* def, SAM_error* err);


	/**
	 * Set batt_minimum_SOC: Minimum allowed state-of-charge
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatchCommon_batt_minimum_SOC_set(SAM_Battery_BatteryDispatchCommon ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Battery_BatteryDispatchCommon_batt_minimum_SOC_get(SAM_Battery_BatteryDispatchCommon ptr, SAM_error* err);



	/** 
	 * Create a BatteryDispatchManual variable table for a BatteryNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Battery_BatteryDispatchManual SAM_Battery_BatteryDispatchManual_create(const char* def, SAM_error* err);


	/**
	 * Set batt_discharge_percent_1: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatchManual_batt_discharge_percent_1_set(SAM_Battery_BatteryDispatchManual ptr, float number, SAM_error* err);

	/**
	 * Set batt_discharge_percent_2: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatchManual_batt_discharge_percent_2_set(SAM_Battery_BatteryDispatchManual ptr, float number, SAM_error* err);

	/**
	 * Set batt_discharge_percent_3: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatchManual_batt_discharge_percent_3_set(SAM_Battery_BatteryDispatchManual ptr, float number, SAM_error* err);

	/**
	 * Set batt_discharge_percent_4: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatchManual_batt_discharge_percent_4_set(SAM_Battery_BatteryDispatchManual ptr, float number, SAM_error* err);

	/**
	 * Set batt_discharge_percent_5: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatchManual_batt_discharge_percent_5_set(SAM_Battery_BatteryDispatchManual ptr, float number, SAM_error* err);

	/**
	 * Set batt_discharge_percent_6: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatchManual_batt_discharge_percent_6_set(SAM_Battery_BatteryDispatchManual ptr, float number, SAM_error* err);

	/**
	 * Set batt_gridcharge_percent_1: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatchManual_batt_gridcharge_percent_1_set(SAM_Battery_BatteryDispatchManual ptr, float number, SAM_error* err);

	/**
	 * Set batt_gridcharge_percent_2: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatchManual_batt_gridcharge_percent_2_set(SAM_Battery_BatteryDispatchManual ptr, float number, SAM_error* err);

	/**
	 * Set batt_gridcharge_percent_3: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatchManual_batt_gridcharge_percent_3_set(SAM_Battery_BatteryDispatchManual ptr, float number, SAM_error* err);

	/**
	 * Set batt_gridcharge_percent_4: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatchManual_batt_gridcharge_percent_4_set(SAM_Battery_BatteryDispatchManual ptr, float number, SAM_error* err);

	/**
	 * Set batt_gridcharge_percent_5: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatchManual_batt_gridcharge_percent_5_set(SAM_Battery_BatteryDispatchManual ptr, float number, SAM_error* err);

	/**
	 * Set batt_gridcharge_percent_6: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatchManual_batt_gridcharge_percent_6_set(SAM_Battery_BatteryDispatchManual ptr, float number, SAM_error* err);

	/**
	 * Set pv.storage.p1.discharge: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatchManual_pv.storage.p1.discharge_set(SAM_Battery_BatteryDispatchManual ptr, float number, SAM_error* err);

	/**
	 * Set pv.storage.p1.gridcharge: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatchManual_pv.storage.p1.gridcharge_set(SAM_Battery_BatteryDispatchManual ptr, float number, SAM_error* err);

	/**
	 * Set pv.storage.p2.discharge: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatchManual_pv.storage.p2.discharge_set(SAM_Battery_BatteryDispatchManual ptr, float number, SAM_error* err);

	/**
	 * Set pv.storage.p2.gridcharge: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatchManual_pv.storage.p2.gridcharge_set(SAM_Battery_BatteryDispatchManual ptr, float number, SAM_error* err);

	/**
	 * Set pv.storage.p3.discharge: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatchManual_pv.storage.p3.discharge_set(SAM_Battery_BatteryDispatchManual ptr, float number, SAM_error* err);

	/**
	 * Set pv.storage.p3.gridcharge: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatchManual_pv.storage.p3.gridcharge_set(SAM_Battery_BatteryDispatchManual ptr, float number, SAM_error* err);

	/**
	 * Set pv.storage.p4.discharge: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatchManual_pv.storage.p4.discharge_set(SAM_Battery_BatteryDispatchManual ptr, float number, SAM_error* err);

	/**
	 * Set pv.storage.p4.gridcharge: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatchManual_pv.storage.p4.gridcharge_set(SAM_Battery_BatteryDispatchManual ptr, float number, SAM_error* err);

	/**
	 * Set pv.storage.p5.discharge: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatchManual_pv.storage.p5.discharge_set(SAM_Battery_BatteryDispatchManual ptr, float number, SAM_error* err);

	/**
	 * Set pv.storage.p5.gridcharge: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatchManual_pv.storage.p5.gridcharge_set(SAM_Battery_BatteryDispatchManual ptr, float number, SAM_error* err);

	/**
	 * Set pv.storage.p6.discharge: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatchManual_pv.storage.p6.discharge_set(SAM_Battery_BatteryDispatchManual ptr, float number, SAM_error* err);

	/**
	 * Set pv.storage.p6.gridcharge: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatchManual_pv.storage.p6.gridcharge_set(SAM_Battery_BatteryDispatchManual ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Battery_BatteryDispatchManual_batt_discharge_percent_1_get(SAM_Battery_BatteryDispatchManual ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryDispatchManual_batt_discharge_percent_2_get(SAM_Battery_BatteryDispatchManual ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryDispatchManual_batt_discharge_percent_3_get(SAM_Battery_BatteryDispatchManual ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryDispatchManual_batt_discharge_percent_4_get(SAM_Battery_BatteryDispatchManual ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryDispatchManual_batt_discharge_percent_5_get(SAM_Battery_BatteryDispatchManual ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryDispatchManual_batt_discharge_percent_6_get(SAM_Battery_BatteryDispatchManual ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryDispatchManual_batt_gridcharge_percent_1_get(SAM_Battery_BatteryDispatchManual ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryDispatchManual_batt_gridcharge_percent_2_get(SAM_Battery_BatteryDispatchManual ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryDispatchManual_batt_gridcharge_percent_3_get(SAM_Battery_BatteryDispatchManual ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryDispatchManual_batt_gridcharge_percent_4_get(SAM_Battery_BatteryDispatchManual ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryDispatchManual_batt_gridcharge_percent_5_get(SAM_Battery_BatteryDispatchManual ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryDispatchManual_batt_gridcharge_percent_6_get(SAM_Battery_BatteryDispatchManual ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryDispatchManual_pv.storage.p1.discharge_get(SAM_Battery_BatteryDispatchManual ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryDispatchManual_pv.storage.p1.gridcharge_get(SAM_Battery_BatteryDispatchManual ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryDispatchManual_pv.storage.p2.discharge_get(SAM_Battery_BatteryDispatchManual ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryDispatchManual_pv.storage.p2.gridcharge_get(SAM_Battery_BatteryDispatchManual ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryDispatchManual_pv.storage.p3.discharge_get(SAM_Battery_BatteryDispatchManual ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryDispatchManual_pv.storage.p3.gridcharge_get(SAM_Battery_BatteryDispatchManual ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryDispatchManual_pv.storage.p4.discharge_get(SAM_Battery_BatteryDispatchManual ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryDispatchManual_pv.storage.p4.gridcharge_get(SAM_Battery_BatteryDispatchManual ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryDispatchManual_pv.storage.p5.discharge_get(SAM_Battery_BatteryDispatchManual ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryDispatchManual_pv.storage.p5.gridcharge_get(SAM_Battery_BatteryDispatchManual ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryDispatchManual_pv.storage.p6.discharge_get(SAM_Battery_BatteryDispatchManual ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryDispatchManual_pv.storage.p6.gridcharge_get(SAM_Battery_BatteryDispatchManual ptr, SAM_error* err);



	/** 
	 * Create a BatteryLifetime variable table for a BatteryNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Battery_BatteryLifetime SAM_Battery_BatteryLifetime_create(const char* def, SAM_error* err);


	/**
	 * Set batt_calendar_choice: Calendar life degradation input option
	 * type: numeric
	 * units: 0/1/2
	 * options: 0=NoCalendarDegradation,1=LithiomIonModel,2=InputLossTable
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryLifetime_batt_calendar_choice_set(SAM_Battery_BatteryLifetime ptr, float number, SAM_error* err);

	/**
	 * Set batt_lifetime_matrix: Cycles vs capacity at different depths-of-discharge
	 * type: matrix
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryLifetime_batt_lifetime_matrix_set(SAM_Battery_BatteryLifetime ptr, float* matrix, int nr, int nc, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Battery_BatteryLifetime_batt_calendar_choice_get(SAM_Battery_BatteryLifetime ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Battery_BatteryLifetime_batt_lifetime_matrix_get(SAM_Battery_BatteryLifetime ptr, SAM_error* err);



	/** 
	 * Create a BatteryReplacements variable table for a BatteryNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Battery_BatteryReplacements SAM_Battery_BatteryReplacements_create(const char* def, SAM_error* err);


	/**
	 * Set batt_replacement_option: Enable battery replacement?
	 * type: numeric
	 * units: 0=none,1=capacity based,2=user schedule
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=2
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_BatteryReplacements_batt_replacement_option_set(SAM_Battery_BatteryReplacements ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Battery_BatteryReplacements_batt_replacement_option_get(SAM_Battery_BatteryReplacements ptr, SAM_error* err);



	/** 
	 * Create a BatteryVoltage variable table for a BatteryNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Battery_BatteryVoltage SAM_Battery_BatteryVoltage_create(const char* def, SAM_error* err);


	/**
	 * Set batt_C_rate: Rate at which voltage vs. capacity curve input
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryVoltage_batt_C_rate_set(SAM_Battery_BatteryVoltage ptr, float number, SAM_error* err);

	/**
	 * Set batt_Qexp_percent: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryVoltage_batt_Qexp_percent_set(SAM_Battery_BatteryVoltage ptr, float number, SAM_error* err);

	/**
	 * Set batt_Qnom_percent: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryVoltage_batt_Qnom_percent_set(SAM_Battery_BatteryVoltage ptr, float number, SAM_error* err);

	/**
	 * Set batt_Vexp: Cell voltage at end of exponential zone
	 * type: numeric
	 * units: V
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryVoltage_batt_Vexp_set(SAM_Battery_BatteryVoltage ptr, float number, SAM_error* err);

	/**
	 * Set batt_Vfull: Fully charged cell voltage
	 * type: numeric
	 * units: V
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryVoltage_batt_Vfull_set(SAM_Battery_BatteryVoltage ptr, float number, SAM_error* err);

	/**
	 * Set batt_Vnom: Cell voltage at end of nominal zone
	 * type: numeric
	 * units: V
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryVoltage_batt_Vnom_set(SAM_Battery_BatteryVoltage ptr, float number, SAM_error* err);

	/**
	 * Set batt_Vnom_default: Default nominal cell voltage
	 * type: numeric
	 * units: V
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryVoltage_batt_Vnom_default_set(SAM_Battery_BatteryVoltage ptr, float number, SAM_error* err);

	/**
	 * Set batt_bank_voltage: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryVoltage_batt_bank_voltage_set(SAM_Battery_BatteryVoltage ptr, float number, SAM_error* err);

	/**
	 * Set batt_resistance: Internal resistance
	 * type: numeric
	 * units: Ohm
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryVoltage_batt_resistance_set(SAM_Battery_BatteryVoltage ptr, float number, SAM_error* err);

	/**
	 * Set batt_voltage_choice: Battery voltage input option
	 * type: numeric
	 * units: 0/1
	 * options: 0=UseVoltageModel,1=InputVoltageTable
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_BatteryVoltage_batt_voltage_choice_set(SAM_Battery_BatteryVoltage ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Battery_BatteryVoltage_batt_C_rate_get(SAM_Battery_BatteryVoltage ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryVoltage_batt_Qexp_percent_get(SAM_Battery_BatteryVoltage ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryVoltage_batt_Qnom_percent_get(SAM_Battery_BatteryVoltage ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryVoltage_batt_Vexp_get(SAM_Battery_BatteryVoltage ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryVoltage_batt_Vfull_get(SAM_Battery_BatteryVoltage ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryVoltage_batt_Vnom_get(SAM_Battery_BatteryVoltage ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryVoltage_batt_Vnom_default_get(SAM_Battery_BatteryVoltage ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryVoltage_batt_bank_voltage_get(SAM_Battery_BatteryVoltage ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryVoltage_batt_resistance_get(SAM_Battery_BatteryVoltage ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_BatteryVoltage_batt_voltage_choice_get(SAM_Battery_BatteryVoltage ptr, SAM_error* err);



	/** 
	 * Create a BatteryAncillaryLosses variable table for a BatteryNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Battery_BatteryAncillaryLosses SAM_Battery_BatteryAncillaryLosses_create(const char* def, SAM_error* err);


	/**
	 * Set batt_loss_choice: Loss power input option
	 * type: numeric
	 * units: 0/1
	 * options: 0=Monthly,1=TimeSeries
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_BatteryAncillaryLosses_batt_loss_choice_set(SAM_Battery_BatteryAncillaryLosses ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Battery_BatteryAncillaryLosses_batt_loss_choice_get(SAM_Battery_BatteryAncillaryLosses ptr, SAM_error* err);



	/** 
	 * Create a BatteryThermal variable table for a BatteryNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Battery_BatteryThermal SAM_Battery_BatteryThermal_create(const char* def, SAM_error* err);


	/**
	 * Set batt_specific_energy_per_mass: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryThermal_batt_specific_energy_per_mass_set(SAM_Battery_BatteryThermal ptr, float number, SAM_error* err);

	/**
	 * Set cap_vs_temp: Effective capacity as function of temperature
	 * type: matrix
	 * units: C,%
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryThermal_cap_vs_temp_set(SAM_Battery_BatteryThermal ptr, float* matrix, int nr, int nc, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Battery_BatteryThermal_batt_specific_energy_per_mass_get(SAM_Battery_BatteryThermal ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Battery_BatteryThermal_cap_vs_temp_get(SAM_Battery_BatteryThermal ptr, SAM_error* err);



	/** 
	 * Create a Common variable table for a BatteryNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Battery_Common SAM_Battery_Common_create(const char* def, SAM_error* err);


	/**
	 * Set annual_energy: Annual Energy
	 * type: numeric
	 * units: kWh
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_Common_annual_energy_set(SAM_Battery_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_target_choice: Target power input option
	 * type: numeric
	 * units: 0/1
	 * options: 0=InputMonthlyTarget,1=InputFullTimeSeries
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=0&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Battery_Common_batt_target_choice_set(SAM_Battery_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_target_power: Grid target power for every time step
	 * type: array
	 * units: kW
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=0&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Battery_Common_batt_target_power_set(SAM_Battery_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set batt_target_power_monthly: Grid target power on monthly basis
	 * type: array
	 * units: kW
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=0&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Battery_Common_batt_target_power_monthly_set(SAM_Battery_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set capacity_factor: Capacity factor
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_Common_capacity_factor_set(SAM_Battery_Common ptr, float number, SAM_error* err);

	/**
	 * Set dispatch_manual_fuelcellcharge: Periods 1-6 charging from fuel cell allowed?
	 * type: array
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Common_dispatch_manual_fuelcellcharge_set(SAM_Battery_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set fuelcell_power: Electricity from fuel cell
	 * type: array
	 * units: kW
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Common_fuelcell_power_set(SAM_Battery_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set gen: System power generated
	 * type: array
	 * units: kW
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Common_gen_set(SAM_Battery_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set load: Electricity load (year 1)
	 * type: array
	 * units: kW
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Common_load_set(SAM_Battery_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set percent_complete: Estimated simulation status
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Common_percent_complete_set(SAM_Battery_Common ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Battery_Common_annual_energy_get(SAM_Battery_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_Common_batt_target_choice_get(SAM_Battery_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Battery_Common_batt_target_power_get(SAM_Battery_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Battery_Common_batt_target_power_monthly_get(SAM_Battery_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_Common_capacity_factor_get(SAM_Battery_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Battery_Common_dispatch_manual_fuelcellcharge_get(SAM_Battery_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Battery_Common_fuelcell_power_get(SAM_Battery_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Battery_Common_gen_get(SAM_Battery_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Battery_Common_load_get(SAM_Battery_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Battery_Common_percent_complete_get(SAM_Battery_Common ptr, SAM_error* err);



#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
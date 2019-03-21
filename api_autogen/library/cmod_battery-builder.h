#ifndef _CMOD_BATTERY_BUILDER_H_
#define _CMOD_BATTERY_BUILDER_H_

#include "vartab.h"


//
// Evaluates batt_computed_voltage, batt_computed_series, batt_computed_strings, batt_num_cells, batt_computed_bank_capacity, batt_power_discharge_max, batt_power_charge_max, batt_time_capacity, batt_C_rate_max_charge, batt_C_rate_max_discharge, batt_current_charge_max, batt_current_discharge_max, batt_computed_stacks_series for a Battery Current and Capacity module
// @param *vt: a var_table* that contains: batt_size_choice, batt_chem, batt_bank_power, batt_bank_size, batt_bank_size_dc_ac, batt_dc_ac_efficiency, batt_bank_power_dc_ac, batt_Qfull, batt_bank_voltage, batt_Vnom_default, batt_bank_ncells_serial, batt_bank_nstrings, batt_C_rate_max_discharge_input, batt_C_rate_max_charge_input, batt_current_choice, batt_cell_power_discharge_max, batt_cell_current_discharge_max, batt_bank_nseries_stacks, batt_bank_size_specify, batt_cell_power_charge_max, batt_cell_current_charge_max
// @returns single value or var_table
//
var_table Battery_batt_computed_voltage_MIMO_eval(var_table* vt);

//
// Evaluates batt_width for a Battery Thermal module
// @param *vt: a var_table* that contains: batt_volume
// @returns single value or var_table
//
float Battery_batt_width_eval(var_table* vt);

//
// Evaluates LeadAcid_q10_computed, LeadAcid_q20_computed, LeadAcid_qn_computed for a Battery Current and Capacity module
// @param *vt: a var_table* that contains: batt_computed_strings, LeadAcid_q10, batt_Qfull, LeadAcid_q20, LeadAcid_qn
// @returns single value or var_table
//
var_table Battery_LeadAcid_q10_computed_MIMO_eval(var_table* vt);

//
// Evaluates batt_Qexp, batt_Qnom, batt_Qfull_flow for a Battery Current and Capacity module
// @param *vt: a var_table* that contains: batt_Qexp_percent, batt_Qfull, batt_Qnom_percent, batt_computed_bank_capacity, batt_computed_voltage
// @returns single value or var_table
//
var_table Battery_batt_Qexp_MIMO_eval(var_table* vt);

//
// Evaluates dispatch_manual_percent_gridcharge for a Battery Dispatch Manual module
// @param *vt: a var_table* that contains: dispatch_manual_gridcharge, batt_gridcharge_percent_1, batt_gridcharge_percent_2, batt_gridcharge_percent_3, batt_gridcharge_percent_4, batt_gridcharge_percent_5, batt_gridcharge_percent_6
// @returns single value or var_table
//
util::matrix_t<ssc_number_t> Battery_dispatch_manual_percent_gridcharge_eval(var_table* vt);

#endif
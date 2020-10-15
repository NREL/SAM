#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Battery.h"

SAM_EXPORT int SAM_Battery_execute(SAM_table data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("battery", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_Battery_Simulation_percent_complete_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "percent_complete", number);
	});
}

SAM_EXPORT void SAM_Battery_Lifetime_analysis_period_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "analysis_period", number);
	});
}

SAM_EXPORT void SAM_Battery_Lifetime_system_use_lifetime_output_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_use_lifetime_output", number);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_batt_ac_dc_efficiency_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_ac_dc_efficiency", number);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_batt_ac_or_dc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_ac_or_dc", number);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_batt_computed_bank_capacity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_computed_bank_capacity", number);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_batt_computed_series_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_computed_series", number);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_batt_computed_strings_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_computed_strings", number);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_batt_current_charge_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_current_charge_max", number);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_batt_current_choice_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_current_choice", number);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_batt_current_discharge_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_current_discharge_max", number);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_batt_cycle_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_cycle_cost", number);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_batt_cycle_cost_choice_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_cycle_cost_choice", number);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_batt_dc_ac_efficiency_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_dc_ac_efficiency", number);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_batt_dc_dc_efficiency_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_dc_dc_efficiency", number);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_batt_inverter_efficiency_cutoff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_inverter_efficiency_cutoff", number);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_batt_loss_choice_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_loss_choice", number);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_batt_losses_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_losses", arr, length);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_batt_losses_charging_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_losses_charging", arr, length);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_batt_losses_discharging_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_losses_discharging", arr, length);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_batt_losses_idle_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_losses_idle", arr, length);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_batt_mass_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_mass", number);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_batt_meter_position_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_meter_position", number);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_batt_power_charge_max_kwac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_power_charge_max_kwac", number);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_batt_power_charge_max_kwdc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_power_charge_max_kwdc", number);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_batt_power_discharge_max_kwac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_power_discharge_max_kwac", number);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_batt_power_discharge_max_kwdc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_power_discharge_max_kwdc", number);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_batt_replacement_capacity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_replacement_capacity", number);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_batt_replacement_option_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_replacement_option", number);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_batt_replacement_schedule_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_replacement_schedule", arr, length);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_batt_replacement_schedule_percent_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_replacement_schedule_percent", arr, length);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_batt_surface_area_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_surface_area", number);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_en_batt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_batt", number);
	});
}

SAM_EXPORT void SAM_Battery_BatterySystem_om_replacement_cost1_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_replacement_cost1", arr, length);
	});
}

SAM_EXPORT void SAM_Battery_SystemOutput_annual_energy_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "annual_energy", number);
	});
}

SAM_EXPORT void SAM_Battery_SystemOutput_capacity_factor_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "capacity_factor", number);
	});
}

SAM_EXPORT void SAM_Battery_SystemOutput_gen_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "gen", arr, length);
	});
}

SAM_EXPORT void SAM_Battery_Load_crit_load_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "crit_load", arr, length);
	});
}

SAM_EXPORT void SAM_Battery_Load_load_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "load", arr, length);
	});
}

SAM_EXPORT void SAM_Battery_Load_load_escalation_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "load_escalation", arr, length);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_LeadAcid_q10_computed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "LeadAcid_q10_computed", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_LeadAcid_q20_computed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "LeadAcid_q20_computed", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_LeadAcid_qn_computed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "LeadAcid_qn_computed", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_LeadAcid_tn_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "LeadAcid_tn", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_batt_C_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_C_rate", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_batt_Cp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_Cp", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_batt_Qexp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_Qexp", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_batt_Qfull_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_Qfull", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_batt_Qfull_flow_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_Qfull_flow", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_batt_Qnom_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_Qnom", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_batt_Vexp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_Vexp", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_batt_Vfull_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_Vfull", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_batt_Vnom_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_Vnom", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_batt_Vnom_default_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_Vnom_default", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_batt_calendar_a_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_calendar_a", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_batt_calendar_b_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_calendar_b", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_batt_calendar_c_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_calendar_c", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_batt_calendar_choice_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_calendar_choice", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_batt_calendar_lifetime_matrix_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "batt_calendar_lifetime_matrix", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_batt_calendar_q0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_calendar_q0", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_batt_chem_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_chem", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_batt_h_to_ambient_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_h_to_ambient", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_batt_initial_SOC_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_initial_SOC", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_batt_lifetime_matrix_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "batt_lifetime_matrix", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_batt_maximum_SOC_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_maximum_SOC", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_batt_minimum_SOC_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_minimum_SOC", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_batt_minimum_modetime_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_minimum_modetime", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_batt_resistance_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_resistance", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_batt_room_temperature_celsius_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_room_temperature_celsius", arr, length);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_batt_voltage_choice_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_voltage_choice", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_batt_voltage_matrix_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "batt_voltage_matrix", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Battery_BatteryCell_cap_vs_temp_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "cap_vs_temp", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Battery_Inverter_inv_cec_cg_eff_cec_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_cec_cg_eff_cec", number);
	});
}

SAM_EXPORT void SAM_Battery_Inverter_inv_cec_cg_paco_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_cec_cg_paco", number);
	});
}

SAM_EXPORT void SAM_Battery_Inverter_inv_ds_eff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_ds_eff", number);
	});
}

SAM_EXPORT void SAM_Battery_Inverter_inv_ds_paco_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_ds_paco", number);
	});
}

SAM_EXPORT void SAM_Battery_Inverter_inv_pd_eff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_pd_eff", number);
	});
}

SAM_EXPORT void SAM_Battery_Inverter_inv_pd_paco_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_pd_paco", number);
	});
}

SAM_EXPORT void SAM_Battery_Inverter_inv_snl_eff_cec_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_snl_eff_cec", number);
	});
}

SAM_EXPORT void SAM_Battery_Inverter_inv_snl_paco_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_snl_paco", number);
	});
}

SAM_EXPORT void SAM_Battery_Inverter_inverter_count_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inverter_count", number);
	});
}

SAM_EXPORT void SAM_Battery_Inverter_inverter_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inverter_model", number);
	});
}

SAM_EXPORT void SAM_Battery_Losses_dcoptimizer_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dcoptimizer_loss", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryDispatch_batt_auto_gridcharge_max_daily_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_auto_gridcharge_max_daily", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryDispatch_batt_custom_dispatch_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_custom_dispatch", arr, length);
	});
}

SAM_EXPORT void SAM_Battery_BatteryDispatch_batt_dispatch_auto_can_charge_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_dispatch_auto_can_charge", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryDispatch_batt_dispatch_auto_can_clipcharge_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_dispatch_auto_can_clipcharge", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryDispatch_batt_dispatch_auto_can_fuelcellcharge_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_dispatch_auto_can_fuelcellcharge", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryDispatch_batt_dispatch_auto_can_gridcharge_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_dispatch_auto_can_gridcharge", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryDispatch_batt_dispatch_choice_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_dispatch_choice", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryDispatch_batt_dispatch_update_frequency_hours_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_dispatch_update_frequency_hours", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryDispatch_batt_look_ahead_hours_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_look_ahead_hours", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryDispatch_batt_pv_ac_forecast_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_pv_ac_forecast", arr, length);
	});
}

SAM_EXPORT void SAM_Battery_BatteryDispatch_batt_pv_clipping_forecast_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_pv_clipping_forecast", arr, length);
	});
}

SAM_EXPORT void SAM_Battery_BatteryDispatch_batt_target_choice_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_target_choice", number);
	});
}

SAM_EXPORT void SAM_Battery_BatteryDispatch_batt_target_power_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_target_power", arr, length);
	});
}

SAM_EXPORT void SAM_Battery_BatteryDispatch_batt_target_power_monthly_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_target_power_monthly", arr, length);
	});
}

SAM_EXPORT void SAM_Battery_BatteryDispatch_dispatch_manual_charge_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_manual_charge", arr, length);
	});
}

SAM_EXPORT void SAM_Battery_BatteryDispatch_dispatch_manual_discharge_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_manual_discharge", arr, length);
	});
}

SAM_EXPORT void SAM_Battery_BatteryDispatch_dispatch_manual_fuelcellcharge_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_manual_fuelcellcharge", arr, length);
	});
}

SAM_EXPORT void SAM_Battery_BatteryDispatch_dispatch_manual_gridcharge_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_manual_gridcharge", arr, length);
	});
}

SAM_EXPORT void SAM_Battery_BatteryDispatch_dispatch_manual_percent_discharge_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_manual_percent_discharge", arr, length);
	});
}

SAM_EXPORT void SAM_Battery_BatteryDispatch_dispatch_manual_percent_gridcharge_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_manual_percent_gridcharge", arr, length);
	});
}

SAM_EXPORT void SAM_Battery_BatteryDispatch_dispatch_manual_sched_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "dispatch_manual_sched", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Battery_BatteryDispatch_dispatch_manual_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "dispatch_manual_sched_weekend", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Battery_ElectricityRates_en_electricity_rates_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_electricity_rates", number);
	});
}

SAM_EXPORT void SAM_Battery_ElectricityRates_ur_ec_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_ec_sched_weekday", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Battery_ElectricityRates_ur_ec_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_ec_sched_weekend", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Battery_ElectricityRates_ur_ec_tou_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ur_ec_tou_mat", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Battery_ElectricityRates_ur_en_ts_sell_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ur_en_ts_sell_rate", number);
	});
}

SAM_EXPORT void SAM_Battery_ElectricityRates_ur_ts_buy_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ur_ts_buy_rate", arr, length);
	});
}

SAM_EXPORT void SAM_Battery_FuelCell_fuelcell_power_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "fuelcell_power", arr, length);
	});
}

SAM_EXPORT void SAM_Battery_PriceSignal_dispatch_factors_ts_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_factors_ts", arr, length);
	});
}

SAM_EXPORT void SAM_Battery_PriceSignal_dispatch_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "dispatch_sched_weekday", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Battery_PriceSignal_dispatch_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "dispatch_sched_weekend", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Battery_PriceSignal_dispatch_tod_factors_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_tod_factors", arr, length);
	});
}

SAM_EXPORT void SAM_Battery_PriceSignal_forecast_price_signal_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "forecast_price_signal_model", number);
	});
}

SAM_EXPORT void SAM_Battery_PriceSignal_mp_ancserv1_revenue_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "mp_ancserv1_revenue", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Battery_PriceSignal_mp_ancserv2_revenue_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "mp_ancserv2_revenue", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Battery_PriceSignal_mp_ancserv3_revenue_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "mp_ancserv3_revenue", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Battery_PriceSignal_mp_ancserv4_revenue_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "mp_ancserv4_revenue", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Battery_PriceSignal_mp_enable_ancserv1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mp_enable_ancserv1", number);
	});
}

SAM_EXPORT void SAM_Battery_PriceSignal_mp_enable_ancserv2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mp_enable_ancserv2", number);
	});
}

SAM_EXPORT void SAM_Battery_PriceSignal_mp_enable_ancserv3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mp_enable_ancserv3", number);
	});
}

SAM_EXPORT void SAM_Battery_PriceSignal_mp_enable_ancserv4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mp_enable_ancserv4", number);
	});
}

SAM_EXPORT void SAM_Battery_PriceSignal_mp_enable_energy_market_revenue_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mp_enable_energy_market_revenue", number);
	});
}

SAM_EXPORT void SAM_Battery_PriceSignal_mp_energy_market_revenue_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "mp_energy_market_revenue", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Battery_PriceSignal_ppa_multiplier_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ppa_multiplier_model", number);
	});
}

SAM_EXPORT void SAM_Battery_PriceSignal_ppa_price_input_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ppa_price_input", arr, length);
	});
}

SAM_EXPORT double SAM_Battery_Simulation_percent_complete_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "percent_complete", &result))
		make_access_error("SAM_Battery", "percent_complete");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_Lifetime_analysis_period_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "analysis_period", &result))
		make_access_error("SAM_Battery", "analysis_period");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_Lifetime_system_use_lifetime_output_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_use_lifetime_output", &result))
		make_access_error("SAM_Battery", "system_use_lifetime_output");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatterySystem_batt_ac_dc_efficiency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_ac_dc_efficiency", &result))
		make_access_error("SAM_Battery", "batt_ac_dc_efficiency");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatterySystem_batt_ac_or_dc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_ac_or_dc", &result))
		make_access_error("SAM_Battery", "batt_ac_or_dc");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatterySystem_batt_computed_bank_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_computed_bank_capacity", &result))
		make_access_error("SAM_Battery", "batt_computed_bank_capacity");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatterySystem_batt_computed_series_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_computed_series", &result))
		make_access_error("SAM_Battery", "batt_computed_series");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatterySystem_batt_computed_strings_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_computed_strings", &result))
		make_access_error("SAM_Battery", "batt_computed_strings");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatterySystem_batt_current_charge_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_current_charge_max", &result))
		make_access_error("SAM_Battery", "batt_current_charge_max");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatterySystem_batt_current_choice_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_current_choice", &result))
		make_access_error("SAM_Battery", "batt_current_choice");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatterySystem_batt_current_discharge_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_current_discharge_max", &result))
		make_access_error("SAM_Battery", "batt_current_discharge_max");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatterySystem_batt_cycle_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_cycle_cost", &result))
		make_access_error("SAM_Battery", "batt_cycle_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatterySystem_batt_cycle_cost_choice_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_cycle_cost_choice", &result))
		make_access_error("SAM_Battery", "batt_cycle_cost_choice");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatterySystem_batt_dc_ac_efficiency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_dc_ac_efficiency", &result))
		make_access_error("SAM_Battery", "batt_dc_ac_efficiency");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatterySystem_batt_dc_dc_efficiency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_dc_dc_efficiency", &result))
		make_access_error("SAM_Battery", "batt_dc_dc_efficiency");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatterySystem_batt_inverter_efficiency_cutoff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_inverter_efficiency_cutoff", &result))
		make_access_error("SAM_Battery", "batt_inverter_efficiency_cutoff");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatterySystem_batt_loss_choice_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_loss_choice", &result))
		make_access_error("SAM_Battery", "batt_loss_choice");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_BatterySystem_batt_losses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_losses", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_losses");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_BatterySystem_batt_losses_charging_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_losses_charging", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_losses_charging");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_BatterySystem_batt_losses_discharging_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_losses_discharging", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_losses_discharging");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_BatterySystem_batt_losses_idle_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_losses_idle", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_losses_idle");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatterySystem_batt_mass_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_mass", &result))
		make_access_error("SAM_Battery", "batt_mass");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatterySystem_batt_meter_position_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_meter_position", &result))
		make_access_error("SAM_Battery", "batt_meter_position");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatterySystem_batt_power_charge_max_kwac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_power_charge_max_kwac", &result))
		make_access_error("SAM_Battery", "batt_power_charge_max_kwac");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatterySystem_batt_power_charge_max_kwdc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_power_charge_max_kwdc", &result))
		make_access_error("SAM_Battery", "batt_power_charge_max_kwdc");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatterySystem_batt_power_discharge_max_kwac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_power_discharge_max_kwac", &result))
		make_access_error("SAM_Battery", "batt_power_discharge_max_kwac");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatterySystem_batt_power_discharge_max_kwdc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_power_discharge_max_kwdc", &result))
		make_access_error("SAM_Battery", "batt_power_discharge_max_kwdc");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatterySystem_batt_replacement_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_replacement_capacity", &result))
		make_access_error("SAM_Battery", "batt_replacement_capacity");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatterySystem_batt_replacement_option_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_replacement_option", &result))
		make_access_error("SAM_Battery", "batt_replacement_option");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_BatterySystem_batt_replacement_schedule_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_replacement_schedule", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_replacement_schedule");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_BatterySystem_batt_replacement_schedule_percent_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_replacement_schedule_percent", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_replacement_schedule_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatterySystem_batt_surface_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_surface_area", &result))
		make_access_error("SAM_Battery", "batt_surface_area");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatterySystem_en_batt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_batt", &result))
		make_access_error("SAM_Battery", "en_batt");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_BatterySystem_om_replacement_cost1_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_replacement_cost1", length);
	if (!result)
		make_access_error("SAM_Battery", "om_replacement_cost1");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_SystemOutput_annual_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_Battery", "annual_energy");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_SystemOutput_capacity_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_Battery", "capacity_factor");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_SystemOutput_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_Battery", "gen");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Load_crit_load_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "crit_load", length);
	if (!result)
		make_access_error("SAM_Battery", "crit_load");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Load_load_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "load", length);
	if (!result)
		make_access_error("SAM_Battery", "load");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Load_load_escalation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "load_escalation", length);
	if (!result)
		make_access_error("SAM_Battery", "load_escalation");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryCell_LeadAcid_q10_computed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LeadAcid_q10_computed", &result))
		make_access_error("SAM_Battery", "LeadAcid_q10_computed");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryCell_LeadAcid_q20_computed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LeadAcid_q20_computed", &result))
		make_access_error("SAM_Battery", "LeadAcid_q20_computed");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryCell_LeadAcid_qn_computed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LeadAcid_qn_computed", &result))
		make_access_error("SAM_Battery", "LeadAcid_qn_computed");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryCell_LeadAcid_tn_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LeadAcid_tn", &result))
		make_access_error("SAM_Battery", "LeadAcid_tn");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryCell_batt_C_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_C_rate", &result))
		make_access_error("SAM_Battery", "batt_C_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryCell_batt_Cp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_Cp", &result))
		make_access_error("SAM_Battery", "batt_Cp");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryCell_batt_Qexp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_Qexp", &result))
		make_access_error("SAM_Battery", "batt_Qexp");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryCell_batt_Qfull_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_Qfull", &result))
		make_access_error("SAM_Battery", "batt_Qfull");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryCell_batt_Qfull_flow_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_Qfull_flow", &result))
		make_access_error("SAM_Battery", "batt_Qfull_flow");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryCell_batt_Qnom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_Qnom", &result))
		make_access_error("SAM_Battery", "batt_Qnom");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryCell_batt_Vexp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_Vexp", &result))
		make_access_error("SAM_Battery", "batt_Vexp");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryCell_batt_Vfull_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_Vfull", &result))
		make_access_error("SAM_Battery", "batt_Vfull");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryCell_batt_Vnom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_Vnom", &result))
		make_access_error("SAM_Battery", "batt_Vnom");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryCell_batt_Vnom_default_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_Vnom_default", &result))
		make_access_error("SAM_Battery", "batt_Vnom_default");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryCell_batt_calendar_a_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_calendar_a", &result))
		make_access_error("SAM_Battery", "batt_calendar_a");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryCell_batt_calendar_b_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_calendar_b", &result))
		make_access_error("SAM_Battery", "batt_calendar_b");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryCell_batt_calendar_c_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_calendar_c", &result))
		make_access_error("SAM_Battery", "batt_calendar_c");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryCell_batt_calendar_choice_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_calendar_choice", &result))
		make_access_error("SAM_Battery", "batt_calendar_choice");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_BatteryCell_batt_calendar_lifetime_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "batt_calendar_lifetime_matrix", nrows, ncols);
	if (!result)
		make_access_error("SAM_Battery", "batt_calendar_lifetime_matrix");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryCell_batt_calendar_q0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_calendar_q0", &result))
		make_access_error("SAM_Battery", "batt_calendar_q0");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryCell_batt_chem_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_chem", &result))
		make_access_error("SAM_Battery", "batt_chem");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryCell_batt_h_to_ambient_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_h_to_ambient", &result))
		make_access_error("SAM_Battery", "batt_h_to_ambient");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryCell_batt_initial_SOC_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_initial_SOC", &result))
		make_access_error("SAM_Battery", "batt_initial_SOC");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_BatteryCell_batt_lifetime_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "batt_lifetime_matrix", nrows, ncols);
	if (!result)
		make_access_error("SAM_Battery", "batt_lifetime_matrix");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryCell_batt_maximum_SOC_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_maximum_SOC", &result))
		make_access_error("SAM_Battery", "batt_maximum_SOC");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryCell_batt_minimum_SOC_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_minimum_SOC", &result))
		make_access_error("SAM_Battery", "batt_minimum_SOC");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryCell_batt_minimum_modetime_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_minimum_modetime", &result))
		make_access_error("SAM_Battery", "batt_minimum_modetime");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryCell_batt_resistance_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_resistance", &result))
		make_access_error("SAM_Battery", "batt_resistance");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_BatteryCell_batt_room_temperature_celsius_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_room_temperature_celsius", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_room_temperature_celsius");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryCell_batt_voltage_choice_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_voltage_choice", &result))
		make_access_error("SAM_Battery", "batt_voltage_choice");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_BatteryCell_batt_voltage_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "batt_voltage_matrix", nrows, ncols);
	if (!result)
		make_access_error("SAM_Battery", "batt_voltage_matrix");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_BatteryCell_cap_vs_temp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "cap_vs_temp", nrows, ncols);
	if (!result)
		make_access_error("SAM_Battery", "cap_vs_temp");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_Inverter_inv_cec_cg_eff_cec_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_cec_cg_eff_cec", &result))
		make_access_error("SAM_Battery", "inv_cec_cg_eff_cec");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_Inverter_inv_cec_cg_paco_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_cec_cg_paco", &result))
		make_access_error("SAM_Battery", "inv_cec_cg_paco");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_Inverter_inv_ds_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_ds_eff", &result))
		make_access_error("SAM_Battery", "inv_ds_eff");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_Inverter_inv_ds_paco_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_ds_paco", &result))
		make_access_error("SAM_Battery", "inv_ds_paco");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_Inverter_inv_pd_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_pd_eff", &result))
		make_access_error("SAM_Battery", "inv_pd_eff");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_Inverter_inv_pd_paco_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_pd_paco", &result))
		make_access_error("SAM_Battery", "inv_pd_paco");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_Inverter_inv_snl_eff_cec_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_snl_eff_cec", &result))
		make_access_error("SAM_Battery", "inv_snl_eff_cec");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_Inverter_inv_snl_paco_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_snl_paco", &result))
		make_access_error("SAM_Battery", "inv_snl_paco");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_Inverter_inverter_count_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inverter_count", &result))
		make_access_error("SAM_Battery", "inverter_count");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_Inverter_inverter_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inverter_model", &result))
		make_access_error("SAM_Battery", "inverter_model");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_Losses_dcoptimizer_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dcoptimizer_loss", &result))
		make_access_error("SAM_Battery", "dcoptimizer_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryDispatch_batt_auto_gridcharge_max_daily_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_auto_gridcharge_max_daily", &result))
		make_access_error("SAM_Battery", "batt_auto_gridcharge_max_daily");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_BatteryDispatch_batt_custom_dispatch_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_custom_dispatch", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_custom_dispatch");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryDispatch_batt_dispatch_auto_can_charge_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_dispatch_auto_can_charge", &result))
		make_access_error("SAM_Battery", "batt_dispatch_auto_can_charge");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryDispatch_batt_dispatch_auto_can_clipcharge_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_dispatch_auto_can_clipcharge", &result))
		make_access_error("SAM_Battery", "batt_dispatch_auto_can_clipcharge");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryDispatch_batt_dispatch_auto_can_fuelcellcharge_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_dispatch_auto_can_fuelcellcharge", &result))
		make_access_error("SAM_Battery", "batt_dispatch_auto_can_fuelcellcharge");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryDispatch_batt_dispatch_auto_can_gridcharge_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_dispatch_auto_can_gridcharge", &result))
		make_access_error("SAM_Battery", "batt_dispatch_auto_can_gridcharge");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryDispatch_batt_dispatch_choice_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_dispatch_choice", &result))
		make_access_error("SAM_Battery", "batt_dispatch_choice");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryDispatch_batt_dispatch_update_frequency_hours_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_dispatch_update_frequency_hours", &result))
		make_access_error("SAM_Battery", "batt_dispatch_update_frequency_hours");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryDispatch_batt_look_ahead_hours_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_look_ahead_hours", &result))
		make_access_error("SAM_Battery", "batt_look_ahead_hours");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_BatteryDispatch_batt_pv_ac_forecast_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_pv_ac_forecast", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_pv_ac_forecast");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_BatteryDispatch_batt_pv_clipping_forecast_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_pv_clipping_forecast", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_pv_clipping_forecast");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_BatteryDispatch_batt_target_choice_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_target_choice", &result))
		make_access_error("SAM_Battery", "batt_target_choice");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_BatteryDispatch_batt_target_power_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_target_power", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_target_power");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_BatteryDispatch_batt_target_power_monthly_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_target_power_monthly", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_target_power_monthly");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_BatteryDispatch_dispatch_manual_charge_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_manual_charge", length);
	if (!result)
		make_access_error("SAM_Battery", "dispatch_manual_charge");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_BatteryDispatch_dispatch_manual_discharge_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_manual_discharge", length);
	if (!result)
		make_access_error("SAM_Battery", "dispatch_manual_discharge");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_BatteryDispatch_dispatch_manual_fuelcellcharge_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_manual_fuelcellcharge", length);
	if (!result)
		make_access_error("SAM_Battery", "dispatch_manual_fuelcellcharge");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_BatteryDispatch_dispatch_manual_gridcharge_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_manual_gridcharge", length);
	if (!result)
		make_access_error("SAM_Battery", "dispatch_manual_gridcharge");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_BatteryDispatch_dispatch_manual_percent_discharge_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_manual_percent_discharge", length);
	if (!result)
		make_access_error("SAM_Battery", "dispatch_manual_percent_discharge");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_BatteryDispatch_dispatch_manual_percent_gridcharge_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_manual_percent_gridcharge", length);
	if (!result)
		make_access_error("SAM_Battery", "dispatch_manual_percent_gridcharge");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_BatteryDispatch_dispatch_manual_sched_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "dispatch_manual_sched", nrows, ncols);
	if (!result)
		make_access_error("SAM_Battery", "dispatch_manual_sched");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_BatteryDispatch_dispatch_manual_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "dispatch_manual_sched_weekend", nrows, ncols);
	if (!result)
		make_access_error("SAM_Battery", "dispatch_manual_sched_weekend");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_ElectricityRates_en_electricity_rates_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_electricity_rates", &result))
		make_access_error("SAM_Battery", "en_electricity_rates");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_ElectricityRates_ur_ec_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_ec_sched_weekday", nrows, ncols);
	if (!result)
		make_access_error("SAM_Battery", "ur_ec_sched_weekday");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_ElectricityRates_ur_ec_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_ec_sched_weekend", nrows, ncols);
	if (!result)
		make_access_error("SAM_Battery", "ur_ec_sched_weekend");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_ElectricityRates_ur_ec_tou_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ur_ec_tou_mat", nrows, ncols);
	if (!result)
		make_access_error("SAM_Battery", "ur_ec_tou_mat");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_ElectricityRates_ur_en_ts_sell_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ur_en_ts_sell_rate", &result))
		make_access_error("SAM_Battery", "ur_en_ts_sell_rate");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_ElectricityRates_ur_ts_buy_rate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ur_ts_buy_rate", length);
	if (!result)
		make_access_error("SAM_Battery", "ur_ts_buy_rate");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_FuelCell_fuelcell_power_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "fuelcell_power", length);
	if (!result)
		make_access_error("SAM_Battery", "fuelcell_power");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_PriceSignal_dispatch_factors_ts_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_factors_ts", length);
	if (!result)
		make_access_error("SAM_Battery", "dispatch_factors_ts");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_PriceSignal_dispatch_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "dispatch_sched_weekday", nrows, ncols);
	if (!result)
		make_access_error("SAM_Battery", "dispatch_sched_weekday");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_PriceSignal_dispatch_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "dispatch_sched_weekend", nrows, ncols);
	if (!result)
		make_access_error("SAM_Battery", "dispatch_sched_weekend");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_PriceSignal_dispatch_tod_factors_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_tod_factors", length);
	if (!result)
		make_access_error("SAM_Battery", "dispatch_tod_factors");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_PriceSignal_forecast_price_signal_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "forecast_price_signal_model", &result))
		make_access_error("SAM_Battery", "forecast_price_signal_model");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_PriceSignal_mp_ancserv1_revenue_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "mp_ancserv1_revenue", nrows, ncols);
	if (!result)
		make_access_error("SAM_Battery", "mp_ancserv1_revenue");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_PriceSignal_mp_ancserv2_revenue_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "mp_ancserv2_revenue", nrows, ncols);
	if (!result)
		make_access_error("SAM_Battery", "mp_ancserv2_revenue");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_PriceSignal_mp_ancserv3_revenue_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "mp_ancserv3_revenue", nrows, ncols);
	if (!result)
		make_access_error("SAM_Battery", "mp_ancserv3_revenue");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_PriceSignal_mp_ancserv4_revenue_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "mp_ancserv4_revenue", nrows, ncols);
	if (!result)
		make_access_error("SAM_Battery", "mp_ancserv4_revenue");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_PriceSignal_mp_enable_ancserv1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mp_enable_ancserv1", &result))
		make_access_error("SAM_Battery", "mp_enable_ancserv1");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_PriceSignal_mp_enable_ancserv2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mp_enable_ancserv2", &result))
		make_access_error("SAM_Battery", "mp_enable_ancserv2");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_PriceSignal_mp_enable_ancserv3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mp_enable_ancserv3", &result))
		make_access_error("SAM_Battery", "mp_enable_ancserv3");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_PriceSignal_mp_enable_ancserv4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mp_enable_ancserv4", &result))
		make_access_error("SAM_Battery", "mp_enable_ancserv4");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_PriceSignal_mp_enable_energy_market_revenue_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mp_enable_energy_market_revenue", &result))
		make_access_error("SAM_Battery", "mp_enable_energy_market_revenue");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_PriceSignal_mp_energy_market_revenue_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "mp_energy_market_revenue", nrows, ncols);
	if (!result)
		make_access_error("SAM_Battery", "mp_energy_market_revenue");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_PriceSignal_ppa_multiplier_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ppa_multiplier_model", &result))
		make_access_error("SAM_Battery", "ppa_multiplier_model");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_PriceSignal_ppa_price_input_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ppa_price_input", length);
	if (!result)
		make_access_error("SAM_Battery", "ppa_price_input");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_annual_export_to_grid_energy_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "annual_export_to_grid_energy", length);
	if (!result)
		make_access_error("SAM_Battery", "annual_export_to_grid_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_annual_import_to_grid_energy_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "annual_import_to_grid_energy", length);
	if (!result)
		make_access_error("SAM_Battery", "annual_import_to_grid_energy");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_Outputs_average_battery_conversion_efficiency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "average_battery_conversion_efficiency", &result))
		make_access_error("SAM_Battery", "average_battery_conversion_efficiency");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_Outputs_average_battery_roundtrip_efficiency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "average_battery_roundtrip_efficiency", &result))
		make_access_error("SAM_Battery", "average_battery_roundtrip_efficiency");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_Outputs_avg_critical_load_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "avg_critical_load", &result))
		make_access_error("SAM_Battery", "avg_critical_load");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_DOD_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_DOD", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_DOD");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_DOD_cycle_average_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_DOD_cycle_average", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_DOD_cycle_average");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_I_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_I", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_I");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_SOC_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_SOC", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_SOC");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_annual_charge_energy_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_annual_charge_energy", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_annual_charge_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_annual_charge_from_grid_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_annual_charge_from_grid", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_annual_charge_from_grid");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_annual_charge_from_system_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_annual_charge_from_system", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_annual_charge_from_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_annual_discharge_energy_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_annual_discharge_energy", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_annual_discharge_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_annual_energy_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_annual_energy_loss", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_annual_energy_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_annual_energy_system_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_annual_energy_system_loss", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_annual_energy_system_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_Outputs_batt_bank_installed_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_bank_installed_capacity", &result))
		make_access_error("SAM_Battery", "batt_bank_installed_capacity");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_bank_replacement_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_bank_replacement", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_bank_replacement");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_capacity_percent_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_capacity_percent", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_capacity_percent");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_capacity_percent_calendar_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_capacity_percent_calendar", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_capacity_percent_calendar");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_capacity_percent_cycle_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_capacity_percent_cycle", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_capacity_percent_cycle");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_capacity_thermal_percent_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_capacity_thermal_percent", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_capacity_thermal_percent");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_conversion_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_conversion_loss", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_conversion_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_cost_to_cycle_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_cost_to_cycle", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_cost_to_cycle");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_cycles_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_cycles", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_cycles");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_dispatch_sched_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "batt_dispatch_sched", nrows, ncols);
	if (!result)
		make_access_error("SAM_Battery", "batt_dispatch_sched");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_power_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_power", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_power");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_power_target_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_power_target", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_power_target");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_q0_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_q0", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_q0");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_q1_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_q1", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_q1");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_q2_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_q2", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_q2");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_qmax_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_qmax", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_qmax");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_qmaxI_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_qmaxI", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_qmaxI");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_qmax_thermal_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_qmax_thermal", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_qmax_thermal");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_revenue_charge_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_revenue_charge", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_revenue_charge");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_revenue_clipcharge_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_revenue_clipcharge", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_revenue_clipcharge");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_revenue_discharge_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_revenue_discharge", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_revenue_discharge");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_revenue_gridcharge_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_revenue_gridcharge", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_revenue_gridcharge");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_Outputs_batt_system_charge_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_system_charge_percent", &result))
		make_access_error("SAM_Battery", "batt_system_charge_percent");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_system_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_system_loss", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_system_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_temperature_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_temperature", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_temperature");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_to_grid_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_to_grid", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_to_grid");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_to_load_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_to_load", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_to_load");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_voltage_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_voltage", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_voltage");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_batt_voltage_cell_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_voltage_cell", length);
	if (!result)
		make_access_error("SAM_Battery", "batt_voltage_cell");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_cdf_of_surviving_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cdf_of_surviving", length);
	if (!result)
		make_access_error("SAM_Battery", "cdf_of_surviving");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_fuelcell_to_batt_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "fuelcell_to_batt", length);
	if (!result)
		make_access_error("SAM_Battery", "fuelcell_to_batt");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_grid_power_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "grid_power", length);
	if (!result)
		make_access_error("SAM_Battery", "grid_power");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_grid_power_target_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "grid_power_target", length);
	if (!result)
		make_access_error("SAM_Battery", "grid_power_target");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_grid_to_batt_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "grid_to_batt", length);
	if (!result)
		make_access_error("SAM_Battery", "grid_to_batt");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_grid_to_load_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "grid_to_load", length);
	if (!result)
		make_access_error("SAM_Battery", "grid_to_load");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_market_sell_rate_series_yr1_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "market_sell_rate_series_yr1", length);
	if (!result)
		make_access_error("SAM_Battery", "market_sell_rate_series_yr1");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_monthly_batt_to_grid_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_batt_to_grid", length);
	if (!result)
		make_access_error("SAM_Battery", "monthly_batt_to_grid");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_monthly_batt_to_load_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_batt_to_load", length);
	if (!result)
		make_access_error("SAM_Battery", "monthly_batt_to_load");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_monthly_grid_to_batt_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_grid_to_batt", length);
	if (!result)
		make_access_error("SAM_Battery", "monthly_grid_to_batt");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_monthly_grid_to_load_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_grid_to_load", length);
	if (!result)
		make_access_error("SAM_Battery", "monthly_grid_to_load");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_monthly_system_to_batt_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_system_to_batt", length);
	if (!result)
		make_access_error("SAM_Battery", "monthly_system_to_batt");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_monthly_system_to_grid_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_system_to_grid", length);
	if (!result)
		make_access_error("SAM_Battery", "monthly_system_to_grid");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_monthly_system_to_load_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_system_to_load", length);
	if (!result)
		make_access_error("SAM_Battery", "monthly_system_to_load");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_outage_durations_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "outage_durations", length);
	if (!result)
		make_access_error("SAM_Battery", "outage_durations");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_pdf_of_surviving_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pdf_of_surviving", length);
	if (!result)
		make_access_error("SAM_Battery", "pdf_of_surviving");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_resilience_hrs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "resilience_hrs", length);
	if (!result)
		make_access_error("SAM_Battery", "resilience_hrs");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_Outputs_resilience_hrs_avg_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "resilience_hrs_avg", &result))
		make_access_error("SAM_Battery", "resilience_hrs_avg");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_Outputs_resilience_hrs_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "resilience_hrs_max", &result))
		make_access_error("SAM_Battery", "resilience_hrs_max");
	});
	return result;
}



SAM_EXPORT double SAM_Battery_Outputs_resilience_hrs_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "resilience_hrs_min", &result))
		make_access_error("SAM_Battery", "resilience_hrs_min");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_survival_function_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "survival_function", length);
	if (!result)
		make_access_error("SAM_Battery", "survival_function");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_system_to_batt_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "system_to_batt", length);
	if (!result)
		make_access_error("SAM_Battery", "system_to_batt");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_system_to_grid_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "system_to_grid", length);
	if (!result)
		make_access_error("SAM_Battery", "system_to_grid");
	});
	return result;
}



SAM_EXPORT double* SAM_Battery_Outputs_system_to_load_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "system_to_load", length);
	if (!result)
		make_access_error("SAM_Battery", "system_to_load");
	});
	return result;
}




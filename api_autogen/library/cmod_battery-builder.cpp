#include <string>
#include <vector>

#include "vartab.h"

#include "cmod_battery-builder.h"

var_table Battery_BatteryCurrentAndCapacity_batt_computed_voltage_MIMO_eval(var_table* vt, invoke_t* cxt)
{
	// inputs
	float batt_size_choice = vt->lookup("batt_size_choice")->num;
	float batt_chem = vt->lookup("batt_chem")->num;
	float batt_bank_power = vt->lookup("batt_bank_power")->num;
	float batt_bank_size = vt->lookup("batt_bank_size")->num;
	float batt_bank_size_dc_ac = vt->lookup("batt_bank_size_dc_ac")->num;
	float batt_dc_ac_efficiency = vt->lookup("batt_dc_ac_efficiency")->num;
	float batt_bank_power_dc_ac = vt->lookup("batt_bank_power_dc_ac")->num;
	float batt_Qfull = vt->lookup("batt_Qfull")->num;
	float batt_bank_voltage = vt->lookup("batt_bank_voltage")->num;
	float batt_Vnom_default = vt->lookup("batt_Vnom_default")->num;
	float batt_bank_ncells_serial = vt->lookup("batt_bank_ncells_serial")->num;
	float batt_bank_nstrings = vt->lookup("batt_bank_nstrings")->num;
	float batt_C_rate_max_discharge_input = vt->lookup("batt_C_rate_max_discharge_input")->num;
	float batt_C_rate_max_charge_input = vt->lookup("batt_C_rate_max_charge_input")->num;
	float batt_current_choice = vt->lookup("batt_current_choice")->num;
	float batt_cell_power_discharge_max = vt->lookup("batt_cell_power_discharge_max")->num;
	float batt_cell_current_discharge_max = vt->lookup("batt_cell_current_discharge_max")->num;
	float batt_bank_nseries_stacks = vt->lookup("batt_bank_nseries_stacks")->num;
	float batt_bank_size_specify = vt->lookup("batt_bank_size_specify")->num;
	float batt_cell_power_charge_max = vt->lookup("batt_cell_power_charge_max")->num;
	float batt_cell_current_charge_max = vt->lookup("batt_cell_current_charge_max")->num;

	// outputs
	float batt_computed_voltage;
	float batt_computed_series;
	float batt_computed_strings;
	float batt_num_cells;
	float batt_computed_bank_capacity;
	float batt_power_discharge_max;
	float batt_power_charge_max;
	float batt_time_capacity;
	float batt_C_rate_max_charge;
	float batt_C_rate_max_discharge;
	float batt_current_charge_max;
	float batt_current_discharge_max;
	float batt_computed_stacks_series;

	float choice = batt_size_choice;
	float isflow = batt_chem == 2.000000 || batt_chem == 3.000000;
	float batt_bank_power = batt_bank_power;
	float batt_bank_capacity = batt_bank_size;
	if ( batt_bank_size_dc_ac == 1.000000 ) {
		batt_bank_capacity /= batt_dc_ac_efficiency * 0.010000
	}
	if ( batt_bank_power_dc_ac == 1.000000 ) {
		batt_bank_power /= batt_dc_ac_efficiency * 0.010000
	}
	if ( !isflow ) {
		float batt_C_rate_max_discharge = 0.000000;
		float batt_C_rate_max_charge = 0.000000;
		float string_current = 0.000000;
		float bank_desired_voltage = 0.000000;
		float num_strings = 0.000000;
		float num_series = 0.000000;
		float num_parallel = 0.000000;
		if ( choice == 0.000000 ) {
			float batt_C_rate_max_discharge = batt_bank_power / batt_bank_capacity;
			float batt_C_rate_max_charge = batt_C_rate_max_discharge;
			float string_current = batt_Qfull * batt_C_rate_max_discharge;
			float bank_desired_voltage = batt_bank_voltage;
			float num_series = ceil( bank_desired_voltage / batt_Vnom_default );
			float num_strings = round( batt_bank_capacity * 1000.000000 / batt_Qfull * batt_Vnom_default * num_series );
		
		}
		else {
			float num_series = batt_bank_ncells_serial;
			float num_strings = batt_bank_nstrings;
			float batt_C_rate_max_discharge = batt_C_rate_max_discharge_input;
			float batt_C_rate_max_charge = batt_C_rate_max_charge_input;
		}
		float batt_computed_voltage = batt_Vnom_default * num_series;
		float bank_capacity = batt_Qfull * batt_computed_voltage * num_strings * 0.001000;
		float bank_power = bank_capacity * batt_C_rate_max_discharge;
		float bank_power_charge = bank_capacity * batt_C_rate_max_charge;
		batt_computed_voltage = batt_computed_voltage;
		batt_computed_series = num_series;
		batt_computed_strings = num_strings;
		batt_num_cells = num_series * num_strings;
		batt_computed_bank_capacity = bank_capacity;
		batt_power_discharge_max = bank_power;
		batt_power_charge_max = bank_power_charge;
		batt_time_capacity = bank_capacity / bank_power;
		batt_C_rate_max_charge = batt_C_rate_max_charge;
		batt_C_rate_max_discharge = batt_C_rate_max_discharge;
		batt_current_charge_max = batt_Qfull * num_strings * batt_C_rate_max_charge;
		batt_current_discharge_max = batt_Qfull * num_strings * batt_C_rate_max_discharge;
	
	}
	else {
		float power_limited = batt_current_choice == 0.000000;
		float current_limited = !power_limited;
		float num_series = 0.000000;
		float num_stacks_parallel = 0.000000;
		float num_stacks_series = 1.000000;
		float batt_computed_voltage = 0.000000;
		float bank_capacity = 0.000000;
		float bank_current_discharge = 0.000000;
		float bank_current_charge = 0.000000;
		float bank_power_discharge = 0.000000;
		float bank_power_charge = 0.000000;
		if ( choice == 0.000000 ) {
			float bank_capacity = batt_bank_capacity;
			float num_series = ceil( batt_bank_voltage / num_stacks_series * batt_Vnom_default );
			float batt_computed_voltage = batt_Vnom_default * num_series * num_stacks_series;
			if ( power_limited ) {
				float num_stacks_parallel = ceil( batt_bank_power / batt_cell_power_discharge_max * num_series * num_stacks_series * 0.001000 )
			}
			else if ( current_limited ) {
				float num_stacks_parallel = ceil( batt_bank_power / batt_computed_voltage * batt_cell_current_discharge_max * 0.001000 )
			}
		
		}
		else {
			float num_series = batt_bank_ncells_serial;
			float num_stacks_parallel = batt_bank_nstrings;
			float num_stacks_series = batt_bank_nseries_stacks;
			float bank_capacity = batt_bank_size_specify;
			float batt_computed_voltage = batt_Vnom_default * num_series * num_stacks_series;
		}
		if ( power_limited ) {
			float bank_power_discharge = batt_cell_power_discharge_max * 0.001000 * num_series * num_stacks_series * num_stacks_parallel;
			float bank_power_charge = batt_cell_power_charge_max * 0.001000 * num_series * num_stacks_series * num_stacks_parallel;
			float bank_current_discharge = bank_power_discharge * 1000.000000 / batt_computed_voltage;
			float bank_current_charge = bank_power_charge * 1000.000000 / batt_computed_voltage;
		
		}
		else if ( current_limited ) {
			float bank_current_discharge = num_stacks_parallel * batt_cell_current_discharge_max;
			float bank_current_charge = num_stacks_parallel * batt_cell_current_charge_max;
			float bank_power_discharge = batt_computed_voltage * bank_current_discharge * 0.001000;
			float bank_power_charge = batt_computed_voltage * bank_current_charge * 0.001000;
		
		}
		float batt_C_rate_max_discharge = bank_power_charge / bank_capacity;
		float batt_C_rate_max_charge = bank_power_discharge / bank_capacity;
		batt_current_charge_max = bank_current_charge;
		batt_current_discharge_max = bank_current_discharge;
		batt_computed_voltage = batt_computed_voltage;
		batt_computed_series = num_series;
		batt_computed_strings = num_stacks_parallel;
		batt_computed_stacks_series = num_stacks_series;
		batt_num_cells = num_series * num_stacks_series * num_stacks_parallel;
		batt_computed_bank_capacity = bank_capacity;
		batt_power_discharge_max = bank_power_discharge;
		batt_power_charge_max = bank_power_charge;
		batt_time_capacity = bank_capacity / bank_power_discharge;
		batt_C_rate_max_charge = batt_C_rate_max_charge;
		batt_C_rate_max_discharge = batt_C_rate_max_discharge;
	}


	if (cxt){
		cxt->result().empty_hash();
		cxt->result().hash_item("batt_computed_voltage", batt_computed_voltage);
		cxt->result().hash_item("batt_computed_series", batt_computed_series);
		cxt->result().hash_item("batt_computed_strings", batt_computed_strings);
		cxt->result().hash_item("batt_num_cells", batt_num_cells);
		cxt->result().hash_item("batt_computed_bank_capacity", batt_computed_bank_capacity);
		cxt->result().hash_item("batt_power_discharge_max", batt_power_discharge_max);
		cxt->result().hash_item("batt_power_charge_max", batt_power_charge_max);
		cxt->result().hash_item("batt_time_capacity", batt_time_capacity);
		cxt->result().hash_item("batt_C_rate_max_charge", batt_C_rate_max_charge);
		cxt->result().hash_item("batt_C_rate_max_discharge", batt_C_rate_max_discharge);
		cxt->result().hash_item("batt_current_charge_max", batt_current_charge_max);
		cxt->result().hash_item("batt_current_discharge_max", batt_current_discharge_max);
		cxt->result().hash_item("batt_computed_stacks_series", batt_computed_stacks_series);
	}

	var_table vt;
	vt.assign( "batt_computed_voltage", batt_computed_voltage );
	vt.assign( "batt_computed_series", batt_computed_series );
	vt.assign( "batt_computed_strings", batt_computed_strings );
	vt.assign( "batt_num_cells", batt_num_cells );
	vt.assign( "batt_computed_bank_capacity", batt_computed_bank_capacity );
	vt.assign( "batt_power_discharge_max", batt_power_discharge_max );
	vt.assign( "batt_power_charge_max", batt_power_charge_max );
	vt.assign( "batt_time_capacity", batt_time_capacity );
	vt.assign( "batt_C_rate_max_charge", batt_C_rate_max_charge );
	vt.assign( "batt_C_rate_max_discharge", batt_C_rate_max_discharge );
	vt.assign( "batt_current_charge_max", batt_current_charge_max );
	vt.assign( "batt_current_discharge_max", batt_current_discharge_max );
	vt.assign( "batt_computed_stacks_series", batt_computed_stacks_series );

}



float Battery_BatteryThermal_batt_width_eval(var_table* vt, invoke_t* cxt)
{
	// inputs
	float batt_volume = vt->lookup("batt_volume")->num;

	// outputs
	float batt_width;

	batt_width = pow( batt_volume, 0.333333 );

	if (cxt){
		cxt->result().assign("batt_width", batt_width);
	}

	return batt_width;

}



var_table Battery_BatteryCurrentAndCapacity_LeadAcid_q10_computed_MIMO_eval(var_table* vt, invoke_t* cxt)
{
	// inputs
	float batt_computed_strings = vt->lookup("batt_computed_strings")->num;
	float LeadAcid_q10 = vt->lookup("LeadAcid_q10")->num;
	float batt_Qfull = vt->lookup("batt_Qfull")->num;
	float LeadAcid_q20 = vt->lookup("LeadAcid_q20")->num;
	float LeadAcid_qn = vt->lookup("LeadAcid_qn")->num;

	// outputs
	float LeadAcid_q10_computed;
	float LeadAcid_q20_computed;
	float LeadAcid_qn_computed;

	LeadAcid_q10_computed = batt_computed_strings * LeadAcid_q10 * batt_Qfull / 100.000000;
	LeadAcid_q20_computed = batt_computed_strings * LeadAcid_q20 * batt_Qfull / 100.000000;
	LeadAcid_qn_computed = batt_computed_strings * LeadAcid_qn * batt_Qfull / 100.000000;


	if (cxt){
		cxt->result().empty_hash();
		cxt->result().hash_item("LeadAcid_q10_computed", LeadAcid_q10_computed);
		cxt->result().hash_item("LeadAcid_q20_computed", LeadAcid_q20_computed);
		cxt->result().hash_item("LeadAcid_qn_computed", LeadAcid_qn_computed);
	}

	var_table vt;
	vt.assign( "LeadAcid_q10_computed", LeadAcid_q10_computed );
	vt.assign( "LeadAcid_q20_computed", LeadAcid_q20_computed );
	vt.assign( "LeadAcid_qn_computed", LeadAcid_qn_computed );

}



var_table Battery_BatteryCurrentAndCapacity_batt_Qexp_MIMO_eval(var_table* vt, invoke_t* cxt)
{
	// inputs
	float batt_Qexp_percent = vt->lookup("batt_Qexp_percent")->num;
	float batt_Qfull = vt->lookup("batt_Qfull")->num;
	float batt_Qnom_percent = vt->lookup("batt_Qnom_percent")->num;
	float batt_computed_bank_capacity = vt->lookup("batt_computed_bank_capacity")->num;
	float batt_computed_voltage = vt->lookup("batt_computed_voltage")->num;

	// outputs
	float batt_Qexp;
	float batt_Qnom;
	float batt_Qfull_flow;

	batt_Qexp = batt_Qexp_percent * batt_Qfull * 0.010000;
	batt_Qnom = batt_Qnom_percent * batt_Qfull * 0.010000;
	batt_Qfull_flow = batt_computed_bank_capacity * 1000.000000 / batt_computed_voltage;


	if (cxt){
		cxt->result().empty_hash();
		cxt->result().hash_item("batt_Qexp", batt_Qexp);
		cxt->result().hash_item("batt_Qnom", batt_Qnom);
		cxt->result().hash_item("batt_Qfull_flow", batt_Qfull_flow);
	}

	var_table vt;
	vt.assign( "batt_Qexp", batt_Qexp );
	vt.assign( "batt_Qnom", batt_Qnom );
	vt.assign( "batt_Qfull_flow", batt_Qfull_flow );

}



util::matrix_t<ssc_number_t> Battery_BatteryDispatchManual_dispatch_manual_percent_gridcharge_eval(var_table* vt, invoke_t* cxt)
{
	// inputs
	util::matrix_t<ssc_number_t> dispatch_manual_gridcharge = vt->lookup("dispatch_manual_gridcharge")->num;
	float batt_gridcharge_percent_1 = vt->lookup("batt_gridcharge_percent_1")->num;
	float batt_gridcharge_percent_2 = vt->lookup("batt_gridcharge_percent_2")->num;
	float batt_gridcharge_percent_3 = vt->lookup("batt_gridcharge_percent_3")->num;
	float batt_gridcharge_percent_4 = vt->lookup("batt_gridcharge_percent_4")->num;
	float batt_gridcharge_percent_5 = vt->lookup("batt_gridcharge_percent_5")->num;
	float batt_gridcharge_percent_6 = vt->lookup("batt_gridcharge_percent_6")->num;

	// outputs
	util::matrix_t<ssc_number_t> dispatch_manual_percent_gridcharge;

	float count = 0.000000;
	util::matrix_t<float> gridcharge_array = dispatch_manual_gridcharge;
	for ( float i = 0.000000; i != gridcharge_array.size(); i += 1 ){
		std::vector<float> ret_array;
		ret_array.insert(ret_array.begin()+count, 0.000000);
		if ( gridcharge_array[i] == 1.000000 ) {
			if ( i == 0.000000 ) {
				ret_array.insert(ret_array.begin()+count, batt_gridcharge_percent_1);
			}
			else if ( i == 1.000000 ) {
				ret_array.insert(ret_array.begin()+count, batt_gridcharge_percent_2);
			}
			else if ( i == 2.000000 ) {
				ret_array.insert(ret_array.begin()+count, batt_gridcharge_percent_3);
			}
			else if ( i == 3.000000 ) {
				ret_array.insert(ret_array.begin()+count, batt_gridcharge_percent_4);
			}
			else if ( i == 4.000000 ) {
				ret_array.insert(ret_array.begin()+count, batt_gridcharge_percent_5);
			}
			else if ( i == 5.000000 ) {
				ret_array.insert(ret_array.begin()+count, batt_gridcharge_percent_6);
			}
			count += 1;
		
		}
	
	}
	dispatch_manual_percent_gridcharge = ret_array;


	if (cxt){
		cxt->result().assign("dispatch_manual_percent_gridcharge", dispatch_manual_percent_gridcharge);
	}

	return dispatch_manual_percent_gridcharge;

}




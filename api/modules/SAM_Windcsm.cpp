#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Windcsm.h"

SAM_EXPORT int SAM_Windcsm_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("windcsm", data, verbosity, err);
}

SAM_EXPORT void SAM_Windcsm_WindCsm_hub_height_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hub_height", number);
	});
}

SAM_EXPORT void SAM_Windcsm_WindCsm_machine_rating_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "machine_rating", number);
	});
}

SAM_EXPORT void SAM_Windcsm_WindCsm_num_bearings_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "num_bearings", number);
	});
}

SAM_EXPORT void SAM_Windcsm_WindCsm_num_blades_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "num_blades", number);
	});
}

SAM_EXPORT void SAM_Windcsm_WindCsm_onboard_crane_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "onboard_crane", number);
	});
}

SAM_EXPORT void SAM_Windcsm_WindCsm_rotor_torque_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rotor_torque", number);
	});
}

SAM_EXPORT void SAM_Windcsm_WindCsm_turbine_carbon_blades_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "turbine_carbon_blades", number);
	});
}

SAM_EXPORT void SAM_Windcsm_WindCsm_turbine_class_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "turbine_class", number);
	});
}

SAM_EXPORT void SAM_Windcsm_WindCsm_turbine_rotor_diameter_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "turbine_rotor_diameter", number);
	});
}

SAM_EXPORT void SAM_Windcsm_WindCsm_turbine_user_exponent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "turbine_user_exponent", number);
	});
}

SAM_EXPORT double SAM_Windcsm_WindCsm_hub_height_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hub_height", &result))
		make_access_error("SAM_Windcsm", "hub_height");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_WindCsm_machine_rating_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "machine_rating", &result))
		make_access_error("SAM_Windcsm", "machine_rating");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_WindCsm_num_bearings_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "num_bearings", &result))
		make_access_error("SAM_Windcsm", "num_bearings");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_WindCsm_num_blades_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "num_blades", &result))
		make_access_error("SAM_Windcsm", "num_blades");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_WindCsm_onboard_crane_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "onboard_crane", &result))
		make_access_error("SAM_Windcsm", "onboard_crane");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_WindCsm_rotor_torque_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rotor_torque", &result))
		make_access_error("SAM_Windcsm", "rotor_torque");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_WindCsm_turbine_carbon_blades_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "turbine_carbon_blades", &result))
		make_access_error("SAM_Windcsm", "turbine_carbon_blades");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_WindCsm_turbine_class_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "turbine_class", &result))
		make_access_error("SAM_Windcsm", "turbine_class");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_WindCsm_turbine_rotor_diameter_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "turbine_rotor_diameter", &result))
		make_access_error("SAM_Windcsm", "turbine_rotor_diameter");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_WindCsm_turbine_user_exponent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "turbine_user_exponent", &result))
		make_access_error("SAM_Windcsm", "turbine_user_exponent");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_Outputs_bedplate_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bedplate_cost", &result))
		make_access_error("SAM_Windcsm", "bedplate_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_Outputs_blade_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "blade_cost", &result))
		make_access_error("SAM_Windcsm", "blade_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_Outputs_controls_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "controls_cost", &result))
		make_access_error("SAM_Windcsm", "controls_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_Outputs_drivetrain_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "drivetrain_cost", &result))
		make_access_error("SAM_Windcsm", "drivetrain_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_Outputs_drivetrain_mass_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "drivetrain_mass", &result))
		make_access_error("SAM_Windcsm", "drivetrain_mass");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_Outputs_electrical_connections_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "electrical_connections_cost", &result))
		make_access_error("SAM_Windcsm", "electrical_connections_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_Outputs_gearbox_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "gearbox_cost", &result))
		make_access_error("SAM_Windcsm", "gearbox_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_Outputs_generator_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "generator_cost", &result))
		make_access_error("SAM_Windcsm", "generator_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_Outputs_high_speed_side_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "high_speed_side_cost", &result))
		make_access_error("SAM_Windcsm", "high_speed_side_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_Outputs_hub_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hub_cost", &result))
		make_access_error("SAM_Windcsm", "hub_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_Outputs_hvac_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hvac_cost", &result))
		make_access_error("SAM_Windcsm", "hvac_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_Outputs_low_speed_side_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "low_speed_side_cost", &result))
		make_access_error("SAM_Windcsm", "low_speed_side_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_Outputs_main_bearings_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "main_bearings_cost", &result))
		make_access_error("SAM_Windcsm", "main_bearings_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_Outputs_mainframe_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mainframe_cost", &result))
		make_access_error("SAM_Windcsm", "mainframe_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_Outputs_pitch_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pitch_cost", &result))
		make_access_error("SAM_Windcsm", "pitch_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_Outputs_rotor_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rotor_cost", &result))
		make_access_error("SAM_Windcsm", "rotor_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_Outputs_rotor_mass_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rotor_mass", &result))
		make_access_error("SAM_Windcsm", "rotor_mass");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_Outputs_spinner_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spinner_cost", &result))
		make_access_error("SAM_Windcsm", "spinner_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_Outputs_tower_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tower_cost", &result))
		make_access_error("SAM_Windcsm", "tower_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_Outputs_tower_mass_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tower_mass", &result))
		make_access_error("SAM_Windcsm", "tower_mass");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_Outputs_transformer_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "transformer_cost", &result))
		make_access_error("SAM_Windcsm", "transformer_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_Outputs_turbine_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "turbine_cost", &result))
		make_access_error("SAM_Windcsm", "turbine_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_Outputs_variable_speed_electronics_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "variable_speed_electronics_cost", &result))
		make_access_error("SAM_Windcsm", "variable_speed_electronics_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windcsm_Outputs_yaw_system_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "yaw_system_cost", &result))
		make_access_error("SAM_Windcsm", "yaw_system_cost");
	});
	return result;
}




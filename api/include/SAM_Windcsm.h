#ifndef SAM_WINDCSM_H_
#define SAM_WINDCSM_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

//
// Windcsm Technology Model
//

/**
 * Create a Windcsm variable table.
 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
 * @param[in,out] err: a pointer to an error object
 */

SAM_EXPORT typedef void *SAM_Windcsm;

SAM_EXPORT SAM_Windcsm SAM_Windcsm_construct(const char *def, SAM_error *err);

/// verbosity level 0 or 1. Returns 1 on success
SAM_EXPORT int SAM_Windcsm_execute(SAM_Windcsm data, int verbosity, SAM_error *err);

SAM_EXPORT void SAM_Windcsm_destruct(SAM_Windcsm system);


//
// WindCsm parameters
//

/**
 * Set hub_height: Hub height [m]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Windcsm_WindCsm_hub_height_nset(SAM_Windcsm ptr, double number, SAM_error *err);

/**
 * Set machine_rating: Machine rating [kW]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Windcsm_WindCsm_machine_rating_nset(SAM_Windcsm ptr, double number, SAM_error *err);

/**
 * Set num_bearings: Number of main bearings
 * options: None
 * constraints: INTEGER,MIN=1
 * required if: ?=2
 */
SAM_EXPORT void SAM_Windcsm_WindCsm_num_bearings_nset(SAM_Windcsm ptr, double number, SAM_error *err);

/**
 * Set num_blades: Number of blades
 * options: None
 * constraints: INTEGER,MIN=1
 * required if: ?=3
 */
SAM_EXPORT void SAM_Windcsm_WindCsm_num_blades_nset(SAM_Windcsm ptr, double number, SAM_error *err);

/**
 * Set onboard_crane: Onboard crane [0/1]
 * options: None
 * constraints: INTEGER,MIN=0,MAX=1
 * required if: ?=0
 */
SAM_EXPORT void SAM_Windcsm_WindCsm_onboard_crane_nset(SAM_Windcsm ptr, double number, SAM_error *err);

/**
 * Set rotor_torque: Rotor torque [Nm]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Windcsm_WindCsm_rotor_torque_nset(SAM_Windcsm ptr, double number, SAM_error *err);

/**
 * Set turbine_carbon_blades: Turbine carbon blades [0/1]
 * options: None
 * constraints: INTEGER,MIN=0,MAX=1
 * required if: ?=0
 */
SAM_EXPORT void SAM_Windcsm_WindCsm_turbine_carbon_blades_nset(SAM_Windcsm ptr, double number, SAM_error *err);

/**
 * Set turbine_class: Turbine class
 * options: None
 * constraints: INTEGER,MIN=0,MAX=3
 * required if: ?=0
 */
SAM_EXPORT void SAM_Windcsm_WindCsm_turbine_class_nset(SAM_Windcsm ptr, double number, SAM_error *err);

/**
 * Set turbine_rotor_diameter: Turbine rotor diameter [m]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Windcsm_WindCsm_turbine_rotor_diameter_nset(SAM_Windcsm ptr, double number, SAM_error *err);

/**
 * Set turbine_user_exponent: Turbine user exponent
 * options: None
 * constraints: None
 * required if: ?=2.5
 */
SAM_EXPORT void SAM_Windcsm_WindCsm_turbine_user_exponent_nset(SAM_Windcsm ptr, double number, SAM_error *err);


/**
 * WindCsm Getters
 */

SAM_EXPORT double SAM_Windcsm_WindCsm_hub_height_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_WindCsm_machine_rating_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_WindCsm_num_bearings_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_WindCsm_num_blades_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_WindCsm_onboard_crane_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_WindCsm_rotor_torque_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_WindCsm_turbine_carbon_blades_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_WindCsm_turbine_class_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_WindCsm_turbine_rotor_diameter_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_WindCsm_turbine_user_exponent_nget(SAM_Windcsm ptr, SAM_error *err);


/**
 * Outputs Getters
 */

SAM_EXPORT double SAM_Windcsm_Outputs_bedplate_cost_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_Outputs_blade_cost_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_Outputs_controls_cost_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_Outputs_drivetrain_cost_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_Outputs_drivetrain_mass_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_Outputs_electrical_connections_cost_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_Outputs_gearbox_cost_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_Outputs_generator_cost_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_Outputs_high_speed_side_cost_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_Outputs_hub_cost_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_Outputs_hvac_cost_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_Outputs_low_speed_side_cost_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_Outputs_main_bearings_cost_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_Outputs_mainframe_cost_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_Outputs_pitch_cost_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_Outputs_rotor_cost_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_Outputs_rotor_mass_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_Outputs_spinner_cost_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_Outputs_tower_cost_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_Outputs_tower_mass_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_Outputs_transformer_cost_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_Outputs_turbine_cost_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_Outputs_variable_speed_electronics_cost_nget(SAM_Windcsm ptr, SAM_error *err);

SAM_EXPORT double SAM_Windcsm_Outputs_yaw_system_cost_nget(SAM_Windcsm ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif

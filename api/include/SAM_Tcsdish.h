#ifndef SAM_TCSDISH_H_
#define SAM_TCSDISH_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Tcsdish Technology Model
	//

	/** 
	 * Create a Tcsdish variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Tcsdish;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Tcsdish_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// Weather parameters
	//

	/**
	 * Set file_name: local weather file path
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Weather_file_name_sset(SAM_table ptr, const char* str, SAM_error *err);


	//
	// Dish parameters
	//

	/**
	 * Set system_capacity: Nameplate capacity [kW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Dish_system_capacity_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Type295 parameters
	//

	/**
	 * Set A_proj: Projected mirror area [m^2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type295_A_proj_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set A_total: Total Area [m^2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type295_A_total_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set I_cut_in: Insolation cut in value [W/m^2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type295_I_cut_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set d_ap: Dish aperture diameter [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type295_d_ap_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set d_ap_test: Receiver aperture diameter during test [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type295_d_ap_test_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ew_dish_sep: Collector separation East-West [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type295_ew_dish_sep_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set h_slot_gap: Slot gap height [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type295_h_slot_gap_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set n_ew: Number of collectors East-West [-]
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type295_n_ew_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set n_ns: Number of collectors North-South [-]
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type295_n_ns_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ns_dish_sep: Collector separation North-South [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type295_ns_dish_sep_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rho: Mirror surface reflectivity [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type295_rho_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set slope_ew: East-West ground slope [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type295_slope_ew_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set slope_ns: North-South ground slope [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type295_slope_ns_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set test_L_focal: Focal length of mirror system [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type295_test_L_focal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set test_if: Test intercept factor [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type295_test_if_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set w_slot_gap: Slot gap width [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type295_w_slot_gap_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set wind_stow_speed: Wind stow speed [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type295_wind_stow_speed_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Type296 parameters
	//

	/**
	 * Set A_absorber: Absorber surface area [m^2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type296_A_absorber_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set A_wall: Cavity surface area [m^2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type296_A_wall_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set DELTA_T_DIR: Delta temperature for DIR receiver [K]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type296_DELTA_T_DIR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set DELTA_T_REFLUX: Delta temp for REFLUX receiver (always = 40) [K]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type296_DELTA_T_REFLUX_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set L_cav: Internal depth of cavity perp to aperture [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type296_L_cav_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set L_insulation: Insulation thickness [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type296_L_insulation_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set P_cav: Internal cavity pressure with aperture covered [kPa]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type296_P_cav_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_heater_head_high: Heater Head Set Temperature [K]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type296_T_heater_head_high_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_heater_head_low: Header Head Lowest Temperature [K]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type296_T_heater_head_low_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set alpha_absorber: Absorber absorptance [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type296_alpha_absorber_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set alpha_wall: Cavity absorptance [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type296_alpha_wall_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set d_cav: Internal diameter of cavity perp to aperture [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type296_d_cav_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set k_insulation: Insulation thermal conductivity [W/m-K]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type296_k_insulation_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_type: Receiver type (always = 1) [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type296_rec_type_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set transmittance_cover: Transmittance cover (always = 1) [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type296_transmittance_cover_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Type297 parameters
	//

	/**
	 * Set Beale_const_coef: Beale Constant Coefficient [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type297_Beale_const_coef_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Beale_first_coef: Beale first-order coefficient [1/W]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type297_Beale_first_coef_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Beale_fourth_coef: Beale fourth-order coefficient [1/W^4]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type297_Beale_fourth_coef_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Beale_square_coef: Beale second-order coefficient [1/W^2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type297_Beale_square_coef_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Beale_third_coef: Beale third-order coefficient [1/W^3]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type297_Beale_third_coef_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Pressure_coef: Pressure constant coefficient [MPa]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type297_Pressure_coef_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Pressure_first: Pressure first-order coefficient [MPa/W]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type297_Pressure_first_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_compression_in: Receiver efficiency [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type297_T_compression_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set V_displaced: Displaced engine volume [m3]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type297_V_displaced_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set engine_speed: Engine operating speed [rpm]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type297_engine_speed_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Type298 parameters
	//

	/**
	 * Set P_controls: Control System Parasitic Power, Avg. [W]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_P_controls_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set P_tower_fan: Tower fan power (set to 0) [kJ/hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_P_tower_fan_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_cool_speed2: Cooling Fluid Temp. For Fan Speed 2 Cut-In [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_T_cool_speed2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_cool_speed3: Cooling Fluid Temp. For Fan Speed 3 Cut-In [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_T_cool_speed3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Tower_water_outlet_temp: Tower water outlet temperature (set to 20) [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_Tower_water_outlet_temp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set b_cooler: b_cooler parameter [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_b_cooler_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set b_radiator: b_radiator parameter [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_b_radiator_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cooling_fluid: Reference Condition Cooling Fluid: 1=Water,2=V50%EG,3=V25%EG,4=V40%PG,5=V25%PG [-]
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_cooling_fluid_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cooling_tower_on: Option to use a cooling tower (set to 0=off) [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_cooling_tower_on_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set d_pipe_tower: Runner pipe diameter to the cooling tower (set to 0.4m) [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_d_pipe_tower_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set epsilon_cooler_test: Cooler effectiveness [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_epsilon_cooler_test_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set epsilon_power_test: Test value for cooling tower effectiveness (set to 0.7) [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_epsilon_power_test_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set epsilon_radiator_test: Radiator effectiveness [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_epsilon_radiator_test_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set eta_tower_pump: Tower pump efficiency (set to 0.6) [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_eta_tower_pump_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ew_dish_separation: East-West dish separation used in the simulation [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_ew_dish_separation_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set fan_control_signal: Fan control signal (set to 1, not used in this model) [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_fan_control_signal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set fan_speed1: Cooling system fan speed 1 [rpm]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_fan_speed1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set fan_speed2: Cooling system fan speed 2 [rpm]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_fan_speed2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set fan_speed3: Cooling system fan speed 3 [rpm]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_fan_speed3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ns_dish_separation: North-South dish separation used in the simulation [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_ns_dish_separation_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pump_speed: Reference Condition Pump Speed [rpm]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_pump_speed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set system_availability: System availability (set to 1.0) [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_system_availability_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set test_P_fan: Reference Condition Cooling System Fan Power [W]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_test_P_fan_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set test_P_pump: Reference Condition Pump Parasitic Power [W]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_test_P_pump_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set test_T_fluid: Reference Condition Cooling Fluid Temperature [K]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_test_T_fluid_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set test_V_dot_fluid: Reference Condition Cooling Fluid Volumetric Flow Rate [gpm]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_test_V_dot_fluid_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set test_cooling_fluid: Reference Condition Cooling Fluid [-]
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_test_cooling_fluid_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set test_fan_cfm: Reference condition van volumentric flow rate [cfm]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_test_fan_cfm_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set test_fan_rho_air: Reference condition fan air density [kg/m^3]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_test_fan_rho_air_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set test_fan_speed: Reference Condition Cooling System Fan Speed [rpm]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_test_fan_speed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set test_pump_speed: Reference Condition Pump Speed [rpm]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_test_pump_speed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tower_m_dot_water: Tower cooling water flow rate (set to 134,000 kg/hr) [kg/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_tower_m_dot_water_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tower_m_dot_water_test: Test value for the cooling water flow rate (set to 134,000 kg/hr) [kg/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_tower_m_dot_water_test_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tower_mode: Cooling tower type (natural or forced draft) [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_tower_mode_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tower_pipe_material: Tower pipe material (1=plastic, 2=new cast iron, 3=riveted steel) [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Tcsdish_Type298_tower_pipe_material_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * Weather Getters
	 */

	SAM_EXPORT const char* SAM_Tcsdish_Weather_file_name_sget(SAM_table ptr, SAM_error *err);


	/**
	 * Dish Getters
	 */

	SAM_EXPORT double SAM_Tcsdish_Dish_system_capacity_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Type295 Getters
	 */

	SAM_EXPORT double SAM_Tcsdish_Type295_A_proj_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type295_A_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type295_I_cut_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type295_d_ap_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type295_d_ap_test_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type295_ew_dish_sep_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type295_h_slot_gap_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type295_n_ew_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type295_n_ns_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type295_ns_dish_sep_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type295_rho_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type295_slope_ew_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type295_slope_ns_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type295_test_L_focal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type295_test_if_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type295_w_slot_gap_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type295_wind_stow_speed_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Type296 Getters
	 */

	SAM_EXPORT double SAM_Tcsdish_Type296_A_absorber_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type296_A_wall_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type296_DELTA_T_DIR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type296_DELTA_T_REFLUX_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type296_L_cav_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type296_L_insulation_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type296_P_cav_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type296_T_heater_head_high_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type296_T_heater_head_low_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type296_alpha_absorber_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type296_alpha_wall_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type296_d_cav_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type296_k_insulation_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type296_rec_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type296_transmittance_cover_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Type297 Getters
	 */

	SAM_EXPORT double SAM_Tcsdish_Type297_Beale_const_coef_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type297_Beale_first_coef_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type297_Beale_fourth_coef_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type297_Beale_square_coef_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type297_Beale_third_coef_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type297_Pressure_coef_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type297_Pressure_first_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type297_T_compression_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type297_V_displaced_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type297_engine_speed_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Type298 Getters
	 */

	SAM_EXPORT double SAM_Tcsdish_Type298_P_controls_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_P_tower_fan_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_T_cool_speed2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_T_cool_speed3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_Tower_water_outlet_temp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_b_cooler_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_b_radiator_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_cooling_fluid_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_cooling_tower_on_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_d_pipe_tower_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_epsilon_cooler_test_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_epsilon_power_test_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_epsilon_radiator_test_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_eta_tower_pump_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_ew_dish_separation_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_fan_control_signal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_fan_speed1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_fan_speed2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_fan_speed3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_ns_dish_separation_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_pump_speed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_system_availability_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_test_P_fan_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_test_P_pump_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_test_T_fluid_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_test_V_dot_fluid_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_test_cooling_fluid_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_test_fan_cfm_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_test_fan_rho_air_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_test_fan_speed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_test_pump_speed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_tower_m_dot_water_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_tower_m_dot_water_test_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_tower_mode_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Type298_tower_pipe_material_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_Tcsdish_Outputs_Collector_Losses_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_P_SE_losses_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_P_out_SE_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_P_out_rec_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_P_parasitic_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_Phi_shade_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_Power_in_collector_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_Power_in_rec_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_Power_out_col_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_Q_rec_losses_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_T_compression_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_T_heater_head_operate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_T_tower_in_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_T_tower_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Outputs_annual_Collector_Losses_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Outputs_annual_P_out_SE_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Outputs_annual_P_out_rec_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Outputs_annual_P_parasitic_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Outputs_annual_Power_in_collector_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Outputs_annual_Power_in_rec_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Outputs_annual_Power_out_col_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Outputs_annual_Q_rec_losses_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_beam_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Outputs_conversion_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_engine_pressure_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_eta_SE_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_eta_collector_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_eta_net_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_eta_rec_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_hour_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_hourly_Collector_Losses_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_hourly_P_out_SE_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_hourly_P_out_rec_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_hourly_P_parasitic_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_hourly_Power_in_collector_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_hourly_Power_in_rec_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_hourly_Power_out_col_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_hourly_Q_rec_losses_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Tcsdish_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_month_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_monthly_Collector_Losses_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_monthly_P_out_SE_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_monthly_P_out_rec_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_monthly_P_parasitic_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_monthly_Power_in_collector_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_monthly_Power_in_rec_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_monthly_Power_out_col_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_monthly_Q_rec_losses_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_monthly_energy_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_net_power_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_pres_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_solazi_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_solzen_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_tdry_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_twet_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Tcsdish_Outputs_wspd_aget(SAM_table ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
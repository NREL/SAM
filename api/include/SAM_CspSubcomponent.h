#ifndef SAM_CSPSUBCOMPONENT_H_
#define SAM_CSPSUBCOMPONENT_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// CspSubcomponent Technology Model
	//

	/** 
	 * Create a CspSubcomponent variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_CspSubcomponent;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_CspSubcomponent_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// System parameters
	//

	/**
	 * Set solar_mult: Actual solar multiple of system [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_System_solar_mult_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set t_step: Timestep duration [s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_System_t_step_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Weather parameters
	//

	/**
	 * Set T_amb: Ambient temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_Weather_T_amb_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// TES parameters
	//

	/**
	 * Set T_sink_out: Temperature from heat sink or power block [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_T_sink_out_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set T_src_out: Temperature from heat source [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_T_src_out_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set T_tank_cold_ini: Temperature of fluid in cold tank at beginning of step [C]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_T_tank_cold_ini_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_tank_hot_ini: Temperature of fluid in hot tank at beginning of step [C]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_T_tank_hot_ini_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cold_tank_Thtr: Minimum allowable cold tank HTF temp [C]
	 * options: None
	 * constraints: None
	 * required if: tes_type=1|tes_type=3
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_cold_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cold_tank_max_heat: Rated heater capacity for cold tank heating [MWe]
	 * options: None
	 * constraints: None
	 * required if: tes_type=1|tes_type=3
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_cold_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set d_tank_in: Tank diameter input [m]
	 * options: None
	 * constraints: None
	 * required if: is_h_tank_fixed=0|is_h_tank_fixed=2
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_d_tank_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dt_hot: Hot side HX approach temp [C]
	 * options: None
	 * constraints: None
	 * required if: tes_type=1
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_dt_hot_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set h_tank_in: Total height of tank input (height of HTF when tank is full [m]
	 * options: None
	 * constraints: None
	 * required if: is_h_tank_fixed=1
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_h_tank_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set h_tank_min: Minimum allowable HTF height in storage tank [m]
	 * options: None
	 * constraints: None
	 * required if: tes_type=1|tes_type=3
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_h_tank_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hot_tank_Thtr: Minimum allowable hot tank HTF temp [C]
	 * options: None
	 * constraints: None
	 * required if: tes_type=1|tes_type=3
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_hot_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hot_tank_bypassed: Is mass flow from source going straight to cold tank? [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_hot_tank_bypassed_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set hot_tank_max_heat: Rated heater capacity for hot tank heating [MWe]
	 * options: None
	 * constraints: None
	 * required if: tes_type=1|tes_type=3
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_hot_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set init_hot_htf_percent: Initial fraction of avail. vol that is hot [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_init_hot_htf_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_h_tank_fixed: [1] Use fixed height (calculate diameter) [0] Use fixed diameter [2] Use fixed d and h (for packed bed) [-]
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_is_h_tank_fixed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mdot_sink: Mass flow to heat sink or power block [kg/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_mdot_sink_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set mdot_src: Mass flow from heat source [kg/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_mdot_src_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set store_fl_props: User defined storage fluid property data [-]
	 * options: None
	 * constraints: None
	 * required if: tes_type=1
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_store_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set store_fluid: Material number for storage fluid [-]
	 * options: None
	 * constraints: None
	 * required if: tes_type=1
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_store_fluid_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tank_pairs: Number of equivalent tank pairs [-]
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_tank_pairs_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_cyl_piston_loss_poly: Polynomial coefficients describing piston heat loss function (f(kg/s)=%)
	 * options: None
	 * constraints: None
	 * required if: tes_type=3
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_tes_cyl_piston_loss_poly_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set tes_cyl_tank_cp: Tank wall cp (used for Piston Cylinder) [kJ/kg-K]
	 * options: None
	 * constraints: None
	 * required if: tes_type=3
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_tes_cyl_tank_cp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_cyl_tank_dens: Tank wall thickness (used for Piston Cylinder) [kg/m3]
	 * options: None
	 * constraints: None
	 * required if: tes_type=3
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_tes_cyl_tank_dens_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_cyl_tank_insul_percent: Percent additional wall mass due to insulation (used for Piston Cylinder) [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_tes_cyl_tank_insul_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_cyl_tank_thick: Tank wall thickness (used for Piston Cylinder) [m]
	 * options: None
	 * constraints: None
	 * required if: tes_type=3
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_tes_cyl_tank_thick_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_n_tsteps: Number of subtimesteps (for NT and packed bed)
	 * options: None
	 * constraints: None
	 * required if: tes_type>1
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_tes_n_tsteps_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_pb_T_charge_min: Min charge temp [C]
	 * options: None
	 * constraints: None
	 * required if: tes_type=2
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_tes_pb_T_charge_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_pb_T_cold_delta: Max allowable increase in cold discharge temp [C]
	 * options: None
	 * constraints: None
	 * required if: tes_type=2
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_tes_pb_T_cold_delta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_pb_T_grad_ini: TES Temperature gradient at beginning of timestep [C]
	 * options: None
	 * constraints: None
	 * required if: ?=[-274]
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_tes_pb_T_grad_ini_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set tes_pb_T_hot_delta: Max allowable decrease in hot discharge temp [C]
	 * options: None
	 * constraints: None
	 * required if: tes_type=2
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_tes_pb_T_hot_delta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_pb_cp_solid: TES particle specific heat [kJ/kg K]
	 * options: None
	 * constraints: None
	 * required if: tes_type=2
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_tes_pb_cp_solid_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_pb_dens_solid: TES packed bed media density [kg/m3]
	 * options: None
	 * constraints: None
	 * required if: tes_type=2
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_tes_pb_dens_solid_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_pb_f_oversize: Packed bed oversize factor
	 * options: None
	 * constraints: None
	 * required if: tes_type=2
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_tes_pb_f_oversize_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_pb_k_eff: TES packed bed effective conductivity [W/m K]
	 * options: None
	 * constraints: None
	 * required if: tes_type=2
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_tes_pb_k_eff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_pb_n_xsteps: Number of spatial segments
	 * options: None
	 * constraints: None
	 * required if: tes_type=2
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_tes_pb_n_xsteps_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_pb_void_frac: TES packed bed void fraction
	 * options: None
	 * constraints: None
	 * required if: tes_type=2
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_tes_pb_void_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_type: Standard two tank (1), Packed Bed (2), Piston Cylinder (3) [-]
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_tes_type_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tshours: Equivalent full-load thermal storage hours [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_tshours_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set u_tank: Loss coefficient from the tank [W/m2-K]
	 * options: None
	 * constraints: None
	 * required if: tes_type=1|tes_type=3
	 */
	SAM_EXPORT void SAM_CspSubcomponent_TES_u_tank_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Powerblock parameters
	//

	/**
	 * Set P_ref: Rated plant capacity [MWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_Powerblock_P_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set eta_ref: Power cycle efficiency at design [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_Powerblock_eta_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pb_pump_coef: Pumping power to move 1kg of HTF through PB loop [kW/kg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_Powerblock_pb_pump_coef_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// SolarField parameters
	//

	/**
	 * Set Fluid: Field HTF fluid ID number [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_SolarField_Fluid_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set HDR_rough: Header pipe roughness [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_SolarField_HDR_rough_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_loop_in_des: Design loop inlet temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_SolarField_T_loop_in_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_loop_out: Target loop outlet temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_SolarField_T_loop_out_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set calc_design_pipe_vals: Calculate temps and pressures at design conditions for runners and headers [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_SolarField_calc_design_pipe_vals_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set eta_pump: HTF pump efficiency [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_SolarField_eta_pump_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set field_fl_props: User defined field fluid property data [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_SolarField_field_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);


	//
	// Controller parameters
	//

	/**
	 * Set DP_SGS: Pressure drop within the steam generator [bar]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_Controller_DP_SGS_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_tank_hot_inlet_min: Minimum hot tank htf inlet temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_Controller_T_tank_hot_inlet_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set V_tes_des: Design-point velocity to size the TES pipe diameters [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_Controller_V_tes_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set custom_tes_p_loss: TES pipe losses are based on custom lengths and coeffs [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_Controller_custom_tes_p_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set custom_tes_pipe_sizes: Use custom TES pipe diams, wallthks, and lengths [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_Controller_custom_tes_pipe_sizes_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set has_hot_tank_bypass: Bypass valve connects field outlet to cold tank [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_Controller_has_hot_tank_bypass_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set k_tes_loss_coeffs: Minor loss coeffs for the coll, gen, and bypass loops [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_Controller_k_tes_loss_coeffs_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set tanks_in_parallel: Tanks are in parallel, not in series, with solar field [-]
	 * options: None
	 * constraints: None
	 * required if: tes_type=1
	 */
	SAM_EXPORT void SAM_CspSubcomponent_Controller_tanks_in_parallel_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_diams: Custom TES diameters [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_Controller_tes_diams_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set tes_lengths: Custom TES lengths [m]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_CspSubcomponent_Controller_tes_lengths_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set tes_pump_coef: Pumping power to move 1kg of HTF through tes loop [kW/(kg/s)]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_Controller_tes_pump_coef_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_wallthicks: Custom TES wall thicknesses [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspSubcomponent_Controller_tes_wallthicks_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);


	/**
	 * System Getters
	 */

	SAM_EXPORT double SAM_CspSubcomponent_System_solar_mult_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_System_t_step_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Weather Getters
	 */

	SAM_EXPORT double* SAM_CspSubcomponent_Weather_T_amb_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * TES Getters
	 */

	SAM_EXPORT double* SAM_CspSubcomponent_TES_T_sink_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_TES_T_src_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_T_tank_cold_ini_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_T_tank_hot_ini_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_cold_tank_Thtr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_cold_tank_max_heat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_d_tank_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_dt_hot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_h_tank_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_h_tank_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_hot_tank_Thtr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_TES_hot_tank_bypassed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_hot_tank_max_heat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_init_hot_htf_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_is_h_tank_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_TES_mdot_sink_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_TES_mdot_src_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_TES_store_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_store_fluid_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_tank_pairs_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_TES_tes_cyl_piston_loss_poly_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_tes_cyl_tank_cp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_tes_cyl_tank_dens_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_tes_cyl_tank_insul_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_tes_cyl_tank_thick_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_tes_n_tsteps_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_tes_pb_T_charge_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_tes_pb_T_cold_delta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_TES_tes_pb_T_grad_ini_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_tes_pb_T_hot_delta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_tes_pb_cp_solid_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_tes_pb_dens_solid_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_tes_pb_f_oversize_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_tes_pb_k_eff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_tes_pb_n_xsteps_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_tes_pb_void_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_tes_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_tshours_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_TES_u_tank_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Powerblock Getters
	 */

	SAM_EXPORT double SAM_CspSubcomponent_Powerblock_P_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_Powerblock_eta_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_Powerblock_pb_pump_coef_nget(SAM_table ptr, SAM_error *err);


	/**
	 * SolarField Getters
	 */

	SAM_EXPORT double SAM_CspSubcomponent_SolarField_Fluid_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_SolarField_HDR_rough_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_SolarField_T_loop_in_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_SolarField_T_loop_out_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_SolarField_calc_design_pipe_vals_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_SolarField_eta_pump_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_SolarField_field_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * Controller Getters
	 */

	SAM_EXPORT double SAM_CspSubcomponent_Controller_DP_SGS_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_Controller_T_tank_hot_inlet_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_Controller_V_tes_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_Controller_custom_tes_p_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_Controller_custom_tes_pipe_sizes_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_Controller_has_hot_tank_bypass_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_Controller_k_tes_loss_coeffs_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_Controller_tanks_in_parallel_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_Controller_tes_diams_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_Controller_tes_lengths_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_Controller_tes_pump_coef_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_Controller_tes_wallthicks_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_CspSubcomponent_Outputs_T_grad_final_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_Outputs_T_sink_in_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_Outputs_T_src_in_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_Outputs_T_tank_cold_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_Outputs_T_tank_hot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_Outputs_hot_tank_mass_perc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_Outputs_hot_tank_vol_frac_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_Outputs_piston_frac_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_Outputs_piston_loc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_Outputs_q_ch_from_htf_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_Outputs_q_dc_to_htf_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_Outputs_q_dot_ch_from_htf_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_Outputs_q_dot_dc_to_htf_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_Outputs_tes_E_cold_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_Outputs_tes_E_hot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_Outputs_tes_V_cold_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_Outputs_tes_V_hot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_Outputs_tes_diameter_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_Outputs_tes_error_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_Outputs_tes_error_corrected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_Outputs_tes_error_percent_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_Outputs_tes_exp_length_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_Outputs_tes_exp_wall_mass_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_Outputs_tes_height_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_Outputs_tes_leak_error_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_Outputs_tes_mass_cold_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_Outputs_tes_mass_hot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_CspSubcomponent_Outputs_tes_radius_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_CspSubcomponent_Outputs_tes_wall_error_aget(SAM_table ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
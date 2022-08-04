#ifndef SAM_MSPTSFANDRECISOLATED_H_
#define SAM_MSPTSFANDRECISOLATED_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// MsptSfAndRecIsolated Technology Model
	//

	/** 
	 * Create a MsptSfAndRecIsolated variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_MsptSfAndRecIsolated;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_MsptSfAndRecIsolated_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// Simulation parameters
	//

	/**
	 * Set sim_type: 1 (default): timeseries, 2: design only
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_Simulation_sim_type_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// TowerAndReceiver parameters
	//

	/**
	 * Set D_rec: The overall outer diameter of the receiver [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_D_rec_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Flow_type: Receiver flow pattern: see figure on SAM Receiver page
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_Flow_type_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set N_panels: Number of individual panels on the receiver
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_N_panels_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_htf_cold_des: Cold HTF inlet temperature at design conditions [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_T_htf_cold_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_htf_hot_des: Hot HTF outlet temperature at design conditions [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_T_htf_hot_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set crossover_shift: Number of panels shift in receiver crossover position
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_crossover_shift_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set csp.pt.rec.max_oper_frac: Maximum receiver mass flow rate fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_csp_pt_rec_max_oper_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set d_tube_out: The outer diameter of an individual receiver tube [mm]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_d_tube_out_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set downc_tm_mult: Downcomer thermal mass multiplier
	 * options: None
	 * constraints: None
	 * required if: is_rec_model_trans=1
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_downc_tm_mult_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set epsilon: The emissivity of the receiver surface coating
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_epsilon_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set eta_pump: Receiver HTF pump efficiency
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_eta_pump_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set f_rec_min: Minimum receiver mass flow rate turn down fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_f_rec_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set field_fl_props: User defined field fluid property data [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_field_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set h_tower: Tower height [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_h_tower_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hl_ffact: The heat loss factor (thermal loss fudge factor)
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_hl_ffact_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_rec_clearsky_control: 0: use measured dni, 1: use clear-sky control w/ rec_clearsky_frac input
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_is_rec_clearsky_control_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_rec_model_trans: Formulate receiver model as transient?
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_is_rec_model_trans_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mat_tube: Receiver tube material, 2=Stainless AISI316
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_mat_tube_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set piping_length_const: Piping constant length [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_piping_length_const_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set piping_length_mult: Piping length multiplier
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_piping_length_mult_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set piping_loss_coefficient: Thermal loss per meter of piping [Wt/m2-K]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_piping_loss_coefficient_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set q_dot_rec_des: Receiver thermal power to HTF at design [MWt]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_q_dot_rec_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_height: Receiver height [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_rec_height_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_htf: Receiver HTF, 17=Salt (60% NaNO3, 40% KNO3) 10=Salt (46.5% LiF 11.5% NaF 42% KF) 50=Lookup tables
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_rec_htf_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_qf_delay: Energy-based receiver startup delay (fraction of rated thermal power)
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_rec_qf_delay_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_su_delay: Fixed startup delay time for the receiver [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_rec_su_delay_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_tm_mult: Receiver thermal mass multiplier
	 * options: None
	 * constraints: None
	 * required if: is_rec_model_trans=1
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_rec_tm_mult_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set riser_tm_mult: Riser thermal mass multiplier
	 * options: None
	 * constraints: None
	 * required if: is_rec_model_trans=1
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_riser_tm_mult_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set th_riser: Riser or downcomer tube wall thickness [mm]
	 * options: None
	 * constraints: None
	 * required if: is_rec_model_trans=1
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_th_riser_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set th_tube: The wall thickness of a single receiver tube [mm]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_th_tube_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set u_riser: Design point HTF velocity in riser [m/s]
	 * options: None
	 * constraints: None
	 * required if: is_rec_model_trans=1
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_u_riser_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// ReceiverControl parameters
	//

	/**
	 * Set T_htf_cold_in_od: HTF inlet temperature [C]
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_ReceiverControl_T_htf_cold_in_od_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set plant_defocus_od: Plant defocus
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_ReceiverControl_plant_defocus_od_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set rec_clearsky_fraction: Weighting fraction on clear-sky DNI for receiver flow control
	 * options: None
	 * constraints: None
	 * required if: is_rec_clearsky_control=1
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_ReceiverControl_rec_clearsky_fraction_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Timeseries parameters
	//

	/**
	 * Set timestep_od: Timestep [s]
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_Timeseries_timestep_od_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// Weather parameters
	//

	/**
	 * Set P_amb_od: Ambient pressure [mbar]
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_Weather_P_amb_od_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set T_amb_od: Ambient temperature [C]
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_Weather_T_amb_od_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set clearsky_to_measured_dni_od: Ratio of clearsky to measured DNI
	 * options: None
	 * constraints: None
	 * required if: sim_type=1&is_rec_clearsky_control=1
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_Weather_clearsky_to_measured_dni_od_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set deltaT_sky_od: Difference between ambient and sky temps [C]
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_Weather_deltaT_sky_od_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set v_wind_10_od: Wind speed at 10 meters [m/s]
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_Weather_v_wind_10_od_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// Flux parameters
	//

	/**
	 * Set flux_map_od: rows: timestep, columns: panels. Flux *after* rec reflectance losses [W/m2]
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_MsptSfAndRecIsolated_Flux_flux_map_od_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);


	/**
	 * Simulation Getters
	 */

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_Simulation_sim_type_nget(SAM_table ptr, SAM_error *err);


	/**
	 * TowerAndReceiver Getters
	 */

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_D_rec_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_Flow_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_N_panels_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_T_htf_cold_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_T_htf_hot_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_crossover_shift_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_csp_pt_rec_max_oper_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_d_tube_out_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_downc_tm_mult_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_epsilon_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_eta_pump_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_f_rec_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_MsptSfAndRecIsolated_TowerAndReceiver_field_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_h_tower_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_hl_ffact_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_is_rec_clearsky_control_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_is_rec_model_trans_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_mat_tube_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_piping_length_const_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_piping_length_mult_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_piping_loss_coefficient_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_q_dot_rec_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_rec_height_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_rec_htf_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_rec_qf_delay_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_rec_su_delay_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_rec_tm_mult_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_riser_tm_mult_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_th_riser_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_th_tube_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_u_riser_nget(SAM_table ptr, SAM_error *err);


	/**
	 * ReceiverControl Getters
	 */

	SAM_EXPORT double* SAM_MsptSfAndRecIsolated_ReceiverControl_T_htf_cold_in_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_MsptSfAndRecIsolated_ReceiverControl_plant_defocus_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_ReceiverControl_rec_clearsky_fraction_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Timeseries Getters
	 */

	SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Timeseries_timestep_od_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * Weather Getters
	 */

	SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Weather_P_amb_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Weather_T_amb_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Weather_clearsky_to_measured_dni_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Weather_deltaT_sky_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Weather_v_wind_10_od_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * Flux Getters
	 */

	SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Flux_flux_map_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Outputs_T_htf_rec_out_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Outputs_W_dot_pump_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Outputs_eta_rec_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_MsptSfAndRecIsolated_Outputs_m_dot_rec_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Outputs_m_dot_rec_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Outputs_q_dot_htf_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Outputs_q_dot_rec_conv_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Outputs_q_dot_rec_inc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Outputs_q_dot_rec_inc_pre_defocus_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Outputs_q_dot_rec_piping_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Outputs_q_dot_rec_rad_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Outputs_rec_component_defocus_od_aget(SAM_table ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
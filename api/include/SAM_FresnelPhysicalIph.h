#ifndef SAM_FRESNELPHYSICALIPH_H_
#define SAM_FRESNELPHYSICALIPH_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// FresnelPhysicalIph Technology Model
	//

	/** 
	 * Create a FresnelPhysicalIph variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_FresnelPhysicalIph;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_FresnelPhysicalIph_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// SystemControl parameters
	//

	/**
	 * Set disp_inventory_incentive: Dispatch storage terminal inventory incentive multiplier
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SystemControl_disp_inventory_incentive_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sim_type: 1 (default): timeseries, 2: design only
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SystemControl_sim_type_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Weather parameters
	//

	/**
	 * Set file_name: Local weather file with path [none]
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Weather_file_name_sset(SAM_table ptr, const char* str, SAM_error *err);


	//
	// SystemDesign parameters
	//

	/**
	 * Set I_bn_des: Solar irradiation at design [W/m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SystemDesign_I_bn_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_loop_in_des: Design loop inlet temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SystemDesign_T_loop_in_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_loop_out: Target loop outlet temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SystemDesign_T_loop_out_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set q_pb_design: Design heat input to power block [MWt]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SystemDesign_q_pb_design_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set solar_mult_in: Solar multiple Input
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SystemDesign_solar_mult_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set solar_mult_or_Ap: Design using specified solar mult or field aperture [m3]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SystemDesign_solar_mult_or_Ap_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set total_Ap_in: Field aperture Input [m3]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SystemDesign_total_Ap_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tshours: Equivalent full-load thermal storage hours [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SystemDesign_tshours_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// SolarField parameters
	//

	/**
	 * Set FieldConfig: Number of subfield headers
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_FieldConfig_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Fluid: Field HTF fluid number
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_Fluid_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set HDR_rough: Header pipe roughness [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_HDR_rough_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set L_rnr_pb: Length of runner pipe in power block [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_L_rnr_pb_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Pipe_hl_coef: Loss coefficient from the header - runner pipe - and non-HCE piping [W/m2-K]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_Pipe_hl_coef_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set SCA_drives_elec: Tracking power in Watts per SCA drive [W/module]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_SCA_drives_elec_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_amb_sf_des: Ambient design-point temperature for the solar field [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_T_amb_sf_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_fp: Freeze protection temperature (heat trace activation temperature) [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_T_fp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_startup: Power block startup temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_T_startup_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set V_hdr_max: Maximum HTF velocity in the header at design [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_V_hdr_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set V_hdr_min: Minimum HTF velocity in the header at design [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_V_hdr_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set V_wind_des: Design-point wind velocity [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_V_wind_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set eta_pump: HTF pump efficiency
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_eta_pump_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set f_htfmax: Maximum loop mass flow rate fraction of design
	 * options: None
	 * constraints: None
	 * required if: use_abs_or_rel_mdot_limit=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_f_htfmax_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set f_htfmin: Minimum loop mass flow rate fraction of design
	 * options: None
	 * constraints: None
	 * required if: use_abs_or_rel_mdot_limit=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_f_htfmin_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set field_fl_props: Fluid property data
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_field_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set land_mult: Non-solar field land area multiplier [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_land_mult_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set m_dot_htfmax: Maximum loop HTF flow rate [kg/s]
	 * options: None
	 * constraints: None
	 * required if: use_abs_or_rel_mdot_limit=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_m_dot_htfmax_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set m_dot_htfmin: Minimum loop HTF flow rate [kg/s]
	 * options: None
	 * constraints: None
	 * required if: use_abs_or_rel_mdot_limit=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_m_dot_htfmin_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mc_bal_cold: The heat capacity of the balance of plant on the cold side [kWht/K-MWt]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_mc_bal_cold_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mc_bal_hot: The heat capacity of the balance of plant on the hot side [kWht/K-MWt]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_mc_bal_hot_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mc_bal_sca: Non-HTF heat capacity associated with each SCA - per meter basis [Wht/K-m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_mc_bal_sca_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set nMod: Number of collector modules in a loop
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_nMod_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set p_start: Collector startup energy, per SCA [kWhe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_p_start_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_htf_vol: Volume of HTF in a single collector unit per unit aperture area [L/m2-ap]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_rec_htf_vol_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_qf_delay: Energy-based receiver startup delay (fraction of rated thermal power) [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_rec_qf_delay_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_su_delay: Fixed startup delay time for the receiver [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_rec_su_delay_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set theta_dep: deploy angle [deg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_theta_dep_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set theta_stow: stow angle [deg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_theta_stow_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set use_abs_or_rel_mdot_limit: Use mass flow abs (0) or relative (1) limits
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_use_abs_or_rel_mdot_limit_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set washes_per_year: Mirror washing frequency [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_washes_per_year_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set water_per_wash: Water usage per wash [L/m2_aper]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SolarField_water_per_wash_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// ColRec parameters
	//

	/**
	 * Set A_aperture: Reflective aperture area of the collector [m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_A_aperture_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set AbsorberMaterial: Absorber material type
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_AbsorberMaterial_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set AnnulusGas: Annulus gas type (1=air; 26=Ar; 27=H2)
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_AnnulusGas_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set ColAz: Collector azimuth angle [deg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_ColAz_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set DP_coefs: Pressure drop mass flow based part-load curve
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_DP_coefs_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set DP_nominal: Pressure drop across a single collector assembly at design [bar]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_DP_nominal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set D_abs_in: The inner absorber tube diameter [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_D_abs_in_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set D_abs_out: The outer absorber tube diameter [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_D_abs_out_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set D_glass_in: The inner glass envelope diameter [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_D_glass_in_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set D_glass_out: The outer glass envelope diameter [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_D_glass_out_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set D_plug: The diameter of the absorber flow plug (optional) [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_D_plug_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set Design_loss: Receiver heat loss at design [W/m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_Design_loss_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set Dirt_mirror: User-defined dirt on mirror derate
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_Dirt_mirror_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Error: User-defined general optical error derate
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_Error_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Flow_type: The flow type through the absorber
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_Flow_type_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set GeomEffects: Geometry effects derate
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_GeomEffects_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set GlazingIntactIn: The glazing intact flag
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_GlazingIntactIn_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set HCE_FieldFrac: The fraction of the field occupied by this HCE type
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_HCE_FieldFrac_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set HL_T_coefs: HTF temperature-dependent heat loss coefficients [W/m-K]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_HL_T_coefs_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set HL_w_coefs: Wind-speed-dependent heat loss coefficients [W/m-(m/s)]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_HL_w_coefs_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set IAM_L_coefs: Incidence angle modifier coefficients - longitudinal plane
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_IAM_L_coefs_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set IAM_T_coefs: Incidence angle modifier coefficients - transversal plane
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_IAM_T_coefs_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set L_crossover: Length of crossover piping in a loop [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_L_crossover_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set L_mod: The length of the collector module [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_L_mod_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set L_mod_spacing: Piping distance between sequential modules in a loop [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_L_mod_spacing_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set OpticalTable: Values of the optical efficiency table
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_OpticalTable_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set P_a: Annulus gas pressure [torr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_P_a_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set Rough: Roughness of the internal surface [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_Rough_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set Shadowing: Receiver bellows shadowing loss factor
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_Shadowing_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set Tau_envelope: Envelope transmittance
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_Tau_envelope_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set TrackingError: Tracking error derate
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_TrackingError_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set alpha_abs: Absorber absorptance
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_alpha_abs_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set alpha_env: Envelope absorptance
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_alpha_env_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dirt_env: Loss due to dirt on the receiver envelope
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_dirt_env_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set epsilon_abs_1: Absorber emittance - HCE variation 1
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_epsilon_abs_1_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_abs_2: Absorber emittance - HCE variation 2
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_epsilon_abs_2_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_abs_3: Absorber emittance - HCE variation 3
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_epsilon_abs_3_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_abs_4: Absorber emittance - HCE variation 4
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_epsilon_abs_4_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_glass: Glass envelope emissivity
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_epsilon_glass_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set nRecVar: Number of receiver variations
	 * options: None
	 * constraints: INTEGER
	 * required if: ?=4
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_nRecVar_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set opt_model: The optical model
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_opt_model_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_model: Receiver model type (1=Polynomial ; 2=Evac tube)
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_rec_model_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set reflectivity: Solar-weighted mirror reflectivity value
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ColRec_reflectivity_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// HeatSink parameters
	//

	/**
	 * Set hs_phys_N_sub: Number physical heat sink HX nodes
	 * options: None
	 * constraints: None
	 * required if: hs_type=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_HeatSink_hs_phys_N_sub_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hs_phys_P_steam_hot_des: Steam outlet (and inlet) pressure for physical heat sink [bar]
	 * options: None
	 * constraints: None
	 * required if: hs_type=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_HeatSink_hs_phys_P_steam_hot_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hs_phys_Q_steam_hot_des: Steam outlet quality for physical heat sink
	 * options: None
	 * constraints: None
	 * required if: hs_type=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_HeatSink_hs_phys_Q_steam_hot_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hs_phys_T_steam_cold_des: Steam inlet temperature for physical heat sink [C]
	 * options: None
	 * constraints: None
	 * required if: hs_type=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_HeatSink_hs_phys_T_steam_cold_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hs_phys_f_mdot_steam_max: Max steam mdot fraction for physical heat sink
	 * options: None
	 * constraints: None
	 * required if: hs_type=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_HeatSink_hs_phys_f_mdot_steam_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hs_phys_f_mdot_steam_min: Min steam mdot fraction for physical heat sink
	 * options: None
	 * constraints: None
	 * required if: hs_type=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_HeatSink_hs_phys_f_mdot_steam_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hs_phys_tol: Physical heat sink solve tolerance
	 * options: None
	 * constraints: None
	 * required if: hs_type=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_HeatSink_hs_phys_tol_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hs_type: 0: ideal model, 1: physical steam model
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_HeatSink_hs_type_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pb_pump_coef: Pumping power to move 1kg of HTF through PB loop [kW/kg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_HeatSink_pb_pump_coef_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Storage parameters
	//

	/**
	 * Set V_tes_des: Design-point velocity to size the TES pipe diameters [m/s]
	 * options: None
	 * constraints: None
	 * required if: ?=1.85
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_V_tes_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cold_tank_Thtr: Cold tank heater set point
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_cold_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cold_tank_max_heat: Rated heater capacity for cold tank heating [MWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_cold_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dt_cold: Cold side HX approach temp
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_dt_cold_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dt_hot: Hot side HX approach temp
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_dt_hot_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set h_tank: Height of HTF when tank is full
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_h_tank_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set h_tank_min: Minimum tank fluid height
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_h_tank_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hot_tank_Thtr: Hot tank heater set point
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_hot_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hot_tank_max_heat: Rated heater capacity for hot tank heating [MWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_hot_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set init_hot_htf_percent: Initial fraction of avail. vol that is hot [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_init_hot_htf_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set store_fl_props: Storage user-defined HTF Properties
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_store_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set store_fluid: Storage HTF ID
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_store_fluid_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tank_pairs: Number of equivalent tank pairs
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_tank_pairs_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tanks_in_parallel: Tanks are in parallel, not in series, with solar field [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_tanks_in_parallel_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_pump_coef: Pumping power to move 1kg of HTF through tes loop [kW/(kg/s)]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_tes_pump_coef_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set u_tank: Loss coefficient from tank
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Storage_u_tank_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Tou parameters
	//

	/**
	 * Set disp_reporting: Dispatch optimization reporting level [-]
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_disp_reporting_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_spec_bb: Dispatch optimization B&B heuristic [-]
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_disp_spec_bb_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_spec_presolve: Dispatch optimization presolve heuristic [-]
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_disp_spec_presolve_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_spec_scaling: Dispatch optimization scaling heuristic [-]
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_disp_spec_scaling_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_steps_per_hour: Time steps per hour for dispatch optimization calculations [-]
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_disp_steps_per_hour_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factors_ts: Dispatch payment factor array
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=1&csp_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_dispatch_factors_ts_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set f_turb_tou_periods: Dispatch logic for turbine load fraction [-]
	 * options: None
	 * constraints: None
	 * required if: is_timestep_load_fractions=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_f_turb_tou_periods_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set is_timestep_load_fractions: 0: block dispatch, 1: hourly load fraction, 2: absolute load
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_is_timestep_load_fractions_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_tod_pc_target_also_pc_max: Is the TOD target cycle heat input also the max cycle heat input?
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_is_tod_pc_target_also_pc_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ppa_multiplier_model: PPA multiplier model 0: dispatch factors dispatch_factorX, 1: hourly multipliers dispatch_factors_ts [0/1]
	 * options: None
	 * constraints: INTEGER,MIN=0
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_ppa_multiplier_model_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set q_rec_heattrace: Receiver heat trace energy consumption during startup [kWhe]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_q_rec_heattrace_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set q_rec_standby: Receiver standby energy consumption [kWt]
	 * options: None
	 * constraints: None
	 * required if: ?=9e99
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_q_rec_standby_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set timestep_load_abs: Heat sink hourly load (not normalized) [kWt]
	 * options: None
	 * constraints: None
	 * required if: is_timestep_load_fractions=2
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_timestep_load_abs_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set timestep_load_abs_factor: Heat sink hourly load scale factor
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_timestep_load_abs_factor_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set timestep_load_fractions: Turbine load fraction for each timestep, alternative to block dispatch
	 * options: None
	 * constraints: None
	 * required if: is_timestep_load_fractions=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_timestep_load_fractions_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set weekday_schedule: 12x24 Time of Use Values for week days
	 * options: None
	 * constraints: None
	 * required if: is_timestep_load_fractions=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_weekday_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set weekend_schedule: 12x24 Time of Use Values for week end days
	 * options: None
	 * constraints: None
	 * required if: is_timestep_load_fractions=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Tou_weekend_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);


	//
	// SysControl parameters
	//

	/**
	 * Set aux_array: Aux heater, boiler parasitic
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SysControl_aux_array_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set bop_array: Balance of plant parasitic power fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SysControl_bop_array_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set disp_frequency: Frequency for dispatch optimization calculations [hour]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SysControl_disp_frequency_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_horizon: Time horizon for dispatch optimization [hour]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SysControl_disp_horizon_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_max_iter: Max. no. dispatch optimization iterations [-]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SysControl_disp_max_iter_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_mip_gap: Dispatch optimization solution tolerance [-]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SysControl_disp_mip_gap_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_rsu_cost_rel: Receiver startup cost [$/MWt/start]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SysControl_disp_rsu_cost_rel_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_time_weighting: Dispatch optimization future time discounting factor [-]
	 * options: None
	 * constraints: None
	 * required if: ?=0.99
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SysControl_disp_time_weighting_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_timeout: Max. dispatch optimization solve duration [s]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SysControl_disp_timeout_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_dispatch: Allow dispatch optimization? [-]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SysControl_is_dispatch_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pb_fixed_par: Fixed parasitic load - runs at all times
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_SysControl_pb_fixed_par_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// FinancialModel parameters
	//

	/**
	 * Set csp_financial_model:  [1-8]
	 * options: None
	 * constraints: INTEGER,MIN=0
	 * required if: *
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialModel_csp_financial_model_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// FinancialSolutionMode parameters
	//

	/**
	 * Set ppa_soln_mode: PPA solution mode (0=Specify IRR target, 1=Specify PPA price)
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&csp_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialSolutionMode_ppa_soln_mode_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// TimeOfDeliveryFactors parameters
	//

	/**
	 * Set dispatch_sched_weekday: PPA pricing weekday schedule, 12x24
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&csp_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_TimeOfDeliveryFactors_dispatch_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set dispatch_sched_weekend: PPA pricing weekend schedule, 12x24
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&csp_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_TimeOfDeliveryFactors_dispatch_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set dispatch_tod_factors: TOD factors for periods 1 through 9
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&csp_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_TimeOfDeliveryFactors_dispatch_tod_factors_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// Revenue parameters
	//

	/**
	 * Set ppa_price_input_heat_btu: PPA prices - yearly [$/MMBtu]
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&csp_financial_model<5&is_dispatch=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Revenue_ppa_price_input_heat_btu_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// CapitalCosts parameters
	//

	/**
	 * Set bop_spec_cost: Balance of Plant Cost per kWe [$/kWe]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_bop_spec_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set contingency_percent: Contingency Percent [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_contingency_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set epc_cost_fixed: Fixed EPC Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_epc_cost_fixed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set epc_cost_per_acre: EPC Costs per acre [$/acre]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_epc_cost_per_acre_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set epc_cost_per_watt: EPC Cost Wac [$/Wac]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_epc_cost_per_watt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set epc_cost_percent_direct: EPC Costs % direct [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_epc_cost_percent_direct_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set heat_sink_spec_cost: Heat Sink Cost per kWt [$/kWt]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_heat_sink_spec_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set htf_system_spec_cost: HTF System Cost Per m2 [$/m2]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_htf_system_spec_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set plm_cost_fixed: Fixed Land Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_plm_cost_fixed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set plm_cost_per_acre: Land Cost per acre [$/acre]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_plm_cost_per_acre_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set plm_cost_per_watt: Land Cost Wac [$/Wac]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_plm_cost_per_watt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set plm_cost_percent_direct: Land Cost % direct [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_plm_cost_percent_direct_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sales_tax_percent: Sales Tax Percentage of Direct Cost [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_sales_tax_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sales_tax_rate: Sales Tax Rate [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_sales_tax_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set site_improvements_spec_cost: Site Improvement Cost per m2 [$/m2]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_site_improvements_spec_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set solar_field_spec_cost: Solar Field Cost per m2 [$/m2]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_solar_field_spec_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set storage_spec_cost: Storage cost per kWht [$/kWht]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_CapitalCosts_storage_spec_cost_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// FinancialParameters parameters
	//

	/**
	 * Set const_per_interest_rate1: Interest rate, loan 1 [%]
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_interest_rate1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate2: Interest rate, loan 2 [%]
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_interest_rate2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate3: Interest rate, loan 3 [%]
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_interest_rate3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate4: Interest rate, loan 4 [%]
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_interest_rate4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate5: Interest rate, loan 5 [%]
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_interest_rate5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months1: Months prior to operation, loan 1
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_months1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months2: Months prior to operation, loan 2
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_months2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months3: Months prior to operation, loan 3
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_months3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months4: Months prior to operation, loan 4
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_months4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months5: Months prior to operation, loan 5
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_months5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent1: Percent of total installed cost, loan 1 [%]
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_percent1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent2: Percent of total installed cost, loan 2 [%]
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_percent2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent3: Percent of total installed cost, loan 3 [%]
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_percent3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent4: Percent of total installed cost, loan 4 [%]
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_percent4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent5: Percent of total installed cost, loan 5 [%]
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_percent5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate1: Upfront fee on principal, loan 1 [%]
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_upfront_rate1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate2: Upfront fee on principal, loan 2 [%]
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_upfront_rate2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate3: Upfront fee on principal, loan 3 [%]
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_upfront_rate3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate4: Upfront fee on principal, loan 4 [%]
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_upfront_rate4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate5: Upfront fee on principal, loan 5 [%]
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_FinancialParameters_const_per_upfront_rate5_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// AdjustmentFactors parameters
	//

	/**
	 * Set adjust_constant: Constant loss adjustment [%]
	 * options: 'adjust' and 'constant' separated by _ instead of : after SAM 2022.12.21
	 * constraints: MAX=100
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_AdjustmentFactors_adjust_constant_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set adjust_en_periods: Enable period-based adjustment factors [0/1]
	 * options: 'adjust' and 'en_periods' separated by _ instead of : after SAM 2022.12.21
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_AdjustmentFactors_adjust_en_periods_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set adjust_en_timeindex: Enable lifetime adjustment factors [0/1]
	 * options: 'adjust' and 'en_timeindex' separated by _ instead of : after SAM 2022.12.21
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_AdjustmentFactors_adjust_en_timeindex_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set adjust_periods: Period-based adjustment factors [%]
	 * options: Syntax: n x 3 matrix [ start, end, loss ]; Version upgrade: 'adjust' and 'periods' separated by _ instead of : after SAM 2022.12.21
	 * constraints: COLS=3
	 * required if: adjust_en_periods=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_AdjustmentFactors_adjust_periods_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set adjust_timeindex: Lifetime adjustment factors [%]
	 * options: 'adjust' and 'timeindex' separated by _ instead of : after SAM 2022.12.21
	 * constraints: None
	 * required if: adjust_en_timeindex=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_AdjustmentFactors_adjust_timeindex_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// ElectricityRates parameters
	//

	/**
	 * Set en_electricity_rates: Optionally enable/disable electricity_rate [years]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: None
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_en_electricity_rates_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rate_escalation: Annual electricity rate escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_rate_escalation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set ur_annual_min_charge: Annual minimum charge [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_annual_min_charge_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_billing_demand_lookback_percentages: Billing demand lookback percentages by month and consider actual peak demand [%]
	 * options: 12x2
	 * constraints: None
	 * required if: ur_enable_billing_demand=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_billing_demand_lookback_percentages_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_billing_demand_lookback_period: Billing demand lookback period [mn]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=12
	 * required if: ur_enable_billing_demand=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_billing_demand_lookback_period_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_billing_demand_minimum: Minimum billing demand [kW]
	 * options: None
	 * constraints: None
	 * required if: ur_enable_billing_demand=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_billing_demand_minimum_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_dc_billing_demand_periods: Billing demand applicability to a given demand charge time of use period
	 * options: None
	 * constraints: None
	 * required if: ur_enable_billing_demand=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_dc_billing_demand_periods_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_dc_enable: Enable demand charge [0/1]
	 * options: 0=disable,1=enable
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_dc_enable_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_dc_flat_mat: Demand rates (flat) table [col 0=month, col 1=tier no, col 2=tier peak (kW), col 3=charge ($/kW)]
	 * options: nx4
	 * constraints: None
	 * required if: ur_dc_enable=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_dc_flat_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_dc_sched_weekday: Demand charge weekday schedule [Periods defined in ur_dc_tou_mat]
	 * options: 12x24
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_dc_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_dc_sched_weekend: Demand charge weekend schedule [Periods defined in ur_dc_tou_mat]
	 * options: 12x24
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_dc_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_dc_tou_mat: Demand rates (TOU) table [col 0=period no, col 1=tier no, col 2=tier peak (kW), col 3=charge ($/kW)]
	 * options: nx4
	 * constraints: None
	 * required if: ur_dc_enable=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_dc_tou_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_ec_sched_weekday: Energy charge weekday schedule [Periods defined in ur_ec_tou_mat]
	 * options: 12x24
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_ec_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_ec_sched_weekend: Energy charge weekend schedule [Periods defined in ur_ec_tou_mat]
	 * options: 12x24
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_ec_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_ec_tou_mat: Energy rates table [col 0=period no, col 1=tier no, col 2=max usage, col 3=max usage units (0=kWh, 1=kWh/kW, 2=kWh daily, 3=kWh/kW daily), col 4=buy rate ($/kWh), col 5=sell rate ($/kWh)]
	 * options: nx6
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_ec_tou_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_en_ts_buy_rate: Enable time step buy rates [0/1]
	 * options: 0=disable,1=enable
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_en_ts_buy_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_en_ts_sell_rate: Enable time step sell rates [0/1]
	 * options: 0=disable,1=enable
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_en_ts_sell_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_enable_billing_demand: Enable billing demand ratchets [0/1]
	 * options: 0=disable,1=enable
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_enable_billing_demand_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_metering_option: Metering options [0=net energy metering,1=net energy metering with $ credits,2=net billing,3=net billing with carryover to next month,4=buy all - sell all]
	 * options: Net metering monthly excess
	 * constraints: INTEGER,MIN=0,MAX=4
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_metering_option_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_monthly_fixed_charge: Monthly fixed charge [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_monthly_fixed_charge_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_monthly_min_charge: Monthly minimum charge [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_monthly_min_charge_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_nb_apply_credit_current_month: Apply earned credits to balance before rolling over excess         [0/1]
	 * options: 0=disable,1=enable
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_nb_apply_credit_current_month_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_nb_credit_expire: Credit is lost upon end of year         [0/1]
	 * options: 0=disable,1=enable
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_nb_credit_expire_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_nm_credit_month: Month of year end payout (true-up) [mn]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=11
	 * required if: ?=11
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_nm_credit_month_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_nm_credit_rollover: Apply net metering true-up credits to future bills [0/1]
	 * options: 0=disable,1=enable
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_nm_credit_rollover_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_nm_yearend_sell_rate: Net metering true-up credit sell rate [$/kWh]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_nm_yearend_sell_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_sell_eq_buy: Set sell rate equal to buy rate [0/1]
	 * options: Optional override
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_sell_eq_buy_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_ts_buy_rate: Time step buy rates [$/kWh]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_ts_buy_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set ur_ts_sell_rate: Time step sell rates [$/kWh]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_ts_sell_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set ur_yearzero_usage_peaks: Peak usage by month for year zero [kW]
	 * options: 12
	 * constraints: None
	 * required if: ur_enable_billing_demand=1
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_ElectricityRates_ur_yearzero_usage_peaks_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// Lifetime parameters
	//

	/**
	 * Set inflation_rate: Inflation rate [%]
	 * options: None
	 * constraints: MIN=-99
	 * required if: None
	 */
	SAM_EXPORT void SAM_FresnelPhysicalIph_Lifetime_inflation_rate_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * SystemControl Getters
	 */

	SAM_EXPORT double SAM_FresnelPhysicalIph_SystemControl_disp_inventory_incentive_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SystemControl_sim_type_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Weather Getters
	 */

	SAM_EXPORT const char* SAM_FresnelPhysicalIph_Weather_file_name_sget(SAM_table ptr, SAM_error *err);


	/**
	 * SystemDesign Getters
	 */

	SAM_EXPORT double SAM_FresnelPhysicalIph_SystemDesign_I_bn_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SystemDesign_T_loop_in_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SystemDesign_T_loop_out_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SystemDesign_q_pb_design_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SystemDesign_solar_mult_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SystemDesign_solar_mult_or_Ap_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SystemDesign_total_Ap_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SystemDesign_tshours_nget(SAM_table ptr, SAM_error *err);


	/**
	 * SolarField Getters
	 */

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_FieldConfig_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_Fluid_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_HDR_rough_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_L_rnr_pb_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_Pipe_hl_coef_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_SCA_drives_elec_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_T_amb_sf_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_T_fp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_T_startup_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_V_hdr_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_V_hdr_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_V_wind_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_eta_pump_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_f_htfmax_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_f_htfmin_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_SolarField_field_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_land_mult_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_m_dot_htfmax_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_m_dot_htfmin_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_mc_bal_cold_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_mc_bal_hot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_mc_bal_sca_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_nMod_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_p_start_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_rec_htf_vol_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_rec_qf_delay_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_rec_su_delay_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_theta_dep_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_theta_stow_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_use_abs_or_rel_mdot_limit_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_washes_per_year_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SolarField_water_per_wash_nget(SAM_table ptr, SAM_error *err);


	/**
	 * ColRec Getters
	 */

	SAM_EXPORT double SAM_FresnelPhysicalIph_ColRec_A_aperture_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_AbsorberMaterial_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_AnnulusGas_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_ColRec_ColAz_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_DP_coefs_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_ColRec_DP_nominal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_D_abs_in_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_D_abs_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_D_glass_in_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_D_glass_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_D_plug_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_Design_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_ColRec_Dirt_mirror_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_ColRec_Error_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_Flow_type_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_ColRec_GeomEffects_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_GlazingIntactIn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_HCE_FieldFrac_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_HL_T_coefs_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_HL_w_coefs_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_IAM_L_coefs_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_IAM_T_coefs_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_ColRec_L_crossover_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_ColRec_L_mod_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_ColRec_L_mod_spacing_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_OpticalTable_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_P_a_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_Rough_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_Shadowing_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_Tau_envelope_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_ColRec_TrackingError_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_alpha_abs_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_alpha_env_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_dirt_env_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_epsilon_abs_1_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_epsilon_abs_2_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_epsilon_abs_3_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_epsilon_abs_4_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ColRec_epsilon_glass_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_ColRec_nRecVar_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_ColRec_opt_model_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_ColRec_rec_model_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_ColRec_reflectivity_nget(SAM_table ptr, SAM_error *err);


	/**
	 * HeatSink Getters
	 */

	SAM_EXPORT double SAM_FresnelPhysicalIph_HeatSink_hs_phys_N_sub_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_HeatSink_hs_phys_P_steam_hot_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_HeatSink_hs_phys_Q_steam_hot_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_HeatSink_hs_phys_T_steam_cold_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_HeatSink_hs_phys_f_mdot_steam_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_HeatSink_hs_phys_f_mdot_steam_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_HeatSink_hs_phys_tol_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_HeatSink_hs_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_HeatSink_pb_pump_coef_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Storage Getters
	 */

	SAM_EXPORT double SAM_FresnelPhysicalIph_Storage_V_tes_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Storage_cold_tank_Thtr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Storage_cold_tank_max_heat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Storage_dt_cold_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Storage_dt_hot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Storage_h_tank_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Storage_h_tank_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Storage_hot_tank_Thtr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Storage_hot_tank_max_heat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Storage_init_hot_htf_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Storage_store_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Storage_store_fluid_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Storage_tank_pairs_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Storage_tanks_in_parallel_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Storage_tes_pump_coef_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Storage_u_tank_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Tou Getters
	 */

	SAM_EXPORT double SAM_FresnelPhysicalIph_Tou_disp_reporting_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Tou_disp_spec_bb_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Tou_disp_spec_presolve_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Tou_disp_spec_scaling_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Tou_disp_steps_per_hour_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Tou_dispatch_factors_ts_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Tou_f_turb_tou_periods_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Tou_is_timestep_load_fractions_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Tou_is_tod_pc_target_also_pc_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Tou_ppa_multiplier_model_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Tou_q_rec_heattrace_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Tou_q_rec_standby_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Tou_timestep_load_abs_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Tou_timestep_load_abs_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Tou_timestep_load_fractions_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Tou_weekday_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Tou_weekend_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * SysControl Getters
	 */

	SAM_EXPORT double* SAM_FresnelPhysicalIph_SysControl_aux_array_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_SysControl_bop_array_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SysControl_disp_frequency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SysControl_disp_horizon_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SysControl_disp_max_iter_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SysControl_disp_mip_gap_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SysControl_disp_rsu_cost_rel_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SysControl_disp_time_weighting_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SysControl_disp_timeout_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SysControl_is_dispatch_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_SysControl_pb_fixed_par_nget(SAM_table ptr, SAM_error *err);


	/**
	 * FinancialModel Getters
	 */

	SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialModel_csp_financial_model_nget(SAM_table ptr, SAM_error *err);


	/**
	 * FinancialSolutionMode Getters
	 */

	SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialSolutionMode_ppa_soln_mode_nget(SAM_table ptr, SAM_error *err);


	/**
	 * TimeOfDeliveryFactors Getters
	 */

	SAM_EXPORT double* SAM_FresnelPhysicalIph_TimeOfDeliveryFactors_dispatch_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_TimeOfDeliveryFactors_dispatch_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_TimeOfDeliveryFactors_dispatch_tod_factors_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * Revenue Getters
	 */

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Revenue_ppa_price_input_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * CapitalCosts Getters
	 */

	SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_bop_spec_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_contingency_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_epc_cost_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_epc_cost_per_acre_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_epc_cost_per_watt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_epc_cost_percent_direct_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_heat_sink_spec_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_htf_system_spec_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_plm_cost_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_plm_cost_per_acre_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_plm_cost_per_watt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_plm_cost_percent_direct_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_sales_tax_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_sales_tax_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_site_improvements_spec_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_solar_field_spec_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_CapitalCosts_storage_spec_cost_nget(SAM_table ptr, SAM_error *err);


	/**
	 * FinancialParameters Getters
	 */

	SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_interest_rate1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_interest_rate2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_interest_rate3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_interest_rate4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_interest_rate5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_months1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_months2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_months3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_months4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_months5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_percent1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_percent2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_percent3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_percent4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_percent5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_upfront_rate1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_upfront_rate2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_upfront_rate3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_upfront_rate4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_FinancialParameters_const_per_upfront_rate5_nget(SAM_table ptr, SAM_error *err);


	/**
	 * AdjustmentFactors Getters
	 */

	SAM_EXPORT double SAM_FresnelPhysicalIph_AdjustmentFactors_adjust_constant_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_AdjustmentFactors_adjust_en_periods_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_AdjustmentFactors_adjust_en_timeindex_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_AdjustmentFactors_adjust_periods_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_AdjustmentFactors_adjust_timeindex_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * ElectricityRates Getters
	 */

	SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_en_electricity_rates_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ElectricityRates_rate_escalation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_annual_min_charge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ElectricityRates_ur_billing_demand_lookback_percentages_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_billing_demand_lookback_period_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_billing_demand_minimum_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ElectricityRates_ur_dc_billing_demand_periods_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_dc_enable_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ElectricityRates_ur_dc_flat_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ElectricityRates_ur_dc_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ElectricityRates_ur_dc_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ElectricityRates_ur_dc_tou_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ElectricityRates_ur_ec_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ElectricityRates_ur_ec_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ElectricityRates_ur_ec_tou_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_en_ts_buy_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_en_ts_sell_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_enable_billing_demand_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_metering_option_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_monthly_fixed_charge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_monthly_min_charge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_nb_apply_credit_current_month_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_nb_credit_expire_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_nm_credit_month_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_nm_credit_rollover_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_nm_yearend_sell_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_ElectricityRates_ur_sell_eq_buy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ElectricityRates_ur_ts_buy_rate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ElectricityRates_ur_ts_sell_rate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_ElectricityRates_ur_yearzero_usage_peaks_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * Lifetime Getters
	 */

	SAM_EXPORT double SAM_FresnelPhysicalIph_Lifetime_inflation_rate_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_A_field_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_A_loop_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_DP_pressure_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_EqOpteff_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_P_fixed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_P_plant_balance_tot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_Q_field_des_SS_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_Q_loop_des_SS_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_Q_loss_hdr_rnr_des_SS_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_Q_loss_receiver_des_SS_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_Q_tes_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_SCAs_def_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_T_field_cold_in_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_T_field_hot_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_T_field_out_des_SS_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_T_heat_sink_in_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_T_heat_sink_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_T_loop_out_des_SS_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_T_rec_cold_in_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_T_rec_hot_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_T_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_T_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_V_hdr_max_des_SS_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_V_hdr_min_des_SS_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_W_dot_bop_design_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_W_dot_field_pump_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_W_dot_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_W_dot_par_tot_haf_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_W_dot_parasitic_tot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_W_dot_pc_pump_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_W_dot_pump_des_SS_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_W_dot_sca_track_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_annual_electricity_consumption_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_annual_energy_distribution_time_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_annual_energy_heat_btu_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_annual_field_freeze_protection_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_annual_tes_freeze_protection_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_annual_thermal_consumption_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_annual_total_water_use_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_aux_design_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_avg_dt_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_avg_suboptimal_rel_mip_gap_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_beam_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_bop_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_interest1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_interest2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_interest3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_interest4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_interest5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_interest_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_percent_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_principal1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_principal2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_principal3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_principal4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_principal5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_principal_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_total1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_total2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_total3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_total4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_const_per_total5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_construction_financing_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_contingency_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_cp_battery_nameplate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_cp_system_nameplate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_dP_field_des_SS_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_d_tank_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_defocus_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_deltaP_field_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_obj_relax_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_objective_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_pceff_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_presolve_nconstr_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_presolve_nvar_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_qpbsu_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_qsf_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_qsfprod_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_qsfsu_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_rel_mip_gap_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_rev_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_solve_iter_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_solve_state_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_solve_time_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_subopt_flag_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_tes_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_thermeff_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_disp_wpb_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_e_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_e_dot_field_int_energy_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_eff_des_SS_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_eff_loop_des_SS_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_epc_total_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_eta_optical_des_SS_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_f_htfmax_actual_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_f_htfmin_actual_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_field_area_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_field_htf_max_temp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_field_htf_min_temp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_gen_heat_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_gen_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_heat_sink_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_hl_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_hour_day_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_htf_system_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_installed_per_capacity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_is_pc_sb_allowed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_is_pc_su_allowed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_is_rec_su_allowed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_loop_eff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_loop_opt_eff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_loop_therm_eff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_m_dot_balance_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_m_dot_cold_tank_to_hot_tank_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_m_dot_cr_to_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_m_dot_cycle_to_field_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_m_dot_des_SS_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_m_dot_field_delivered_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_m_dot_field_recirc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_m_dot_field_to_cycle_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_m_dot_htf_heat_sink_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_m_dot_htfmax_actual_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_m_dot_htfmin_actual_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_m_dot_loop_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_m_dot_loop_des_SS_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_m_dot_pc_to_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_m_dot_tes_cold_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_m_dot_tes_hot_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_mass_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_mass_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_mdot_field_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_month_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_monthly_energy_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_monthly_energy_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_nLoops_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_n_op_modes_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_nameplate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_op_mode_1_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_op_mode_2_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_op_mode_3_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_operating_modes_a_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_operating_modes_b_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_operating_modes_c_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_opt_derate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_opt_normal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_pipe_tes_P_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_pipe_tes_T_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_pipe_tes_diams_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_pipe_tes_lengths_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_pipe_tes_mdot_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_pipe_tes_vel_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_pipe_tes_wallthk_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_plm_total_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_ppa_price_input_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_pres_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_pricing_mult_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_balance_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dc_tes_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dot_est_cr_on_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dot_est_cr_su_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dot_est_tes_ch_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dot_est_tes_dc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dot_freeze_prot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dot_htf_sf_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_q_dot_loss_tes_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dot_pc_max_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dot_pc_min_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dot_pc_sb_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dot_pc_target_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dot_piping_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dot_rec_abs_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dot_rec_inc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dot_rec_thermal_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_dot_to_heat_sink_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_q_field_des_actual_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_q_field_des_ideal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_inc_sf_tot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_q_tes_heater_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_rec_thermal_eff_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_recirculating_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_rh_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_sales_tax_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_sim_duration_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_site_improvements_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_sm1_aperture_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_sm1_nLoops_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_solar_field_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_solar_mult_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_solazi_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_solzen_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_system_capacity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_tank_losses_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_tdry_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_tes_htf_cp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_tes_htf_dens_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_tes_htf_max_temp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_tes_htf_min_temp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_tes_htf_pump_power_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_therm_eff_des_SS_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_therm_eff_loop_des_SS_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_thermal_load_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_time_hr_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_timestep_load_abs_calc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_timestep_load_fractions_calc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_total_Ap_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_total_direct_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_total_indirect_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_total_installed_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_total_land_area_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_total_tracking_power_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_tou_value_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_ts_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_twet_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_vol_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_FresnelPhysicalIph_Outputs_vol_tank_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_FresnelPhysicalIph_Outputs_wspd_aget(SAM_table ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
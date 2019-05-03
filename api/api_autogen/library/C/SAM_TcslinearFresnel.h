#ifndef SAM_TCSLINEARFRESNEL_H_
#define SAM_TCSLINEARFRESNEL_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// TcslinearFresnel Technology Model
	//

	/** 
	 * Create a TcslinearFresnel variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_TcslinearFresnel;

	SAM_EXPORT SAM_TcslinearFresnel SAM_TcslinearFresnel_construct(const char* def, SAM_error* err);

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_TcslinearFresnel_execute(SAM_TcslinearFresnel data, int verbosity, SAM_error* err);

	SAM_EXPORT void SAM_TcslinearFresnel_destruct(SAM_TcslinearFresnel system);


	//
	// Weather parameters
	//

	/**
	 * Set azimuth: Azimuth angle of surface/axis
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Weather_azimuth_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set file_name: local weather file path
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Weather_file_name_sset(SAM_TcslinearFresnel ptr, const char* str, SAM_error *err);

	/**
	 * Set tilt: Tilt angle of surface/axis
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Weather_tilt_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set track_mode: Tracking mode
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Weather_track_mode_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);


	//
	// LinearFresnelr parameters
	//

	/**
	 * Set system_capacity: Nameplate capacity [kW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_LinearFresnelr_system_capacity_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);


	//
	// TouTranslator parameters
	//

	/**
	 * Set weekday_schedule: 12x24 Time of Use Values for week days
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_TouTranslator_weekday_schedule_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set weekend_schedule: 12x24 Time of Use Values for week end days
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_TouTranslator_weekend_schedule_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);


	//
	// Solarfield parameters
	//

	/**
	 * Set A_aperture: (boiler, SH) Reflective aperture area of the collector module [m^2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_A_aperture_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set AbsorberMaterial: (boiler, SH) Absorber material type [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_AbsorberMaterial_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set AnnulusGas: (boiler, SH) Annulus gas type {1=air; 26=Ar; 27=H2} (4: # field fracs) [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_AnnulusGas_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ColAz: Collector azimuth angle [deg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_ColAz_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set D_2: (boiler, SH) The inner absorber tube diameter [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_D_2_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set D_3: (boiler, SH) The outer absorber tube diameter [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_D_3_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set D_4: (boiler, SH) The inner glass envelope diameter [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_D_4_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set D_5: (boiler, SH) The outer glass envelope diameter [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_D_5_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set D_p: (boiler, SH) The diameter of the absorber flow plug (optional) [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_D_p_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set Design_loss: (boiler, SH) Receiver heat loss at design (4: # field fracs) [W/m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_Design_loss_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set Dirt_HCE: (boiler, SH) Loss due to dirt on the receiver envelope (4: # field fracs) [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_Dirt_HCE_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set EPSILON_4: (boiler, SH) Inner glass envelope emissivities (Pyrex) (4: # field fracs) [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_EPSILON_4_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set Flow_type: (boiler, SH) The flow type through the absorber [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_Flow_type_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set GeomEffects: (boiler, SH) User-defined geometry effects derate [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_GeomEffects_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set GlazingIntactIn: (boiler, SH) The glazing intact flag {true=0; false=1} (4: # field fracs) [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_GlazingIntactIn_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set HCE_FieldFrac: (boiler, SH) The fraction of the field occupied by this HCE type (4: # field fracs) [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_HCE_FieldFrac_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set HLCharType: (boiler, SH) Flag indicating the heat loss model type {1=poly.; 2=Forristall} [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_HLCharType_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set HL_W: (boiler, SH) Heat loss coef adj wind velocity (0,1,2,3,4 order terms) [1/(m/s)^order]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_HL_W_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set HL_dT: (boiler, SH) Heat loss coefficient - HTF temperature (0,1,2,3,4 order terms) [W/m-K^order]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_HL_dT_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set IAM_L: (boiler, SH) Longitudinal Incident angle modifiers (0,1,2,3,4 order terms) [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_IAM_L_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set IAM_T: (boiler, SH) Transverse Incident angle modifiers (0,1,2,3,4 order terms) [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_IAM_T_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set I_bn: Beam normal radiation (input kJ/m2-hr) [W/m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_I_bn_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set I_bn_des: Design point irradiation value [W/m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_I_bn_des_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set LHV_eff: Fuel LHV efficiency (0..1) [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_LHV_eff_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set L_col: (boiler, SH) Active length of the superheater section collector module [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_L_col_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set OptCharType: (boiler, SH) The optical characterization method [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_OptCharType_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set PB_fixed_par: fraction of rated gross power consumed at all hours of the year [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_PB_fixed_par_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set PB_pump_coef: Pumping power required to move 1kg of HTF through power block flow [kW/kg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_PB_pump_coef_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set P_a: (boiler, SH) Annulus gas pressure (4: # field fracs) [torr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_P_a_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set P_amb: Ambient pressure [atm]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_P_amb_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set P_turb_des: Design-point turbine inlet pressure [bar]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_P_turb_des_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set Pipe_hl_coef: Loss coefficient from the header.. runner pipe.. and non-HCE pipin [W/m2-K]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_Pipe_hl_coef_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set Rough: (boiler, SH) Roughness of the internal surface [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_Rough_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set SCA_drives_elec: Tracking power.. in Watts per SCA drive [W/m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_SCA_drives_elec_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set Shadowing: (boiler, SH) Receiver bellows shadowing loss factor (4: # field fracs) [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_Shadowing_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set SolarAz_init: Solar azimuth angle [deg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_SolarAz_init_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set SolarZen: Solar zenith angle [deg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_SolarZen_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set T_amb_des_sf: Design-point ambient temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_T_amb_des_sf_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set T_db: Dry bulb air temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_T_db_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set T_dp: The dewpoint temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_T_dp_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set T_fp: Freeze protection temperature (heat trace activation temperature) [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_T_fp_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set T_pb_out_init: Fluid temperature from the power block [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_T_pb_out_init_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set Tau_envelope: (boiler, SH) Envelope transmittance (4: # field fracs) [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_Tau_envelope_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set TrackingError: (boiler, SH) User-defined tracking error derate [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_TrackingError_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set V_wind: Ambient windspeed [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_V_wind_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set V_wind_max: Maximum allowable wind velocity before safety stow [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_V_wind_max_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set alpha_abs: (boiler, SH) Absorber absorptance (4: # field fracs) [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_alpha_abs_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set alpha_env: (boiler, SH) Envelope absorptance (4: # field fracs) [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_alpha_env_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set aux_array: Aux_parVal, Aux_parPF, Aux_par0, Aux_par1, Aux_par2 [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_aux_array_aset(SAM_TcslinearFresnel ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set b_OpticalTable: Values of the optical efficiency table [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_b_OpticalTable_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set b_eps_HCE1: (temperature) Absorber emittance (eps) [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_b_eps_HCE1_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set b_eps_HCE2: (temperature) Absorber emittance (eps) [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_b_eps_HCE2_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set b_eps_HCE3: (temperature) Absorber emittance (eps) [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_b_eps_HCE3_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set b_eps_HCE4: (temperature) Absorber emittance (eps) [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_b_eps_HCE4_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set bop_array: BOP_parVal, BOP_parPF, BOP_par0, BOP_par1, BOP_par2 [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_bop_array_aset(SAM_TcslinearFresnel ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set cycle_cutoff_frac: Minimum turbine operation fraction before shutdown [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_cycle_cutoff_frac_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set cycle_max_fraction: Maximum turbine over design operation fraction [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_cycle_max_fraction_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set dirt_mirror: (boiler, SH) User-defined dirt on mirror derate [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_dirt_mirror_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set dnifc: Forecast DNI [W/m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_dnifc_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set e_startup: Thermal inertia contribution per sq meter of solar field [kJ/K-m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_e_startup_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set error: (boiler, SH) User-defined general optical error derate [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_error_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set eta_pump: Feedwater pump efficiency [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_eta_pump_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set fP_boil_to_sh: Design-point pressure drop between the boiler and superheater frac [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_fP_boil_to_sh_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set fP_hdr_c: Average design-point cold header pressure drop fraction [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_fP_hdr_c_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set fP_hdr_h: Average design-point hot header pressure drop fraction [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_fP_hdr_h_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set fP_sf_boil: Design-point pressure drop across the solar field boiler fraction [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_fP_sf_boil_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set fP_sf_sh: Design-point pressure drop across the solar field superheater frac [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_fP_sf_sh_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set ffrac: Fossil dispatch logic - TOU periods [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_ffrac_aset(SAM_TcslinearFresnel ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set fossil_mode: Operation mode for the fossil backup {1=Normal,2=supp,3=toppin} [none]
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_fossil_mode_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set is_multgeom: Does the superheater have a different geometry from the boiler {1=yes} [none]
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_is_multgeom_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set is_oncethru: Flag indicating whether flow is once through with superheat [none]
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_is_oncethru_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set is_sh: Does the solar field include a superheating section [none]
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_is_sh_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set latitude: Site latitude resource page [deg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_latitude_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set m_dot_htf_ref: Reference HTF flow rate at design conditions [kg/hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_m_dot_htf_ref_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set m_dot_min: Minimum loop flow rate [kg/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_m_dot_min_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set m_pb_demand: Demand htf flow from the power block [kg/hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_m_pb_demand_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set nLoops: Number of loops [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_nLoops_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set nModBoil: Number of modules in the boiler section [none]
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_nModBoil_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set nModSH: Number of modules in the superheater section [none]
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_nModSH_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set q_max_aux: Maximum heat rate of the auxiliary heater [MW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_q_max_aux_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set q_pb_des: Design heat input to the power block [MW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_q_pb_des_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set q_sby_frac: Fraction of thermal power required for standby [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_q_sby_frac_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set rho_mirror_clean: (boiler, SH) User-defined clean mirror reflectivity [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_rho_mirror_clean_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set sh_OpticalTable: Values of the optical efficiency table [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_sh_OpticalTable_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set sh_eps_HCE1: (temperature) Absorber emittance (eps) [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_sh_eps_HCE1_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set sh_eps_HCE2: (temperature) Absorber emittance (eps) [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_sh_eps_HCE2_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set sh_eps_HCE3: (temperature) Absorber emittance (eps) [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_sh_eps_HCE3_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set sh_eps_HCE4: (temperature) Absorber emittance (eps) [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_sh_eps_HCE4_mset(SAM_TcslinearFresnel ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set shift: Shift in longitude from local standard meridian [deg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_shift_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set solarm: Solar multiple [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_solarm_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set t_sby: Low resource standby period [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_t_sby_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set tes_hours: Equivalent full-load thermal storage hours [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_tes_hours_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set theta_dep: deploy angle [deg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_theta_dep_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set theta_stow: stow angle [deg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_theta_stow_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set x_b_des: Design point boiler outlet steam quality [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_x_b_des_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);


	//
	// Heliostat parameters
	//

	/**
	 * Set csp.lf.sf.washes_per_year: Mirror washing frequency
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Heliostat_csp_lf_sf_washes_per_year_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set csp.lf.sf.water_per_wash: Water usage per wash [L/m2_aper]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Heliostat_csp_lf_sf_water_per_wash_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);


	//
	// Powerblock parameters
	//

	/**
	 * Set CT: Flag for using dry cooling or wet cooling system [none]
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_CT_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set F_wc: Fraction indicating wet cooling use for hybrid system [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_F_wc_aset(SAM_TcslinearFresnel ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set P_amb_pwb: Ambient pressure [atm]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_P_amb_pwb_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set P_boil_des: Boiler operating pressure @ design [bar]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_P_boil_des_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set P_cond_min: Minimum condenser pressure [inHg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_P_cond_min_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set P_cond_ratio: Condenser pressure ratio [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_P_cond_ratio_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set P_rh_ref: Reheater operating pressure at design [bar]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_P_rh_ref_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set T_ITD_des: ITD at design for dry system [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_T_ITD_des_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set T_amb_des: Reference ambient temperature at design point [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_T_amb_des_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set T_approach: Cooling tower approach temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_T_approach_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set T_cold_ref: Reference HTF outlet temperature at design [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_T_cold_ref_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set T_db_pwb: Ambient dry bulb temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_T_db_pwb_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set T_hot: Hot HTF inlet temperature, from storage tank [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_T_hot_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set T_wb: Ambient wet bulb temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_T_wb_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set dT_cw_ref: Reference condenser cooling water inlet/outlet T diff [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_dT_cw_ref_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set demand_var: Control signal indicating operational mode [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_demand_var_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set dp_b: Pressure drop in boiler [Pa]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_dp_b_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set dp_rh: Pressure drop in reheater [Pa]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_dp_rh_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set dp_sh: Pressure drop in superheater [Pa]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_dp_sh_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set eta_ref: Reference conversion efficiency at design condition [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_eta_ref_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set f_recSU: Fraction powerblock can run due to receiver startup [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_f_recSU_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set m_dot_st: HTF mass flow rate [kg/hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_m_dot_st_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set n_pl_inc: Number of part-load increments for the heat rejection system [none]
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_n_pl_inc_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set pb_bd_frac: Power block blowdown steam fraction  [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_pb_bd_frac_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set pc_mode: Cycle part load control, from plant controller [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_pc_mode_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set q_sby_frac: Fraction of thermal power required for standby mode [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_q_sby_frac_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set relhum: Relative humidity of the ambient air [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_relhum_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set rh_frac_ref: Reheater flow fraction at design [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_rh_frac_ref_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set standby_control: Control signal indicating standby mode [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_standby_control_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set startup_frac: Fraction of design thermal power needed for startup [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_startup_frac_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);

	/**
	 * Set startup_time: Time needed for power block startup [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_startup_time_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err);


	/**
	 * Weather Getters
	 */

	SAM_EXPORT double SAM_TcslinearFresnel_Weather_azimuth_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_TcslinearFresnel_Weather_file_name_sget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Weather_tilt_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Weather_track_mode_nget(SAM_TcslinearFresnel ptr, SAM_error *err);


	/**
	 * LinearFresnelr Getters
	 */

	SAM_EXPORT double SAM_TcslinearFresnel_LinearFresnelr_system_capacity_nget(SAM_TcslinearFresnel ptr, SAM_error *err);


	/**
	 * TouTranslator Getters
	 */

	SAM_EXPORT double* SAM_TcslinearFresnel_TouTranslator_weekday_schedule_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_TouTranslator_weekend_schedule_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * Solarfield Getters
	 */

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_A_aperture_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_AbsorberMaterial_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_AnnulusGas_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_ColAz_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_D_2_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_D_3_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_D_4_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_D_5_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_D_p_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_Design_loss_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_Dirt_HCE_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_EPSILON_4_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_Flow_type_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_GeomEffects_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_GlazingIntactIn_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_HCE_FieldFrac_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_HLCharType_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_HL_W_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_HL_dT_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_IAM_L_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_IAM_T_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_I_bn_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_I_bn_des_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_LHV_eff_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_L_col_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_OptCharType_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_PB_fixed_par_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_PB_pump_coef_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_P_a_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_P_amb_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_P_turb_des_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_Pipe_hl_coef_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_Rough_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_SCA_drives_elec_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_Shadowing_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_SolarAz_init_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_SolarZen_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_T_amb_des_sf_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_T_db_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_T_dp_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_T_fp_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_T_pb_out_init_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_Tau_envelope_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_TrackingError_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_V_wind_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_V_wind_max_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_alpha_abs_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_alpha_env_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_aux_array_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_b_OpticalTable_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_b_eps_HCE1_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_b_eps_HCE2_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_b_eps_HCE3_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_b_eps_HCE4_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_bop_array_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_cycle_cutoff_frac_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_cycle_max_fraction_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_dirt_mirror_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_dnifc_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_e_startup_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_error_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_eta_pump_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_fP_boil_to_sh_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_fP_hdr_c_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_fP_hdr_h_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_fP_sf_boil_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_fP_sf_sh_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_ffrac_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_fossil_mode_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_is_multgeom_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_is_oncethru_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_is_sh_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_latitude_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_m_dot_htf_ref_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_m_dot_min_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_m_pb_demand_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_nLoops_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_nModBoil_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_nModSH_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_q_max_aux_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_q_pb_des_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_q_sby_frac_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_rho_mirror_clean_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_sh_OpticalTable_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_sh_eps_HCE1_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_sh_eps_HCE2_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_sh_eps_HCE3_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Solarfield_sh_eps_HCE4_mget(SAM_TcslinearFresnel ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_shift_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_solarm_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_t_sby_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_tes_hours_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_theta_dep_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_theta_stow_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_x_b_des_nget(SAM_TcslinearFresnel ptr, SAM_error *err);


	/**
	 * Heliostat Getters
	 */

	SAM_EXPORT double SAM_TcslinearFresnel_Heliostat_csp_lf_sf_washes_per_year_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Heliostat_csp_lf_sf_water_per_wash_nget(SAM_TcslinearFresnel ptr, SAM_error *err);


	/**
	 * Powerblock Getters
	 */

	SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_CT_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Powerblock_F_wc_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_P_amb_pwb_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_P_boil_des_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_P_cond_min_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_P_cond_ratio_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_P_rh_ref_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_T_ITD_des_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_T_amb_des_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_T_approach_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_T_cold_ref_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_T_db_pwb_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_T_hot_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_T_wb_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_dT_cw_ref_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_demand_var_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_dp_b_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_dp_rh_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_dp_sh_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_eta_ref_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_f_recSU_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_m_dot_st_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_n_pl_inc_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_pb_bd_frac_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_pc_mode_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_q_sby_frac_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_relhum_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_rh_frac_ref_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_standby_control_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_startup_frac_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_startup_time_nget(SAM_TcslinearFresnel ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_E_bal_startup_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_P_cond_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_P_sf_in_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_P_turb_in_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_T_field_in_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_T_field_out_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_T_loop_out_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_T_pb_in_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_T_pb_out_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_W_cool_par_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_W_cycle_gross_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_W_dot_aux_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_W_dot_bop_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_W_dot_col_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_W_dot_fixed_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_W_dot_pump_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_W_net_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Outputs_annual_W_cycle_gross_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Outputs_annual_energy_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Outputs_annual_fuel_usage_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Outputs_annual_total_water_use_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_beam_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Outputs_capacity_factor_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Outputs_conversion_factor_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_dP_tot_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_defocus_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_eta_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_eta_opt_ave_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_eta_sf_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_eta_thermal_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_f_bays_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_gen_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_hour_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Outputs_kwh_per_kw_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_m_dot_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_m_dot_aux_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_m_dot_b_tot_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_m_dot_field_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_m_dot_makeup_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_m_dot_to_pb_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_month_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_monthly_energy_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_pres_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_q_aux_fluid_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_q_aux_fuel_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_q_dump_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_q_field_delivered_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_q_inc_tot_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_q_loss_piping_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_q_loss_rec_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_q_loss_sf_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_q_to_pb_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_solazi_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_solzen_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcslinearFresnel_Outputs_system_heat_rate_nget(SAM_TcslinearFresnel ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_tdry_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_tou_value_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_twet_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcslinearFresnel_Outputs_wspd_aget(SAM_TcslinearFresnel ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
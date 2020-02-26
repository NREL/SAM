#ifndef SAM_LINEARFRESNELDSGIPH_H_
#define SAM_LINEARFRESNELDSGIPH_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

//
// LinearFresnelDsgIph Technology Model
//

/**
 * Create a LinearFresnelDsgIph variable table.
 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
 * @param[in,out] err: a pointer to an error object
 */

SAM_EXPORT typedef void *SAM_LinearFresnelDsgIph;

SAM_EXPORT SAM_LinearFresnelDsgIph SAM_LinearFresnelDsgIph_construct(const char *def, SAM_error *err);

/// verbosity level 0 or 1. Returns 1 on success
SAM_EXPORT int SAM_LinearFresnelDsgIph_execute(SAM_LinearFresnelDsgIph data, int verbosity, SAM_error *err);

SAM_EXPORT void SAM_LinearFresnelDsgIph_destruct(SAM_LinearFresnelDsgIph system);


//
// Weather parameters
//

/**
 * Set file_name: local weather file path
 * options: None
 * constraints: LOCAL_FILE
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Weather_file_name_sset(SAM_LinearFresnelDsgIph ptr, const char *str, SAM_error *err);


//
// Solarfield parameters
//

/**
 * Set A_aperture: (boiler, SH) Reflective aperture area of the collector module [m^2]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_A_aperture_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                                   SAM_error *err);

/**
 * Set AbsorberMaterial: (boiler, SH) Absorber material type [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_AbsorberMaterial_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                                         SAM_error *err);

/**
 * Set AnnulusGas: (boiler, SH) Annulus gas type {1=air; 26=Ar; 27=H2} (4: # field fracs) [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_AnnulusGas_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                                   SAM_error *err);

/**
 * Set ColAz: Collector azimuth angle [deg]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_ColAz_nset(SAM_LinearFresnelDsgIph ptr, double number, SAM_error *err);

/**
 * Set D_2: (boiler, SH) The inner absorber tube diameter [m]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_D_2_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                            SAM_error *err);

/**
 * Set D_3: (boiler, SH) The outer absorber tube diameter [m]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_D_3_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                            SAM_error *err);

/**
 * Set D_4: (boiler, SH) The inner glass envelope diameter [m]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_D_4_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                            SAM_error *err);

/**
 * Set D_5: (boiler, SH) The outer glass envelope diameter [m]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_D_5_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                            SAM_error *err);

/**
 * Set D_p: (boiler, SH) The diameter of the absorber flow plug (optional) [m]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_D_p_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                            SAM_error *err);

/**
 * Set Design_loss: (boiler, SH) Receiver heat loss at design (4: # field fracs) [W/m]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_Design_loss_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                                    SAM_error *err);

/**
 * Set Dirt_HCE: (boiler, SH) Loss due to dirt on the receiver envelope (4: # field fracs) [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_Dirt_HCE_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                                 SAM_error *err);

/**
 * Set EPSILON_4: (boiler, SH) Inner glass envelope emissivities (Pyrex) (4: # field fracs) [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_EPSILON_4_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                                  SAM_error *err);

/**
 * Set Flow_type: (boiler, SH) The flow type through the absorber [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_Flow_type_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                                  SAM_error *err);

/**
 * Set GeomEffects: (boiler, SH) User-defined geometry effects derate [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_GeomEffects_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                                    SAM_error *err);

/**
 * Set GlazingIntactIn: (boiler, SH) The glazing intact flag {true=0; false=1} (4: # field fracs) [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_GlazingIntactIn_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                                        SAM_error *err);

/**
 * Set HCE_FieldFrac: (boiler, SH) The fraction of the field occupied by this HCE type (4: # field fracs) [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_HCE_FieldFrac_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                                      SAM_error *err);

/**
 * Set HLCharType: (boiler, SH) Flag indicating the heat loss model type {1=poly.; 2=Forristall} [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_HLCharType_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                                   SAM_error *err);

/**
 * Set HL_W: (boiler, SH) Heat loss coef adj wind velocity (0,1,2,3,4 order terms) [1/(m/s)^order]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_HL_W_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                             SAM_error *err);

/**
 * Set HL_dT: (boiler, SH) Heat loss coefficient - HTF temperature (0,1,2,3,4 order terms) [W/m-K^order]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_HL_dT_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                              SAM_error *err);

/**
 * Set IAM_L: (boiler, SH) Longitudinal Incident angle modifiers (0,1,2,3,4 order terms) [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_IAM_L_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                              SAM_error *err);

/**
 * Set IAM_T: (boiler, SH) Transverse Incident angle modifiers (0,1,2,3,4 order terms) [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_IAM_T_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                              SAM_error *err);

/**
 * Set I_bn_des: Design point irradiation value [W/m2]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_I_bn_des_nset(SAM_LinearFresnelDsgIph ptr, double number, SAM_error *err);

/**
 * Set L_col: (boiler, SH) Active length of the superheater section collector module [m]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_L_col_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                              SAM_error *err);

/**
 * Set OptCharType: (boiler, SH) The optical characterization method [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_OptCharType_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                                    SAM_error *err);

/**
 * Set P_a: (boiler, SH) Annulus gas pressure (4: # field fracs) [torr]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_P_a_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                            SAM_error *err);

/**
 * Set P_turb_des: Design-point turbine inlet pressure [bar]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_P_turb_des_nset(SAM_LinearFresnelDsgIph ptr, double number, SAM_error *err);

/**
 * Set Pipe_hl_coef: Loss coefficient from the header.. runner pipe.. and non-HCE pipin [W/m2-K]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_Pipe_hl_coef_nset(SAM_LinearFresnelDsgIph ptr, double number, SAM_error *err);

/**
 * Set Rough: (boiler, SH) Roughness of the internal surface [m]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_Rough_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                              SAM_error *err);

/**
 * Set SCA_drives_elec: Tracking power.. in Watts per m2 [W/m2]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_SCA_drives_elec_nset(SAM_LinearFresnelDsgIph ptr, double number, SAM_error *err);

/**
 * Set Shadowing: (boiler, SH) Receiver bellows shadowing loss factor (4: # field fracs) [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_Shadowing_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                                  SAM_error *err);

/**
 * Set T_amb_des_sf: Design-point ambient temperature [C]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_T_amb_des_sf_nset(SAM_LinearFresnelDsgIph ptr, double number, SAM_error *err);

/**
 * Set T_fp: Freeze protection temperature (heat trace activation temperature) [C]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_T_fp_nset(SAM_LinearFresnelDsgIph ptr, double number, SAM_error *err);

/**
 * Set Tau_envelope: (boiler, SH) Envelope transmittance (4: # field fracs) [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_Tau_envelope_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                                     SAM_error *err);

/**
 * Set TrackingError: (boiler, SH) User-defined tracking error derate [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_TrackingError_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                                      SAM_error *err);

/**
 * Set V_wind_max: Maximum allowable wind velocity before safety stow [m/s]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_V_wind_max_nset(SAM_LinearFresnelDsgIph ptr, double number, SAM_error *err);

/**
 * Set alpha_abs: (boiler, SH) Absorber absorptance (4: # field fracs) [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_alpha_abs_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                                  SAM_error *err);

/**
 * Set alpha_env: (boiler, SH) Envelope absorptance (4: # field fracs) [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_alpha_env_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                                  SAM_error *err);

/**
 * Set b_OpticalTable: Values of the optical efficiency table [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_b_OpticalTable_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                                       SAM_error *err);

/**
 * Set b_eps_HCE1: (temperature) Absorber emittance (eps) [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_b_eps_HCE1_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                                   SAM_error *err);

/**
 * Set b_eps_HCE2: (temperature) Absorber emittance (eps) [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_b_eps_HCE2_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                                   SAM_error *err);

/**
 * Set b_eps_HCE3: (temperature) Absorber emittance (eps) [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_b_eps_HCE3_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                                   SAM_error *err);

/**
 * Set b_eps_HCE4: (temperature) Absorber emittance (eps) [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_b_eps_HCE4_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                                   SAM_error *err);

/**
 * Set dirt_mirror: (boiler, SH) User-defined dirt on mirror derate [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_dirt_mirror_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                                    SAM_error *err);

/**
 * Set e_startup: Thermal inertia contribution per sq meter of solar field [kJ/K-m2]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_e_startup_nset(SAM_LinearFresnelDsgIph ptr, double number, SAM_error *err);

/**
 * Set error: (boiler, SH) User-defined general optical error derate [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_error_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                              SAM_error *err);

/**
 * Set eta_pump: Feedwater pump efficiency [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_eta_pump_nset(SAM_LinearFresnelDsgIph ptr, double number, SAM_error *err);

/**
 * Set fP_hdr_c: Average design-point cold header pressure drop fraction [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_fP_hdr_c_nset(SAM_LinearFresnelDsgIph ptr, double number, SAM_error *err);

/**
 * Set fP_hdr_h: Average design-point hot header pressure drop fraction [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_fP_hdr_h_nset(SAM_LinearFresnelDsgIph ptr, double number, SAM_error *err);

/**
 * Set fP_sf_boil: Design-point pressure drop across the solar field boiler fraction [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_fP_sf_boil_nset(SAM_LinearFresnelDsgIph ptr, double number, SAM_error *err);

/**
 * Set nLoops: Number of loops [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_nLoops_nset(SAM_LinearFresnelDsgIph ptr, double number, SAM_error *err);

/**
 * Set nModBoil: Number of modules in the boiler section [none]
 * options: None
 * constraints: INTEGER
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_nModBoil_nset(SAM_LinearFresnelDsgIph ptr, double number, SAM_error *err);

/**
 * Set q_pb_des: Design heat input to the power block [MW]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_q_pb_des_nset(SAM_LinearFresnelDsgIph ptr, double number, SAM_error *err);

/**
 * Set rho_mirror_clean: (boiler, SH) User-defined clean mirror reflectivity [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_rho_mirror_clean_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                                         SAM_error *err);

/**
 * Set sh_OpticalTable: Values of the optical efficiency table [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_sh_OpticalTable_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                                        SAM_error *err);

/**
 * Set sh_eps_HCE1: (temperature) Absorber emittance (eps) [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_sh_eps_HCE1_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                                    SAM_error *err);

/**
 * Set sh_eps_HCE2: (temperature) Absorber emittance (eps) [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_sh_eps_HCE2_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                                    SAM_error *err);

/**
 * Set sh_eps_HCE3: (temperature) Absorber emittance (eps) [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_sh_eps_HCE3_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                                    SAM_error *err);

/**
 * Set sh_eps_HCE4: (temperature) Absorber emittance (eps) [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_sh_eps_HCE4_mset(SAM_LinearFresnelDsgIph ptr, double *mat, int nrows, int ncols,
                                                    SAM_error *err);

/**
 * Set theta_dep: deploy angle [deg]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_theta_dep_nset(SAM_LinearFresnelDsgIph ptr, double number, SAM_error *err);

/**
 * Set theta_stow: stow angle [deg]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_theta_stow_nset(SAM_LinearFresnelDsgIph ptr, double number, SAM_error *err);

/**
 * Set x_b_des: Design point boiler outlet steam quality [none]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Solarfield_x_b_des_nset(SAM_LinearFresnelDsgIph ptr, double number, SAM_error *err);


//
// Powerblock parameters
//

/**
 * Set T_cold_ref: Reference HTF outlet temperature at design [C]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Powerblock_T_cold_ref_nset(SAM_LinearFresnelDsgIph ptr, double number, SAM_error *err);

/**
 * Set T_hot: Hot HTF inlet temperature, from storage tank [C]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Powerblock_T_hot_nset(SAM_LinearFresnelDsgIph ptr, double number, SAM_error *err);


//
// Heliostat parameters
//

/**
 * Set csp.lf.sf.washes_per_year: Mirror washing frequency [-/year]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Heliostat_csp_lf_sf_washes_per_year_nset(SAM_LinearFresnelDsgIph ptr, double number,
                                                                 SAM_error *err);

/**
 * Set csp.lf.sf.water_per_wash: Water usage per wash [L/m2_aper]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_Heliostat_csp_lf_sf_water_per_wash_nset(SAM_LinearFresnelDsgIph ptr, double number,
                                                                SAM_error *err);


//
// HeatSink parameters
//

/**
 * Set heat_sink_dP_frac: Fractional pressure drop through heat sink
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_LinearFresnelDsgIph_HeatSink_heat_sink_dP_frac_nset(SAM_LinearFresnelDsgIph ptr, double number, SAM_error *err);


/**
 * Weather Getters
 */

SAM_EXPORT const char *SAM_LinearFresnelDsgIph_Weather_file_name_sget(SAM_LinearFresnelDsgIph ptr, SAM_error *err);


/**
 * Solarfield Getters
 */

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_A_aperture_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_AbsorberMaterial_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols,
                                                         SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_AnnulusGas_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_ColAz_nget(SAM_LinearFresnelDsgIph ptr, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_D_2_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_D_3_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_D_4_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_D_5_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_D_p_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_Design_loss_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols,
                                                    SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_Dirt_HCE_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_EPSILON_4_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_Flow_type_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_GeomEffects_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols,
                                                    SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_GlazingIntactIn_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols,
                                                        SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_HCE_FieldFrac_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols,
                                                      SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_HLCharType_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_HL_W_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_HL_dT_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_IAM_L_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_IAM_T_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_I_bn_des_nget(SAM_LinearFresnelDsgIph ptr, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_L_col_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_OptCharType_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols,
                                                    SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_P_a_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_P_turb_des_nget(SAM_LinearFresnelDsgIph ptr, SAM_error *err);

SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_Pipe_hl_coef_nget(SAM_LinearFresnelDsgIph ptr, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_Rough_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_SCA_drives_elec_nget(SAM_LinearFresnelDsgIph ptr, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_Shadowing_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_T_amb_des_sf_nget(SAM_LinearFresnelDsgIph ptr, SAM_error *err);

SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_T_fp_nget(SAM_LinearFresnelDsgIph ptr, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_Tau_envelope_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols,
                                                     SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_TrackingError_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols,
                                                      SAM_error *err);

SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_V_wind_max_nget(SAM_LinearFresnelDsgIph ptr, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_alpha_abs_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_alpha_env_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_b_OpticalTable_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols,
                                                       SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_b_eps_HCE1_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_b_eps_HCE2_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_b_eps_HCE3_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_b_eps_HCE4_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_dirt_mirror_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols,
                                                    SAM_error *err);

SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_e_startup_nget(SAM_LinearFresnelDsgIph ptr, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_error_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_eta_pump_nget(SAM_LinearFresnelDsgIph ptr, SAM_error *err);

SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_fP_hdr_c_nget(SAM_LinearFresnelDsgIph ptr, SAM_error *err);

SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_fP_hdr_h_nget(SAM_LinearFresnelDsgIph ptr, SAM_error *err);

SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_fP_sf_boil_nget(SAM_LinearFresnelDsgIph ptr, SAM_error *err);

SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_nLoops_nget(SAM_LinearFresnelDsgIph ptr, SAM_error *err);

SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_nModBoil_nget(SAM_LinearFresnelDsgIph ptr, SAM_error *err);

SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_q_pb_des_nget(SAM_LinearFresnelDsgIph ptr, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_rho_mirror_clean_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols,
                                                         SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_sh_OpticalTable_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols,
                                                        SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_sh_eps_HCE1_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols,
                                                    SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_sh_eps_HCE2_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols,
                                                    SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_sh_eps_HCE3_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols,
                                                    SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Solarfield_sh_eps_HCE4_mget(SAM_LinearFresnelDsgIph ptr, int *nrows, int *ncols,
                                                    SAM_error *err);

SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_theta_dep_nget(SAM_LinearFresnelDsgIph ptr, SAM_error *err);

SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_theta_stow_nget(SAM_LinearFresnelDsgIph ptr, SAM_error *err);

SAM_EXPORT double SAM_LinearFresnelDsgIph_Solarfield_x_b_des_nget(SAM_LinearFresnelDsgIph ptr, SAM_error *err);


/**
 * Powerblock Getters
 */

SAM_EXPORT double SAM_LinearFresnelDsgIph_Powerblock_T_cold_ref_nget(SAM_LinearFresnelDsgIph ptr, SAM_error *err);

SAM_EXPORT double SAM_LinearFresnelDsgIph_Powerblock_T_hot_nget(SAM_LinearFresnelDsgIph ptr, SAM_error *err);


/**
 * Heliostat Getters
 */

SAM_EXPORT double
SAM_LinearFresnelDsgIph_Heliostat_csp_lf_sf_washes_per_year_nget(SAM_LinearFresnelDsgIph ptr, SAM_error *err);

SAM_EXPORT double
SAM_LinearFresnelDsgIph_Heliostat_csp_lf_sf_water_per_wash_nget(SAM_LinearFresnelDsgIph ptr, SAM_error *err);


/**
 * HeatSink Getters
 */

SAM_EXPORT double SAM_LinearFresnelDsgIph_HeatSink_heat_sink_dP_frac_nget(SAM_LinearFresnelDsgIph ptr, SAM_error *err);


/**
 * Outputs Getters
 */

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_T_field_cold_in_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_T_field_hot_out_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_T_rec_cold_in_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_T_rec_hot_out_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_W_dot_field_pump_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_W_dot_heat_sink_pump_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_W_dot_parasitic_tot_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_W_dot_sca_track_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double
SAM_LinearFresnelDsgIph_Outputs_annual_electricity_consumption_nget(SAM_LinearFresnelDsgIph ptr, SAM_error *err);

SAM_EXPORT double SAM_LinearFresnelDsgIph_Outputs_annual_energy_nget(SAM_LinearFresnelDsgIph ptr, SAM_error *err);

SAM_EXPORT double SAM_LinearFresnelDsgIph_Outputs_annual_field_energy_nget(SAM_LinearFresnelDsgIph ptr, SAM_error *err);

SAM_EXPORT double
SAM_LinearFresnelDsgIph_Outputs_annual_thermal_consumption_nget(SAM_LinearFresnelDsgIph ptr, SAM_error *err);

SAM_EXPORT double
SAM_LinearFresnelDsgIph_Outputs_annual_total_water_use_nget(SAM_LinearFresnelDsgIph ptr, SAM_error *err);

SAM_EXPORT double *SAM_LinearFresnelDsgIph_Outputs_beam_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_defocus_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_deltaP_field_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_e_dot_field_int_energy_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_eta_opt_ave_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_LinearFresnelDsgIph_Outputs_gen_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_hour_day_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_m_dot_field_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_m_dot_loop_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_LinearFresnelDsgIph_Outputs_month_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_op_mode_1_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_op_mode_2_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_op_mode_3_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_LinearFresnelDsgIph_Outputs_pres_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_q_dot_freeze_prot_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_q_dot_piping_loss_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_q_dot_rec_abs_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_q_dot_rec_inc_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_q_dot_rec_thermal_loss_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_q_dot_sf_out_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_q_dot_to_heat_sink_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_q_inc_sf_tot_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_solazi_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_solzen_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_LinearFresnelDsgIph_Outputs_tdry_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_theta_longitudinal_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_theta_traverse_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_time_hr_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_LinearFresnelDsgIph_Outputs_twet_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_LinearFresnelDsgIph_Outputs_wspd_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_x_field_hot_out_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_LinearFresnelDsgIph_Outputs_x_rec_hot_out_aget(SAM_LinearFresnelDsgIph ptr, int *length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif

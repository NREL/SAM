#ifndef SAM_TROUGHPHYSICALIPH_H_
#define SAM_TROUGHPHYSICALIPH_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// TroughPhysicalIph Technology Model
	//

	/** 
	 * Create a TroughPhysicalIph variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_TroughPhysicalIph;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_TroughPhysicalIph_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// SystemControl parameters
	//

	/**
	 * Set is_dispatch: Allow dispatch optimization?
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SystemControl_is_dispatch_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_parallel_htr: Does plant include a HTF heater parallel to solar field?
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SystemControl_is_parallel_htr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sim_type: 1 (default): timeseries, 2: design only
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SystemControl_sim_type_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Weather parameters
	//

	/**
	 * Set file_name: Local weather file with path [none]
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: ?
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Weather_file_name_sset(SAM_table ptr, const char* str, SAM_error *err);

	/**
	 * Set solar_resource_data: Weather resource data in memory
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Weather_solar_resource_data_tset(SAM_table ptr, SAM_table tab, SAM_error *err);


	//
	// SystemDesign parameters
	//

	/**
	 * Set q_pb_design: Design heat input to power block [MWt]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SystemDesign_q_pb_design_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// SolarField parameters
	//

	/**
	 * Set A_aperture: Reflective aperture area of the collector [m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_A_aperture_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set AbsorberMaterial: Absorber material type [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_AbsorberMaterial_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set AnnulusGas: Annulus gas type (1=air, 26=Ar, 27=H2) [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_AnnulusGas_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set Ave_Focal_Length: Average focal length of the collector  [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_Ave_Focal_Length_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set ColperSCA: Number of individual collector sections in an SCA  [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_ColperSCA_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set D_2: Inner absorber tube diameter [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_D_2_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set D_3: Outer absorber tube diameter [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_D_3_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set D_4: Inner glass envelope diameter  [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_D_4_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set D_5: Outer glass envelope diameter  [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_D_5_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set D_p: Diameter of the absorber flow plug (optional)  [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_D_p_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set Design_loss: Receiver heat loss at design [W/m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_Design_loss_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set Dirt_HCE: Loss due to dirt on the receiver envelope [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_Dirt_HCE_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set Dirt_mirror: User-defined dirt on mirror derate [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_Dirt_mirror_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set Distance_SCA: Piping distance between SCA's in the field [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_Distance_SCA_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set EPSILON_4: Inner glass envelope emissivities (Pyrex)  [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_EPSILON_4_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set EPSILON_5: Outer glass envelope emissivities (Pyrex)  [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_EPSILON_5_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set Error: User-defined general optical error derate  [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_Error_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set FieldConfig: Number of subfield headers [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_FieldConfig_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Flow_type: Flow type through the absorber [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_Flow_type_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set Fluid: Field HTF fluid ID number [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_Fluid_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set GeomEffects: User-defined geometry effects derate [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_GeomEffects_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set GlazingIntactIn: Glazing intact (broken glass) flag {1=true, else=false} [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_GlazingIntactIn_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set HCE_FieldFrac: Fraction of the field occupied by this HCE type  [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_HCE_FieldFrac_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set HDR_rough: Header pipe roughness [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_HDR_rough_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set IAM_matrix: IAM coefficients, matrix for 4 collectors [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_IAM_matrix_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set I_bn_des: Solar irradiation at design [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_I_bn_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set L_SCA: Length of the SCA  [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_L_SCA_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set L_aperture: Length of a single mirror/HCE unit [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_L_aperture_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set L_rnr_per_xpan: Threshold length of straight runner pipe without an expansion loop [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_L_rnr_per_xpan_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set L_xpan_hdr: Compined perpendicular lengths of each header expansion loop [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_L_xpan_hdr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set L_xpan_rnr: Compined perpendicular lengths of each runner expansion loop [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_L_xpan_rnr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Min_rnr_xpans: Minimum number of expansion loops per single-diameter runner section [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_Min_rnr_xpans_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set N_hdr_per_xpan: Number of collector loops per expansion loop [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_N_hdr_per_xpan_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set N_max_hdr_diams: Maximum number of diameters in each of the hot and cold headers [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_N_max_hdr_diams_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set P_a: Annulus gas pressure [torr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_P_a_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set Pipe_hl_coef: Loss coefficient from the header, runner pipe, and non-HCE piping [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_Pipe_hl_coef_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Rho_mirror_clean: User-defined clean mirror reflectivity [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_Rho_mirror_clean_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set Rough: Relative roughness of the internal HCE surface  [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_Rough_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set Row_Distance: Spacing between rows (centerline to centerline) [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_Row_Distance_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set SCA_drives_elec: Tracking power, in Watts per SCA drive [W/m2-K]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_SCA_drives_elec_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Shadowing: Receiver bellows shadowing loss factor [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_Shadowing_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set T_fp: Freeze protection temperature (heat trace activation temperature) [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_T_fp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_loop_in_des: Design loop inlet temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_T_loop_in_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_loop_out: Target loop outlet temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_T_loop_out_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_shutdown: Temperature when solar field begins recirculating [C]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_T_shutdown_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_startup: Required temperature of the system before the power block can be switched on [C]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_T_startup_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Tau_envelope: Envelope transmittance [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_Tau_envelope_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set TrackingError: User-defined tracking error derate [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_TrackingError_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set V_hdr_cold_max: Maximum HTF velocity in the cold headers at design [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_V_hdr_cold_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set V_hdr_cold_min: Minimum HTF velocity in the cold headers at design [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_V_hdr_cold_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set V_hdr_hot_max: Maximum HTF velocity in the hot headers at design [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_V_hdr_hot_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set V_hdr_hot_min: Minimum HTF velocity in the hot headers at design [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_V_hdr_hot_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set W_aperture: The collector aperture width (Total structural area used for shadowing) [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_W_aperture_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set accept_init: In acceptance testing mode - require steady-state startup [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_accept_init_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set accept_loc: In acceptance testing mode - temperature sensor location [1/2]
	 * options: hx/loop
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_accept_loc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set accept_mode: Acceptance testing mode? [0/1]
	 * options: no/yes
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_accept_mode_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set alpha_abs: Absorber absorptance  [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_alpha_abs_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set alpha_env: Envelope absorptance  [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_alpha_env_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set azimuth: Azimuth angle of surface/axis [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_azimuth_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set calc_design_pipe_vals: Calculate temps and pressures at design conditions for runners and headers [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_calc_design_pipe_vals_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set custom_sf_pipe_sizes: Use custom solar field pipe diams, wallthks, and lengths [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_custom_sf_pipe_sizes_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set epsilon_3_11: Absorber emittance for receiver type 1 variation 1 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_epsilon_3_11_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_12: Absorber emittance for receiver type 1 variation 2 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_epsilon_3_12_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_13: Absorber emittance for receiver type 1 variation 3 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_epsilon_3_13_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_14: Absorber emittance for receiver type 1 variation 4 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_epsilon_3_14_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_21: Absorber emittance for receiver type 2 variation 1 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_epsilon_3_21_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_22: Absorber emittance for receiver type 2 variation 2 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_epsilon_3_22_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_23: Absorber emittance for receiver type 2 variation 3 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_epsilon_3_23_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_24: Absorber emittance for receiver type 2 variation 4 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_epsilon_3_24_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_31: Absorber emittance for receiver type 3 variation 1 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_epsilon_3_31_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_32: Absorber emittance for receiver type 3 variation 2 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_epsilon_3_32_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_33: Absorber emittance for receiver type 3 variation 3 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_epsilon_3_33_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_34: Absorber emittance for receiver type 3 variation 4 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_epsilon_3_34_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_41: Absorber emittance for receiver type 4 variation 1 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_epsilon_3_41_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_42: Absorber emittance for receiver type 4 variation 2 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_epsilon_3_42_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_43: Absorber emittance for receiver type 4 variation 3 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_epsilon_3_43_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_44: Absorber emittance for receiver type 4 variation 4 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_epsilon_3_44_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set eta_pump: HTF pump efficiency [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_eta_pump_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set f_htfmax: Maximum loop mass flow rate fraction of design
	 * options: None
	 * constraints: None
	 * required if: use_abs_or_rel_mdot_limit=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_f_htfmax_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set f_htfmin: Minimum loop mass flow rate fraction of design
	 * options: None
	 * constraints: None
	 * required if: use_abs_or_rel_mdot_limit=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_f_htfmin_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set field_fl_props: User defined field fluid property data [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_field_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set m_dot_htfmax: Maximum loop HTF flow rate [kg/s]
	 * options: None
	 * constraints: None
	 * required if: use_abs_or_rel_mdot_limit=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_m_dot_htfmax_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set m_dot_htfmin: Minimum loop HTF flow rate [kg/s]
	 * options: None
	 * constraints: None
	 * required if: use_abs_or_rel_mdot_limit=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_m_dot_htfmin_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mc_bal_cold: Heat capacity of the balance of plant on the cold side [kWht/K-MWt]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_mc_bal_cold_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mc_bal_hot: Heat capacity of the balance of plant on the hot side [kWht/K-MWt]
	 * options: none
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_mc_bal_hot_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mc_bal_sca: Non-HTF heat capacity associated with each SCA - per meter basis [Wht/K-m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_mc_bal_sca_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set nColt: Number of collector types [none]
	 * options: constant=4
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_nColt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set nHCEVar: Number of HCE variants per type [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_nHCEVar_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set nHCEt: Number of HCE types [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_nHCEt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set northsouth_field_sep: North/south separation between subfields. 0 = SCAs are touching [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_northsouth_field_sep_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set offset_xpan_hdr: Location of first header expansion loop. 1 = after first collector loop [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_offset_xpan_hdr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set p_start: Collector startup energy, per SCA [kWhe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_p_start_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_qf_delay: Energy-based receiver startup delay (fraction of rated thermal power) [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_rec_qf_delay_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_su_delay: Fixed startup delay time for the receiver [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_rec_su_delay_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sf_hdr_diams: Custom header diameters [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_sf_hdr_diams_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set sf_hdr_lengths: Custom header lengths [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_sf_hdr_lengths_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set sf_hdr_wallthicks: Custom header wall thicknesses [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_sf_hdr_wallthicks_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set sf_rnr_diams: Custom runner diameters [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_sf_rnr_diams_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set sf_rnr_lengths: Custom runner lengths [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_sf_rnr_lengths_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set sf_rnr_wallthicks: Custom runner wall thicknesses [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_sf_rnr_wallthicks_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set theta_dep: Deploy angle [deg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_theta_dep_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set theta_stow: Stow angle [deg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_theta_stow_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tilt: Tilt angle of surface/axis [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_tilt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set use_abs_or_rel_mdot_limit: Use mass flow abs (0) or relative (1) limits
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_use_abs_or_rel_mdot_limit_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set wind_stow_speed: Trough wind stow speed [m/s]
	 * options: None
	 * constraints: None
	 * required if: ?=50
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SolarField_wind_stow_speed_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// HeatSink parameters
	//

	/**
	 * Set hs_phys_N_sub: Number physical heat sink HX nodes
	 * options: None
	 * constraints: None
	 * required if: hs_type=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_HeatSink_hs_phys_N_sub_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hs_phys_P_steam_hot_des: Steam outlet (and inlet) pressure for physical heat sink [bar]
	 * options: None
	 * constraints: None
	 * required if: hs_type=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_HeatSink_hs_phys_P_steam_hot_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hs_phys_Q_steam_hot_des: Steam outlet quality for physical heat sink
	 * options: None
	 * constraints: None
	 * required if: hs_type=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_HeatSink_hs_phys_Q_steam_hot_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hs_phys_T_steam_cold_des: Steam inlet temperature for physical heat sink [C]
	 * options: None
	 * constraints: None
	 * required if: hs_type=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_HeatSink_hs_phys_T_steam_cold_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hs_phys_f_mdot_steam_max: Max steam mdot fraction for physical heat sink
	 * options: None
	 * constraints: None
	 * required if: hs_type=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_HeatSink_hs_phys_f_mdot_steam_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hs_phys_f_mdot_steam_min: Min steam mdot fraction for physical heat sink
	 * options: None
	 * constraints: None
	 * required if: hs_type=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_HeatSink_hs_phys_f_mdot_steam_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hs_phys_tol: Physical heat sink solve tolerance
	 * options: None
	 * constraints: None
	 * required if: hs_type=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_HeatSink_hs_phys_tol_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hs_type: 0: ideal model, 1: physical steam model
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_HeatSink_hs_type_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pb_pump_coef: Pumping power to move 1kg of HTF through PB loop [kW/kg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_HeatSink_pb_pump_coef_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// ParallelHeater parameters
	//

	/**
	 * Set f_q_dot_des_allowable_su: Fraction of design power allowed during startup [-]
	 * options: None
	 * constraints: None
	 * required if: is_parallel_htr=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ParallelHeater_f_q_dot_des_allowable_su_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set f_q_dot_heater_min: Minimum allowable heater output as fraction of design
	 * options: None
	 * constraints: None
	 * required if: is_parallel_htr=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ParallelHeater_f_q_dot_heater_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set heater_efficiency: Heater electric to thermal efficiency [%]
	 * options: None
	 * constraints: None
	 * required if: is_parallel_htr=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ParallelHeater_heater_efficiency_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set heater_mult: Heater multiple relative to design cycle thermal power [-]
	 * options: None
	 * constraints: None
	 * required if: is_parallel_htr=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ParallelHeater_heater_mult_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hrs_startup_at_max_rate: Duration of startup at max startup power [hr]
	 * options: None
	 * constraints: None
	 * required if: is_parallel_htr=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ParallelHeater_hrs_startup_at_max_rate_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// SystemCosts parameters
	//

	/**
	 * Set allow_heater_no_dispatch_opt: Allow heater with no dispatch optimization? SAM UI relies on cmod default
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SystemCosts_allow_heater_no_dispatch_opt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set heater_spec_cost: Heater specific cost [$/kWht]
	 * options: None
	 * constraints: None
	 * required if: is_parallel_htr=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SystemCosts_heater_spec_cost_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// TES parameters
	//

	/**
	 * Set cold_tank_Thtr: Minimum allowable cold tank HTF temp [C]
	 * options: None
	 * constraints: None
	 * required if: tes_type=1|tes_type=3
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_cold_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cold_tank_max_heat: Rated heater capacity for cold tank heating [MWe]
	 * options: None
	 * constraints: None
	 * required if: tes_type=1|tes_type=3
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_cold_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set d_tank_in: Tank diameter input [m]
	 * options: None
	 * constraints: None
	 * required if: is_h_tank_fixed=0|is_h_tank_fixed=2
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_d_tank_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dt_hot: Hot side HX approach temp [C]
	 * options: None
	 * constraints: None
	 * required if: tes_type=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_dt_hot_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set h_tank_in: Total height of tank input (height of HTF when tank is full [m]
	 * options: None
	 * constraints: None
	 * required if: is_h_tank_fixed=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_h_tank_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set h_tank_min: Minimum allowable HTF height in storage tank [m]
	 * options: None
	 * constraints: None
	 * required if: tes_type=1|tes_type=3
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_h_tank_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hot_tank_Thtr: Minimum allowable hot tank HTF temp [C]
	 * options: None
	 * constraints: None
	 * required if: tes_type=1|tes_type=3
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_hot_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hot_tank_max_heat: Rated heater capacity for hot tank heating [MWe]
	 * options: None
	 * constraints: None
	 * required if: tes_type=1|tes_type=3
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_hot_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set init_hot_htf_percent: Initial fraction of avail. vol that is hot [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_init_hot_htf_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_h_tank_fixed: [1] Use fixed height (calculate diameter) [0] Use fixed diameter [2] Use fixed d and h (for packed bed) [-]
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_is_h_tank_fixed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set store_fl_props: User defined storage fluid property data [-]
	 * options: None
	 * constraints: None
	 * required if: tes_type=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_store_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set store_fluid: Material number for storage fluid [-]
	 * options: None
	 * constraints: None
	 * required if: tes_type=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_store_fluid_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tank_pairs: Number of equivalent tank pairs [-]
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_tank_pairs_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_cyl_piston_loss_poly: Polynomial coefficients describing piston heat loss function (f(kg/s)=%)
	 * options: None
	 * constraints: None
	 * required if: tes_type=3
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_tes_cyl_piston_loss_poly_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set tes_cyl_tank_cp: Tank wall cp (used for Piston Cylinder) [kJ/kg-K]
	 * options: None
	 * constraints: None
	 * required if: tes_type=3
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_tes_cyl_tank_cp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_cyl_tank_dens: Tank wall thickness (used for Piston Cylinder) [kg/m3]
	 * options: None
	 * constraints: None
	 * required if: tes_type=3
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_tes_cyl_tank_dens_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_cyl_tank_insul_percent: Percent additional wall mass due to insulation (used for Piston Cylinder) [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_tes_cyl_tank_insul_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_cyl_tank_thick: Tank wall thickness (used for Piston Cylinder) [m]
	 * options: None
	 * constraints: None
	 * required if: tes_type=3
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_tes_cyl_tank_thick_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_n_tsteps: Number of subtimesteps (for NT and packed bed)
	 * options: None
	 * constraints: None
	 * required if: tes_type>1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_tes_n_tsteps_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_pb_T_charge_min: Min charge temp [C]
	 * options: None
	 * constraints: None
	 * required if: tes_type=2
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_tes_pb_T_charge_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_pb_T_cold_delta: Max allowable increase in cold discharge temp [C]
	 * options: None
	 * constraints: None
	 * required if: tes_type=2
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_tes_pb_T_cold_delta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_pb_T_hot_delta: Max allowable decrease in hot discharge temp [C]
	 * options: None
	 * constraints: None
	 * required if: tes_type=2
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_tes_pb_T_hot_delta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_pb_cp_solid: TES particle specific heat [kJ/kg K]
	 * options: None
	 * constraints: None
	 * required if: tes_type=2
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_tes_pb_cp_solid_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_pb_dens_solid: TES packed bed media density [kg/m3]
	 * options: None
	 * constraints: None
	 * required if: tes_type=2
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_tes_pb_dens_solid_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_pb_f_oversize: Packed bed oversize factor
	 * options: None
	 * constraints: None
	 * required if: tes_type=2
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_tes_pb_f_oversize_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_pb_k_eff: TES packed bed effective conductivity [W/m K]
	 * options: None
	 * constraints: None
	 * required if: tes_type=2
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_tes_pb_k_eff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_pb_n_xsteps: Number of spatial segments
	 * options: None
	 * constraints: None
	 * required if: tes_type=2
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_tes_pb_n_xsteps_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_pb_void_frac: TES packed bed void fraction
	 * options: None
	 * constraints: None
	 * required if: tes_type=2
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_tes_pb_void_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_type: Standard two tank (1), Packed Bed (2), Piston Cylinder (3) [-]
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_tes_type_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tshours: Equivalent full-load thermal storage hours [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_tshours_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set u_tank: Loss coefficient from the tank [W/m2-K]
	 * options: None
	 * constraints: None
	 * required if: tes_type=1|tes_type=3
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TES_u_tank_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Controller parameters
	//

	/**
	 * Set T_tank_hot_inlet_min: Minimum hot tank htf inlet temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Controller_T_tank_hot_inlet_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set V_tes_des: Design-point velocity to size the TES pipe diameters [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Controller_V_tes_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set custom_tes_p_loss: TES pipe losses are based on custom lengths and coeffs [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Controller_custom_tes_p_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set custom_tes_pipe_sizes: Use custom TES pipe diams, wallthks, and lengths [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Controller_custom_tes_pipe_sizes_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set has_hot_tank_bypass: Bypass valve connects field outlet to cold tank [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Controller_has_hot_tank_bypass_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set k_tes_loss_coeffs: Minor loss coeffs for the coll, gen, and bypass loops [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Controller_k_tes_loss_coeffs_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set non_solar_field_land_area_multiplier: non_solar_field_land_area_multiplier [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Controller_non_solar_field_land_area_multiplier_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set specified_solar_multiple: specified_solar_multiple [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Controller_specified_solar_multiple_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set specified_total_aperture: specified_total_aperture [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Controller_specified_total_aperture_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tanks_in_parallel: Tanks are in parallel, not in series, with solar field [-]
	 * options: None
	 * constraints: None
	 * required if: tes_type=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Controller_tanks_in_parallel_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_diams: Custom TES diameters [m]
	 * options: None
	 * constraints: None
	 * required if: custom_tes_pipe_sizes=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Controller_tes_diams_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set tes_lengths: Custom TES lengths [m]
	 * options: None
	 * constraints: None
	 * required if: custom_tes_pipe_sizes=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Controller_tes_lengths_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set tes_pump_coef: Pumping power to move 1kg of HTF through tes loop [kW/(kg/s)]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Controller_tes_pump_coef_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_wallthicks: Custom TES wall thicknesses [m]
	 * options: None
	 * constraints: None
	 * required if: custom_tes_pipe_sizes=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Controller_tes_wallthicks_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set trough_loop_control: trough_loop_control [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Controller_trough_loop_control_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set use_solar_mult_or_aperture_area: Use solar multiple or total field aperture area [-]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Controller_use_solar_mult_or_aperture_area_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// SysControl parameters
	//

	/**
	 * Set disp_frequency: Frequency for dispatch optimization calculations [hour]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SysControl_disp_frequency_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_horizon: Time horizon for dispatch optimization [hour]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SysControl_disp_horizon_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_max_iter: Max. no. dispatch optimization iterations [-]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SysControl_disp_max_iter_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_mip_gap: Dispatch optimization solution tolerance [-]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SysControl_disp_mip_gap_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_time_weighting: Dispatch optimization future time discounting factor [-]
	 * options: None
	 * constraints: None
	 * required if: ?=0.999
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SysControl_disp_time_weighting_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_timeout: Max. dispatch optimization solve duration [s]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_SysControl_disp_timeout_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Tou parameters
	//

	/**
	 * Set disp_reporting: Dispatch optimization reporting level [-]
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Tou_disp_reporting_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_spec_bb: Dispatch optimization B&B heuristic [-]
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Tou_disp_spec_bb_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_spec_presolve: Dispatch optimization presolve heuristic [-]
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Tou_disp_spec_presolve_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_spec_scaling: Dispatch optimization scaling heuristic [-]
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Tou_disp_spec_scaling_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_steps_per_hour: Time steps per hour for dispatch optimization calculations [-]
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Tou_disp_steps_per_hour_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factors_ts: Time series electricity price multipliers
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=1&csp_financial_model<5&is_dispatch=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Tou_dispatch_factors_ts_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_sched_weekday: 12x24 PPA pricing Weekday schedule
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&csp_financial_model<5&is_dispatch=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Tou_dispatch_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set dispatch_sched_weekend: 12x24 PPA pricing Weekend schedule
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&csp_financial_model<5&is_dispatch=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Tou_dispatch_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set f_turb_tou_periods: Time series heat sink load fractions
	 * options: None
	 * constraints: None
	 * required if: is_timestep_load_fractions=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Tou_f_turb_tou_periods_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set is_timestep_load_fractions: 0: block dispatch, 1: hourly load fraction, 2: absolute load
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Tou_is_timestep_load_fractions_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_tod_pc_target_also_pc_max: Is the TOD target cycle heat input also the max cycle heat input?
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Tou_is_tod_pc_target_also_pc_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ppa_multiplier_model: PPA multiplier model 0: dispatch factors dispatch_factorX, 1: hourly multipliers dispatch_factors_ts [0/1]
	 * options: 0=diurnal,1=timestep
	 * constraints: INTEGER,MIN=0
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Tou_ppa_multiplier_model_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set timestep_load_abs: Heat sink hourly load (not normalized) [kWt]
	 * options: None
	 * constraints: None
	 * required if: is_timestep_load_fractions=2
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Tou_timestep_load_abs_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set timestep_load_abs_factor: Heat sink hourly load scale factor
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Tou_timestep_load_abs_factor_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set timestep_load_fractions: Heat sink load fraction for each timestep, alternative to block dispatch
	 * options: None
	 * constraints: None
	 * required if: is_timestep_load_fractions=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Tou_timestep_load_fractions_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set weekday_schedule: 12x24 CSP operation Time-of-Use Weekday schedule
	 * options: None
	 * constraints: None
	 * required if: is_timestep_load_fractions=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Tou_weekday_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set weekend_schedule: 12x24 CSP operation Time-of-Use Weekend schedule
	 * options: None
	 * constraints: None
	 * required if: is_timestep_load_fractions=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Tou_weekend_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);


	//
	// FinancialModel parameters
	//

	/**
	 * Set csp_financial_model:  [1-8]
	 * options: None
	 * constraints: INTEGER,MIN=0
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_FinancialModel_csp_financial_model_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// FinancialSolutionMode parameters
	//

	/**
	 * Set ppa_soln_mode: PPA solution mode (0=Specify IRR target, 1=Specify PPA price)
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&csp_financial_model<5&is_dispatch=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_FinancialSolutionMode_ppa_soln_mode_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Revenue parameters
	//

	/**
	 * Set ppa_price_input_heat_btu: PPA prices - yearly [$/MMBtu]
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&csp_financial_model<5&is_dispatch=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Revenue_ppa_price_input_heat_btu_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// TimeOfDeliveryFactors parameters
	//

	/**
	 * Set dispatch_tod_factors: TOD factors for periods 1 through 9
	 * options: We added this array input after SAM 2022.12.21 to replace the functionality of former single value inputs dispatch_factor1 through dispatch_factor9
	 * constraints: None
	 * required if: ppa_multiplier_model=0&csp_financial_model<5&is_dispatch=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_TimeOfDeliveryFactors_dispatch_tod_factors_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// System parameters
	//

	/**
	 * Set aux_array: Auxiliary heater, mult frac and const, linear and quad coeff
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_System_aux_array_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set bop_array: Balance of plant parasitic power fraction, mult frac and const, linear and quad coeff
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_System_bop_array_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set pb_fixed_par: Fraction of rated gross power constantly consumed [MWe/MWcap]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_System_pb_fixed_par_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set washing_frequency: Mirror washing frequency [-/year]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_System_washing_frequency_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set water_usage_per_wash: Water usage per wash [L/m2_aper]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_System_water_usage_per_wash_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Powerblock parameters
	//

	/**
	 * Set L_rnr_pb: Length of runner pipe in power block [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Powerblock_L_rnr_pb_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// CapitalCosts parameters
	//

	/**
	 * Set csp.dtr.cost.bop_per_kwe: Balance of Plant Cost per kWe [$/kWe]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_bop_per_kwe_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set csp.dtr.cost.contingency_percent: Contingency Percent [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_contingency_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set csp.dtr.cost.epc.fixed: Fixed EPC Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_epc_fixed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set csp.dtr.cost.epc.per_acre: EPC Costs per acre [$/acre]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_epc_per_acre_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set csp.dtr.cost.epc.per_watt: EPC Cost Wac [$/Wac]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_epc_per_watt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set csp.dtr.cost.epc.percent: EPC Costs % direct [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_epc_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set csp.dtr.cost.heat_sink.cost_per_kwe: Heat Sink Cost per kWe [$/kWe]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_heat_sink_cost_per_kwe_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set csp.dtr.cost.htf_system.cost_per_m2: HTF System Cost Per m2 [$/m2]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_htf_system_cost_per_m2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set csp.dtr.cost.plm.fixed: Fixed Land Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_plm_fixed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set csp.dtr.cost.plm.per_acre: Land Cost per acre [$/acre]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_plm_per_acre_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set csp.dtr.cost.plm.per_watt: Land Cost Wac [$/Wac]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_plm_per_watt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set csp.dtr.cost.plm.percent: Land Cost % direct [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_plm_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set csp.dtr.cost.sales_tax.percent: Sales Tax Percentage of Direct Cost [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_sales_tax_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set csp.dtr.cost.site_improvements.cost_per_m2: Site Improvement Cost per m2 [$/m2]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_site_improvements_cost_per_m2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set csp.dtr.cost.solar_field.cost_per_m2: Solar Field Cost per m2 [$/m2]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_solar_field_cost_per_m2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set csp.dtr.cost.storage.cost_per_kwht: Storage cost per kWht [$/kWht]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_storage_cost_per_kwht_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sales_tax_rate: Sales Tax Rate [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_CapitalCosts_sales_tax_rate_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// FinancialParameters parameters
	//

	/**
	 * Set const_per_interest_rate1: Interest rate, loan 1 [%]
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_FinancialParameters_const_per_interest_rate1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate2: Interest rate, loan 2 [%]
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_FinancialParameters_const_per_interest_rate2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate3: Interest rate, loan 3 [%]
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_FinancialParameters_const_per_interest_rate3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate4: Interest rate, loan 4 [%]
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_FinancialParameters_const_per_interest_rate4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate5: Interest rate, loan 5 [%]
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_FinancialParameters_const_per_interest_rate5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months1: Months prior to operation, loan 1
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_FinancialParameters_const_per_months1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months2: Months prior to operation, loan 2
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_FinancialParameters_const_per_months2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months3: Months prior to operation, loan 3
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_FinancialParameters_const_per_months3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months4: Months prior to operation, loan 4
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_FinancialParameters_const_per_months4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months5: Months prior to operation, loan 5
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_FinancialParameters_const_per_months5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent1: Percent of total installed cost, loan 1 [%]
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_FinancialParameters_const_per_percent1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent2: Percent of total installed cost, loan 2 [%]
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_FinancialParameters_const_per_percent2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent3: Percent of total installed cost, loan 3 [%]
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_FinancialParameters_const_per_percent3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent4: Percent of total installed cost, loan 4 [%]
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_FinancialParameters_const_per_percent4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent5: Percent of total installed cost, loan 5 [%]
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_FinancialParameters_const_per_percent5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate1: Upfront fee on principal, loan 1 [%]
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_FinancialParameters_const_per_upfront_rate1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate2: Upfront fee on principal, loan 2 [%]
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_FinancialParameters_const_per_upfront_rate2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate3: Upfront fee on principal, loan 3 [%]
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_FinancialParameters_const_per_upfront_rate3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate4: Upfront fee on principal, loan 4 [%]
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_FinancialParameters_const_per_upfront_rate4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate5: Upfront fee on principal, loan 5 [%]
	 * options: None
	 * constraints: None
	 * required if: csp_financial_model=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_FinancialParameters_const_per_upfront_rate5_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// AdjustmentFactors parameters
	//

	/**
	 * Set adjust_constant: Constant loss adjustment [%]
	 * options: 'adjust' and 'constant' separated by _ instead of : after SAM 2022.12.21
	 * constraints: MAX=100
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_AdjustmentFactors_adjust_constant_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set adjust_en_periods: Enable period-based adjustment factors [0/1]
	 * options: 'adjust' and 'en_periods' separated by _ instead of : after SAM 2022.12.21
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_AdjustmentFactors_adjust_en_periods_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set adjust_en_timeindex: Enable lifetime adjustment factors [0/1]
	 * options: 'adjust' and 'en_timeindex' separated by _ instead of : after SAM 2022.12.21
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_AdjustmentFactors_adjust_en_timeindex_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set adjust_periods: Period-based adjustment factors [%]
	 * options: Syntax: n x 3 matrix [ start, end, loss ]; Version upgrade: 'adjust' and 'periods' separated by _ instead of : after SAM 2022.12.21
	 * constraints: COLS=3
	 * required if: adjust_en_periods=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_AdjustmentFactors_adjust_periods_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set adjust_timeindex: Lifetime adjustment factors [%]
	 * options: 'adjust' and 'timeindex' separated by _ instead of : after SAM 2022.12.21
	 * constraints: None
	 * required if: adjust_en_timeindex=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_AdjustmentFactors_adjust_timeindex_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// ElectricityRates parameters
	//

	/**
	 * Set en_electricity_rates: Optionally enable/disable electricity_rate [years]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ElectricityRates_en_electricity_rates_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rate_escalation: Annual electricity rate escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ElectricityRates_rate_escalation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set ur_annual_min_charge: Annual minimum charge [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ElectricityRates_ur_annual_min_charge_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_billing_demand_lookback_percentages: Billing demand lookback percentages by month and consider actual peak demand [%]
	 * options: 12x2
	 * constraints: None
	 * required if: ur_enable_billing_demand=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ElectricityRates_ur_billing_demand_lookback_percentages_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_billing_demand_lookback_period: Billing demand lookback period [mn]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=12
	 * required if: ur_enable_billing_demand=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ElectricityRates_ur_billing_demand_lookback_period_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_billing_demand_minimum: Minimum billing demand [kW]
	 * options: None
	 * constraints: None
	 * required if: ur_enable_billing_demand=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ElectricityRates_ur_billing_demand_minimum_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_dc_billing_demand_periods: Billing demand applicability to a given demand charge time of use period
	 * options: None
	 * constraints: None
	 * required if: ur_enable_billing_demand=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ElectricityRates_ur_dc_billing_demand_periods_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_dc_enable: Enable demand charge [0/1]
	 * options: 0=disable,1=enable
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ElectricityRates_ur_dc_enable_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_dc_flat_mat: Demand rates (flat) table [col 0=month, col 1=tier no, col 2=tier peak (kW), col 3=charge ($/kW)]
	 * options: nx4
	 * constraints: None
	 * required if: ur_dc_enable=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ElectricityRates_ur_dc_flat_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_dc_sched_weekday: Demand charge weekday schedule [Periods defined in ur_dc_tou_mat]
	 * options: 12x24
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ElectricityRates_ur_dc_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_dc_sched_weekend: Demand charge weekend schedule [Periods defined in ur_dc_tou_mat]
	 * options: 12x24
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ElectricityRates_ur_dc_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_dc_tou_mat: Demand rates (TOU) table [col 0=period no, col 1=tier no, col 2=tier peak (kW), col 3=charge ($/kW)]
	 * options: nx4
	 * constraints: None
	 * required if: ur_dc_enable=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ElectricityRates_ur_dc_tou_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_ec_sched_weekday: Energy charge weekday schedule [Periods defined in ur_ec_tou_mat]
	 * options: 12x24
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ElectricityRates_ur_ec_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_ec_sched_weekend: Energy charge weekend schedule [Periods defined in ur_ec_tou_mat]
	 * options: 12x24
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ElectricityRates_ur_ec_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_ec_tou_mat: Energy rates table [col 0=period no, col 1=tier no, col 2=max usage, col 3=max usage units (0=kWh, 1=kWh/kW, 2=kWh daily, 3=kWh/kW daily), col 4=buy rate ($/kWh), col 5=sell rate ($/kWh)]
	 * options: nx6
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ElectricityRates_ur_ec_tou_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_en_ts_buy_rate: Enable time step buy rates [0/1]
	 * options: 0=disable,1=enable
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ElectricityRates_ur_en_ts_buy_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_en_ts_sell_rate: Enable time step sell rates [0/1]
	 * options: 0=disable,1=enable
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ElectricityRates_ur_en_ts_sell_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_enable_billing_demand: Enable billing demand ratchets [0/1]
	 * options: 0=disable,1=enable
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ElectricityRates_ur_enable_billing_demand_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_metering_option: Metering options [0=net energy metering,1=net energy metering with $ credits,2=net billing,3=net billing with carryover to next month,4=buy all - sell all]
	 * options: Net metering monthly excess
	 * constraints: INTEGER,MIN=0,MAX=4
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ElectricityRates_ur_metering_option_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_monthly_fixed_charge: Monthly fixed charge [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ElectricityRates_ur_monthly_fixed_charge_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_monthly_min_charge: Monthly minimum charge [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ElectricityRates_ur_monthly_min_charge_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_nb_apply_credit_current_month: Apply earned credits to balance before rolling over excess         [0/1]
	 * options: 0=disable,1=enable
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ElectricityRates_ur_nb_apply_credit_current_month_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_nb_credit_expire: Credit is lost upon end of year         [0/1]
	 * options: 0=disable,1=enable
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ElectricityRates_ur_nb_credit_expire_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_nm_credit_month: Month of year end payout (true-up) [mn]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=11
	 * required if: ?=11
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ElectricityRates_ur_nm_credit_month_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_nm_credit_rollover: Apply net metering true-up credits to future bills [0/1]
	 * options: 0=disable,1=enable
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ElectricityRates_ur_nm_credit_rollover_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_nm_yearend_sell_rate: Net metering true-up credit sell rate [$/kWh]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ElectricityRates_ur_nm_yearend_sell_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_sell_eq_buy: Set sell rate equal to buy rate [0/1]
	 * options: Optional override
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ElectricityRates_ur_sell_eq_buy_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_ts_buy_rate: Time step buy rates [$/kWh]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ElectricityRates_ur_ts_buy_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set ur_ts_sell_rate: Time step sell rates [$/kWh]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ElectricityRates_ur_ts_sell_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set ur_yearzero_usage_peaks: Peak usage by month for year zero [kW]
	 * options: 12
	 * constraints: None
	 * required if: ur_enable_billing_demand=1
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_ElectricityRates_ur_yearzero_usage_peaks_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// Lifetime parameters
	//

	/**
	 * Set inflation_rate: Inflation rate [%]
	 * options: None
	 * constraints: MIN=-99
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalIph_Lifetime_inflation_rate_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * SystemControl Getters
	 */

	SAM_EXPORT double SAM_TroughPhysicalIph_SystemControl_is_dispatch_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SystemControl_is_parallel_htr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SystemControl_sim_type_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Weather Getters
	 */

	SAM_EXPORT const char* SAM_TroughPhysicalIph_Weather_file_name_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT SAM_table SAM_TroughPhysicalIph_Weather_solar_resource_data_tget(SAM_table ptr, SAM_error *err);


	/**
	 * SystemDesign Getters
	 */

	SAM_EXPORT double SAM_TroughPhysicalIph_SystemDesign_q_pb_design_nget(SAM_table ptr, SAM_error *err);


	/**
	 * SolarField Getters
	 */

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_A_aperture_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_AbsorberMaterial_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_AnnulusGas_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_Ave_Focal_Length_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_ColperSCA_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_D_2_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_D_3_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_D_4_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_D_5_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_D_p_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_Design_loss_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_Dirt_HCE_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_Dirt_mirror_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_Distance_SCA_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_EPSILON_4_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_EPSILON_5_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_Error_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_FieldConfig_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_Flow_type_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_Fluid_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_GeomEffects_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_GlazingIntactIn_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_HCE_FieldFrac_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_HDR_rough_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_IAM_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_I_bn_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_L_SCA_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_L_aperture_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_L_rnr_per_xpan_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_L_xpan_hdr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_L_xpan_rnr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_Min_rnr_xpans_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_N_hdr_per_xpan_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_N_max_hdr_diams_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_P_a_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_Pipe_hl_coef_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_Rho_mirror_clean_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_Rough_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_Row_Distance_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_SCA_drives_elec_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_Shadowing_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_T_fp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_T_loop_in_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_T_loop_out_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_T_shutdown_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_T_startup_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_Tau_envelope_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_TrackingError_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_V_hdr_cold_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_V_hdr_cold_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_V_hdr_hot_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_V_hdr_hot_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_W_aperture_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_accept_init_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_accept_loc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_accept_mode_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_alpha_abs_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_alpha_env_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_azimuth_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_calc_design_pipe_vals_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_custom_sf_pipe_sizes_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_epsilon_3_11_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_epsilon_3_12_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_epsilon_3_13_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_epsilon_3_14_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_epsilon_3_21_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_epsilon_3_22_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_epsilon_3_23_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_epsilon_3_24_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_epsilon_3_31_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_epsilon_3_32_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_epsilon_3_33_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_epsilon_3_34_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_epsilon_3_41_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_epsilon_3_42_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_epsilon_3_43_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_epsilon_3_44_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_eta_pump_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_f_htfmax_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_f_htfmin_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_field_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_m_dot_htfmax_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_m_dot_htfmin_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_mc_bal_cold_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_mc_bal_hot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_mc_bal_sca_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_nColt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_nHCEVar_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_nHCEt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_northsouth_field_sep_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_offset_xpan_hdr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_p_start_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_rec_qf_delay_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_rec_su_delay_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_sf_hdr_diams_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_sf_hdr_lengths_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_sf_hdr_wallthicks_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_sf_rnr_diams_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_sf_rnr_lengths_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_SolarField_sf_rnr_wallthicks_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_theta_dep_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_theta_stow_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_tilt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_use_abs_or_rel_mdot_limit_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SolarField_wind_stow_speed_nget(SAM_table ptr, SAM_error *err);


	/**
	 * HeatSink Getters
	 */

	SAM_EXPORT double SAM_TroughPhysicalIph_HeatSink_hs_phys_N_sub_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_HeatSink_hs_phys_P_steam_hot_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_HeatSink_hs_phys_Q_steam_hot_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_HeatSink_hs_phys_T_steam_cold_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_HeatSink_hs_phys_f_mdot_steam_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_HeatSink_hs_phys_f_mdot_steam_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_HeatSink_hs_phys_tol_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_HeatSink_hs_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_HeatSink_pb_pump_coef_nget(SAM_table ptr, SAM_error *err);


	/**
	 * ParallelHeater Getters
	 */

	SAM_EXPORT double SAM_TroughPhysicalIph_ParallelHeater_f_q_dot_des_allowable_su_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_ParallelHeater_f_q_dot_heater_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_ParallelHeater_heater_efficiency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_ParallelHeater_heater_mult_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_ParallelHeater_hrs_startup_at_max_rate_nget(SAM_table ptr, SAM_error *err);


	/**
	 * SystemCosts Getters
	 */

	SAM_EXPORT double SAM_TroughPhysicalIph_SystemCosts_allow_heater_no_dispatch_opt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SystemCosts_heater_spec_cost_nget(SAM_table ptr, SAM_error *err);


	/**
	 * TES Getters
	 */

	SAM_EXPORT double SAM_TroughPhysicalIph_TES_cold_tank_Thtr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_TES_cold_tank_max_heat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_TES_d_tank_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_TES_dt_hot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_TES_h_tank_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_TES_h_tank_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_TES_hot_tank_Thtr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_TES_hot_tank_max_heat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_TES_init_hot_htf_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_TES_is_h_tank_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_TES_store_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_TES_store_fluid_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_TES_tank_pairs_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_TES_tes_cyl_piston_loss_poly_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_TES_tes_cyl_tank_cp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_TES_tes_cyl_tank_dens_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_TES_tes_cyl_tank_insul_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_TES_tes_cyl_tank_thick_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_TES_tes_n_tsteps_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_TES_tes_pb_T_charge_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_TES_tes_pb_T_cold_delta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_TES_tes_pb_T_hot_delta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_TES_tes_pb_cp_solid_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_TES_tes_pb_dens_solid_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_TES_tes_pb_f_oversize_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_TES_tes_pb_k_eff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_TES_tes_pb_n_xsteps_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_TES_tes_pb_void_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_TES_tes_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_TES_tshours_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_TES_u_tank_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Controller Getters
	 */

	SAM_EXPORT double SAM_TroughPhysicalIph_Controller_T_tank_hot_inlet_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Controller_V_tes_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Controller_custom_tes_p_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Controller_custom_tes_pipe_sizes_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Controller_has_hot_tank_bypass_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Controller_k_tes_loss_coeffs_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Controller_non_solar_field_land_area_multiplier_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Controller_specified_solar_multiple_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Controller_specified_total_aperture_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Controller_tanks_in_parallel_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Controller_tes_diams_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Controller_tes_lengths_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Controller_tes_pump_coef_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Controller_tes_wallthicks_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Controller_trough_loop_control_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Controller_use_solar_mult_or_aperture_area_nget(SAM_table ptr, SAM_error *err);


	/**
	 * SysControl Getters
	 */

	SAM_EXPORT double SAM_TroughPhysicalIph_SysControl_disp_frequency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SysControl_disp_horizon_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SysControl_disp_max_iter_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SysControl_disp_mip_gap_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SysControl_disp_time_weighting_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_SysControl_disp_timeout_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Tou Getters
	 */

	SAM_EXPORT double SAM_TroughPhysicalIph_Tou_disp_reporting_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Tou_disp_spec_bb_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Tou_disp_spec_presolve_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Tou_disp_spec_scaling_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Tou_disp_steps_per_hour_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Tou_dispatch_factors_ts_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Tou_dispatch_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Tou_dispatch_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Tou_f_turb_tou_periods_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Tou_is_timestep_load_fractions_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Tou_is_tod_pc_target_also_pc_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Tou_ppa_multiplier_model_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Tou_timestep_load_abs_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Tou_timestep_load_abs_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Tou_timestep_load_fractions_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Tou_weekday_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Tou_weekend_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * FinancialModel Getters
	 */

	SAM_EXPORT double SAM_TroughPhysicalIph_FinancialModel_csp_financial_model_nget(SAM_table ptr, SAM_error *err);


	/**
	 * FinancialSolutionMode Getters
	 */

	SAM_EXPORT double SAM_TroughPhysicalIph_FinancialSolutionMode_ppa_soln_mode_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Revenue Getters
	 */

	SAM_EXPORT double* SAM_TroughPhysicalIph_Revenue_ppa_price_input_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * TimeOfDeliveryFactors Getters
	 */

	SAM_EXPORT double* SAM_TroughPhysicalIph_TimeOfDeliveryFactors_dispatch_tod_factors_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * System Getters
	 */

	SAM_EXPORT double* SAM_TroughPhysicalIph_System_aux_array_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_System_bop_array_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_System_pb_fixed_par_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_System_washing_frequency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_System_water_usage_per_wash_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Powerblock Getters
	 */

	SAM_EXPORT double SAM_TroughPhysicalIph_Powerblock_L_rnr_pb_nget(SAM_table ptr, SAM_error *err);


	/**
	 * CapitalCosts Getters
	 */

	SAM_EXPORT double SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_bop_per_kwe_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_contingency_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_epc_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_epc_per_acre_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_epc_per_watt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_epc_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_heat_sink_cost_per_kwe_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_htf_system_cost_per_m2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_plm_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_plm_per_acre_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_plm_per_watt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_plm_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_sales_tax_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_site_improvements_cost_per_m2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_solar_field_cost_per_m2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_CapitalCosts_csp_dtr_cost_storage_cost_per_kwht_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_CapitalCosts_sales_tax_rate_nget(SAM_table ptr, SAM_error *err);


	/**
	 * FinancialParameters Getters
	 */

	SAM_EXPORT double SAM_TroughPhysicalIph_FinancialParameters_const_per_interest_rate1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_FinancialParameters_const_per_interest_rate2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_FinancialParameters_const_per_interest_rate3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_FinancialParameters_const_per_interest_rate4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_FinancialParameters_const_per_interest_rate5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_FinancialParameters_const_per_months1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_FinancialParameters_const_per_months2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_FinancialParameters_const_per_months3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_FinancialParameters_const_per_months4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_FinancialParameters_const_per_months5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_FinancialParameters_const_per_percent1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_FinancialParameters_const_per_percent2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_FinancialParameters_const_per_percent3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_FinancialParameters_const_per_percent4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_FinancialParameters_const_per_percent5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_FinancialParameters_const_per_upfront_rate1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_FinancialParameters_const_per_upfront_rate2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_FinancialParameters_const_per_upfront_rate3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_FinancialParameters_const_per_upfront_rate4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_FinancialParameters_const_per_upfront_rate5_nget(SAM_table ptr, SAM_error *err);


	/**
	 * AdjustmentFactors Getters
	 */

	SAM_EXPORT double SAM_TroughPhysicalIph_AdjustmentFactors_adjust_constant_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_AdjustmentFactors_adjust_en_periods_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_AdjustmentFactors_adjust_en_timeindex_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_AdjustmentFactors_adjust_periods_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_AdjustmentFactors_adjust_timeindex_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * ElectricityRates Getters
	 */

	SAM_EXPORT double SAM_TroughPhysicalIph_ElectricityRates_en_electricity_rates_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_ElectricityRates_rate_escalation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_ElectricityRates_ur_annual_min_charge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_ElectricityRates_ur_billing_demand_lookback_percentages_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_ElectricityRates_ur_billing_demand_lookback_period_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_ElectricityRates_ur_billing_demand_minimum_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_ElectricityRates_ur_dc_billing_demand_periods_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_ElectricityRates_ur_dc_enable_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_ElectricityRates_ur_dc_flat_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_ElectricityRates_ur_dc_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_ElectricityRates_ur_dc_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_ElectricityRates_ur_dc_tou_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_ElectricityRates_ur_ec_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_ElectricityRates_ur_ec_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_ElectricityRates_ur_ec_tou_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_ElectricityRates_ur_en_ts_buy_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_ElectricityRates_ur_en_ts_sell_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_ElectricityRates_ur_enable_billing_demand_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_ElectricityRates_ur_metering_option_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_ElectricityRates_ur_monthly_fixed_charge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_ElectricityRates_ur_monthly_min_charge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_ElectricityRates_ur_nb_apply_credit_current_month_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_ElectricityRates_ur_nb_credit_expire_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_ElectricityRates_ur_nm_credit_month_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_ElectricityRates_ur_nm_credit_rollover_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_ElectricityRates_ur_nm_yearend_sell_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_ElectricityRates_ur_sell_eq_buy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_ElectricityRates_ur_ts_buy_rate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_ElectricityRates_ur_ts_sell_rate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_ElectricityRates_ur_yearzero_usage_peaks_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * Lifetime Getters
	 */

	SAM_EXPORT double SAM_TroughPhysicalIph_Lifetime_inflation_rate_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_CosTh_ave_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_D_cpnt_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_E_heater_su_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_EndLoss_ave_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_EqOpteff_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_IAM_ave_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_K_cpnt_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_L_cpnt_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_P_fixed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_P_plant_balance_tot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_RowShadow_ave_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_SCADefocusArray_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_SCAInfoArray_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_SCAs_def_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_T_field_cold_in_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_T_field_hot_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_T_grad_0_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_T_grad_1_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_T_grad_2_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_T_grad_3_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_T_grad_4_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_T_grad_5_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_T_grad_6_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_T_grad_7_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_T_grad_8_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_T_grad_9_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_T_heat_sink_in_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_T_heat_sink_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_T_htf_heater_in_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_T_htf_heater_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_T_rec_cold_in_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_T_rec_hot_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_T_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_T_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_Theta_ave_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_Type_cpnt_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_V_tank_hot_ini_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_W_dot_field_pump_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_W_dot_heater_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_W_dot_heater_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_W_dot_par_tot_haf_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_W_dot_parasitic_tot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_W_dot_pc_pump_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_W_dot_pump_SS_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_W_dot_sca_track_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_annual_electricity_consumption_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_annual_energy_distribution_time_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_annual_energy_heat_btu_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_annual_field_freeze_protection_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_annual_tes_freeze_protection_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_annual_thermal_consumption_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_annual_total_water_use_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_aux_design_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_avg_suboptimal_rel_mip_gap_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_beam_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_bop_design_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_const_per_interest1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_const_per_interest2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_const_per_interest3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_const_per_interest4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_const_per_interest5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_const_per_interest_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_const_per_percent_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_const_per_principal1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_const_per_principal2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_const_per_principal3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_const_per_principal4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_const_per_principal5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_const_per_principal_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_const_per_total1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_const_per_total2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_const_per_total3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_const_per_total4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_const_per_total5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_construction_financing_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_cp_battery_nameplate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_cp_system_nameplate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_csp_dtr_cost_bop_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_csp_dtr_cost_contingency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_csp_dtr_cost_epc_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_csp_dtr_cost_heat_sink_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_csp_dtr_cost_htf_system_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_csp_dtr_cost_installed_per_capacity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_csp_dtr_cost_plm_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_csp_dtr_cost_sales_tax_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_csp_dtr_cost_site_improvements_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_csp_dtr_cost_solar_field_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_csp_dtr_cost_storage_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_csp_dtr_hce_design_heat_losses_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_csp_dtr_hce_optical_effs_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_csp_dtr_loop_hce_heat_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_csp_dtr_sca_ap_lengths_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_csp_dtr_sca_calc_costh_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_csp_dtr_sca_calc_end_gains_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_csp_dtr_sca_calc_end_losses_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_csp_dtr_sca_calc_iams_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_csp_dtr_sca_calc_latitude_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_csp_dtr_sca_calc_sca_effs_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_csp_dtr_sca_calc_theta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_csp_dtr_sca_calc_zenith_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_csp_pt_tes_htf_density_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_csp_pt_tes_tank_diameter_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_csp_pt_tes_tank_height_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_dP_sf_SS_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_defocus_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_deltaP_field_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_direct_subtotal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_disp_obj_relax_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_disp_objective_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_disp_presolve_nconstr_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_disp_presolve_nvar_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_disp_qsf_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_disp_qsfprod_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_disp_qsfsu_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_disp_rel_mip_gap_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_disp_solve_iter_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_disp_solve_state_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_disp_solve_time_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_disp_subopt_flag_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_disp_tes_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_disp_thermeff_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_dni_costh_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_e_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_e_dot_field_int_energy_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_elec_price_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_f_htfmax_actual_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_f_htfmin_actual_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_field_htf_cp_avg_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_field_htf_max_temp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_field_htf_min_temp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_field_thermal_output_actual_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_field_thermal_output_ideal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_fixed_land_area_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_gen_heat_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_gen_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_heat_load_capacity_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_hour_day_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_is_PAR_HTR_allowed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_is_hx_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_is_pc_sb_allowed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_is_pc_su_allowed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_is_rec_su_allowed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_loop_optical_efficiency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_m_dot_balance_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_m_dot_cold_tank_to_hot_tank_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_m_dot_cr_to_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_m_dot_cycle_to_field_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_m_dot_field_delivered_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_m_dot_field_recirc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_m_dot_field_to_cycle_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_m_dot_htf_heat_sink_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_m_dot_htf_heater_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_m_dot_htfmax_actual_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_m_dot_htfmin_actual_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_m_dot_loop_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_m_dot_pc_to_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_m_dot_tes_cold_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_m_dot_tes_hot_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_mass_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_mass_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_max_field_flow_velocity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_max_loop_flow_vel_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_min_field_flow_velocity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_min_inner_diameter_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_min_loop_flow_vel_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_month_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_monthly_energy_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_monthly_energy_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_nLoops_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_nSCA_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_n_op_modes_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_nameplate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_op_mode_1_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_op_mode_2_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_op_mode_3_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_operating_modes_a_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_operating_modes_b_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_operating_modes_c_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_pipe_header_P_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_pipe_header_T_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_pipe_header_diams_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_pipe_header_expansions_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_pipe_header_lengths_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_pipe_header_mdot_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_pipe_header_vel_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_pipe_header_wallthk_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_pipe_loop_P_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_pipe_loop_T_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_pipe_runner_P_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_pipe_runner_T_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_pipe_runner_diams_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_pipe_runner_expansions_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_pipe_runner_lengths_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_pipe_runner_mdot_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_pipe_runner_vel_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_pipe_runner_wallthk_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_pipe_tes_P_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_pipe_tes_T_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_pipe_tes_diams_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_pipe_tes_lengths_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_pipe_tes_mdot_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_pipe_tes_vel_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_pipe_tes_wallthk_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_ppa_price_input_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_pres_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_pricing_mult_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_q_balance_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_q_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_q_dc_tes_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_q_dot_elec_to_PAR_HTR_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_q_dot_est_cr_on_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_q_dot_est_cr_su_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_q_dot_est_tes_ch_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_q_dot_est_tes_dc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_q_dot_freeze_prot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_q_dot_heater_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_q_dot_heater_startup_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_q_dot_heater_to_htf_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_q_dot_htf_sf_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_q_dot_pc_max_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_q_dot_pc_min_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_q_dot_pc_sb_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_q_dot_pc_target_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_q_dot_piping_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_q_dot_rec_abs_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_q_dot_rec_inc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_q_dot_rec_thermal_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_q_dot_tes_est_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_q_dot_to_heat_sink_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_q_inc_sf_tot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_q_tes_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_q_tes_heater_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_qinc_costh_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_recirculating_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_required_number_of_loops_for_SM1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_rh_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_sim_duration_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_single_loop_aperture_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_solar_mult_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_solazi_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_solzen_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_system_capacity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_tank_losses_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_tdry_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_tes_SA_cold_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_tes_SA_hot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_tes_SA_tot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_tes_avail_vol_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_tes_cold_vol_frac_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_tes_error_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_tes_error_corrected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_tes_error_percent_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_tes_htf_avg_temp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_tes_htf_max_temp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_tes_htf_min_temp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_tes_htf_pump_power_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_tes_leak_error_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_tes_mass_tot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_tes_piston_frac_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_tes_piston_loc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_tes_wall_error_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_thermal_load_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_time_hr_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_timestep_load_abs_calc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_timestep_load_fractions_calc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_total_aperture_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_total_direct_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_total_indirect_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_total_installed_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_total_land_area_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_total_loop_conversion_efficiency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_total_required_aperture_for_SM1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_total_tracking_power_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_tou_value_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_tshours_field_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_tshours_heater_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_twet_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_vel_loop_max_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_vel_loop_min_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_vol_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysicalIph_Outputs_vol_tank_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_vol_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_vol_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_vol_tes_tot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysicalIph_Outputs_wspd_aget(SAM_table ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
#ifndef SAM_TROUGHPHYSICAL_H_
#define SAM_TROUGHPHYSICAL_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// TroughPhysical Technology Model
	//

	/** 
	 * Create a TroughPhysical variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_TroughPhysical;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_TroughPhysical_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// Weather parameters
	//

	/**
	 * Set file_name: Local weather file with path [none]
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_Weather_file_name_sset(SAM_table ptr, const char* str, SAM_error *err);


	//
	// SolarField parameters
	//

	/**
	 * Set A_aperture: Reflective aperture area of the collector [m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_A_aperture_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set AbsorberMaterial: Absorber material type [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_AbsorberMaterial_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set AnnulusGas: Annulus gas type (1=air, 26=Ar, 27=H2) [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_AnnulusGas_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set Ave_Focal_Length: Average focal length of the collector  [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_Ave_Focal_Length_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set ColperSCA: Number of individual collector sections in an SCA  [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_ColperSCA_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set D_2: Inner absorber tube diameter [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_D_2_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set D_3: Outer absorber tube diameter [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_D_3_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set D_4: Inner glass envelope diameter  [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_D_4_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set D_5: Outer glass envelope diameter  [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_D_5_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set D_cpnt: Interconnect component diameters, row=intc, col=cpnt [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_D_cpnt_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set D_p: Diameter of the absorber flow plug (optional)  [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_D_p_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set Design_loss: Receiver heat loss at design [W/m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_Design_loss_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set Dirt_HCE: Loss due to dirt on the receiver envelope [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_Dirt_HCE_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set Dirt_mirror: User-defined dirt on mirror derate [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_Dirt_mirror_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set Distance_SCA: Piping distance between SCA's in the field [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_Distance_SCA_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set EPSILON_4: Inner glass envelope emissivities (Pyrex)  [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_EPSILON_4_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set EPSILON_5: Outer glass envelope emissivities (Pyrex)  [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_EPSILON_5_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set Error: User-defined general optical error derate  [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_Error_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set FieldConfig: Number of subfield headers [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_FieldConfig_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Flow_type: Flow type through the absorber [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_Flow_type_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set Fluid: Field HTF fluid ID number [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_Fluid_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set GeomEffects: User-defined geometry effects derate [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_GeomEffects_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set GlazingIntactIn: Glazing intact (broken glass) flag {1=true, else=false} [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_GlazingIntactIn_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set HCE_FieldFrac: Fraction of the field occupied by this HCE type  [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_HCE_FieldFrac_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set HDR_rough: Header pipe roughness [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_HDR_rough_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set IAM_matrix: IAM coefficients, matrix for 4 collectors [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_IAM_matrix_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set I_bn_des: Solar irradiation at design [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_I_bn_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set K_cpnt: Interconnect component minor loss coefficients, row=intc, col=cpnt [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_K_cpnt_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set L_SCA: Length of the SCA  [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_L_SCA_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set L_aperture: Length of a single mirror/HCE unit [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_L_aperture_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set L_cpnt: Interconnect component lengths, row=intc, col=cpnt [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_L_cpnt_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set L_power_block_piping: Length of piping (full mass flow) through heat sink (if applicable) [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_L_power_block_piping_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set L_rnr_per_xpan: Threshold length of straight runner pipe without an expansion loop [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_L_rnr_per_xpan_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set L_xpan_hdr: Compined perpendicular lengths of each header expansion loop [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_L_xpan_hdr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set L_xpan_rnr: Compined perpendicular lengths of each runner expansion loop [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_L_xpan_rnr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Min_rnr_xpans: Minimum number of expansion loops per single-diameter runner section [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_Min_rnr_xpans_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set N_hdr_per_xpan: Number of collector loops per expansion loop [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_N_hdr_per_xpan_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set N_max_hdr_diams: Maximum number of diameters in each of the hot and cold headers [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_N_max_hdr_diams_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set P_a: Annulus gas pressure [torr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_P_a_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set Pipe_hl_coef: Loss coefficient from the header, runner pipe, and non-HCE piping [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_Pipe_hl_coef_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Rho_mirror_clean: User-defined clean mirror reflectivity [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_Rho_mirror_clean_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set Rough: Relative roughness of the internal HCE surface  [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_Rough_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set Row_Distance: Spacing between rows (centerline to centerline) [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_Row_Distance_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set SCADefocusArray: Collector defocus order [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_SCADefocusArray_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set SCAInfoArray: Receiver (,1) and collector (,2) type for each assembly in loop [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_SCAInfoArray_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set SCA_drives_elec: Tracking power, in Watts per SCA drive [W/m2-K]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_SCA_drives_elec_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Shadowing: Receiver bellows shadowing loss factor [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_Shadowing_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set T_fp: Freeze protection temperature (heat trace activation temperature) [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_T_fp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_loop_in_des: Design loop inlet temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_T_loop_in_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_loop_out: Target loop outlet temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_T_loop_out_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Tau_envelope: Envelope transmittance [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_Tau_envelope_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set TrackingError: User-defined tracking error derate [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_TrackingError_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set Type_cpnt: Interconnect component type, row=intc, col=cpnt [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_Type_cpnt_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set V_hdr_cold_max: Maximum HTF velocity in the cold headers at design [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_V_hdr_cold_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set V_hdr_cold_min: Minimum HTF velocity in the cold headers at design [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_V_hdr_cold_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set V_hdr_hot_max: Maximum HTF velocity in the hot headers at design [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_V_hdr_hot_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set V_hdr_hot_min: Minimum HTF velocity in the hot headers at design [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_V_hdr_hot_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set W_aperture: The collector aperture width (Total structural area used for shadowing) [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_W_aperture_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set accept_init: In acceptance testing mode - require steady-state startup [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_accept_init_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set accept_loc: In acceptance testing mode - temperature sensor location [1/2]
	 * options: hx/loop
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_accept_loc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set accept_mode: Acceptance testing mode? [0/1]
	 * options: no/yes
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_accept_mode_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set alpha_abs: Absorber absorptance  [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_alpha_abs_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set alpha_env: Envelope absorptance  [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_alpha_env_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set azimuth: Azimuth angle of surface/axis [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_azimuth_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set calc_design_pipe_vals: Calculate temps and pressures at design conditions for runners and headers [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_calc_design_pipe_vals_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set custom_sf_pipe_sizes: Use custom solar field pipe diams, wallthks, and lengths [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_custom_sf_pipe_sizes_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set epsilon_3_11: Absorber emittance for receiver type 1 variation 1 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_11_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_12: Absorber emittance for receiver type 1 variation 2 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_12_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_13: Absorber emittance for receiver type 1 variation 3 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_13_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_14: Absorber emittance for receiver type 1 variation 4 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_14_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_21: Absorber emittance for receiver type 2 variation 1 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_21_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_22: Absorber emittance for receiver type 2 variation 2 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_22_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_23: Absorber emittance for receiver type 2 variation 3 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_23_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_24: Absorber emittance for receiver type 2 variation 4 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_24_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_31: Absorber emittance for receiver type 3 variation 1 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_31_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_32: Absorber emittance for receiver type 3 variation 2 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_32_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_33: Absorber emittance for receiver type 3 variation 3 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_33_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_34: Absorber emittance for receiver type 3 variation 4 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_34_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_41: Absorber emittance for receiver type 4 variation 1 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_41_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_42: Absorber emittance for receiver type 4 variation 2 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_42_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_43: Absorber emittance for receiver type 4 variation 3 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_43_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_44: Absorber emittance for receiver type 4 variation 4 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_epsilon_3_44_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set eta_pump: HTF pump efficiency [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_eta_pump_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set field_fl_props: User defined field fluid property data [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_field_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set include_fixed_power_block_runner: Should model consider piping through power block? [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_include_fixed_power_block_runner_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set m_dot_htfmax: Maximum loop HTF flow rate [kg/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_m_dot_htfmax_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set m_dot_htfmin: Minimum loop HTF flow rate [kg/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_m_dot_htfmin_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mc_bal_cold: Heat capacity of the balance of plant on the cold side [kWht/K-MWt]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_mc_bal_cold_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mc_bal_hot: Heat capacity of the balance of plant on the hot side [kWht/K-MWt]
	 * options: none
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_mc_bal_hot_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mc_bal_sca: Non-HTF heat capacity associated with each SCA - per meter basis [Wht/K-m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_mc_bal_sca_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set nColt: Number of collector types [none]
	 * options: constant=4
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_nColt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set nHCEVar: Number of HCE variants per type [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_nHCEVar_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set nHCEt: Number of HCE types [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_nHCEt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set nLoops: Number of loops in the field [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_nLoops_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set nSCA: Number of SCAs in a loop [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_nSCA_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set northsouth_field_sep: North/south separation between subfields. 0 = SCAs are touching [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_northsouth_field_sep_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set offset_xpan_hdr: Location of first header expansion loop. 1 = after first collector loop [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_offset_xpan_hdr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set p_start: Collector startup energy, per SCA [kWe-hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_p_start_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_qf_delay: Energy-based receiver startup delay (fraction of rated thermal power) [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_rec_qf_delay_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rec_su_delay: Fixed startup delay time for the receiver [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_rec_su_delay_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sf_hdr_diams: Custom header diameters [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_sf_hdr_diams_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set sf_hdr_lengths: Custom header lengths [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_sf_hdr_lengths_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set sf_hdr_wallthicks: Custom header wall thicknesses [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_sf_hdr_wallthicks_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set sf_rnr_diams: Custom runner diameters [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_sf_rnr_diams_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set sf_rnr_lengths: Custom runner lengths [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_sf_rnr_lengths_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set sf_rnr_wallthicks: Custom runner wall thicknesses [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_sf_rnr_wallthicks_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set solar_mult: Solar multiple [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_solar_mult_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set theta_dep: Deploy angle [deg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_theta_dep_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set theta_stow: Stow angle [deg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_theta_stow_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tilt: Tilt angle of surface/axis [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_tilt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set wind_stow_speed: Trough wind stow speed [m/s]
	 * options: None
	 * constraints: None
	 * required if: ?=50
	 */
	SAM_EXPORT void SAM_TroughPhysical_SolarField_wind_stow_speed_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Powerblock parameters
	//

	/**
	 * Set CT: Flag for using dry cooling or wet cooling system [none]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TroughPhysical_Powerblock_CT_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set F_wc: Fraction indicating wet cooling use for hybrid system [none]
	 * options: constant=[0,0,0,0,0,0,0,0,0]
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TroughPhysical_Powerblock_F_wc_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set L_rnr_pb: Length of runner pipe in power block [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_Powerblock_L_rnr_pb_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set P_boil: Boiler operating pressure [bar]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TroughPhysical_Powerblock_P_boil_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set P_cond_min: Minimum condenser pressure [inHg]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TroughPhysical_Powerblock_P_cond_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set P_cond_ratio: Condenser pressure ratio [none]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TroughPhysical_Powerblock_P_cond_ratio_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set P_ref: Rated plant capacity [MWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_Powerblock_P_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_ITD_des: ITD at design for dry system [C]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TroughPhysical_Powerblock_T_ITD_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_amb_des: Reference ambient temperature at design point [C]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TroughPhysical_Powerblock_T_amb_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_approach: Cooling tower approach temperature [C]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TroughPhysical_Powerblock_T_approach_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cycle_cutoff_frac: Minimum turbine operation fraction before shutdown [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_Powerblock_cycle_cutoff_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cycle_max_frac: Maximum turbine over design operation fraction [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_Powerblock_cycle_max_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dT_cw_ref: Reference condenser cooling water inlet/outlet T diff [C]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TroughPhysical_Powerblock_dT_cw_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set eta_ref: Power cycle efficiency at design [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_Powerblock_eta_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set n_pl_inc: Number of part-load increments for the heat rejection system [none]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TroughPhysical_Powerblock_n_pl_inc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pb_bd_frac: Power block blowdown steam fraction  [none]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TroughPhysical_Powerblock_pb_bd_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pb_pump_coef: Pumping power to move 1kg of HTF through PB loop [kW/kg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_Powerblock_pb_pump_coef_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pc_config: 0: Steam Rankine (224), 1: user defined [-]
	 * options: None
	 * constraints: INTEGER
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysical_Powerblock_pc_config_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set q_sby_frac: Fraction of thermal power required for standby mode [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_Powerblock_q_sby_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set startup_frac: Fraction of design thermal power needed for startup [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_Powerblock_startup_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set startup_time: Time needed for power block startup [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_Powerblock_startup_time_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tech_type: Turbine inlet pressure control flag (sliding=user, fixed=trough) [1/2/3]
	 * options: tower/trough/user
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TroughPhysical_Powerblock_tech_type_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ud_f_W_dot_cool_des: Percent of user-defined power cycle design gross output consumed by cooling [%]
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TroughPhysical_Powerblock_ud_f_W_dot_cool_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ud_ind_od: Off design user-defined power cycle performance as function of T_htf, m_dot_htf [ND], and T_amb
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TroughPhysical_Powerblock_ud_ind_od_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ud_m_dot_water_cool_des: Mass flow rate of water required at user-defined power cycle design point [kg/s]
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TroughPhysical_Powerblock_ud_m_dot_water_cool_des_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// TES parameters
	//

	/**
	 * Set cold_tank_Thtr: Minimum allowable cold tank HTF temp [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_TES_cold_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cold_tank_max_heat: Rated heater capacity for cold tank heating [MWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_TES_cold_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dt_hot: Hot side HX approach temp [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_TES_dt_hot_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set h_tank: Total height of tank (height of HTF when tank is full [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_TES_h_tank_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set h_tank_min: Minimum allowable HTF height in storage tank [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_TES_h_tank_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hot_tank_Thtr: Minimum allowable hot tank HTF temp [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_TES_hot_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hot_tank_max_heat: Rated heater capacity for hot tank heating [MWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_TES_hot_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set init_hot_htf_percent: Initial fraction of avail. vol that is hot [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_TES_init_hot_htf_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_hx: Heat exchanger (HX) exists (1=yes, 0=no) [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_TES_is_hx_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set store_fl_props: User defined storage fluid property data [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_TES_store_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set store_fluid: Material number for storage fluid [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_TES_store_fluid_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tank_pairs: Number of equivalent tank pairs [-]
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_TES_tank_pairs_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tshours: Equivalent full-load thermal storage hours [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_TES_tshours_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set u_tank: Loss coefficient from the tank [W/m2-K]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_TES_u_tank_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Tou parameters
	//

	/**
	 * Set ampl_data_dir: AMPL data file directory [-]
	 * options: None
	 * constraints: None
	 * required if: ?=''
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_ampl_data_dir_sset(SAM_table ptr, const char* str, SAM_error *err);

	/**
	 * Set ampl_exec_call: System command to run AMPL code [-]
	 * options: None
	 * constraints: None
	 * required if: ?='ampl sdk_solution.run'
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_ampl_exec_call_sset(SAM_table ptr, const char* str, SAM_error *err);

	/**
	 * Set disp_csu_cost: Cycle startup cost [$]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_disp_csu_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_frequency: Frequency for dispatch optimization calculations [hour]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_disp_frequency_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_horizon: Time horizon for dispatch optimization [hour]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_disp_horizon_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_max_iter: Max. no. dispatch optimization iterations [-]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_disp_max_iter_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_mip_gap: Dispatch optimization solution tolerance [-]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_disp_mip_gap_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_pen_delta_w: Dispatch cycle production change penalty [$/kWe-change]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_disp_pen_delta_w_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_reporting: Dispatch optimization reporting level [-]
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_disp_reporting_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_rsu_cost: Receiver startup cost [$]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_disp_rsu_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_spec_bb: Dispatch optimization B&B heuristic [-]
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_disp_spec_bb_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_spec_presolve: Dispatch optimization presolve heuristic [-]
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_disp_spec_presolve_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_spec_scaling: Dispatch optimization scaling heuristic [-]
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_disp_spec_scaling_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_steps_per_hour: Time steps per hour for dispatch optimization calculations [-]
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_disp_steps_per_hour_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_time_weighting: Dispatch optimization future time discounting factor [-]
	 * options: None
	 * constraints: None
	 * required if: ?=0.99
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_disp_time_weighting_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_timeout: Max. dispatch optimization solve duration [s]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_disp_timeout_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor1: Dispatch payment factor 1
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_dispatch_factor1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor2: Dispatch payment factor 2
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_dispatch_factor2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor3: Dispatch payment factor 3
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_dispatch_factor3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor4: Dispatch payment factor 4
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_dispatch_factor4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor5: Dispatch payment factor 5
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_dispatch_factor5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor6: Dispatch payment factor 6
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_dispatch_factor6_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor7: Dispatch payment factor 7
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_dispatch_factor7_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor8: Dispatch payment factor 8
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_dispatch_factor8_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor9: Dispatch payment factor 9
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_dispatch_factor9_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factors_ts: Dispatch payment factor array
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=1
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_dispatch_factors_ts_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_sched_weekday: 12x24 PPA pricing Weekday schedule
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_dispatch_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set dispatch_sched_weekend: 12x24 PPA pricing Weekend schedule
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_dispatch_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set dispatch_series: Time series dispatch factors
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_dispatch_series_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set f_turb_tou_periods: Dispatch logic for turbine load fraction [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_f_turb_tou_periods_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set is_ampl_engine: Run dispatch optimization with external AMPL engine [-]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_is_ampl_engine_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_dispatch: Allow dispatch optimization? [-]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_is_dispatch_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_dispatch_series: Use time-series dispatch factors
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_is_dispatch_series_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_tod_pc_target_also_pc_max: Is the TOD target cycle heat input also the max cycle heat input?
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_is_tod_pc_target_also_pc_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_wlim_series: Use time-series net electricity generation limits
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_is_wlim_series_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_write_ampl_dat: Write AMPL data files for dispatch run [-]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_is_write_ampl_dat_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ppa_multiplier_model: PPA multiplier model [0/1]
	 * options: 0=diurnal,1=timestep
	 * constraints: INTEGER,MIN=0
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_ppa_multiplier_model_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set q_rec_heattrace: Receiver heat trace energy consumption during startup [kWe-hr]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_q_rec_heattrace_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set q_rec_standby: Receiver standby energy consumption [kWt]
	 * options: None
	 * constraints: None
	 * required if: ?=9e99
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_q_rec_standby_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set weekday_schedule: 12x24 CSP operation Time-of-Use Weekday schedule [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_weekday_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set weekend_schedule: 12x24 CSP operation Time-of-Use Weekend schedule [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_weekend_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set wlim_series: Time series net electicity generation limits [kWe]
	 * options: None
	 * constraints: None
	 * required if: is_wlim_series=1
	 */
	SAM_EXPORT void SAM_TroughPhysical_Tou_wlim_series_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// System parameters
	//

	/**
	 * Set aux_array: Auxiliary heater, mult frac and const, linear and quad coeff
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_System_aux_array_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set bop_array: Balance of plant parasitic power fraction, mult frac and const, linear and quad coeff
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_System_bop_array_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set gross_net_conversion_factor: Estimated gross to net conversion factor
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_System_gross_net_conversion_factor_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pb_fixed_par: Fraction of rated gross power constantly consumed [MWe/MWcap]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_System_pb_fixed_par_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set washing_frequency: Mirror washing frequency [-/year]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_System_washing_frequency_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set water_usage_per_wash: Water usage per wash [L/m2_aper]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_System_water_usage_per_wash_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Controller parameters
	//

	/**
	 * Set DP_SGS: Pressure drop within the steam generator [bar]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_Controller_DP_SGS_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_tank_hot_inlet_min: Minimum hot tank htf inlet temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_Controller_T_tank_hot_inlet_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set V_tes_des: Design-point velocity to size the TES pipe diameters [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_Controller_V_tes_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set custom_tes_p_loss: TES pipe losses are based on custom lengths and coeffs [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_Controller_custom_tes_p_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set custom_tes_pipe_sizes: Use custom TES pipe diams, wallthks, and lengths [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_Controller_custom_tes_pipe_sizes_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set has_hot_tank_bypass: Bypass valve connects field outlet to cold tank [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_Controller_has_hot_tank_bypass_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set k_tes_loss_coeffs: Minor loss coeffs for the coll, gen, and bypass loops [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_Controller_k_tes_loss_coeffs_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set tanks_in_parallel: Tanks are in parallel, not in series, with solar field [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_Controller_tanks_in_parallel_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_diams: Custom TES diameters [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_Controller_tes_diams_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set tes_lengths: Custom TES lengths [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_Controller_tes_lengths_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set tes_pump_coef: Pumping power to move 1kg of HTF through tes loop [kW/(kg/s)]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_Controller_tes_pump_coef_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_wallthicks: Custom TES wall thicknesses [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TroughPhysical_Controller_tes_wallthicks_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);


	/**
	 * Weather Getters
	 */

	SAM_EXPORT const char* SAM_TroughPhysical_Weather_file_name_sget(SAM_table ptr, SAM_error *err);


	/**
	 * SolarField Getters
	 */

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_A_aperture_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_AbsorberMaterial_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_AnnulusGas_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_Ave_Focal_Length_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_ColperSCA_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_D_2_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_D_3_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_D_4_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_D_5_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_D_cpnt_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_D_p_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_Design_loss_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_Dirt_HCE_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_Dirt_mirror_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_Distance_SCA_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_EPSILON_4_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_EPSILON_5_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_Error_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_FieldConfig_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_Flow_type_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_Fluid_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_GeomEffects_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_GlazingIntactIn_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_HCE_FieldFrac_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_HDR_rough_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_IAM_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_I_bn_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_K_cpnt_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_L_SCA_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_L_aperture_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_L_cpnt_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_L_power_block_piping_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_L_rnr_per_xpan_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_L_xpan_hdr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_L_xpan_rnr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_Min_rnr_xpans_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_N_hdr_per_xpan_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_N_max_hdr_diams_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_P_a_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_Pipe_hl_coef_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_Rho_mirror_clean_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_Rough_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_Row_Distance_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_SCADefocusArray_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_SCAInfoArray_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_SCA_drives_elec_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_Shadowing_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_T_fp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_T_loop_in_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_T_loop_out_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_Tau_envelope_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_TrackingError_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_Type_cpnt_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_V_hdr_cold_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_V_hdr_cold_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_V_hdr_hot_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_V_hdr_hot_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_W_aperture_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_accept_init_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_accept_loc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_accept_mode_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_alpha_abs_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_alpha_env_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_azimuth_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_calc_design_pipe_vals_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_custom_sf_pipe_sizes_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_11_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_12_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_13_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_14_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_21_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_22_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_23_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_24_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_31_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_32_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_33_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_34_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_41_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_42_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_43_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_epsilon_3_44_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_eta_pump_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_field_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_include_fixed_power_block_runner_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_m_dot_htfmax_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_m_dot_htfmin_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_mc_bal_cold_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_mc_bal_hot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_mc_bal_sca_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_nColt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_nHCEVar_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_nHCEt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_nLoops_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_nSCA_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_northsouth_field_sep_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_offset_xpan_hdr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_p_start_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_rec_qf_delay_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_rec_su_delay_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_sf_hdr_diams_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_sf_hdr_lengths_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_sf_hdr_wallthicks_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_sf_rnr_diams_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_sf_rnr_lengths_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_SolarField_sf_rnr_wallthicks_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_solar_mult_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_theta_dep_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_theta_stow_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_tilt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_SolarField_wind_stow_speed_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Powerblock Getters
	 */

	SAM_EXPORT double SAM_TroughPhysical_Powerblock_CT_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Powerblock_F_wc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Powerblock_L_rnr_pb_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Powerblock_P_boil_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Powerblock_P_cond_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Powerblock_P_cond_ratio_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Powerblock_P_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Powerblock_T_ITD_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Powerblock_T_amb_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Powerblock_T_approach_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Powerblock_cycle_cutoff_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Powerblock_cycle_max_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Powerblock_dT_cw_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Powerblock_eta_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Powerblock_n_pl_inc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Powerblock_pb_bd_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Powerblock_pb_pump_coef_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Powerblock_pc_config_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Powerblock_q_sby_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Powerblock_startup_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Powerblock_startup_time_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Powerblock_tech_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Powerblock_ud_f_W_dot_cool_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Powerblock_ud_ind_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Powerblock_ud_m_dot_water_cool_des_nget(SAM_table ptr, SAM_error *err);


	/**
	 * TES Getters
	 */

	SAM_EXPORT double SAM_TroughPhysical_TES_cold_tank_Thtr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_TES_cold_tank_max_heat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_TES_dt_hot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_TES_h_tank_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_TES_h_tank_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_TES_hot_tank_Thtr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_TES_hot_tank_max_heat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_TES_init_hot_htf_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_TES_is_hx_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_TES_store_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_TES_store_fluid_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_TES_tank_pairs_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_TES_tshours_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_TES_u_tank_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Tou Getters
	 */

	SAM_EXPORT const char* SAM_TroughPhysical_Tou_ampl_data_dir_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_TroughPhysical_Tou_ampl_exec_call_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_disp_csu_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_disp_frequency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_disp_horizon_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_disp_max_iter_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_disp_mip_gap_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_disp_pen_delta_w_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_disp_reporting_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_disp_rsu_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_disp_spec_bb_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_disp_spec_presolve_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_disp_spec_scaling_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_disp_steps_per_hour_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_disp_time_weighting_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_disp_timeout_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_dispatch_factor1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_dispatch_factor2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_dispatch_factor3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_dispatch_factor4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_dispatch_factor5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_dispatch_factor6_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_dispatch_factor7_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_dispatch_factor8_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_dispatch_factor9_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Tou_dispatch_factors_ts_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Tou_dispatch_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Tou_dispatch_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Tou_dispatch_series_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Tou_f_turb_tou_periods_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_is_ampl_engine_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_is_dispatch_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_is_dispatch_series_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_is_tod_pc_target_also_pc_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_is_wlim_series_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_is_write_ampl_dat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_ppa_multiplier_model_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_q_rec_heattrace_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Tou_q_rec_standby_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Tou_weekday_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Tou_weekend_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Tou_wlim_series_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * System Getters
	 */

	SAM_EXPORT double* SAM_TroughPhysical_System_aux_array_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_System_bop_array_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_System_gross_net_conversion_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_System_pb_fixed_par_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_System_washing_frequency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_System_water_usage_per_wash_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Controller Getters
	 */

	SAM_EXPORT double SAM_TroughPhysical_Controller_DP_SGS_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Controller_T_tank_hot_inlet_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Controller_V_tes_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Controller_custom_tes_p_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Controller_custom_tes_pipe_sizes_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Controller_has_hot_tank_bypass_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Controller_k_tes_loss_coeffs_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Controller_tanks_in_parallel_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Controller_tes_diams_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Controller_tes_lengths_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Controller_tes_pump_coef_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Controller_tes_wallthicks_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_CosTh_ave_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_EndLoss_ave_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_EqOpteff_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_IAM_ave_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_P_cooling_tower_tot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_P_cycle_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_P_fixed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_P_out_net_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_P_plant_balance_tot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_RowShadow_ave_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_SCAs_def_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_T_field_cold_in_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_T_field_hot_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_T_pc_in_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_T_pc_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_T_rec_cold_in_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_T_rec_hot_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_T_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_T_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_Theta_ave_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_W_dot_field_pump_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_W_dot_sca_track_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Outputs_annual_W_cycle_gross_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Outputs_annual_field_freeze_protection_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Outputs_annual_tes_freeze_protection_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Outputs_annual_thermal_consumption_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Outputs_annual_total_water_use_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_beam_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Outputs_conversion_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_defocus_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_deltaP_field_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_obj_relax_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_objective_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_pceff_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_presolve_nconstr_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_presolve_nvar_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_qpbsu_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_qsf_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_qsfprod_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_qsfsu_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_rev_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_solve_iter_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_solve_state_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_solve_time_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_tes_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_thermeff_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_disp_wpb_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_dni_costh_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_e_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_e_dot_field_int_energy_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_eta_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_hour_day_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_htf_pump_power_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_is_pc_sb_allowed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_is_pc_su_allowed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_is_rec_su_allowed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TroughPhysical_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_m_dot_balance_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_m_dot_field_delivered_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_m_dot_field_recirc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_m_dot_loop_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_m_dot_pc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_m_dot_tes_ch_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_m_dot_tes_dc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_m_dot_water_pc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_month_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_monthly_energy_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_n_op_modes_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_op_mode_1_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_op_mode_2_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_op_mode_3_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_operating_modes_a_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_operating_modes_b_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_operating_modes_c_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_header_P_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_header_T_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_header_diams_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_header_expansions_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_header_lengths_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_header_mdot_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_header_vel_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_header_wallthk_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_loop_P_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_loop_T_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_runner_P_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_runner_T_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_runner_diams_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_runner_expansions_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_runner_lengths_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_runner_mdot_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_runner_vel_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_runner_wallthk_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_tes_P_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_tes_T_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_tes_diams_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_tes_mdot_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_tes_vel_dsn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_pipe_tes_wallthk_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_pres_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_pricing_mult_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_balance_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dc_tes_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dot_est_cr_on_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dot_est_cr_su_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dot_est_tes_ch_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dot_est_tes_dc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dot_freeze_prot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dot_htf_sf_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dot_pc_max_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dot_pc_min_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dot_pc_sb_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dot_pc_startup_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dot_pc_target_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dot_piping_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dot_rec_abs_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dot_rec_inc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_dot_rec_thermal_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_inc_sf_tot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_pb_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_pc_startup_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_q_tes_heater_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_qinc_costh_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_recirculating_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_rh_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_solazi_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_solzen_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_tank_losses_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_tdry_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_time_hr_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_tou_value_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_twet_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TroughPhysical_Outputs_wspd_aget(SAM_table ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
#ifndef SAM_TCSTROUGHPHYSICAL_H_
#define SAM_TCSTROUGHPHYSICAL_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// TcstroughPhysical Technology Model
	//

	/** 
	 * Create a TcstroughPhysical variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_TcstroughPhysical;

	SAM_EXPORT SAM_TcstroughPhysical SAM_TcstroughPhysical_construct(const char* def, SAM_error* err);

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_TcstroughPhysical_execute(SAM_TcstroughPhysical data, int verbosity, SAM_error* err);

	SAM_EXPORT void SAM_TcstroughPhysical_destruct(SAM_TcstroughPhysical system);


	//
	// Weather parameters
	//

	/**
	 * Set azimuth: Azimuth angle of surface/axis [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Weather_azimuth_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set file_name: Local weather file with path [none]
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Weather_file_name_sset(SAM_TcstroughPhysical ptr, const char* str, SAM_error *err);

	/**
	 * Set tilt: Tilt angle of surface/axis [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Weather_tilt_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set track_mode: Tracking mode [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Weather_track_mode_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);


	//
	// Trough parameters
	//

	/**
	 * Set system_capacity: Nameplate capacity [kW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Trough_system_capacity_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);


	//
	// SolarField parameters
	//

	/**
	 * Set A_aperture: Reflective aperture area of the collector [m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_A_aperture_aset(SAM_TcstroughPhysical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set AbsorberMaterial: Absorber material type [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_AbsorberMaterial_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set AnnulusGas: Annulus gas type (1=air, 26=Ar, 27=H2) [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_AnnulusGas_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set Ave_Focal_Length: Average focal length of the collector  [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Ave_Focal_Length_aset(SAM_TcstroughPhysical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set ColperSCA: Number of individual collector sections in an SCA  [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_ColperSCA_aset(SAM_TcstroughPhysical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set D_2: Inner absorber tube diameter [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_D_2_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set D_3: Outer absorber tube diameter [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_D_3_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set D_4: Inner glass envelope diameter  [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_D_4_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set D_5: Outer glass envelope diameter  [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_D_5_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set D_cpnt: Interconnect component diameters, row=intc, col=cpnt [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_D_cpnt_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set D_p: Diameter of the absorber flow plug (optional)  [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_D_p_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set Design_loss: Receiver heat loss at design [W/m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Design_loss_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set Dirt_HCE: Loss due to dirt on the receiver envelope [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Dirt_HCE_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set Dirt_mirror: User-defined dirt on mirror derate [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Dirt_mirror_aset(SAM_TcstroughPhysical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set Distance_SCA: Piping distance between SCA's in the field [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Distance_SCA_aset(SAM_TcstroughPhysical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set EPSILON_4: Inner glass envelope emissivities (Pyrex)  [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_EPSILON_4_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set EPSILON_5: Outer glass envelope emissivities (Pyrex)  [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_EPSILON_5_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set Error: User-defined general optical error derate  [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Error_aset(SAM_TcstroughPhysical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set FieldConfig: Number of subfield headers [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_FieldConfig_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set Flow_type: Flow type through the absorber [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Flow_type_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set Fluid: Field HTF fluid ID number [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Fluid_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set GeomEffects: User-defined geometry effects derate [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_GeomEffects_aset(SAM_TcstroughPhysical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set GlazingIntactIn: Glazing intact (broken glass) flag {1=true, else=false} [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_GlazingIntactIn_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set HCE_FieldFrac: Fraction of the field occupied by this HCE type  [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_HCE_FieldFrac_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set HDR_rough: Header pipe roughness [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_HDR_rough_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set IAM_matrix: IAM coefficients, matrix for 4 collectors [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_IAM_matrix_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set I_bn_des: Solar irradiation at design [W/m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_I_bn_des_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set K_cpnt: Interconnect component minor loss coefficients, row=intc, col=cpnt [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_K_cpnt_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set L_SCA: Length of the SCA  [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_L_SCA_aset(SAM_TcstroughPhysical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set L_aperture: Length of a single mirror/HCE unit [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_L_aperture_aset(SAM_TcstroughPhysical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set L_cpnt: Interconnect component lengths, row=intc, col=cpnt [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_L_cpnt_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set L_rnr_pb: Length of runner pipe in power block [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_L_rnr_pb_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set L_rnr_per_xpan: Threshold length of straight runner pipe without an expansion loop [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_L_rnr_per_xpan_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set L_xpan_hdr: Compined perpendicular lengths of each header expansion loop [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_L_xpan_hdr_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set L_xpan_rnr: Compined perpendicular lengths of each runner expansion loop [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_L_xpan_rnr_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set Min_rnr_xpans: Minimum number of expansion loops per single-diameter runner section [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Min_rnr_xpans_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set N_hdr_per_xpan: Number of collector loops per expansion loop [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_N_hdr_per_xpan_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set N_max_hdr_diams: Maximum number of diameters in each of the hot and cold headers [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_N_max_hdr_diams_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set P_a: Annulus gas pressure [torr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_P_a_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set P_ref: Rated plant capacity [MWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_P_ref_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set Pipe_hl_coef: Loss coefficient from the header, runner pipe, and non-HCE piping [W/m2-K]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Pipe_hl_coef_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set Rho_mirror_clean: User-defined clean mirror reflectivity [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Rho_mirror_clean_aset(SAM_TcstroughPhysical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set Rough: Relative roughness of the internal HCE surface  [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Rough_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set Row_Distance: Spacing between rows (centerline to centerline) [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Row_Distance_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set SCADefocusArray: Collector defocus order [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_SCADefocusArray_aset(SAM_TcstroughPhysical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set SCAInfoArray: Receiver (,1) and collector (,2) type for each assembly in loop [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_SCAInfoArray_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set SCA_drives_elec: Tracking power, in Watts per SCA drive [W/SCA]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_SCA_drives_elec_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set Shadowing: Receiver bellows shadowing loss factor [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Shadowing_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set T_fp: Freeze protection temperature (heat trace activation temperature) [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_T_fp_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set T_loop_in_des: Design loop inlet temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_T_loop_in_des_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set T_loop_out: Target loop outlet temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_T_loop_out_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set T_startup: Required temperature of the system before the power block can be switched on [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_T_startup_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set Tau_envelope: Envelope transmittance [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Tau_envelope_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set TrackingError: User-defined tracking error derate [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_TrackingError_aset(SAM_TcstroughPhysical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set Type_cpnt: Interconnect component type, row=intc, col=cpnt [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_Type_cpnt_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set V_hdr_cold_max: Maximum HTF velocity in the cold headers at design [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_V_hdr_cold_max_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set V_hdr_cold_min: Minimum HTF velocity in the cold headers at design [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_V_hdr_cold_min_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set V_hdr_hot_max: Maximum HTF velocity in the hot headers at design [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_V_hdr_hot_max_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set V_hdr_hot_min: Minimum HTF velocity in the hot headers at design [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_V_hdr_hot_min_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set W_aperture: The collector aperture width (Total structural area used for shadowing) [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_W_aperture_aset(SAM_TcstroughPhysical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set accept_init: In acceptance testing mode - require steady-state startup [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_accept_init_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set accept_loc: In acceptance testing mode - temperature sensor location [1/2]
	 * options: hx/loop
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_accept_loc_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set accept_mode: Acceptance testing mode? [0/1]
	 * options: no/yes
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_accept_mode_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set alpha_abs: Absorber absorptance  [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_alpha_abs_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set alpha_env: Envelope absorptance  [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_alpha_env_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set calc_design_pipe_vals: Calculate temps and pressures at design conditions for runners and headers [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_calc_design_pipe_vals_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set custom_sf_pipe_sizes: Use custom solar field pipe diams, wallthks, and lengths [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_custom_sf_pipe_sizes_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set epsilon_3_11: Absorber emittance for receiver type 1 variation 1 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_11_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_12: Absorber emittance for receiver type 1 variation 2 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_12_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_13: Absorber emittance for receiver type 1 variation 3 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_13_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_14: Absorber emittance for receiver type 1 variation 4 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_14_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_21: Absorber emittance for receiver type 2 variation 1 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_21_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_22: Absorber emittance for receiver type 2 variation 2 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_22_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_23: Absorber emittance for receiver type 2 variation 3 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_23_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_24: Absorber emittance for receiver type 2 variation 4 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_24_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_31: Absorber emittance for receiver type 3 variation 1 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_31_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_32: Absorber emittance for receiver type 3 variation 2 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_32_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_33: Absorber emittance for receiver type 3 variation 3 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_33_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_34: Absorber emittance for receiver type 3 variation 4 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_34_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_41: Absorber emittance for receiver type 4 variation 1 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_41_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_42: Absorber emittance for receiver type 4 variation 2 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_42_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_43: Absorber emittance for receiver type 4 variation 3 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_43_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set epsilon_3_44: Absorber emittance for receiver type 4 variation 4 [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_epsilon_3_44_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set eta_pump: HTF pump efficiency [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_eta_pump_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set fthrctrl: Defocusing strategy [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_fthrctrl_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set fthrok: Flag to allow partial defocusing of the collectors
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_fthrok_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set m_dot_htfmax: Maximum loop HTF flow rate [kg/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_m_dot_htfmax_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set m_dot_htfmin: Minimum loop HTF flow rate [kg/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_m_dot_htfmin_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set mc_bal_cold: Heat capacity of the balance of plant on the cold side [kWht/K-MWt]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_mc_bal_cold_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set mc_bal_hot: Heat capacity of the balance of plant on the hot side [kWht/K-MWt]
	 * options: none
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_mc_bal_hot_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set mc_bal_sca: Non-HTF heat capacity associated with each SCA - per meter basis [Wht/K-m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_mc_bal_sca_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set nColt: Number of collector types [none]
	 * options: constant=4
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_nColt_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set nHCEVar: Number of HCE variants per type [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_nHCEVar_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set nHCEt: Number of HCE types [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_nHCEt_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set nLoops: Number of loops in the field [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_nLoops_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set nSCA: Number of SCAs in a loop [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_nSCA_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set northsouth_field_sep: North/south separation between subfields. 0 = SCAs are touching [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_northsouth_field_sep_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set offset_xpan_hdr: Location of first header expansion loop. 1 = after first collector loop [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_offset_xpan_hdr_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set sf_hdr_diams: Custom header diameters [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_sf_hdr_diams_aset(SAM_TcstroughPhysical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set sf_hdr_lengths: Custom header lengths [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_sf_hdr_lengths_aset(SAM_TcstroughPhysical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set sf_hdr_wallthicks: Custom header wall thicknesses [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_sf_hdr_wallthicks_aset(SAM_TcstroughPhysical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set sf_rnr_diams: Custom runner diameters [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_sf_rnr_diams_aset(SAM_TcstroughPhysical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set sf_rnr_lengths: Custom runner lengths [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_sf_rnr_lengths_aset(SAM_TcstroughPhysical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set sf_rnr_wallthicks: Custom runner wall thicknesses [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_sf_rnr_wallthicks_aset(SAM_TcstroughPhysical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set solar_mult: Solar multiple [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_solar_mult_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set theta_dep: Deploy angle [deg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_theta_dep_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set theta_stow: Stow angle [deg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_theta_stow_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set washing_frequency: Mirror washing frequency [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_washing_frequency_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set water_usage_per_wash: Water usage per wash [L/m2_aper]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_SolarField_water_usage_per_wash_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);


	//
	// Controller parameters
	//

	/**
	 * Set DP_SGS: Pressure drop within the steam generator [bar]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_DP_SGS_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set HDR_rough: Header pipe roughness - used as general pipe roughness [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_HDR_rough_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set T_set_aux: Aux heater outlet temp set point [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_T_set_aux_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set T_tank_cold_ini: Initial cold tank fluid tmeperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_T_tank_cold_ini_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set T_tank_hot_inlet_min: Minimum hot tank htf inlet temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_T_tank_hot_inlet_min_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set V_tank_hot_ini: Initial hot tank fluid volume [m3]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_V_tank_hot_ini_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set V_tes_des: Design-point velocity to size the TES pipe diameters [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_V_tes_des_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set W_pb_design: Rated plant capacity [MWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_W_pb_design_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set aux_array: Coefficients for auxiliary heater parasitics calcs [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_aux_array_aset(SAM_TcstroughPhysical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set bop_array: Coefficients for balance of plant parasitics calcs [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_bop_array_aset(SAM_TcstroughPhysical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set cold_tank_Thtr: Minimum allowable cold tank HTF temp [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_cold_tank_Thtr_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set custom_sgs_pipe_sizes: Use custom SGS pipe diams, wallthks, and lengths [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_custom_sgs_pipe_sizes_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set custom_tes_p_loss: TES pipe losses are based on custom lengths and coeffs [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_custom_tes_p_loss_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set cycle_cutoff_frac: Minimum turbine operation fraction before shutdown [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_cycle_cutoff_frac_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set cycle_max_frac: Maximum turbine over design operation fraction [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_cycle_max_frac_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set dt_cold: Cold side HX approach temp [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_dt_cold_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set dt_hot: Hot side HX approach temp [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_dt_hot_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set eta_pump: HTF pump efficiency [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_eta_pump_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set f_tc_cold: 0=entire tank is hot, 1=entire tank is cold [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_f_tc_cold_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set ffrac: Fossil dispatch logic [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_ffrac_aset(SAM_TcstroughPhysical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set field_fl_props: User defined field fluid property data [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_field_fl_props_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set fossil_mode: Fossil backup mode 1=Normal 2=Topping [-]
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_fossil_mode_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set h_tank: Total height of tank (height of HTF when tank is full [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_h_tank_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set h_tank_min: Minimum allowable HTF height in storage tank [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_h_tank_min_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set has_hot_tank_bypass: Bypass valve connects field outlet to cold tank [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_has_hot_tank_bypass_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set hot_tank_Thtr: Minimum allowable hot tank HTF temp [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_hot_tank_Thtr_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set hx_config: HX configuration [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_hx_config_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set is_hx: Heat exchanger (HX) exists (1=yes, 0=no) [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_is_hx_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set k_tes_loss_coeffs: Minor loss coeffs for the coll, gen, and bypass loops [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_k_tes_loss_coeffs_aset(SAM_TcstroughPhysical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set nodes: Nodes modeled in the flow path [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_nodes_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set pb_fixed_par: Fraction of rated gross power constantly consumed [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_pb_fixed_par_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set pb_pump_coef: Pumping power to move 1kg of HTF through PB loop [kW/(kg/s)]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_pb_pump_coef_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set q_max_aux: Max heat rate of auxiliary heater [MWt]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_q_max_aux_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set q_pb_design: Design heat input to power block [MWt]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_q_pb_design_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set sf_type: Solar field type, 1 = trough, 2 = tower [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_sf_type_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set sgs_diams: Custom SGS diameters [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_sgs_diams_aset(SAM_TcstroughPhysical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set sgs_lengths: Custom SGS lengths [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_sgs_lengths_aset(SAM_TcstroughPhysical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set sgs_wallthicks: Custom SGS wall thicknesses [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_sgs_wallthicks_aset(SAM_TcstroughPhysical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set store_fl_props: User defined storage fluid property data [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_store_fl_props_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set store_fluid: Material number for storage fluid [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_store_fluid_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set t_ch_out_max: Max allowable cold side outlet temp during charge [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_t_ch_out_max_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set t_dis_out_min: Min allowable hot side outlet temp during discharge [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_t_dis_out_min_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set t_standby_reset: Maximum allowable time for PB standby operation [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_t_standby_reset_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set tank_max_heat: Rated heater capacity for tank heating [MW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_tank_max_heat_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set tank_pairs: Number of equivalent tank pairs [-]
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_tank_pairs_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set tanks_in_parallel: Tanks are in parallel, not in series, with solar field [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_tanks_in_parallel_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set tc_fill: Thermocline fill material [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_tc_fill_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set tc_void: Thermocline void fraction [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_tc_void_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set tes_pump_coef: Pumping power to move 1kg of HTF through tes loop [kW/(kg/s)]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_tes_pump_coef_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set tes_type: 1=2-tank, 2=thermocline [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_tes_type_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set tshours: Equivalent full-load thermal storage hours [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_tshours_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set tslogic_a: Dispatch logic without solar [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_tslogic_a_aset(SAM_TcstroughPhysical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set tslogic_b: Dispatch logic with solar [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_tslogic_b_aset(SAM_TcstroughPhysical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set tslogic_c: Dispatch logic for turbine load fraction [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_tslogic_c_aset(SAM_TcstroughPhysical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set u_tank: Loss coefficient from the tank [W/m2-K]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_u_tank_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set vol_tank: Total tank volume, including unusable HTF at bottom [m3]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Controller_vol_tank_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);


	//
	// TouTranslator parameters
	//

	/**
	 * Set weekday_schedule: Dispatch 12mx24h schedule for week days
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_TouTranslator_weekday_schedule_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set weekend_schedule: Dispatch 12mx24h schedule for weekends
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_TouTranslator_weekend_schedule_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);


	//
	// Powerblock parameters
	//

	/**
	 * Set CT: Flag for using dry cooling or wet cooling system [none]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_CT_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set F_wc: Fraction indicating wet cooling use for hybrid system [none]
	 * options: constant=[0,0,0,0,0,0,0,0,0]
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_F_wc_aset(SAM_TcstroughPhysical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set P_boil: Boiler operating pressure [bar]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_P_boil_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set P_cond_min: Minimum condenser pressure [inHg]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_P_cond_min_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set P_cond_ratio: Condenser pressure ratio [none]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_P_cond_ratio_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set T_ITD_des: ITD at design for dry system [C]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_T_ITD_des_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set T_amb_des: Reference ambient temperature at design point [C]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_T_amb_des_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set T_approach: Cooling tower approach temperature [C]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_T_approach_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set dT_cw_ref: Reference condenser cooling water inlet/outlet T diff [C]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_dT_cw_ref_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set eta_ref: Reference conversion efficiency at design condition [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_eta_ref_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set n_pl_inc: Number of part-load increments for the heat rejection system [none]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_n_pl_inc_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set pb_bd_frac: Power block blowdown steam fraction  [none]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_pb_bd_frac_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set pc_config: 0: Steam Rankine (224), 1: user defined [-]
	 * options: None
	 * constraints: INTEGER
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_pc_config_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set q_sby_frac: Fraction of thermal power required for standby mode [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_q_sby_frac_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set startup_frac: Fraction of design thermal power needed for startup [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_startup_frac_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set startup_time: Time needed for power block startup [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_startup_time_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set tech_type: Turbine inlet pressure control flag (sliding=user, fixed=trough) [1/2/3]
	 * options: tower/trough/user
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Powerblock_tech_type_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);


	//
	// UserDefinedPC parameters
	//

	/**
	 * Set ud_T_amb_des: Ambient temperature at user-defined power cycle design point [C]
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_UserDefinedPC_ud_T_amb_des_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set ud_T_amb_high: High level ambient temperature for HTF mass flow rate parametric [C]
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_UserDefinedPC_ud_T_amb_high_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set ud_T_amb_ind_od: Off design table of user-defined power cycle performance formed from parametric on T_amb [C]
	 * options: None
	 * constraints: None
	 * required if: ?=[[0]]
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_UserDefinedPC_ud_T_amb_ind_od_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ud_T_amb_low: Low level ambient temperature for HTF mass flow rate parametric [C]
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_UserDefinedPC_ud_T_amb_low_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set ud_T_htf_high: High level HTF inlet temperature for T_amb parametric [C]
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_UserDefinedPC_ud_T_htf_high_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set ud_T_htf_ind_od: Off design table of user-defined power cycle performance formed from parametric on T_htf_hot [C]
	 * options: None
	 * constraints: None
	 * required if: ?=[[0]]
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_UserDefinedPC_ud_T_htf_ind_od_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ud_T_htf_low: Low level HTF inlet temperature for T_amb parametric [C]
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_UserDefinedPC_ud_T_htf_low_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set ud_f_W_dot_cool_des: Percent of user-defined power cycle design gross output consumed by cooling [%]
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_UserDefinedPC_ud_f_W_dot_cool_des_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set ud_ind_od: Off design user-defined power cycle performance as function of T_htf, m_dot_htf [ND], and T_amb
	 * options: None
	 * constraints: None
	 * required if: ?=[[0]]
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_UserDefinedPC_ud_ind_od_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ud_m_dot_htf_high: High level normalized HTF mass flow rate for T_HTF parametric [-]
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_UserDefinedPC_ud_m_dot_htf_high_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set ud_m_dot_htf_ind_od: Off design table of user-defined power cycle performance formed from parametric on m_dot_htf [ND]
	 * options: None
	 * constraints: None
	 * required if: ?=[[0]]
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_UserDefinedPC_ud_m_dot_htf_ind_od_mset(SAM_TcstroughPhysical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ud_m_dot_htf_low: Low level normalized HTF mass flow rate for T_HTF parametric [-]
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_UserDefinedPC_ud_m_dot_htf_low_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set ud_m_dot_water_cool_des: Mass flow rate of water required at user-defined power cycle design point [kg/s]
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_UserDefinedPC_ud_m_dot_water_cool_des_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);


	//
	// Enet parameters
	//

	/**
	 * Set eta_lhv: Fossil fuel lower heating value - Thermal power generated per unit fuel [MW/MMBTU]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Enet_eta_lhv_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);

	/**
	 * Set eta_tes_htr: Thermal storage tank heater efficiency (fp_mode=1 only) [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughPhysical_Enet_eta_tes_htr_nset(SAM_TcstroughPhysical ptr, double number, SAM_error *err);


	/**
	 * Weather Getters
	 */

	SAM_EXPORT double SAM_TcstroughPhysical_Weather_azimuth_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_TcstroughPhysical_Weather_file_name_sget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Weather_tilt_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Weather_track_mode_nget(SAM_TcstroughPhysical ptr, SAM_error *err);


	/**
	 * Trough Getters
	 */

	SAM_EXPORT double SAM_TcstroughPhysical_Trough_system_capacity_nget(SAM_TcstroughPhysical ptr, SAM_error *err);


	/**
	 * SolarField Getters
	 */

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_A_aperture_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_AbsorberMaterial_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_AnnulusGas_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_Ave_Focal_Length_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_ColperSCA_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_D_2_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_D_3_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_D_4_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_D_5_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_D_cpnt_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_D_p_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_Design_loss_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_Dirt_HCE_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_Dirt_mirror_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_Distance_SCA_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_EPSILON_4_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_EPSILON_5_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_Error_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_FieldConfig_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_Flow_type_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_Fluid_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_GeomEffects_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_GlazingIntactIn_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_HCE_FieldFrac_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_HDR_rough_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_IAM_matrix_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_I_bn_des_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_K_cpnt_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_L_SCA_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_L_aperture_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_L_cpnt_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_L_rnr_pb_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_L_rnr_per_xpan_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_L_xpan_hdr_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_L_xpan_rnr_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_Min_rnr_xpans_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_N_hdr_per_xpan_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_N_max_hdr_diams_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_P_a_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_P_ref_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_Pipe_hl_coef_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_Rho_mirror_clean_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_Rough_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_Row_Distance_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_SCADefocusArray_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_SCAInfoArray_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_SCA_drives_elec_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_Shadowing_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_T_fp_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_T_loop_in_des_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_T_loop_out_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_T_startup_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_Tau_envelope_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_TrackingError_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_Type_cpnt_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_V_hdr_cold_max_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_V_hdr_cold_min_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_V_hdr_hot_max_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_V_hdr_hot_min_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_W_aperture_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_accept_init_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_accept_loc_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_accept_mode_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_alpha_abs_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_alpha_env_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_calc_design_pipe_vals_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_custom_sf_pipe_sizes_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_11_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_12_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_13_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_14_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_21_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_22_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_23_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_24_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_31_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_32_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_33_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_34_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_41_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_42_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_43_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_epsilon_3_44_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_eta_pump_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_fthrctrl_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_fthrok_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_m_dot_htfmax_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_m_dot_htfmin_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_mc_bal_cold_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_mc_bal_hot_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_mc_bal_sca_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_nColt_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_nHCEVar_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_nHCEt_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_nLoops_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_nSCA_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_northsouth_field_sep_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_offset_xpan_hdr_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_sf_hdr_diams_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_sf_hdr_lengths_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_sf_hdr_wallthicks_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_sf_rnr_diams_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_sf_rnr_lengths_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_SolarField_sf_rnr_wallthicks_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_solar_mult_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_theta_dep_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_theta_stow_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_washing_frequency_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_SolarField_water_usage_per_wash_nget(SAM_TcstroughPhysical ptr, SAM_error *err);


	/**
	 * Controller Getters
	 */

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_DP_SGS_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_HDR_rough_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_T_set_aux_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_T_tank_cold_ini_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_T_tank_hot_inlet_min_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_V_tank_hot_ini_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_V_tes_des_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_W_pb_design_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Controller_aux_array_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Controller_bop_array_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_cold_tank_Thtr_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_custom_sgs_pipe_sizes_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_custom_tes_p_loss_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_cycle_cutoff_frac_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_cycle_max_frac_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_dt_cold_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_dt_hot_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_eta_pump_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_f_tc_cold_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Controller_ffrac_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Controller_field_fl_props_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_fossil_mode_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_h_tank_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_h_tank_min_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_has_hot_tank_bypass_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_hot_tank_Thtr_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_hx_config_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_is_hx_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Controller_k_tes_loss_coeffs_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_nodes_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_pb_fixed_par_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_pb_pump_coef_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_q_max_aux_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_q_pb_design_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_sf_type_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Controller_sgs_diams_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Controller_sgs_lengths_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Controller_sgs_wallthicks_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Controller_store_fl_props_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_store_fluid_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_t_ch_out_max_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_t_dis_out_min_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_t_standby_reset_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_tank_max_heat_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_tank_pairs_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_tanks_in_parallel_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_tc_fill_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_tc_void_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_tes_pump_coef_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_tes_type_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_tshours_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Controller_tslogic_a_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Controller_tslogic_b_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Controller_tslogic_c_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_u_tank_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Controller_vol_tank_nget(SAM_TcstroughPhysical ptr, SAM_error *err);


	/**
	 * TouTranslator Getters
	 */

	SAM_EXPORT double* SAM_TcstroughPhysical_TouTranslator_weekday_schedule_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_TouTranslator_weekend_schedule_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * Powerblock Getters
	 */

	SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_CT_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Powerblock_F_wc_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_P_boil_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_P_cond_min_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_P_cond_ratio_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_T_ITD_des_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_T_amb_des_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_T_approach_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_dT_cw_ref_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_eta_ref_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_n_pl_inc_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_pb_bd_frac_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_pc_config_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_q_sby_frac_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_startup_frac_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_startup_time_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Powerblock_tech_type_nget(SAM_TcstroughPhysical ptr, SAM_error *err);


	/**
	 * UserDefinedPC Getters
	 */

	SAM_EXPORT double SAM_TcstroughPhysical_UserDefinedPC_ud_T_amb_des_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_UserDefinedPC_ud_T_amb_high_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_UserDefinedPC_ud_T_amb_ind_od_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_UserDefinedPC_ud_T_amb_low_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_UserDefinedPC_ud_T_htf_high_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_UserDefinedPC_ud_T_htf_ind_od_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_UserDefinedPC_ud_T_htf_low_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_UserDefinedPC_ud_f_W_dot_cool_des_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_UserDefinedPC_ud_ind_od_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_UserDefinedPC_ud_m_dot_htf_high_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_UserDefinedPC_ud_m_dot_htf_ind_od_mget(SAM_TcstroughPhysical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_UserDefinedPC_ud_m_dot_htf_low_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_UserDefinedPC_ud_m_dot_water_cool_des_nget(SAM_TcstroughPhysical ptr, SAM_error *err);


	/**
	 * Enet Getters
	 */

	SAM_EXPORT double SAM_TcstroughPhysical_Enet_eta_lhv_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Enet_eta_tes_htr_nget(SAM_TcstroughPhysical ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_CosTh_ave_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_DP_tot_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_E_bal_startup_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_EndLoss_ave_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_EqOpteff_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_Fuel_usage_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_IAM_ave_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_Pipe_hl_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_Q_aux_backup_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_Q_par_sf_fp_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_Q_par_tes_fp_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_RowShadow_ave_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_SCA_par_tot_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_SCAs_def_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_T_field_in_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_T_pb_in_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_T_pb_out_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_T_sys_c_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_T_sys_h_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_T_tank_cold_fin_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_T_tank_cold_in_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_T_tank_hot_fin_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_T_tank_hot_in_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_Theta_ave_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_Ts_cold_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_Ts_hot_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_W_cool_par_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_W_cycle_gross_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_W_dot_pump_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_W_net_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Outputs_annual_W_cycle_gross_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Outputs_annual_energy_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Outputs_annual_fuel_usage_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Outputs_annual_q_abs_tot_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Outputs_annual_q_aux_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Outputs_annual_q_avail_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Outputs_annual_q_dump_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Outputs_annual_q_inc_sf_tot_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Outputs_annual_q_pb_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Outputs_annual_q_to_tes_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Outputs_annual_total_water_use_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_aux_par_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_beam_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_bop_par_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Outputs_capacity_factor_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Outputs_conversion_factor_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_dni_costh_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_eta_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_fixed_par_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_gen_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_hour_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_htf_pump_power_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Outputs_kwh_per_kw_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_m_dot_aux_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_m_dot_avail_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_m_dot_charge_field_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_m_dot_discharge_tank_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_m_dot_htf2_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_m_dot_makeup_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_m_dot_pb_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_mass_tank_cold_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_mass_tank_hot_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_month_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_monthly_Fuel_usage_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_monthly_W_cycle_gross_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_monthly_energy_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_monthly_m_dot_makeup_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_monthly_q_abs_tot_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_monthly_q_avail_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_monthly_q_dump_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_monthly_q_inc_sf_tot_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_monthly_q_pb_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_monthly_q_to_tes_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_header_P_dsn_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_header_T_dsn_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_header_diams_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_header_expansions_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_header_lengths_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_header_mdot_dsn_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_header_vel_dsn_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_header_wallthk_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_loop_P_dsn_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_loop_T_dsn_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_runner_P_dsn_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_runner_T_dsn_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_runner_diams_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_runner_expansions_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_runner_lengths_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_runner_mdot_dsn_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_runner_vel_dsn_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_runner_wallthk_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_sgs_P_dsn_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_sgs_T_dsn_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_sgs_diams_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_sgs_mdot_dsn_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_sgs_vel_dsn_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pipe_sgs_wallthk_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_pres_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_q_abs_tot_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_q_avail_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_q_dump_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_q_inc_sf_tot_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_q_loss_spec_tot_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_q_loss_tot_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_q_pb_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_q_to_tes_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_qinc_costh_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_recirculating_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_solazi_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_solzen_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughPhysical_Outputs_system_heat_rate_nget(SAM_TcstroughPhysical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_tank_losses_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_tdry_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_tou_value_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_twet_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_vol_tank_cold_fin_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_vol_tank_hot_fin_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_vol_tank_total_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughPhysical_Outputs_wspd_aget(SAM_TcstroughPhysical ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
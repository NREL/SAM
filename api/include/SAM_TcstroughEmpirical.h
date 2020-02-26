#ifndef SAM_TCSTROUGHEMPIRICAL_H_
#define SAM_TCSTROUGHEMPIRICAL_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// TcstroughEmpirical Technology Model
	//

	/** 
	 * Create a TcstroughEmpirical variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_TcstroughEmpirical;

	SAM_EXPORT SAM_TcstroughEmpirical SAM_TcstroughEmpirical_construct(const char* def, SAM_error* err);

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_TcstroughEmpirical_execute(SAM_TcstroughEmpirical data, int verbosity, SAM_error* err);

	SAM_EXPORT void SAM_TcstroughEmpirical_destruct(SAM_TcstroughEmpirical system);


	//
	// Weather parameters
	//

	/**
	 * Set azimuth: Azimuth angle of surface/axis
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Weather_azimuth_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set file_name: local weather file path
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Weather_file_name_sset(SAM_TcstroughEmpirical ptr, const char* str, SAM_error *err);

	/**
	 * Set tilt: Tilt angle of surface/axis
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Weather_tilt_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set track_mode: Tracking mode
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Weather_track_mode_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);


	//
	// Trough parameters
	//

	/**
	 * Set system_capacity: Nameplate capacity [kW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Trough_system_capacity_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);


	//
	// TouTranslator parameters
	//

	/**
	 * Set weekday_schedule: 12x24 Time of Use Values for week days
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_TouTranslator_weekday_schedule_mset(SAM_TcstroughEmpirical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set weekend_schedule: 12x24 Time of Use Values for week end days
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_TouTranslator_weekend_schedule_mset(SAM_TcstroughEmpirical ptr, double* mat, int nrows, int ncols, SAM_error *err);


	//
	// Solarfield parameters
	//

	/**
	 * Set DepAngle: Deployment Angle [deg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_DepAngle_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set Distance_SCA: Distance between SCAs in Row [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_Distance_SCA_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set HTFFluid: Type of Heat Transfer Fluid used
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_HTFFluid_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set HtfGalArea: HTF Fluids in Gallons per Field Area [gal/m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_HtfGalArea_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set MinHtfTemp: Minimum Heat Transfer Fluid Temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_MinHtfTemp_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set NumScas: Number of SCAs per Row
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_NumScas_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set Row_Distance: Distance between Rows of SCAs [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_Row_Distance_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set SFTempInit: Solar Field Initial Temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_SFTempInit_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set SfInTempD: Solar Field Design Inlet Temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_SfInTempD_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set SfOutTempD: Solar Field Design Outlet Temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_SfOutTempD_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set SfPipeHl1: Solar field piping heat loss at reduced temp. - linear term [C^(-1)]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_SfPipeHl1_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set SfPipeHl2: Solar field piping heat loss at reduced temp. - quadratic term [C^(-2)]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_SfPipeHl2_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set SfPipeHl3: Solar field piping heat loss at reduced temp. - cubic term [C^(-3)]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_SfPipeHl3_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set SfPipeHl300: Solar field piping heat loss at design [W/m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_SfPipeHl300_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set Solar_Field_Area: Solar Field Area [m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_Solar_Field_Area_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set Solar_Field_Mult: Solar Field Multiple
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_Solar_Field_Mult_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set Stow_Angle: Night-Time Trough Stow Angle [deg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_Stow_Angle_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set i_SfTi: Solar Field HTF inlet Temperature (if -999, calculated) [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_i_SfTi_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);


	//
	// Sca parameters
	//

	/**
	 * Set Ave_Focal_Length: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Sca_Ave_Focal_Length_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set ConcFac: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Sca_ConcFac_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set GeoAcc: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Sca_GeoAcc_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set IamF0: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Sca_IamF0_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set IamF1: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Sca_IamF1_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set IamF2: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Sca_IamF2_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set MirCln: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Sca_MirCln_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set MirRef: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Sca_MirRef_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set SCA_aper: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Sca_SCA_aper_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set ScaLen: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Sca_ScaLen_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set SfAvail: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Sca_SfAvail_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set TrkTwstErr: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Sca_TrkTwstErr_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);


	//
	// Hce parameters
	//

	/**
	 * Set HCEBelShad: label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Hce_HCEBelShad_aset(SAM_TcstroughEmpirical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set HCEEnvTrans: label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Hce_HCEEnvTrans_aset(SAM_TcstroughEmpirical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set HCEFrac: Fraction of field that is this type of HCE
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Hce_HCEFrac_aset(SAM_TcstroughEmpirical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set HCE_A0: label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Hce_HCE_A0_aset(SAM_TcstroughEmpirical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set HCE_A1: label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Hce_HCE_A1_aset(SAM_TcstroughEmpirical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set HCE_A2: label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Hce_HCE_A2_aset(SAM_TcstroughEmpirical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set HCE_A3: label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Hce_HCE_A3_aset(SAM_TcstroughEmpirical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set HCE_A4: label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Hce_HCE_A4_aset(SAM_TcstroughEmpirical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set HCE_A5: label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Hce_HCE_A5_aset(SAM_TcstroughEmpirical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set HCE_A6: label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Hce_HCE_A6_aset(SAM_TcstroughEmpirical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set HCEabs: label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Hce_HCEabs_aset(SAM_TcstroughEmpirical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set HCEdust: label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Hce_HCEdust_aset(SAM_TcstroughEmpirical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set HCEmisc: label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Hce_HCEmisc_aset(SAM_TcstroughEmpirical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set HCEtype: Number indicating the receiver type
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Hce_HCEtype_aset(SAM_TcstroughEmpirical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set NumHCETypes: Number of HCE types
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Hce_NumHCETypes_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set PerfFac: label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Hce_PerfFac_aset(SAM_TcstroughEmpirical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set RefMirrAper: label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Hce_RefMirrAper_aset(SAM_TcstroughEmpirical ptr, double* arr, int length, SAM_error *err);


	//
	// Pwrb parameters
	//

	/**
	 * Set E2TPLF0: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_E2TPLF0_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set E2TPLF1: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_E2TPLF1_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set E2TPLF2: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_E2TPLF2_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set E2TPLF3: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_E2TPLF3_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set E2TPLF4: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_E2TPLF4_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set LHVBoilEff: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_LHVBoilEff_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set MaxGrOut: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_MaxGrOut_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set MinGrOut: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_MinGrOut_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set PTTMAX: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_PTTMAX_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set PTTMIN: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_PTTMIN_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set T2EPLF0: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_T2EPLF0_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set T2EPLF1: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_T2EPLF1_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set T2EPLF2: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_T2EPLF2_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set T2EPLF3: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_T2EPLF3_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set T2EPLF4: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_T2EPLF4_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set TempCorr0: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_TempCorr0_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set TempCorr1: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_TempCorr1_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set TempCorr2: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_TempCorr2_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set TempCorr3: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_TempCorr3_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set TempCorr4: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_TempCorr4_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set TempCorrF: Temp Correction Mode (0=wetbulb 1=drybulb basis)
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_TempCorrF_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set TurSUE: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_TurSUE_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set TurbEffG: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_TurbEffG_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set TurbOutG: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_TurbOutG_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);


	//
	// Tes parameters
	//

	/**
	 * Set E_tes_ini: Initial TES energy - fraction of max
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Tes_E_tes_ini_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set FossilFill: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Tes_FossilFill_aset(SAM_TcstroughEmpirical ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set NUMTOU: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Tes_NUMTOU_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set PFSmax: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Tes_PFSmax_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set PTSmax: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Tes_PTSmax_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set TSHOURS: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Tes_TSHOURS_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set TSLogic: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Tes_TSLogic_mset(SAM_TcstroughEmpirical ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set TnkHL: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Tes_TnkHL_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set TurTesEffAdj: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Tes_TurTesEffAdj_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set TurTesOutAdj: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Tes_TurTesOutAdj_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);


	//
	// Parasitic parameters
	//

	/**
	 * Set AntiFrPar: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_AntiFrPar_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set BOPPar: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_BOPPar_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set BOPParF0: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_BOPParF0_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set BOPParF1: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_BOPParF1_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set BOPParF2: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_BOPParF2_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set BOPParPF: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_BOPParPF_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set CHTFParF0: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_CHTFParF0_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set CHTFParF1: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_CHTFParF1_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set CHTFParF2: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_CHTFParF2_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set ChtfPar: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_ChtfPar_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set ChtfParPF: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_ChtfParPF_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set CtOpF: Label
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_CtOpF_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set CtPar: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_CtPar_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set CtParF0: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_CtParF0_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set CtParF1: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_CtParF1_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set CtParF2: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_CtParF2_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set CtParPF: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_CtParPF_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set HhtfPar: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_HhtfPar_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set HhtfParF0: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_HhtfParF0_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set HhtfParF1: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_HhtfParF1_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set HhtfParF2: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_HhtfParF2_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set HhtfParPF: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_HhtfParPF_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set HtrPar: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_HtrPar_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set HtrParF0: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_HtrParF0_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set HtrParF1: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_HtrParF1_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set HtrParF2: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_HtrParF2_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set HtrParPF: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_HtrParPF_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set PbFixPar: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_PbFixPar_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set SfPar: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_SfPar_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);

	/**
	 * Set SfParPF: Label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_SfParPF_nset(SAM_TcstroughEmpirical ptr, double number, SAM_error *err);


	/**
	 * Weather Getters
	 */

	SAM_EXPORT double SAM_TcstroughEmpirical_Weather_azimuth_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_Weather_file_name_sget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Weather_tilt_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Weather_track_mode_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);


	/**
	 * Trough Getters
	 */

	SAM_EXPORT double SAM_TcstroughEmpirical_Trough_system_capacity_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);


	/**
	 * TouTranslator Getters
	 */

	SAM_EXPORT double* SAM_TcstroughEmpirical_TouTranslator_weekday_schedule_mget(SAM_TcstroughEmpirical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_TouTranslator_weekend_schedule_mget(SAM_TcstroughEmpirical ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * Solarfield Getters
	 */

	SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_DepAngle_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_Distance_SCA_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_HTFFluid_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_HtfGalArea_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_MinHtfTemp_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_NumScas_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_Row_Distance_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_SFTempInit_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_SfInTempD_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_SfOutTempD_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_SfPipeHl1_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_SfPipeHl2_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_SfPipeHl3_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_SfPipeHl300_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_Solar_Field_Area_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_Solar_Field_Mult_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_Stow_Angle_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_i_SfTi_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);


	/**
	 * Sca Getters
	 */

	SAM_EXPORT double SAM_TcstroughEmpirical_Sca_Ave_Focal_Length_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Sca_ConcFac_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Sca_GeoAcc_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Sca_IamF0_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Sca_IamF1_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Sca_IamF2_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Sca_MirCln_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Sca_MirRef_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Sca_SCA_aper_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Sca_ScaLen_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Sca_SfAvail_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Sca_TrkTwstErr_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);


	/**
	 * Hce Getters
	 */

	SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_HCEBelShad_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_HCEEnvTrans_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_HCEFrac_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_HCE_A0_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_HCE_A1_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_HCE_A2_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_HCE_A3_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_HCE_A4_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_HCE_A5_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_HCE_A6_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_HCEabs_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_HCEdust_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_HCEmisc_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_HCEtype_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Hce_NumHCETypes_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_PerfFac_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_RefMirrAper_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);


	/**
	 * Pwrb Getters
	 */

	SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_E2TPLF0_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_E2TPLF1_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_E2TPLF2_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_E2TPLF3_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_E2TPLF4_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_LHVBoilEff_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_MaxGrOut_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_MinGrOut_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_PTTMAX_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_PTTMIN_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_T2EPLF0_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_T2EPLF1_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_T2EPLF2_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_T2EPLF3_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_T2EPLF4_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_TempCorr0_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_TempCorr1_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_TempCorr2_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_TempCorr3_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_TempCorr4_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_TempCorrF_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_TurSUE_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_TurbEffG_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_TurbOutG_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);


	/**
	 * Tes Getters
	 */

	SAM_EXPORT double SAM_TcstroughEmpirical_Tes_E_tes_ini_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Tes_FossilFill_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Tes_NUMTOU_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Tes_PFSmax_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Tes_PTSmax_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Tes_TSHOURS_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Tes_TSLogic_mget(SAM_TcstroughEmpirical ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Tes_TnkHL_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Tes_TurTesEffAdj_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Tes_TurTesOutAdj_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);


	/**
	 * Parasitic Getters
	 */

	SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_AntiFrPar_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_BOPPar_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_BOPParF0_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_BOPParF1_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_BOPParF2_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_BOPParPF_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_CHTFParF0_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_CHTFParF1_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_CHTFParF2_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_ChtfPar_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_ChtfParPF_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_CtOpF_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_CtPar_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_CtParF0_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_CtParF1_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_CtParF2_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_CtParPF_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_HhtfPar_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_HhtfParF0_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_HhtfParF1_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_HhtfParF2_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_HhtfParPF_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_HtrPar_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_HtrParF0_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_HtrParF1_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_HtrParF2_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_HtrParPF_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_PbFixPar_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_SfPar_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_SfParPF_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_AveSfTemp_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_ColEff_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_CosTheta_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_Egr_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_EgrFos_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_EgrSol_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_EndLoss_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_Enet_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_Epar_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_EparAnti_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_EparBOP_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_EparCHTF_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_EparCT_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_EparHhtf_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_EparHtr_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_EparOffLine_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_EparOnLine_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_EparPB_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_EparSf_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_Ets_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_Ftrack_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_IAM_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_QTsFull_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_QTsHl_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_QTurSu_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_Qdni_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_Qdump_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_Qfts_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_Qgas_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_QhtfFpHtr_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_QhtfFpTES_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_QhtfFreezeProt_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_Qmin_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_QnipCosTh_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_Qsf_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_QsfAbs_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_QsfHceHL_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_QsfPipeHL_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_QsfWarmup_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_Qsfnipcosth_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_Qtpb_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_Qtts_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_RecHl_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_RowShadow_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_SfMassFlow_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_SfTo_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_Theta_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_TrackAngle_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Outputs_annual_W_cycle_gross_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Outputs_annual_energy_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Outputs_annual_fuel_usage_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_beam_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Outputs_capacity_factor_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Outputs_conversion_factor_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_gen_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_hour_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Outputs_kwh_per_kw_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_month_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_monthly_energy_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_o_SfTi_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_pres_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_solazi_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_solzen_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Outputs_system_heat_rate_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double SAM_TcstroughEmpirical_Outputs_system_use_lifetime_output_nget(SAM_TcstroughEmpirical ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_tdry_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_tou_value_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_twet_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_wspd_aget(SAM_TcstroughEmpirical ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
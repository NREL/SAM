#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_TcstroughEmpirical.h"

SAM_EXPORT int SAM_TcstroughEmpirical_execute(SAM_table data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("tcstrough_empirical", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_TcstroughEmpirical_Weather_azimuth_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "azimuth", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Weather_file_name_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "file_name", str);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Weather_tilt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tilt", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Weather_track_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "track_mode", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Trough_system_capacity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_capacity", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_TouTranslator_weekday_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "weekday_schedule", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_TouTranslator_weekend_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "weekend_schedule", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_DepAngle_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "DepAngle", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_Distance_SCA_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Distance_SCA", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_HTFFluid_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HTFFluid", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_HtfGalArea_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HtfGalArea", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_MinHtfTemp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "MinHtfTemp", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_NumScas_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "NumScas", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_Row_Distance_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Row_Distance", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_SFTempInit_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "SFTempInit", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_SfInTempD_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "SfInTempD", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_SfOutTempD_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "SfOutTempD", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_SfPipeHl1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "SfPipeHl1", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_SfPipeHl2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "SfPipeHl2", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_SfPipeHl3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "SfPipeHl3", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_SfPipeHl300_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "SfPipeHl300", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_Solar_Field_Area_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Solar_Field_Area", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_Solar_Field_Mult_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Solar_Field_Mult", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_Stow_Angle_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Stow_Angle", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Solarfield_i_SfTi_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "i_SfTi", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Sca_Ave_Focal_Length_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Ave_Focal_Length", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Sca_ConcFac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ConcFac", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Sca_GeoAcc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "GeoAcc", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Sca_IamF0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "IamF0", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Sca_IamF1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "IamF1", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Sca_IamF2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "IamF2", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Sca_MirCln_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "MirCln", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Sca_MirRef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "MirRef", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Sca_SCA_aper_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "SCA_aper", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Sca_ScaLen_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ScaLen", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Sca_SfAvail_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "SfAvail", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Sca_TrkTwstErr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "TrkTwstErr", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Hce_HCEBelShad_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "HCEBelShad", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Hce_HCEEnvTrans_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "HCEEnvTrans", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Hce_HCEFrac_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "HCEFrac", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Hce_HCE_A0_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "HCE_A0", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Hce_HCE_A1_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "HCE_A1", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Hce_HCE_A2_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "HCE_A2", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Hce_HCE_A3_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "HCE_A3", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Hce_HCE_A4_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "HCE_A4", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Hce_HCE_A5_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "HCE_A5", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Hce_HCE_A6_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "HCE_A6", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Hce_HCEabs_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "HCEabs", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Hce_HCEdust_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "HCEdust", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Hce_HCEmisc_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "HCEmisc", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Hce_HCEtype_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "HCEtype", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Hce_NumHCETypes_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "NumHCETypes", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Hce_PerfFac_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "PerfFac", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Hce_RefMirrAper_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "RefMirrAper", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_E2TPLF0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "E2TPLF0", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_E2TPLF1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "E2TPLF1", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_E2TPLF2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "E2TPLF2", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_E2TPLF3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "E2TPLF3", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_E2TPLF4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "E2TPLF4", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_LHVBoilEff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "LHVBoilEff", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_MaxGrOut_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "MaxGrOut", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_MinGrOut_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "MinGrOut", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_PTTMAX_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "PTTMAX", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_PTTMIN_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "PTTMIN", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_T2EPLF0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T2EPLF0", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_T2EPLF1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T2EPLF1", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_T2EPLF2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T2EPLF2", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_T2EPLF3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T2EPLF3", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_T2EPLF4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T2EPLF4", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_TempCorr0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "TempCorr0", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_TempCorr1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "TempCorr1", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_TempCorr2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "TempCorr2", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_TempCorr3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "TempCorr3", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_TempCorr4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "TempCorr4", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_TempCorrF_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "TempCorrF", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_TurSUE_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "TurSUE", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_TurbEffG_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "TurbEffG", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Pwrb_TurbOutG_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "TurbOutG", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Tes_E_tes_ini_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "E_tes_ini", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Tes_FossilFill_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "FossilFill", arr, length);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Tes_NUMTOU_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "NUMTOU", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Tes_PFSmax_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "PFSmax", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Tes_PTSmax_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "PTSmax", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Tes_TSHOURS_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "TSHOURS", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Tes_TSLogic_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "TSLogic", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Tes_TnkHL_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "TnkHL", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Tes_TurTesEffAdj_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "TurTesEffAdj", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Tes_TurTesOutAdj_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "TurTesOutAdj", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_AntiFrPar_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "AntiFrPar", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_BOPPar_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "BOPPar", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_BOPParF0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "BOPParF0", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_BOPParF1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "BOPParF1", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_BOPParF2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "BOPParF2", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_BOPParPF_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "BOPParPF", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_CHTFParF0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "CHTFParF0", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_CHTFParF1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "CHTFParF1", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_CHTFParF2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "CHTFParF2", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_ChtfPar_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ChtfPar", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_ChtfParPF_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ChtfParPF", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_CtOpF_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "CtOpF", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_CtPar_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "CtPar", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_CtParF0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "CtParF0", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_CtParF1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "CtParF1", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_CtParF2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "CtParF2", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_CtParPF_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "CtParPF", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_HhtfPar_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HhtfPar", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_HhtfParF0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HhtfParF0", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_HhtfParF1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HhtfParF1", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_HhtfParF2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HhtfParF2", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_HhtfParPF_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HhtfParPF", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_HtrPar_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HtrPar", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_HtrParF0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HtrParF0", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_HtrParF1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HtrParF1", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_HtrParF2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HtrParF2", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_HtrParPF_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HtrParPF", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_PbFixPar_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "PbFixPar", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_SfPar_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "SfPar", number);
	});
}

SAM_EXPORT void SAM_TcstroughEmpirical_Parasitic_SfParPF_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "SfParPF", number);
	});
}

SAM_EXPORT double SAM_TcstroughEmpirical_Weather_azimuth_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "azimuth", &result))
		make_access_error("SAM_TcstroughEmpirical", "azimuth");
	});
	return result;
}



SAM_EXPORT const char* SAM_TcstroughEmpirical_Weather_file_name_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "file_name");
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "file_name");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Weather_tilt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tilt", &result))
		make_access_error("SAM_TcstroughEmpirical", "tilt");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Weather_track_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "track_mode", &result))
		make_access_error("SAM_TcstroughEmpirical", "track_mode");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Trough_system_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_TcstroughEmpirical", "system_capacity");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_TouTranslator_weekday_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "weekday_schedule", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "weekday_schedule");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_TouTranslator_weekend_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "weekend_schedule", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "weekend_schedule");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_DepAngle_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "DepAngle", &result))
		make_access_error("SAM_TcstroughEmpirical", "DepAngle");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_Distance_SCA_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Distance_SCA", &result))
		make_access_error("SAM_TcstroughEmpirical", "Distance_SCA");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_HTFFluid_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTFFluid", &result))
		make_access_error("SAM_TcstroughEmpirical", "HTFFluid");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_HtfGalArea_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HtfGalArea", &result))
		make_access_error("SAM_TcstroughEmpirical", "HtfGalArea");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_MinHtfTemp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "MinHtfTemp", &result))
		make_access_error("SAM_TcstroughEmpirical", "MinHtfTemp");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_NumScas_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "NumScas", &result))
		make_access_error("SAM_TcstroughEmpirical", "NumScas");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_Row_Distance_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Row_Distance", &result))
		make_access_error("SAM_TcstroughEmpirical", "Row_Distance");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_SFTempInit_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "SFTempInit", &result))
		make_access_error("SAM_TcstroughEmpirical", "SFTempInit");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_SfInTempD_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "SfInTempD", &result))
		make_access_error("SAM_TcstroughEmpirical", "SfInTempD");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_SfOutTempD_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "SfOutTempD", &result))
		make_access_error("SAM_TcstroughEmpirical", "SfOutTempD");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_SfPipeHl1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "SfPipeHl1", &result))
		make_access_error("SAM_TcstroughEmpirical", "SfPipeHl1");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_SfPipeHl2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "SfPipeHl2", &result))
		make_access_error("SAM_TcstroughEmpirical", "SfPipeHl2");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_SfPipeHl3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "SfPipeHl3", &result))
		make_access_error("SAM_TcstroughEmpirical", "SfPipeHl3");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_SfPipeHl300_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "SfPipeHl300", &result))
		make_access_error("SAM_TcstroughEmpirical", "SfPipeHl300");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_Solar_Field_Area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Solar_Field_Area", &result))
		make_access_error("SAM_TcstroughEmpirical", "Solar_Field_Area");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_Solar_Field_Mult_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Solar_Field_Mult", &result))
		make_access_error("SAM_TcstroughEmpirical", "Solar_Field_Mult");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_Stow_Angle_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Stow_Angle", &result))
		make_access_error("SAM_TcstroughEmpirical", "Stow_Angle");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Solarfield_i_SfTi_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "i_SfTi", &result))
		make_access_error("SAM_TcstroughEmpirical", "i_SfTi");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Sca_Ave_Focal_Length_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Ave_Focal_Length", &result))
		make_access_error("SAM_TcstroughEmpirical", "Ave_Focal_Length");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Sca_ConcFac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ConcFac", &result))
		make_access_error("SAM_TcstroughEmpirical", "ConcFac");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Sca_GeoAcc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "GeoAcc", &result))
		make_access_error("SAM_TcstroughEmpirical", "GeoAcc");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Sca_IamF0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "IamF0", &result))
		make_access_error("SAM_TcstroughEmpirical", "IamF0");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Sca_IamF1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "IamF1", &result))
		make_access_error("SAM_TcstroughEmpirical", "IamF1");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Sca_IamF2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "IamF2", &result))
		make_access_error("SAM_TcstroughEmpirical", "IamF2");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Sca_MirCln_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "MirCln", &result))
		make_access_error("SAM_TcstroughEmpirical", "MirCln");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Sca_MirRef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "MirRef", &result))
		make_access_error("SAM_TcstroughEmpirical", "MirRef");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Sca_SCA_aper_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "SCA_aper", &result))
		make_access_error("SAM_TcstroughEmpirical", "SCA_aper");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Sca_ScaLen_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ScaLen", &result))
		make_access_error("SAM_TcstroughEmpirical", "ScaLen");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Sca_SfAvail_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "SfAvail", &result))
		make_access_error("SAM_TcstroughEmpirical", "SfAvail");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Sca_TrkTwstErr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "TrkTwstErr", &result))
		make_access_error("SAM_TcstroughEmpirical", "TrkTwstErr");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_HCEBelShad_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "HCEBelShad", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "HCEBelShad");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_HCEEnvTrans_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "HCEEnvTrans", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "HCEEnvTrans");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_HCEFrac_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "HCEFrac", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "HCEFrac");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_HCE_A0_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "HCE_A0", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "HCE_A0");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_HCE_A1_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "HCE_A1", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "HCE_A1");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_HCE_A2_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "HCE_A2", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "HCE_A2");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_HCE_A3_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "HCE_A3", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "HCE_A3");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_HCE_A4_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "HCE_A4", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "HCE_A4");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_HCE_A5_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "HCE_A5", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "HCE_A5");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_HCE_A6_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "HCE_A6", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "HCE_A6");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_HCEabs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "HCEabs", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "HCEabs");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_HCEdust_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "HCEdust", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "HCEdust");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_HCEmisc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "HCEmisc", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "HCEmisc");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_HCEtype_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "HCEtype", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "HCEtype");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Hce_NumHCETypes_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "NumHCETypes", &result))
		make_access_error("SAM_TcstroughEmpirical", "NumHCETypes");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_PerfFac_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "PerfFac", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "PerfFac");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Hce_RefMirrAper_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "RefMirrAper", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "RefMirrAper");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_E2TPLF0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "E2TPLF0", &result))
		make_access_error("SAM_TcstroughEmpirical", "E2TPLF0");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_E2TPLF1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "E2TPLF1", &result))
		make_access_error("SAM_TcstroughEmpirical", "E2TPLF1");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_E2TPLF2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "E2TPLF2", &result))
		make_access_error("SAM_TcstroughEmpirical", "E2TPLF2");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_E2TPLF3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "E2TPLF3", &result))
		make_access_error("SAM_TcstroughEmpirical", "E2TPLF3");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_E2TPLF4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "E2TPLF4", &result))
		make_access_error("SAM_TcstroughEmpirical", "E2TPLF4");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_LHVBoilEff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LHVBoilEff", &result))
		make_access_error("SAM_TcstroughEmpirical", "LHVBoilEff");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_MaxGrOut_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "MaxGrOut", &result))
		make_access_error("SAM_TcstroughEmpirical", "MaxGrOut");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_MinGrOut_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "MinGrOut", &result))
		make_access_error("SAM_TcstroughEmpirical", "MinGrOut");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_PTTMAX_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "PTTMAX", &result))
		make_access_error("SAM_TcstroughEmpirical", "PTTMAX");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_PTTMIN_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "PTTMIN", &result))
		make_access_error("SAM_TcstroughEmpirical", "PTTMIN");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_T2EPLF0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T2EPLF0", &result))
		make_access_error("SAM_TcstroughEmpirical", "T2EPLF0");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_T2EPLF1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T2EPLF1", &result))
		make_access_error("SAM_TcstroughEmpirical", "T2EPLF1");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_T2EPLF2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T2EPLF2", &result))
		make_access_error("SAM_TcstroughEmpirical", "T2EPLF2");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_T2EPLF3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T2EPLF3", &result))
		make_access_error("SAM_TcstroughEmpirical", "T2EPLF3");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_T2EPLF4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T2EPLF4", &result))
		make_access_error("SAM_TcstroughEmpirical", "T2EPLF4");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_TempCorr0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "TempCorr0", &result))
		make_access_error("SAM_TcstroughEmpirical", "TempCorr0");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_TempCorr1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "TempCorr1", &result))
		make_access_error("SAM_TcstroughEmpirical", "TempCorr1");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_TempCorr2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "TempCorr2", &result))
		make_access_error("SAM_TcstroughEmpirical", "TempCorr2");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_TempCorr3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "TempCorr3", &result))
		make_access_error("SAM_TcstroughEmpirical", "TempCorr3");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_TempCorr4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "TempCorr4", &result))
		make_access_error("SAM_TcstroughEmpirical", "TempCorr4");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_TempCorrF_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "TempCorrF", &result))
		make_access_error("SAM_TcstroughEmpirical", "TempCorrF");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_TurSUE_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "TurSUE", &result))
		make_access_error("SAM_TcstroughEmpirical", "TurSUE");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_TurbEffG_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "TurbEffG", &result))
		make_access_error("SAM_TcstroughEmpirical", "TurbEffG");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Pwrb_TurbOutG_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "TurbOutG", &result))
		make_access_error("SAM_TcstroughEmpirical", "TurbOutG");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Tes_E_tes_ini_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "E_tes_ini", &result))
		make_access_error("SAM_TcstroughEmpirical", "E_tes_ini");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Tes_FossilFill_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "FossilFill", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "FossilFill");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Tes_NUMTOU_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "NUMTOU", &result))
		make_access_error("SAM_TcstroughEmpirical", "NUMTOU");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Tes_PFSmax_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "PFSmax", &result))
		make_access_error("SAM_TcstroughEmpirical", "PFSmax");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Tes_PTSmax_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "PTSmax", &result))
		make_access_error("SAM_TcstroughEmpirical", "PTSmax");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Tes_TSHOURS_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "TSHOURS", &result))
		make_access_error("SAM_TcstroughEmpirical", "TSHOURS");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Tes_TSLogic_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "TSLogic", nrows, ncols);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "TSLogic");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Tes_TnkHL_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "TnkHL", &result))
		make_access_error("SAM_TcstroughEmpirical", "TnkHL");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Tes_TurTesEffAdj_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "TurTesEffAdj", &result))
		make_access_error("SAM_TcstroughEmpirical", "TurTesEffAdj");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Tes_TurTesOutAdj_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "TurTesOutAdj", &result))
		make_access_error("SAM_TcstroughEmpirical", "TurTesOutAdj");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_AntiFrPar_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "AntiFrPar", &result))
		make_access_error("SAM_TcstroughEmpirical", "AntiFrPar");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_BOPPar_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "BOPPar", &result))
		make_access_error("SAM_TcstroughEmpirical", "BOPPar");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_BOPParF0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "BOPParF0", &result))
		make_access_error("SAM_TcstroughEmpirical", "BOPParF0");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_BOPParF1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "BOPParF1", &result))
		make_access_error("SAM_TcstroughEmpirical", "BOPParF1");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_BOPParF2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "BOPParF2", &result))
		make_access_error("SAM_TcstroughEmpirical", "BOPParF2");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_BOPParPF_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "BOPParPF", &result))
		make_access_error("SAM_TcstroughEmpirical", "BOPParPF");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_CHTFParF0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "CHTFParF0", &result))
		make_access_error("SAM_TcstroughEmpirical", "CHTFParF0");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_CHTFParF1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "CHTFParF1", &result))
		make_access_error("SAM_TcstroughEmpirical", "CHTFParF1");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_CHTFParF2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "CHTFParF2", &result))
		make_access_error("SAM_TcstroughEmpirical", "CHTFParF2");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_ChtfPar_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ChtfPar", &result))
		make_access_error("SAM_TcstroughEmpirical", "ChtfPar");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_ChtfParPF_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ChtfParPF", &result))
		make_access_error("SAM_TcstroughEmpirical", "ChtfParPF");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_CtOpF_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "CtOpF", &result))
		make_access_error("SAM_TcstroughEmpirical", "CtOpF");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_CtPar_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "CtPar", &result))
		make_access_error("SAM_TcstroughEmpirical", "CtPar");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_CtParF0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "CtParF0", &result))
		make_access_error("SAM_TcstroughEmpirical", "CtParF0");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_CtParF1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "CtParF1", &result))
		make_access_error("SAM_TcstroughEmpirical", "CtParF1");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_CtParF2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "CtParF2", &result))
		make_access_error("SAM_TcstroughEmpirical", "CtParF2");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_CtParPF_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "CtParPF", &result))
		make_access_error("SAM_TcstroughEmpirical", "CtParPF");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_HhtfPar_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HhtfPar", &result))
		make_access_error("SAM_TcstroughEmpirical", "HhtfPar");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_HhtfParF0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HhtfParF0", &result))
		make_access_error("SAM_TcstroughEmpirical", "HhtfParF0");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_HhtfParF1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HhtfParF1", &result))
		make_access_error("SAM_TcstroughEmpirical", "HhtfParF1");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_HhtfParF2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HhtfParF2", &result))
		make_access_error("SAM_TcstroughEmpirical", "HhtfParF2");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_HhtfParPF_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HhtfParPF", &result))
		make_access_error("SAM_TcstroughEmpirical", "HhtfParPF");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_HtrPar_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HtrPar", &result))
		make_access_error("SAM_TcstroughEmpirical", "HtrPar");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_HtrParF0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HtrParF0", &result))
		make_access_error("SAM_TcstroughEmpirical", "HtrParF0");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_HtrParF1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HtrParF1", &result))
		make_access_error("SAM_TcstroughEmpirical", "HtrParF1");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_HtrParF2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HtrParF2", &result))
		make_access_error("SAM_TcstroughEmpirical", "HtrParF2");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_HtrParPF_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HtrParPF", &result))
		make_access_error("SAM_TcstroughEmpirical", "HtrParPF");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_PbFixPar_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "PbFixPar", &result))
		make_access_error("SAM_TcstroughEmpirical", "PbFixPar");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_SfPar_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "SfPar", &result))
		make_access_error("SAM_TcstroughEmpirical", "SfPar");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Parasitic_SfParPF_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "SfParPF", &result))
		make_access_error("SAM_TcstroughEmpirical", "SfParPF");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_AveSfTemp_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "AveSfTemp", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "AveSfTemp");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_ColEff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ColEff", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "ColEff");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_CosTheta_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "CosTheta", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "CosTheta");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_Egr_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Egr", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "Egr");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_EgrFos_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "EgrFos", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "EgrFos");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_EgrSol_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "EgrSol", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "EgrSol");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_EndLoss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "EndLoss", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "EndLoss");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_Enet_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Enet", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "Enet");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_Epar_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Epar", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "Epar");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_EparAnti_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "EparAnti", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "EparAnti");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_EparBOP_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "EparBOP", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "EparBOP");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_EparCHTF_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "EparCHTF", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "EparCHTF");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_EparCT_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "EparCT", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "EparCT");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_EparHhtf_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "EparHhtf", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "EparHhtf");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_EparHtr_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "EparHtr", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "EparHtr");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_EparOffLine_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "EparOffLine", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "EparOffLine");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_EparOnLine_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "EparOnLine", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "EparOnLine");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_EparPB_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "EparPB", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "EparPB");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_EparSf_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "EparSf", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "EparSf");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_Ets_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Ets", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "Ets");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_Ftrack_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Ftrack", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "Ftrack");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_IAM_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "IAM", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "IAM");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_QTsFull_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "QTsFull", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "QTsFull");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_QTsHl_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "QTsHl", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "QTsHl");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_QTurSu_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "QTurSu", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "QTurSu");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_Qdni_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Qdni", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "Qdni");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_Qdump_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Qdump", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "Qdump");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_Qfts_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Qfts", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "Qfts");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_Qgas_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Qgas", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "Qgas");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_QhtfFpHtr_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "QhtfFpHtr", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "QhtfFpHtr");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_QhtfFpTES_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "QhtfFpTES", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "QhtfFpTES");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_QhtfFreezeProt_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "QhtfFreezeProt", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "QhtfFreezeProt");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_Qmin_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Qmin", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "Qmin");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_QnipCosTh_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "QnipCosTh", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "QnipCosTh");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_Qsf_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Qsf", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "Qsf");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_QsfAbs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "QsfAbs", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "QsfAbs");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_QsfHceHL_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "QsfHceHL", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "QsfHceHL");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_QsfPipeHL_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "QsfPipeHL", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "QsfPipeHL");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_QsfWarmup_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "QsfWarmup", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "QsfWarmup");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_Qsfnipcosth_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Qsfnipcosth", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "Qsfnipcosth");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_Qtpb_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Qtpb", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "Qtpb");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_Qtts_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Qtts", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "Qtts");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_RecHl_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "RecHl", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "RecHl");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_RowShadow_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "RowShadow", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "RowShadow");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_SfMassFlow_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "SfMassFlow", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "SfMassFlow");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_SfTo_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "SfTo", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "SfTo");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_Theta_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Theta", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "Theta");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_TrackAngle_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "TrackAngle", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "TrackAngle");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Outputs_annual_W_cycle_gross_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_W_cycle_gross", &result))
		make_access_error("SAM_TcstroughEmpirical", "annual_W_cycle_gross");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_TcstroughEmpirical", "annual_energy");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Outputs_annual_fuel_usage_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_fuel_usage", &result))
		make_access_error("SAM_TcstroughEmpirical", "annual_fuel_usage");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_beam_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "beam", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "beam");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_TcstroughEmpirical", "capacity_factor");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Outputs_conversion_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "conversion_factor", &result))
		make_access_error("SAM_TcstroughEmpirical", "conversion_factor");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "gen");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_hour_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hour", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "hour");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "kwh_per_kw", &result))
		make_access_error("SAM_TcstroughEmpirical", "kwh_per_kw");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_month_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "month", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "month");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_monthly_energy_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_energy", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "monthly_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_o_SfTi_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "o_SfTi", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "o_SfTi");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_pres_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pres", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "pres");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_solazi_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solazi", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "solazi");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_solzen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solzen", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "solzen");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Outputs_system_heat_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_heat_rate", &result))
		make_access_error("SAM_TcstroughEmpirical", "system_heat_rate");
	});
	return result;
}



SAM_EXPORT double SAM_TcstroughEmpirical_Outputs_system_use_lifetime_output_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_use_lifetime_output", &result))
		make_access_error("SAM_TcstroughEmpirical", "system_use_lifetime_output");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_tdry_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tdry", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "tdry");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_tou_value_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tou_value", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "tou_value");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_twet_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "twet", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "twet");
	});
	return result;
}



SAM_EXPORT double* SAM_TcstroughEmpirical_Outputs_wspd_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wspd", length);
	if (!result)
		make_access_error("SAM_TcstroughEmpirical", "wspd");
	});
	return result;
}




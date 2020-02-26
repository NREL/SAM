#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_TcslinearFresnel.h"

SAM_EXPORT SAM_TcslinearFresnel SAM_TcslinearFresnel_construct(const char *def, SAM_error *err) {
    SAM_TcslinearFresnel result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_create();
    });
    return result;
}

SAM_EXPORT int SAM_TcslinearFresnel_execute(SAM_TcslinearFresnel data, int verbosity, SAM_error *err) {
    int n_err = 0;
    translateExceptions(err, [&] {
        n_err += SAM_module_exec("tcslinear_fresnel", data, verbosity, err);
    });
    return n_err;
}


SAM_EXPORT void SAM_TcslinearFresnel_destruct(SAM_TcslinearFresnel system) {
    ssc_data_free(system);
}

SAM_EXPORT void SAM_TcslinearFresnel_Weather_azimuth_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "azimuth", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Weather_file_name_sset(SAM_TcslinearFresnel ptr, const char *str, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_string(ptr, "file_name", str);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Weather_tilt_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "tilt", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Weather_track_mode_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "track_mode", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_LinearFresnelr_system_capacity_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "system_capacity", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_TouTranslator_weekday_schedule_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                         SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "weekday_schedule", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_TouTranslator_weekend_schedule_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                         SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "weekend_schedule", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_A_aperture_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "A_aperture", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_AbsorberMaterial_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                      SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "AbsorberMaterial", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_AnnulusGas_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "AnnulusGas", mat, nrows, ncols);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_ColAz_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "ColAz", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_D_2_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "D_2", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_D_3_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "D_3", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_D_4_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "D_4", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_D_5_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "D_5", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_D_p_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "D_p", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_Design_loss_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                 SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "Design_loss", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_Dirt_HCE_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                              SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "Dirt_HCE", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_EPSILON_4_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                               SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "EPSILON_4", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_Flow_type_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                               SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "Flow_type", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_GeomEffects_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                 SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "GeomEffects", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_GlazingIntactIn_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                     SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "GlazingIntactIn", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_HCE_FieldFrac_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                   SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "HCE_FieldFrac", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_HLCharType_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "HLCharType", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_HL_W_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "HL_W", mat, nrows, ncols);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_HL_dT_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                           SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "HL_dT", mat, nrows, ncols);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_IAM_L_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                           SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "IAM_L", mat, nrows, ncols);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_IAM_T_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                           SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "IAM_T", mat, nrows, ncols);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_I_bn_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "I_bn", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_I_bn_des_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "I_bn_des", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_LHV_eff_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "LHV_eff", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_L_col_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                           SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "L_col", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_OptCharType_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                 SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "OptCharType", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_PB_fixed_par_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "PB_fixed_par", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_PB_pump_coef_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "PB_pump_coef", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_P_a_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "P_a", mat, nrows, ncols);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_P_amb_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "P_amb", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_P_turb_des_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "P_turb_des", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_Pipe_hl_coef_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "Pipe_hl_coef", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_Rough_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                           SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "Rough", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_SCA_drives_elec_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "SCA_drives_elec", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_Shadowing_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                               SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "Shadowing", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_SolarAz_init_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "SolarAz_init", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_SolarZen_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "SolarZen", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_T_amb_des_sf_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "T_amb_des_sf", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_T_db_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "T_db", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_T_dp_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "T_dp", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_T_fp_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "T_fp", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_T_pb_out_init_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "T_pb_out_init", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_Tau_envelope_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                  SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "Tau_envelope", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_TrackingError_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                   SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "TrackingError", mat, nrows, ncols);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_V_wind_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "V_wind", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_V_wind_max_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "V_wind_max", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_alpha_abs_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                               SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "alpha_abs", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_alpha_env_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                               SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "alpha_env", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_aux_array_aset(SAM_TcslinearFresnel ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "aux_array", arr, length);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_b_OpticalTable_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                    SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "b_OpticalTable", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_b_eps_HCE1_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "b_eps_HCE1", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_b_eps_HCE2_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "b_eps_HCE2", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_b_eps_HCE3_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "b_eps_HCE3", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_b_eps_HCE4_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "b_eps_HCE4", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_bop_array_aset(SAM_TcslinearFresnel ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "bop_array", arr, length);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_cycle_cutoff_frac_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "cycle_cutoff_frac", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_cycle_max_fraction_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "cycle_max_fraction", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_dirt_mirror_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                 SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "dirt_mirror", mat, nrows, ncols);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_dnifc_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "dnifc", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_e_startup_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "e_startup", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_error_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                           SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "error", mat, nrows, ncols);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_eta_pump_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "eta_pump", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_fP_boil_to_sh_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "fP_boil_to_sh", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_fP_hdr_c_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "fP_hdr_c", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_fP_hdr_h_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "fP_hdr_h", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_fP_sf_boil_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "fP_sf_boil", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_fP_sf_sh_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "fP_sf_sh", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_ffrac_aset(SAM_TcslinearFresnel ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "ffrac", arr, length);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_fossil_mode_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "fossil_mode", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_is_multgeom_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "is_multgeom", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_is_oncethru_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "is_oncethru", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_is_sh_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "is_sh", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_latitude_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "latitude", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_m_dot_htf_ref_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "m_dot_htf_ref", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_m_dot_min_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "m_dot_min", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_m_pb_demand_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "m_pb_demand", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_nLoops_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "nLoops", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_nModBoil_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "nModBoil", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_nModSH_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "nModSH", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_q_max_aux_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "q_max_aux", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_q_pb_des_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "q_pb_des", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_q_sby_frac_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "q_sby_frac", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_rho_mirror_clean_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                      SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "rho_mirror_clean", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_sh_OpticalTable_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                     SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "sh_OpticalTable", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_sh_eps_HCE1_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                 SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "sh_eps_HCE1", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_sh_eps_HCE2_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                 SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "sh_eps_HCE2", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_sh_eps_HCE3_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                 SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "sh_eps_HCE3", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_sh_eps_HCE4_mset(SAM_TcslinearFresnel ptr, double *mat, int nrows, int ncols,
                                                 SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "sh_eps_HCE4", mat, nrows, ncols);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_shift_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "shift", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_solarm_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "solarm", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_t_sby_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "t_sby", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_tes_hours_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "tes_hours", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_theta_dep_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "theta_dep", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Solarfield_theta_stow_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "theta_stow", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Solarfield_x_b_des_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "x_b_des", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Heliostat_csp_lf_sf_washes_per_year_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "csp.lf.sf.washes_per_year", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Heliostat_csp_lf_sf_water_per_wash_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "csp.lf.sf.water_per_wash", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_CT_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "CT", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Powerblock_F_wc_aset(SAM_TcslinearFresnel ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "F_wc", arr, length);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Powerblock_P_amb_pwb_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "P_amb_pwb", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Powerblock_P_boil_des_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "P_boil_des", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Powerblock_P_cond_min_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "P_cond_min", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Powerblock_P_cond_ratio_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "P_cond_ratio", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_P_rh_ref_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "P_rh_ref", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Powerblock_T_ITD_des_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "T_ITD_des", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Powerblock_T_amb_des_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "T_amb_des", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Powerblock_T_approach_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "T_approach", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Powerblock_T_cold_ref_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "T_cold_ref", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_T_db_pwb_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "T_db_pwb", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_T_hot_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "T_hot", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_T_wb_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "T_wb", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Powerblock_dT_cw_ref_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "dT_cw_ref", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Powerblock_demand_var_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "demand_var", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_dp_b_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "dp_b", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_dp_rh_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "dp_rh", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_dp_sh_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "dp_sh", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_eta_ref_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "eta_ref", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_f_recSU_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "f_recSU", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_m_dot_st_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "m_dot_st", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_n_pl_inc_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "n_pl_inc", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Powerblock_pb_bd_frac_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "pb_bd_frac", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_pc_mode_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "pc_mode", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Powerblock_q_sby_frac_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "q_sby_frac", number);
    });
}

SAM_EXPORT void SAM_TcslinearFresnel_Powerblock_relhum_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "relhum", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Powerblock_rh_frac_ref_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "rh_frac_ref", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Powerblock_standby_control_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "standby_control", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Powerblock_startup_frac_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "startup_frac", number);
    });
}

SAM_EXPORT void
SAM_TcslinearFresnel_Powerblock_startup_time_nset(SAM_TcslinearFresnel ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "startup_time", number);
    });
}

SAM_EXPORT double SAM_TcslinearFresnel_Weather_azimuth_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "azimuth", &result))
            make_access_error("SAM_TcslinearFresnel", "azimuth");
    });
    return result;
}


SAM_EXPORT const char *SAM_TcslinearFresnel_Weather_file_name_sget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    const char *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_string(ptr, "file_name");
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "file_name");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Weather_tilt_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "tilt", &result))
            make_access_error("SAM_TcslinearFresnel", "tilt");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Weather_track_mode_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "track_mode", &result))
            make_access_error("SAM_TcslinearFresnel", "track_mode");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_LinearFresnelr_system_capacity_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "system_capacity", &result))
            make_access_error("SAM_TcslinearFresnel", "system_capacity");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_TouTranslator_weekday_schedule_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols,
                                                         SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "weekday_schedule", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "weekday_schedule");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_TouTranslator_weekend_schedule_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols,
                                                         SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "weekend_schedule", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "weekend_schedule");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_A_aperture_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "A_aperture", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "A_aperture");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_AbsorberMaterial_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols,
                                                      SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "AbsorberMaterial", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "AbsorberMaterial");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_AnnulusGas_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "AnnulusGas", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "AnnulusGas");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_ColAz_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "ColAz", &result))
            make_access_error("SAM_TcslinearFresnel", "ColAz");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_D_2_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "D_2", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "D_2");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_D_3_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "D_3", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "D_3");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_D_4_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "D_4", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "D_4");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_D_5_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "D_5", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "D_5");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_D_p_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "D_p", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "D_p");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_Design_loss_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "Design_loss", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "Design_loss");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_Dirt_HCE_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "Dirt_HCE", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "Dirt_HCE");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_EPSILON_4_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "EPSILON_4", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "EPSILON_4");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_Flow_type_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "Flow_type", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "Flow_type");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_GeomEffects_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "GeomEffects", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "GeomEffects");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_GlazingIntactIn_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "GlazingIntactIn", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "GlazingIntactIn");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_HCE_FieldFrac_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "HCE_FieldFrac", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "HCE_FieldFrac");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_HLCharType_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "HLCharType", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "HLCharType");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_HL_W_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "HL_W", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "HL_W");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_HL_dT_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "HL_dT", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "HL_dT");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_IAM_L_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "IAM_L", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "IAM_L");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_IAM_T_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "IAM_T", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "IAM_T");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_I_bn_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "I_bn", &result))
            make_access_error("SAM_TcslinearFresnel", "I_bn");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_I_bn_des_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "I_bn_des", &result))
            make_access_error("SAM_TcslinearFresnel", "I_bn_des");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_LHV_eff_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "LHV_eff", &result))
            make_access_error("SAM_TcslinearFresnel", "LHV_eff");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_L_col_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "L_col", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "L_col");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_OptCharType_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "OptCharType", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "OptCharType");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_PB_fixed_par_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "PB_fixed_par", &result))
            make_access_error("SAM_TcslinearFresnel", "PB_fixed_par");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_PB_pump_coef_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "PB_pump_coef", &result))
            make_access_error("SAM_TcslinearFresnel", "PB_pump_coef");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_P_a_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "P_a", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "P_a");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_P_amb_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "P_amb", &result))
            make_access_error("SAM_TcslinearFresnel", "P_amb");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_P_turb_des_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "P_turb_des", &result))
            make_access_error("SAM_TcslinearFresnel", "P_turb_des");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_Pipe_hl_coef_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "Pipe_hl_coef", &result))
            make_access_error("SAM_TcslinearFresnel", "Pipe_hl_coef");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_Rough_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "Rough", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "Rough");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_SCA_drives_elec_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "SCA_drives_elec", &result))
            make_access_error("SAM_TcslinearFresnel", "SCA_drives_elec");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_Shadowing_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "Shadowing", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "Shadowing");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_SolarAz_init_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "SolarAz_init", &result))
            make_access_error("SAM_TcslinearFresnel", "SolarAz_init");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_SolarZen_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "SolarZen", &result))
            make_access_error("SAM_TcslinearFresnel", "SolarZen");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_T_amb_des_sf_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "T_amb_des_sf", &result))
            make_access_error("SAM_TcslinearFresnel", "T_amb_des_sf");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_T_db_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "T_db", &result))
            make_access_error("SAM_TcslinearFresnel", "T_db");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_T_dp_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "T_dp", &result))
            make_access_error("SAM_TcslinearFresnel", "T_dp");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_T_fp_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "T_fp", &result))
            make_access_error("SAM_TcslinearFresnel", "T_fp");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_T_pb_out_init_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "T_pb_out_init", &result))
            make_access_error("SAM_TcslinearFresnel", "T_pb_out_init");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_Tau_envelope_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "Tau_envelope", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "Tau_envelope");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_TrackingError_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "TrackingError", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "TrackingError");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_V_wind_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "V_wind", &result))
            make_access_error("SAM_TcslinearFresnel", "V_wind");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_V_wind_max_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "V_wind_max", &result))
            make_access_error("SAM_TcslinearFresnel", "V_wind_max");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_alpha_abs_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "alpha_abs", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "alpha_abs");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_alpha_env_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "alpha_env", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "alpha_env");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_aux_array_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "aux_array", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "aux_array");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_b_OpticalTable_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "b_OpticalTable", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "b_OpticalTable");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_b_eps_HCE1_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "b_eps_HCE1", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "b_eps_HCE1");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_b_eps_HCE2_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "b_eps_HCE2", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "b_eps_HCE2");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_b_eps_HCE3_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "b_eps_HCE3", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "b_eps_HCE3");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_b_eps_HCE4_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "b_eps_HCE4", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "b_eps_HCE4");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_bop_array_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "bop_array", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "bop_array");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_cycle_cutoff_frac_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "cycle_cutoff_frac", &result))
            make_access_error("SAM_TcslinearFresnel", "cycle_cutoff_frac");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_cycle_max_fraction_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "cycle_max_fraction", &result))
            make_access_error("SAM_TcslinearFresnel", "cycle_max_fraction");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_dirt_mirror_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "dirt_mirror", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "dirt_mirror");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_dnifc_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "dnifc", &result))
            make_access_error("SAM_TcslinearFresnel", "dnifc");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_e_startup_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "e_startup", &result))
            make_access_error("SAM_TcslinearFresnel", "e_startup");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_error_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "error", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "error");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_eta_pump_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "eta_pump", &result))
            make_access_error("SAM_TcslinearFresnel", "eta_pump");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_fP_boil_to_sh_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "fP_boil_to_sh", &result))
            make_access_error("SAM_TcslinearFresnel", "fP_boil_to_sh");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_fP_hdr_c_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "fP_hdr_c", &result))
            make_access_error("SAM_TcslinearFresnel", "fP_hdr_c");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_fP_hdr_h_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "fP_hdr_h", &result))
            make_access_error("SAM_TcslinearFresnel", "fP_hdr_h");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_fP_sf_boil_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "fP_sf_boil", &result))
            make_access_error("SAM_TcslinearFresnel", "fP_sf_boil");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_fP_sf_sh_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "fP_sf_sh", &result))
            make_access_error("SAM_TcslinearFresnel", "fP_sf_sh");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Solarfield_ffrac_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "ffrac", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "ffrac");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_fossil_mode_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "fossil_mode", &result))
            make_access_error("SAM_TcslinearFresnel", "fossil_mode");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_is_multgeom_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "is_multgeom", &result))
            make_access_error("SAM_TcslinearFresnel", "is_multgeom");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_is_oncethru_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "is_oncethru", &result))
            make_access_error("SAM_TcslinearFresnel", "is_oncethru");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_is_sh_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "is_sh", &result))
            make_access_error("SAM_TcslinearFresnel", "is_sh");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_latitude_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "latitude", &result))
            make_access_error("SAM_TcslinearFresnel", "latitude");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_m_dot_htf_ref_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "m_dot_htf_ref", &result))
            make_access_error("SAM_TcslinearFresnel", "m_dot_htf_ref");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_m_dot_min_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "m_dot_min", &result))
            make_access_error("SAM_TcslinearFresnel", "m_dot_min");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_m_pb_demand_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "m_pb_demand", &result))
            make_access_error("SAM_TcslinearFresnel", "m_pb_demand");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_nLoops_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "nLoops", &result))
            make_access_error("SAM_TcslinearFresnel", "nLoops");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_nModBoil_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "nModBoil", &result))
            make_access_error("SAM_TcslinearFresnel", "nModBoil");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_nModSH_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "nModSH", &result))
            make_access_error("SAM_TcslinearFresnel", "nModSH");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_q_max_aux_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "q_max_aux", &result))
            make_access_error("SAM_TcslinearFresnel", "q_max_aux");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_q_pb_des_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "q_pb_des", &result))
            make_access_error("SAM_TcslinearFresnel", "q_pb_des");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_q_sby_frac_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "q_sby_frac", &result))
            make_access_error("SAM_TcslinearFresnel", "q_sby_frac");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_rho_mirror_clean_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols,
                                                      SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "rho_mirror_clean", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "rho_mirror_clean");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_sh_OpticalTable_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "sh_OpticalTable", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "sh_OpticalTable");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_sh_eps_HCE1_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "sh_eps_HCE1", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "sh_eps_HCE1");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_sh_eps_HCE2_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "sh_eps_HCE2", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "sh_eps_HCE2");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_sh_eps_HCE3_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "sh_eps_HCE3", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "sh_eps_HCE3");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Solarfield_sh_eps_HCE4_mget(SAM_TcslinearFresnel ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "sh_eps_HCE4", nrows, ncols);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "sh_eps_HCE4");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_shift_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "shift", &result))
            make_access_error("SAM_TcslinearFresnel", "shift");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_solarm_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "solarm", &result))
            make_access_error("SAM_TcslinearFresnel", "solarm");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_t_sby_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "t_sby", &result))
            make_access_error("SAM_TcslinearFresnel", "t_sby");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_tes_hours_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "tes_hours", &result))
            make_access_error("SAM_TcslinearFresnel", "tes_hours");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_theta_dep_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "theta_dep", &result))
            make_access_error("SAM_TcslinearFresnel", "theta_dep");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_theta_stow_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "theta_stow", &result))
            make_access_error("SAM_TcslinearFresnel", "theta_stow");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Solarfield_x_b_des_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "x_b_des", &result))
            make_access_error("SAM_TcslinearFresnel", "x_b_des");
    });
    return result;
}


SAM_EXPORT double
SAM_TcslinearFresnel_Heliostat_csp_lf_sf_washes_per_year_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "csp.lf.sf.washes_per_year", &result))
            make_access_error("SAM_TcslinearFresnel", "csp.lf.sf.washes_per_year");
    });
    return result;
}


SAM_EXPORT double
SAM_TcslinearFresnel_Heliostat_csp_lf_sf_water_per_wash_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "csp.lf.sf.water_per_wash", &result))
            make_access_error("SAM_TcslinearFresnel", "csp.lf.sf.water_per_wash");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_CT_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "CT", &result))
            make_access_error("SAM_TcslinearFresnel", "CT");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Powerblock_F_wc_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "F_wc", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "F_wc");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_P_amb_pwb_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "P_amb_pwb", &result))
            make_access_error("SAM_TcslinearFresnel", "P_amb_pwb");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_P_boil_des_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "P_boil_des", &result))
            make_access_error("SAM_TcslinearFresnel", "P_boil_des");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_P_cond_min_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "P_cond_min", &result))
            make_access_error("SAM_TcslinearFresnel", "P_cond_min");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_P_cond_ratio_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "P_cond_ratio", &result))
            make_access_error("SAM_TcslinearFresnel", "P_cond_ratio");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_P_rh_ref_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "P_rh_ref", &result))
            make_access_error("SAM_TcslinearFresnel", "P_rh_ref");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_T_ITD_des_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "T_ITD_des", &result))
            make_access_error("SAM_TcslinearFresnel", "T_ITD_des");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_T_amb_des_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "T_amb_des", &result))
            make_access_error("SAM_TcslinearFresnel", "T_amb_des");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_T_approach_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "T_approach", &result))
            make_access_error("SAM_TcslinearFresnel", "T_approach");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_T_cold_ref_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "T_cold_ref", &result))
            make_access_error("SAM_TcslinearFresnel", "T_cold_ref");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_T_db_pwb_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "T_db_pwb", &result))
            make_access_error("SAM_TcslinearFresnel", "T_db_pwb");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_T_hot_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "T_hot", &result))
            make_access_error("SAM_TcslinearFresnel", "T_hot");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_T_wb_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "T_wb", &result))
            make_access_error("SAM_TcslinearFresnel", "T_wb");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_dT_cw_ref_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "dT_cw_ref", &result))
            make_access_error("SAM_TcslinearFresnel", "dT_cw_ref");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_demand_var_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "demand_var", &result))
            make_access_error("SAM_TcslinearFresnel", "demand_var");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_dp_b_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "dp_b", &result))
            make_access_error("SAM_TcslinearFresnel", "dp_b");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_dp_rh_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "dp_rh", &result))
            make_access_error("SAM_TcslinearFresnel", "dp_rh");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_dp_sh_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "dp_sh", &result))
            make_access_error("SAM_TcslinearFresnel", "dp_sh");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_eta_ref_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "eta_ref", &result))
            make_access_error("SAM_TcslinearFresnel", "eta_ref");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_f_recSU_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "f_recSU", &result))
            make_access_error("SAM_TcslinearFresnel", "f_recSU");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_m_dot_st_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "m_dot_st", &result))
            make_access_error("SAM_TcslinearFresnel", "m_dot_st");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_n_pl_inc_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "n_pl_inc", &result))
            make_access_error("SAM_TcslinearFresnel", "n_pl_inc");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_pb_bd_frac_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "pb_bd_frac", &result))
            make_access_error("SAM_TcslinearFresnel", "pb_bd_frac");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_pc_mode_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "pc_mode", &result))
            make_access_error("SAM_TcslinearFresnel", "pc_mode");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_q_sby_frac_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "q_sby_frac", &result))
            make_access_error("SAM_TcslinearFresnel", "q_sby_frac");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_relhum_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "relhum", &result))
            make_access_error("SAM_TcslinearFresnel", "relhum");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_rh_frac_ref_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "rh_frac_ref", &result))
            make_access_error("SAM_TcslinearFresnel", "rh_frac_ref");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_standby_control_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "standby_control", &result))
            make_access_error("SAM_TcslinearFresnel", "standby_control");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_startup_frac_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "startup_frac", &result))
            make_access_error("SAM_TcslinearFresnel", "startup_frac");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Powerblock_startup_time_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "startup_time", &result))
            make_access_error("SAM_TcslinearFresnel", "startup_time");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Outputs_E_bal_startup_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "E_bal_startup", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "E_bal_startup");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_P_cond_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "P_cond", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "P_cond");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_P_sf_in_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "P_sf_in", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "P_sf_in");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_P_turb_in_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "P_turb_in", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "P_turb_in");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_T_field_in_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "T_field_in", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "T_field_in");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Outputs_T_field_out_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "T_field_out", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "T_field_out");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_T_loop_out_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "T_loop_out", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "T_loop_out");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_T_pb_in_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "T_pb_in", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "T_pb_in");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_T_pb_out_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "T_pb_out", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "T_pb_out");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_W_cool_par_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "W_cool_par", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "W_cool_par");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Outputs_W_cycle_gross_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "W_cycle_gross", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "W_cycle_gross");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_W_dot_aux_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "W_dot_aux", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "W_dot_aux");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_W_dot_bop_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "W_dot_bop", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "W_dot_bop");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_W_dot_col_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "W_dot_col", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "W_dot_col");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Outputs_W_dot_fixed_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "W_dot_fixed", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "W_dot_fixed");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_W_dot_pump_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "W_dot_pump", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "W_dot_pump");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_W_net_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "W_net", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "W_net");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Outputs_annual_W_cycle_gross_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "annual_W_cycle_gross", &result))
            make_access_error("SAM_TcslinearFresnel", "annual_W_cycle_gross");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Outputs_annual_energy_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "annual_energy", &result))
            make_access_error("SAM_TcslinearFresnel", "annual_energy");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Outputs_annual_fuel_usage_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "annual_fuel_usage", &result))
            make_access_error("SAM_TcslinearFresnel", "annual_fuel_usage");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Outputs_annual_total_water_use_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "annual_total_water_use", &result))
            make_access_error("SAM_TcslinearFresnel", "annual_total_water_use");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_beam_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "beam", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "beam");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Outputs_capacity_factor_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "capacity_factor", &result))
            make_access_error("SAM_TcslinearFresnel", "capacity_factor");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Outputs_conversion_factor_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "conversion_factor", &result))
            make_access_error("SAM_TcslinearFresnel", "conversion_factor");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_dP_tot_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "dP_tot", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "dP_tot");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_defocus_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "defocus", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "defocus");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_eta_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "eta", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "eta");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Outputs_eta_opt_ave_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "eta_opt_ave", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "eta_opt_ave");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_eta_sf_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "eta_sf", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "eta_sf");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Outputs_eta_thermal_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "eta_thermal", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "eta_thermal");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_f_bays_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "f_bays", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "f_bays");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_gen_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "gen", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "gen");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_hour_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "hour", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "hour");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Outputs_kwh_per_kw_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "kwh_per_kw", &result))
            make_access_error("SAM_TcslinearFresnel", "kwh_per_kw");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_m_dot_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "m_dot", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "m_dot");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_m_dot_aux_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "m_dot_aux", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "m_dot_aux");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Outputs_m_dot_b_tot_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "m_dot_b_tot", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "m_dot_b_tot");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Outputs_m_dot_field_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "m_dot_field", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "m_dot_field");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Outputs_m_dot_makeup_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "m_dot_makeup", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "m_dot_makeup");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Outputs_m_dot_to_pb_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "m_dot_to_pb", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "m_dot_to_pb");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_month_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "month", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "month");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Outputs_monthly_energy_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "monthly_energy", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "monthly_energy");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_pres_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "pres", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "pres");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Outputs_q_aux_fluid_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "q_aux_fluid", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "q_aux_fluid");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_q_aux_fuel_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "q_aux_fuel", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "q_aux_fuel");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_q_dump_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "q_dump", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "q_dump");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Outputs_q_field_delivered_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "q_field_delivered", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "q_field_delivered");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_q_inc_tot_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "q_inc_tot", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "q_inc_tot");
    });
    return result;
}


SAM_EXPORT double *
SAM_TcslinearFresnel_Outputs_q_loss_piping_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "q_loss_piping", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "q_loss_piping");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_q_loss_rec_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "q_loss_rec", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "q_loss_rec");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_q_loss_sf_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "q_loss_sf", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "q_loss_sf");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_q_to_pb_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "q_to_pb", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "q_to_pb");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_solazi_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "solazi", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "solazi");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_solzen_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "solzen", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "solzen");
    });
    return result;
}


SAM_EXPORT double SAM_TcslinearFresnel_Outputs_system_heat_rate_nget(SAM_TcslinearFresnel ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "system_heat_rate", &result))
            make_access_error("SAM_TcslinearFresnel", "system_heat_rate");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_tdry_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "tdry", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "tdry");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_tou_value_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "tou_value", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "tou_value");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_twet_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "twet", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "twet");
    });
    return result;
}


SAM_EXPORT double *SAM_TcslinearFresnel_Outputs_wspd_aget(SAM_TcslinearFresnel ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "wspd", length);
        if (!result)
            make_access_error("SAM_TcslinearFresnel", "wspd");
    });
    return result;
}




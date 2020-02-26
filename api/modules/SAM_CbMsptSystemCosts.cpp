#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_CbMsptSystemCosts.h"

SAM_EXPORT SAM_CbMsptSystemCosts SAM_CbMsptSystemCosts_construct(const char *def, SAM_error *err) {
    SAM_CbMsptSystemCosts result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_create();
    });
    return result;
}

SAM_EXPORT int SAM_CbMsptSystemCosts_execute(SAM_CbMsptSystemCosts data, int verbosity, SAM_error *err) {
    int n_err = 0;
    translateExceptions(err, [&] {
        n_err += SAM_module_exec("cb_mspt_system_costs", data, verbosity, err);
    });
    return n_err;
}


SAM_EXPORT void SAM_CbMsptSystemCosts_destruct(SAM_CbMsptSystemCosts system) {
    ssc_data_free(system);
}

SAM_EXPORT void SAM_CbMsptSystemCosts_Heliostat_A_sf_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "A_sf", number);
    });
}

SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_bop_spec_cost_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "bop_spec_cost", number);
    });
}

SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_contingency_rate_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "contingency_rate", number);
    });
}

SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_epc_fixed_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "csp.pt.cost.epc.fixed", number);
    });
}

SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_epc_per_acre_nset(SAM_CbMsptSystemCosts ptr, double number,
                                                                SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "csp.pt.cost.epc.per_acre", number);
    });
}

SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_epc_per_watt_nset(SAM_CbMsptSystemCosts ptr, double number,
                                                                SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "csp.pt.cost.epc.per_watt", number);
    });
}

SAM_EXPORT void SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_epc_percent_nset(SAM_CbMsptSystemCosts ptr, double number,
                                                                               SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "csp.pt.cost.epc.percent", number);
    });
}

SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_fixed_sf_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "csp.pt.cost.fixed_sf", number);
    });
}

SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_plm_fixed_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "csp.pt.cost.plm.fixed", number);
    });
}

SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_plm_per_acre_nset(SAM_CbMsptSystemCosts ptr, double number,
                                                                SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "csp.pt.cost.plm.per_acre", number);
    });
}

SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_plm_per_watt_nset(SAM_CbMsptSystemCosts ptr, double number,
                                                                SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "csp.pt.cost.plm.per_watt", number);
    });
}

SAM_EXPORT void SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_plm_percent_nset(SAM_CbMsptSystemCosts ptr, double number,
                                                                               SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "csp.pt.cost.plm.percent", number);
    });
}

SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_power_block_per_kwe_nset(SAM_CbMsptSystemCosts ptr, double number,
                                                                       SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "csp.pt.cost.power_block_per_kwe", number);
    });
}

SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_total_land_area_nset(SAM_CbMsptSystemCosts ptr, double number,
                                                                   SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "csp.pt.cost.total_land_area", number);
    });
}

SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_fossil_spec_cost_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "fossil_spec_cost", number);
    });
}

SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_heliostat_spec_cost_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "heliostat_spec_cost", number);
    });
}

SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_rec_cost_exp_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "rec_cost_exp", number);
    });
}

SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_rec_ref_area_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "rec_ref_area", number);
    });
}

SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_rec_ref_cost_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "rec_ref_cost", number);
    });
}

SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_sales_tax_frac_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "sales_tax_frac", number);
    });
}

SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_sales_tax_rate_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "sales_tax_rate", number);
    });
}

SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_site_spec_cost_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "site_spec_cost", number);
    });
}

SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_tes_spec_cost_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "tes_spec_cost", number);
    });
}

SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_tower_exp_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "tower_exp", number);
    });
}

SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemCosts_tower_fixed_cost_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "tower_fixed_cost", number);
    });
}

SAM_EXPORT void SAM_CbMsptSystemCosts_Receiver_H_rec_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "H_rec", number);
    });
}

SAM_EXPORT void SAM_CbMsptSystemCosts_Receiver_csp_pt_cost_receiver_area_nset(SAM_CbMsptSystemCosts ptr, double number,
                                                                              SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "csp.pt.cost.receiver.area", number);
    });
}

SAM_EXPORT void SAM_CbMsptSystemCosts_Receiver_h_tower_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "h_tower", number);
    });
}

SAM_EXPORT void
SAM_CbMsptSystemCosts_Receiver_helio_height_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "helio_height", number);
    });
}

SAM_EXPORT void
SAM_CbMsptSystemCosts_TES_csp_pt_cost_storage_mwht_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "csp.pt.cost.storage_mwht", number);
    });
}

SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemDesign_P_ref_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "P_ref", number);
    });
}

SAM_EXPORT void
SAM_CbMsptSystemCosts_SystemDesign_system_capacity_nset(SAM_CbMsptSystemCosts ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "system_capacity", number);
    });
}

SAM_EXPORT double SAM_CbMsptSystemCosts_Heliostat_A_sf_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "A_sf", &result))
            make_access_error("SAM_CbMsptSystemCosts", "A_sf");
    });
    return result;
}


SAM_EXPORT double SAM_CbMsptSystemCosts_SystemCosts_bop_spec_cost_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "bop_spec_cost", &result))
            make_access_error("SAM_CbMsptSystemCosts", "bop_spec_cost");
    });
    return result;
}


SAM_EXPORT double SAM_CbMsptSystemCosts_SystemCosts_contingency_rate_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "contingency_rate", &result))
            make_access_error("SAM_CbMsptSystemCosts", "contingency_rate");
    });
    return result;
}


SAM_EXPORT double
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_epc_fixed_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "csp.pt.cost.epc.fixed", &result))
            make_access_error("SAM_CbMsptSystemCosts", "csp.pt.cost.epc.fixed");
    });
    return result;
}


SAM_EXPORT double
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_epc_per_acre_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "csp.pt.cost.epc.per_acre", &result))
            make_access_error("SAM_CbMsptSystemCosts", "csp.pt.cost.epc.per_acre");
    });
    return result;
}


SAM_EXPORT double
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_epc_per_watt_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "csp.pt.cost.epc.per_watt", &result))
            make_access_error("SAM_CbMsptSystemCosts", "csp.pt.cost.epc.per_watt");
    });
    return result;
}


SAM_EXPORT double
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_epc_percent_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "csp.pt.cost.epc.percent", &result))
            make_access_error("SAM_CbMsptSystemCosts", "csp.pt.cost.epc.percent");
    });
    return result;
}


SAM_EXPORT double
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_fixed_sf_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "csp.pt.cost.fixed_sf", &result))
            make_access_error("SAM_CbMsptSystemCosts", "csp.pt.cost.fixed_sf");
    });
    return result;
}


SAM_EXPORT double
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_plm_fixed_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "csp.pt.cost.plm.fixed", &result))
            make_access_error("SAM_CbMsptSystemCosts", "csp.pt.cost.plm.fixed");
    });
    return result;
}


SAM_EXPORT double
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_plm_per_acre_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "csp.pt.cost.plm.per_acre", &result))
            make_access_error("SAM_CbMsptSystemCosts", "csp.pt.cost.plm.per_acre");
    });
    return result;
}


SAM_EXPORT double
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_plm_per_watt_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "csp.pt.cost.plm.per_watt", &result))
            make_access_error("SAM_CbMsptSystemCosts", "csp.pt.cost.plm.per_watt");
    });
    return result;
}


SAM_EXPORT double
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_plm_percent_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "csp.pt.cost.plm.percent", &result))
            make_access_error("SAM_CbMsptSystemCosts", "csp.pt.cost.plm.percent");
    });
    return result;
}


SAM_EXPORT double
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_power_block_per_kwe_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "csp.pt.cost.power_block_per_kwe", &result))
            make_access_error("SAM_CbMsptSystemCosts", "csp.pt.cost.power_block_per_kwe");
    });
    return result;
}


SAM_EXPORT double
SAM_CbMsptSystemCosts_SystemCosts_csp_pt_cost_total_land_area_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "csp.pt.cost.total_land_area", &result))
            make_access_error("SAM_CbMsptSystemCosts", "csp.pt.cost.total_land_area");
    });
    return result;
}


SAM_EXPORT double SAM_CbMsptSystemCosts_SystemCosts_fossil_spec_cost_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "fossil_spec_cost", &result))
            make_access_error("SAM_CbMsptSystemCosts", "fossil_spec_cost");
    });
    return result;
}


SAM_EXPORT double
SAM_CbMsptSystemCosts_SystemCosts_heliostat_spec_cost_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "heliostat_spec_cost", &result))
            make_access_error("SAM_CbMsptSystemCosts", "heliostat_spec_cost");
    });
    return result;
}


SAM_EXPORT double SAM_CbMsptSystemCosts_SystemCosts_rec_cost_exp_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "rec_cost_exp", &result))
            make_access_error("SAM_CbMsptSystemCosts", "rec_cost_exp");
    });
    return result;
}


SAM_EXPORT double SAM_CbMsptSystemCosts_SystemCosts_rec_ref_area_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "rec_ref_area", &result))
            make_access_error("SAM_CbMsptSystemCosts", "rec_ref_area");
    });
    return result;
}


SAM_EXPORT double SAM_CbMsptSystemCosts_SystemCosts_rec_ref_cost_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "rec_ref_cost", &result))
            make_access_error("SAM_CbMsptSystemCosts", "rec_ref_cost");
    });
    return result;
}


SAM_EXPORT double SAM_CbMsptSystemCosts_SystemCosts_sales_tax_frac_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "sales_tax_frac", &result))
            make_access_error("SAM_CbMsptSystemCosts", "sales_tax_frac");
    });
    return result;
}


SAM_EXPORT double SAM_CbMsptSystemCosts_SystemCosts_sales_tax_rate_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "sales_tax_rate", &result))
            make_access_error("SAM_CbMsptSystemCosts", "sales_tax_rate");
    });
    return result;
}


SAM_EXPORT double SAM_CbMsptSystemCosts_SystemCosts_site_spec_cost_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "site_spec_cost", &result))
            make_access_error("SAM_CbMsptSystemCosts", "site_spec_cost");
    });
    return result;
}


SAM_EXPORT double SAM_CbMsptSystemCosts_SystemCosts_tes_spec_cost_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "tes_spec_cost", &result))
            make_access_error("SAM_CbMsptSystemCosts", "tes_spec_cost");
    });
    return result;
}


SAM_EXPORT double SAM_CbMsptSystemCosts_SystemCosts_tower_exp_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "tower_exp", &result))
            make_access_error("SAM_CbMsptSystemCosts", "tower_exp");
    });
    return result;
}


SAM_EXPORT double SAM_CbMsptSystemCosts_SystemCosts_tower_fixed_cost_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "tower_fixed_cost", &result))
            make_access_error("SAM_CbMsptSystemCosts", "tower_fixed_cost");
    });
    return result;
}


SAM_EXPORT double SAM_CbMsptSystemCosts_Receiver_H_rec_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "H_rec", &result))
            make_access_error("SAM_CbMsptSystemCosts", "H_rec");
    });
    return result;
}


SAM_EXPORT double
SAM_CbMsptSystemCosts_Receiver_csp_pt_cost_receiver_area_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "csp.pt.cost.receiver.area", &result))
            make_access_error("SAM_CbMsptSystemCosts", "csp.pt.cost.receiver.area");
    });
    return result;
}


SAM_EXPORT double SAM_CbMsptSystemCosts_Receiver_h_tower_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "h_tower", &result))
            make_access_error("SAM_CbMsptSystemCosts", "h_tower");
    });
    return result;
}


SAM_EXPORT double SAM_CbMsptSystemCosts_Receiver_helio_height_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "helio_height", &result))
            make_access_error("SAM_CbMsptSystemCosts", "helio_height");
    });
    return result;
}


SAM_EXPORT double SAM_CbMsptSystemCosts_TES_csp_pt_cost_storage_mwht_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "csp.pt.cost.storage_mwht", &result))
            make_access_error("SAM_CbMsptSystemCosts", "csp.pt.cost.storage_mwht");
    });
    return result;
}


SAM_EXPORT double SAM_CbMsptSystemCosts_SystemDesign_P_ref_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "P_ref", &result))
            make_access_error("SAM_CbMsptSystemCosts", "P_ref");
    });
    return result;
}


SAM_EXPORT double SAM_CbMsptSystemCosts_SystemDesign_system_capacity_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "system_capacity", &result))
            make_access_error("SAM_CbMsptSystemCosts", "system_capacity");
    });
    return result;
}


SAM_EXPORT double SAM_CbMsptSystemCosts_Outputs_csp_pt_cost_bop_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "csp.pt.cost.bop", &result))
            make_access_error("SAM_CbMsptSystemCosts", "csp.pt.cost.bop");
    });
    return result;
}


SAM_EXPORT double
SAM_CbMsptSystemCosts_Outputs_csp_pt_cost_contingency_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "csp.pt.cost.contingency", &result))
            make_access_error("SAM_CbMsptSystemCosts", "csp.pt.cost.contingency");
    });
    return result;
}


SAM_EXPORT double SAM_CbMsptSystemCosts_Outputs_csp_pt_cost_epc_total_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "csp.pt.cost.epc.total", &result))
            make_access_error("SAM_CbMsptSystemCosts", "csp.pt.cost.epc.total");
    });
    return result;
}


SAM_EXPORT double SAM_CbMsptSystemCosts_Outputs_csp_pt_cost_fossil_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "csp.pt.cost.fossil", &result))
            make_access_error("SAM_CbMsptSystemCosts", "csp.pt.cost.fossil");
    });
    return result;
}


SAM_EXPORT double SAM_CbMsptSystemCosts_Outputs_csp_pt_cost_heliostats_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "csp.pt.cost.heliostats", &result))
            make_access_error("SAM_CbMsptSystemCosts", "csp.pt.cost.heliostats");
    });
    return result;
}


SAM_EXPORT double
SAM_CbMsptSystemCosts_Outputs_csp_pt_cost_installed_per_capacity_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "csp.pt.cost.installed_per_capacity", &result))
            make_access_error("SAM_CbMsptSystemCosts", "csp.pt.cost.installed_per_capacity");
    });
    return result;
}


SAM_EXPORT double SAM_CbMsptSystemCosts_Outputs_csp_pt_cost_plm_total_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "csp.pt.cost.plm.total", &result))
            make_access_error("SAM_CbMsptSystemCosts", "csp.pt.cost.plm.total");
    });
    return result;
}


SAM_EXPORT double
SAM_CbMsptSystemCosts_Outputs_csp_pt_cost_power_block_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "csp.pt.cost.power_block", &result))
            make_access_error("SAM_CbMsptSystemCosts", "csp.pt.cost.power_block");
    });
    return result;
}


SAM_EXPORT double SAM_CbMsptSystemCosts_Outputs_csp_pt_cost_receiver_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "csp.pt.cost.receiver", &result))
            make_access_error("SAM_CbMsptSystemCosts", "csp.pt.cost.receiver");
    });
    return result;
}


SAM_EXPORT double
SAM_CbMsptSystemCosts_Outputs_csp_pt_cost_sales_tax_total_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "csp.pt.cost.sales_tax.total", &result))
            make_access_error("SAM_CbMsptSystemCosts", "csp.pt.cost.sales_tax.total");
    });
    return result;
}


SAM_EXPORT double
SAM_CbMsptSystemCosts_Outputs_csp_pt_cost_site_improvements_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "csp.pt.cost.site_improvements", &result))
            make_access_error("SAM_CbMsptSystemCosts", "csp.pt.cost.site_improvements");
    });
    return result;
}


SAM_EXPORT double SAM_CbMsptSystemCosts_Outputs_csp_pt_cost_storage_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "csp.pt.cost.storage", &result))
            make_access_error("SAM_CbMsptSystemCosts", "csp.pt.cost.storage");
    });
    return result;
}


SAM_EXPORT double SAM_CbMsptSystemCosts_Outputs_csp_pt_cost_tower_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "csp.pt.cost.tower", &result))
            make_access_error("SAM_CbMsptSystemCosts", "csp.pt.cost.tower");
    });
    return result;
}


SAM_EXPORT double SAM_CbMsptSystemCosts_Outputs_total_direct_cost_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "total_direct_cost", &result))
            make_access_error("SAM_CbMsptSystemCosts", "total_direct_cost");
    });
    return result;
}


SAM_EXPORT double SAM_CbMsptSystemCosts_Outputs_total_indirect_cost_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "total_indirect_cost", &result))
            make_access_error("SAM_CbMsptSystemCosts", "total_indirect_cost");
    });
    return result;
}


SAM_EXPORT double SAM_CbMsptSystemCosts_Outputs_total_installed_cost_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "total_installed_cost", &result))
            make_access_error("SAM_CbMsptSystemCosts", "total_installed_cost");
    });
    return result;
}


SAM_EXPORT double SAM_CbMsptSystemCosts_Outputs_ui_direct_subtotal_nget(SAM_CbMsptSystemCosts ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "ui_direct_subtotal", &result))
            make_access_error("SAM_CbMsptSystemCosts", "ui_direct_subtotal");
    });
    return result;
}




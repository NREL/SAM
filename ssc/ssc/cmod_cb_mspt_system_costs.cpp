#include "core.h"

#include "csp_system_costs.h"
#include "csp_solver_util.h"


static var_info _cm_vtab_cb_mspt_system_costs[] = {

	
	
	{ SSC_INPUT,        SSC_NUMBER,      "A_sf",                        "Total reflective solar field area",              "m2",           "",            "heliostat",      "*",        "",  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "site_spec_cost",              "Site improvement cost",                          "$/m2",         "",            "system_costs",   "*",        "",  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "heliostat_spec_cost",         "Heliostat field cost",                           "$/m2",         "",            "system_costs",   "*",        "",  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.fixed_sf",        "Heliostat field cost fixed",                     "$",            "",            "system_costs",   "*",        "",  "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "h_tower",                     "Tower height",                                   "m",            "",            "receiver",       "*",        "",  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "H_rec",                       "The height of the receiver",                     "m",            "",            "receiver",       "*",        "",  "" },
    { SSC_INPUT,        SSC_NUMBER,      "helio_height",                "Heliostat height",                               "m",            "",            "receiver",       "*",        "",  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tower_fixed_cost",            "Tower fixed cost",                               "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tower_exp",                   "Tower cost scaling exponent",                    "",             "",            "system_costs",   "*",        "",  "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.receiver.area",   "Receiver area",                                  "m2",           "",            "receiver",       "*",        "",  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rec_ref_cost",                "Receiver reference cost",                        "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rec_ref_area",                "Receiver reference area for cost scale",         "",             "",            "system_costs",   "*",        "",  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rec_cost_exp",                "Receiver cost scaling exponent",                 "",             "",            "system_costs",   "*",        "",  "" },

	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.storage_mwht",    "Storage capacity",                               "MWt-hr",       "",            "TES",            "*",        "",  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tes_spec_cost",               "Thermal energy storage cost",                    "$/kWht",       "",            "system_costs",   "*",        "",  "" },
	
  	{ SSC_INPUT,        SSC_NUMBER,      "P_ref",                       "Reference output electric power at design condition",  "MWe",    "",            "system_design",  "*",        "",  "" },	
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.power_block_per_kwe", "Power cycle specific cost",                  "$/kWe",        "",            "system_costs",   "*",        "",  "" },

	{ SSC_INPUT,        SSC_NUMBER,      "bop_spec_cost",               "BOP specific cost",                              "$/kWe",        "",            "system_costs",   "*",        "",  "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "fossil_spec_cost",            "Fossil system specific cost",                    "$/kWe",        "",            "system_costs",   "*",        "",  "" },

	{ SSC_INPUT,        SSC_NUMBER,      "contingency_rate",            "Contingency for cost overrun",                   "%",            "",            "system_costs",   "*",        "",  "" },

	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.total_land_area", "Total land area",                                "acre",         "",            "system_costs",   "*",        "",  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "system_capacity",             "Nameplate capacity",                             "MWe",          "",            "system_design",  "*",        "",  "" },    
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.epc.per_acre",    "EPC cost per acre",                              "$/acre",       "",            "system_costs",   "*",        "",  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.epc.percent",     "EPC cost percent of direct",                     "%",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.epc.per_watt",    "EPC cost per watt",                              "$/W",          "",            "system_costs",   "*",        "",  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.epc.fixed",       "EPC fixed",                                      "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.plm.per_acre",    "PLM cost per acre",                              "$/acre",       "",            "system_costs",   "*",        "",  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.plm.percent",     "PLM cost percent of direct",                     "%",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.plm.per_watt",    "PLM cost per watt",                              "$/W",          "",            "system_costs",   "*",        "",  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.plm.fixed",       "PLM fixed",                                      "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sales_tax_frac",              "Percent of cost to which sales tax applies",     "%",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sales_tax_rate",              "Sales tax rate",                                 "%",            "",            "system_costs",   "*",        "",  "" },

	{ SSC_OUTPUT,       SSC_NUMBER,      "csp.pt.cost.site_improvements",	    "Site improvement cost",                  "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "csp.pt.cost.heliostats",	            "Heliostat cost",                         "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "csp.pt.cost.tower",	                "Tower cost",                             "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "csp.pt.cost.receiver",	            "Receiver cost",                          "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "csp.pt.cost.storage",	                "TES cost",                               "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "csp.pt.cost.power_block",	            "Power cycle cost",                       "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "csp.pt.cost.bop",	                    "BOP cost",                               "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "csp.pt.cost.fossil",	                "Fossil backup cost",                     "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "ui_direct_subtotal",	                "Direct capital precontingency cost",     "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "csp.pt.cost.contingency",	            "Contingency cost",                       "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "total_direct_cost",	                "Total direct cost",                      "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "csp.pt.cost.epc.total",	            "EPC and owner cost",                     "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "csp.pt.cost.plm.total",	            "Total land cost",                        "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "csp.pt.cost.sales_tax.total",	        "Sales tax cost",                         "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "total_indirect_cost",	                "Total indirect cost",                    "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "total_installed_cost",	            "Total installed cost",                   "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "csp.pt.cost.installed_per_capacity",  "Estimated installed cost per cap",       "$",            "",            "system_costs",   "*",        "",  "" },
	


	var_info_invalid };

class cm_cb_mspt_system_costs : public compute_module
{
public:

	cm_cb_mspt_system_costs()
	{
		add_var_info(_cm_vtab_cb_mspt_system_costs);
	}

	void exec() throw(general_error)
	{
		C_mspt_system_costs sys_costs;

		sys_costs.ms_par.A_sf_refl = as_double("A_sf");
		sys_costs.ms_par.site_improv_spec_cost = as_double("site_spec_cost");
		sys_costs.ms_par.heliostat_spec_cost = as_double("heliostat_spec_cost");
		sys_costs.ms_par.heliostat_fixed_cost = as_double("csp.pt.cost.fixed_sf");

		sys_costs.ms_par.h_tower = as_double("h_tower");
		sys_costs.ms_par.h_rec = as_double("H_rec");
		sys_costs.ms_par.h_helio = as_double("helio_height");
		sys_costs.ms_par.tower_fixed_cost = as_double("tower_fixed_cost");
		sys_costs.ms_par.tower_cost_scaling_exp = as_double("tower_exp");

		sys_costs.ms_par.A_rec = as_double("csp.pt.cost.receiver.area");
		sys_costs.ms_par.rec_ref_cost = as_double("rec_ref_cost");
		sys_costs.ms_par.A_rec_ref = as_double("rec_ref_area");
		sys_costs.ms_par.rec_cost_scaling_exp = as_double("rec_cost_exp");

		sys_costs.ms_par.Q_storage = as_double("csp.pt.cost.storage_mwht");
		sys_costs.ms_par.tes_spec_cost = as_double("tes_spec_cost");
		
		sys_costs.ms_par.W_dot_design = as_double("P_ref");
		sys_costs.ms_par.power_cycle_spec_cost = as_double("csp.pt.cost.power_block_per_kwe");

		sys_costs.ms_par.bop_spec_cost = as_double("bop_spec_cost");

		sys_costs.ms_par.fossil_backup_spec_cost = as_double("fossil_spec_cost");

		sys_costs.ms_par.contingency_rate = as_double("contingency_rate");

		sys_costs.ms_par.total_land_area = as_double("csp.pt.cost.total_land_area");
		sys_costs.ms_par.plant_net_capacity = as_double("system_capacity");
		sys_costs.ms_par.EPC_land_spec_cost = as_double("csp.pt.cost.epc.per_acre");
		sys_costs.ms_par.EPC_land_perc_direct_cost = as_double("csp.pt.cost.epc.percent");
		sys_costs.ms_par.EPC_land_per_power_cost = as_double("csp.pt.cost.epc.per_watt");
		sys_costs.ms_par.EPC_land_fixed_cost = as_double("csp.pt.cost.epc.fixed");
		sys_costs.ms_par.total_land_spec_cost = as_double("csp.pt.cost.plm.per_acre");
		sys_costs.ms_par.total_land_perc_direct_cost = as_double("csp.pt.cost.plm.percent");
		sys_costs.ms_par.total_land_per_power_cost = as_double("csp.pt.cost.plm.per_watt");
		sys_costs.ms_par.total_land_fixed_cost = as_double("csp.pt.cost.plm.fixed");
		sys_costs.ms_par.sales_tax_basis = as_double("sales_tax_frac");
		sys_costs.ms_par.sales_tax_rate = as_double("sales_tax_rate");

		try
		{
			sys_costs.calculate_costs();
		}
		catch(C_csp_exception &csp_exception)
		{
			throw exec_error("MSPT system costs", util::format("System cost calculations failed. Check that all inputs are properly defined"));
		}
		
		assign("csp.pt.cost.site_improvements", sys_costs.ms_out.site_improvement_cost);
		assign("csp.pt.cost.heliostats", sys_costs.ms_out.heliostat_cost);
		assign("csp.pt.cost.tower", sys_costs.ms_out.tower_cost);
		assign("csp.pt.cost.receiver", sys_costs.ms_out.receiver_cost);
		assign("csp.pt.cost.storage", sys_costs.ms_out.tes_cost);
		assign("csp.pt.cost.power_block", sys_costs.ms_out.power_cycle_cost);
		assign("csp.pt.cost.bop", sys_costs.ms_out.bop_cost);
		assign("csp.pt.cost.fossil", sys_costs.ms_out.fossil_backup_cost);
		assign("ui_direct_subtotal", sys_costs.ms_out.direct_capital_precontingency_cost);
		assign("csp.pt.cost.contingency", sys_costs.ms_out.contingency_cost);
		assign("total_direct_cost", sys_costs.ms_out.total_direct_cost);
		assign("csp.pt.cost.epc.total", sys_costs.ms_out.epc_and_owner_cost);
		assign("csp.pt.cost.plm.total", sys_costs.ms_out.total_land_cost);
		assign("csp.pt.cost.sales_tax.total", sys_costs.ms_out.sales_tax_cost);
		assign("total_indirect_cost", sys_costs.ms_out.total_indirect_cost);
		assign("total_installed_cost", sys_costs.ms_out.total_installed_cost);
		assign("csp.pt.cost.installed_per_capacity", sys_costs.ms_out.estimated_installed_cost_per_cap);

	}

};

DEFINE_MODULE_ENTRY(cb_mspt_system_costs, "CSP molten salt power tower system costs", 0)

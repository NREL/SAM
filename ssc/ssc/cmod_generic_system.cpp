#include "core.h"
#include "lib_windfile.h"
#include "lib_windwatts.h"
// for adjustment factors
#include "common.h"

static var_info _cm_vtab_generic_system[] = {
//	  VARTYPE           DATATYPE         NAME                           LABEL                                 UNITS           META     GROUP                REQUIRED_IF        CONSTRAINTS           UI_HINTS
	{ SSC_INPUT,        SSC_NUMBER,      "spec_mode",                  "Spec mode: 0=constant CF,1=profile",  "",             "",      "generic_system",      "*",               "",                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "derate",                     "Derate",                              "%",            "",      "generic_system",      "*",               "",                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "system_capacity",         "Nameplace Capcity",                   "kW",           "",      "generic_system",      "*",               "",                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "user_capacity_factor",            "Capacity Factor",                     "%",            "",      "generic_system",      "*",               "",                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "heat_rate",                  "Heat Rate",                           "MMBTUs/MWhe",  "",      "generic_system",      "*",               "",                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "conv_eff",                   "Conversion Efficiency",               "%",            "",      "generic_system",      "*",               "",                    "" },
	{ SSC_INPUT,        SSC_ARRAY,       "energy_output_array",        "Array of Energy Output Profile",      "kWh",          "",      "generic_system",      "*",               "",                    "" }, 
																														      														   
//    OUTPUTS ----------------------------------------------------------------------------								      														   
//	  VARTYPE           DATATYPE         NAME                          LABEL                                   UNITS           META     GROUP                 REQUIRED_IF        CONSTRAINTS           UI_HINTS
//	{ SSC_OUTPUT,       SSC_ARRAY,       "hourly_energy",              "Hourly Energy",                        "kWh",           "",      "Time Series",      "*",               "LENGTH=8760",         "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_energy",             "Monthly Energy",                       "kWh",          "",      "Monthly",      "*",               "LENGTH=12",           "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_energy",              "Annual Energy",                        "kWh",          "",      "Annual",      "*",               "",                    "" },

	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_fuel_usage",                 "Annual Fuel Usage",                    "kWht",         "",      "Annual",      "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "water_usage",                "Annual Water Usage",                   "",             "",      "Annual",      "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system_heat_rate",           "Heat Rate Conversion Factor",          "MMBTUs/MWhe",  "",      "Annual",      "*",               "",                    "" },

	{ SSC_OUTPUT, SSC_NUMBER, "capacity_factor", "Capacity factor", "%", "", "Annual", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "kwh_per_kw", "First year kWh/kW", "kWh/kW", "", "Annual", "*", "", "" },

	{ SSC_OUTPUT,       SSC_NUMBER,      "system_use_lifetime_output",     "Use lifetime output",                         "0/1", "",                        "Miscellaneous", "*",                       "INTEGER",                   "" },

var_info_invalid };

class cm_generic_system : public compute_module
{
private:
public:
	
	cm_generic_system()
	{
		add_var_info( _cm_vtab_generic_system );
		// performance adjustment factors
		add_var_info(vtab_adjustment_factors);
		add_var_info(vtab_technology_outputs);
	}

	void exec( ) throw( general_error )
	{
		int spec_mode = as_integer("spec_mode");
		ssc_number_t *enet = allocate("gen", 8760);
//		ssc_number_t *p_gen = allocate("gen", 8760);

		double derate = (1 - (double)as_number("derate") / 100);
		double annual_output = 0;

		adjustment_factors haf(this, "adjust");
		if (!haf.setup())
			throw exec_error("pvwattsv5", "failed to setup adjustment factors: " + haf.error());

		if (spec_mode == 0)
		{
			double output = (double)as_number("system_capacity")
				* (double)as_number("user_capacity_factor") / 100
				* derate;

			annual_output = 8760 * output;

			for (int i = 0; i < 8760; i++)
			{
				enet[i] = output*haf(i);
//				p_gen[i] = enet[i];
			}
		}
		else
		{
			size_t count = 0;
			ssc_number_t *data = as_array("energy_output_array", &count);

			if (!data)
				throw exec_error("generic", util::format("energy_output_array variable had no values."));

			int nmult = count / 8760;
			if (nmult * 8760 != count)
				throw exec_error("generic", util::format("energy_output_array not a multiple of 8760: len=%d.", count));

			int c = 0;
			int i = 0;
			while (c<8760 && i<count)
			{
				double integ = 0;
				for (int j = 0; j<nmult; j++)
				{
					integ += data[i];
					i++;
				}

				enet[c] = integ*derate*haf(c);
//				p_gen[c] = enet[c];

				annual_output += enet[c];
				c++;
			}
		}

		accumulate_monthly("gen", "monthly_energy");
		accumulate_annual("gen", "annual_energy");

		// if conversion efficiency is zero then set fuel usage to zero per email from Paul 5/17/12
		double fuel_usage = 0.0;
		if (as_double("conv_eff") != 0.0)
			fuel_usage = annual_output * 100.0 / as_double("conv_eff");
		assign("annual_fuel_usage", fuel_usage);

		assign("water_usage", 0.0);
		assign("system_heat_rate", as_double("heat_rate") *  as_double("conv_eff") / 100.0);

		// metric outputs moved to technology
		double kWhperkW = 0.0;
		double nameplate = as_double("system_capacity");
		double annual_energy = 0.0;
		for (int i = 0; i < 8760; i++)
			annual_energy += enet[i];
		if (nameplate > 0) kWhperkW = annual_energy / nameplate;
		assign("capacity_factor", var_data((ssc_number_t)(kWhperkW / 87.6)));
		assign("kwh_per_kw", var_data((ssc_number_t)kWhperkW));

		assign("system_use_lifetime_output", 0);

	} // exec
};

DEFINE_MODULE_ENTRY( generic_system, "Generic System", 1 );


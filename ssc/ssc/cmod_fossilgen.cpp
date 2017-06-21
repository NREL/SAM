#include "core.h"

static var_info _cm_vtab_fossilgen[] = {
/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_NUMBER,      "nameplate",                  "Nameplate generation capacity", "kW",     "",                      "Fossil",        "*",                       "POSITIVE",                      "" },		
	{ SSC_INPUT,        SSC_NUMBER,      "capacity_factor",            "Capacity factor",               "%",      "",                      "Fossil",        "*",                       "MIN=0,MAX=100",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "derate",                     "System derate",                 "frac",   "",                      "Fossil",        "*",                       "MIN=0,MAX=1",                   "" },
	{ SSC_INPUT,        SSC_NUMBER,      "conv_eff",                   "Conversion efficiency",         "%",      "",                      "Fossil",        "*",                       "MIN=0,MAX=100",                 "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "e_net",                      "AC Generation",                 "kWh",    "",                      "Fossil",        "*",                       "LENGTH=8760",                   "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "fuel_usage",                 "Annual fuel usage",             "kWht",   "",                      "Fossil",        "*",                       "",                              "" },

var_info_invalid };

class cm_fossilgen : public compute_module
{
public:
	cm_fossilgen()
	{
		add_var_info( _cm_vtab_fossilgen );
	}

	void exec( ) throw( general_error )
	{

		ssc_number_t output = 8760*as_number("nameplate")
			* as_number("capacity_factor") / 100
			* (1 - as_number("derate")/100);

		ssc_number_t *e = allocate("e_net", 8760);

		// assume constant generation in each hour of the year
		for (size_t i=0;i<8760;i++)	e[i] = output/8760;

	
		assign( "fuel_usage", 
			var_data(output * 100 / as_number("conv_eff")) );
	}
};

DEFINE_MODULE_ENTRY( fossilgen, "Generic fossil fuel generator - capacity factor based approach", 1 )

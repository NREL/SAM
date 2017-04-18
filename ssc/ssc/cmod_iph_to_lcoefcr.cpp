#include "core.h"

static var_info vtab_iph_to_lcoefcr[] = 
{	
/*   VARTYPE            DATATYPE         NAME                        LABEL                             UNITS     META      GROUP          REQUIRED_IF    CONSTRAINTS UI_HINTS*/
	{ SSC_INPUT,       SSC_NUMBER,      "annual_electricity_consumption",  "Annual electricity consumptoin w/ avail derate",            "kWe-hr", "",   "IPH_LCOH",     "*",       "",   "" },
	{ SSC_INPUT,       SSC_NUMBER,      "electricity_rate",                "Cost of electricity used to operate pumps/trackers",        "$/kWe",  "",   "IPH_LCOH",     "*",       "",   "" },

	{ SSC_INOUT,       SSC_NUMBER,      "fixed_operating_cost",     "Annual fixed operating cost",    "$/kW",   "",       "Simple LCOE", "*",           "",         "" },

var_info_invalid };

class cm_iph_to_lcoefcr : public compute_module
{
private:
public:
	
	cm_iph_to_lcoefcr()
	{
		add_var_info( vtab_iph_to_lcoefcr );
	}

	void exec( ) throw( general_error )
	{
		double foc = as_double("fixed_operating_cost");		//[$]
		
		assign("fixed_operating_cost", foc + as_double("electricity_rate")*as_double("annual_electricity_consumption"));		
	}
	
};

DEFINE_MODULE_ENTRY( iph_to_lcoefcr, "Convert annual energy to kWt-hr and adjust fixed cost to include electric parasitic costs.", 1 )

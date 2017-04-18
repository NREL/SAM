#include "core.h"
#include "lib_financial.h"

static var_info vtab_lcoefcr[] = 
{	
/*   VARTYPE            DATATYPE         NAME                        LABEL                             UNITS     META      GROUP          REQUIRED_IF    CONSTRAINTS UI_HINTS*/
	{ SSC_INPUT,        SSC_NUMBER,      "capital_cost",             "Capital cost",                   "$",      "",       "Simple LCOE", "*",           "",         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fixed_operating_cost",     "Annual fixed operating cost",    "$",      "",       "Simple LCOE", "*",           "",         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "variable_operating_cost",  "Annual variable operating cost", "$/kWh",  "",       "Simple LCOE", "*",           "",         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fixed_charge_rate",        "Fixed charge rate",              "",       "",       "Simple LCOE", "*",           "",         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "annual_energy",            "Annual energy production",       "kWh",    "",       "Simple LCOE", "*",           "",         "" },
	
	{ SSC_OUTPUT,       SSC_NUMBER,       "lcoe_fcr",                "Levelized cost of energy",      "$/kWh",  "",       "Simple LCOE", "*",           "",         "" },

var_info_invalid };

class cm_lcoefcr : public compute_module
{
private:
public:
	cm_lcoefcr()
	{
		add_var_info( vtab_lcoefcr );
	}

	void exec( ) throw( general_error )
	{
		double aep = 1; // annual output, get from performance model
		double aoe = 0; // annual operating costs
		double fcr = 0; // fixed charge rate, before tax revenues required
		double icc = 0; // initial investment, or capital cost
		double voc = 0; // variable operating cost
		double foc = 0; // fixed operating cost

		aep = as_double("annual_energy");           // kWh
		foc = as_double("fixed_operating_cost");    // $
		voc = as_double("variable_operating_cost"); // $/kWh
		fcr = as_double("fixed_charge_rate");       // unitless fraction
		icc = as_double("capital_cost");            // $
		
		double lcoe = (fcr*icc + foc) / aep + voc; //$/kWh

		assign("lcoe_fcr", var_data((ssc_number_t)lcoe));
	}


};

DEFINE_MODULE_ENTRY( lcoefcr, "Calculate levelized cost of energy using fixed charge rate method.", 1 )

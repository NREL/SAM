#include <math.h>

#ifndef M_PI
#define M_PI 3.141592653589793238462643
#endif

#include "core.h"
#include "lib_sandia.h"

static var_info _cm_vtab_pvsandiainv[] = {

/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
		
	{ SSC_INPUT,        SSC_ARRAY,       "dc",                      "DC power input to inverter",     "Watt",   "",                   "Sandia Inverter Model",      "*",                          "",                          "" },
	{ SSC_INPUT,        SSC_ARRAY,       "dc_voltage",              "DC voltage input to inverter",   "Volt",   "",                   "Sandia Inverter Model",      "*",                          "LENGTH_EQUAL=dc",                          "" },

	{ SSC_INPUT,        SSC_NUMBER,      "paco",                    "Max AC power rating",            "Wac",      "",                     "Sandia Inverter Model",      "*",                       "",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pdco",                    "DC power level at which Paco is achieved",    "Wdc",       "",                     "Sandia Inverter Model",      "*",                       "",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "vdco",                    "DV voltage level at which Paco is achieved",  "Volt",       "",                     "Sandia Inverter Model",      "*",                       "",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pso",                     "DC power level required to start inversion",  "Wdc",       "",                     "Sandia Inverter Model",      "*",                       "",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pntare",                  "Parasitic AC consumption",        "Wac",       "",                     "Sandia Inverter Model",      "*",                       "",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "c0",                      "C0: Defines parabolic curvature of relationship between ac power and dc power at reference conditions",    "1/W",    "",                     "Sandia Inverter Model",      "*",                       "",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "c1",                      "C1: Parameter allowing Pdco to vary linearly with dc voltage input",  "1/V",    "",                     "Sandia Inverter Model",      "*",                       "",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "c2",                      "C2: Parameter allowing Pso to vary linearly with dc voltage input ",  "1/V",    "",                     "Sandia Inverter Model",      "*",                       "",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "c3",                      "C3: Parameter allowing C0 to vary linearly with dc voltage input",    "1/V",    "",                     "Sandia Inverter Model",      "*",                       "",              "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "ac",                      "AC power output",                "Wac",    "",                      "Sandia Inverter Model",      "*",                       "LENGTH_EQUAL=dc",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "acpar",                   "AC parasitic power",             "Wac",    "",                      "Sandia Inverter Model",      "*",                       "LENGTH_EQUAL=dc",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "plr",                     "Part load ratio",                "0..1",   "",                      "Sandia Inverter Model",      "*",                       "LENGTH_EQUAL=dc",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "eff_inv",                 "Conversion efficiency",          "0..1",   "",                      "Sandia Inverter Model",      "*",                       "LENGTH_EQUAL=dc",                          "" },

	var_info_invalid };

class cm_pvsandiainv : public compute_module
{
private:
public:
	cm_pvsandiainv()
	{
		add_var_info( _cm_vtab_pvsandiainv );
	}

	void exec( ) throw( general_error )
	{
		size_t arr_len;
		ssc_number_t *p_dcp = as_array( "dc", &arr_len );
		ssc_number_t *p_dcv = as_array( "dc_voltage", &arr_len );

		sandia_inverter_t inv;
	
		inv.Paco = as_double("paco");
		inv.Pdco = as_double("pdco");
		inv.Vdco = as_double("vdco");
		inv.Pso = as_double("pso");
		inv.Pntare = as_double("pntare");
		inv.C0 = as_double("c0");
		inv.C1 = as_double("c1");
		inv.C2 = as_double("c2");
		inv.C3 = as_double("c3");
		
		ssc_number_t *p_ac = allocate("ac", arr_len);
		ssc_number_t *p_acpar = allocate("acpar", arr_len);
		ssc_number_t *p_plr = allocate("plr", arr_len);
		ssc_number_t *p_eff = allocate("eff_inv", arr_len);

		for (size_t i = 0; i < arr_len; i++ )
		{
			double pac, ppar, plr, eta, pcliploss, psoloss, pntloss;
			if ( !inv.acpower( p_dcp[i], p_dcv[i], &pac, &ppar, &plr, &eta, &pcliploss, &psoloss, &pntloss ) ) throw general_error("sandia inverter model calculation error with given inputs", (float) i);

			p_ac[i] = (ssc_number_t) pac;
			p_acpar[i] = (ssc_number_t) ppar;
			p_plr[i] = (ssc_number_t) plr;
			p_eff[i] = (ssc_number_t) eta;
		}
	}
};

DEFINE_MODULE_ENTRY( pvsandiainv, "Sandia PV inverter performance calculator.", 1 )

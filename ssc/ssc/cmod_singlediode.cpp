
#include "core.h"
#include "lib_cec6par.h"

static var_info _cm_vtab_singlediode[] = {

/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP                  REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_NUMBER,      "a",                       "Modified nonideality factor",    "1/V",    "",                      "Single Diode Model",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Il",                      "Light current",                  "A",      "",                      "Single Diode Model",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Io",                      "Saturation current",             "A",      "",                      "Single Diode Model",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Rs",                      "Series resistance",              "ohm",    "",                      "Single Diode Model",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Rsh",                     "Shunt resistance",               "ohm",    "",                      "Single Diode Model",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Vop",                     "Module operating voltage",       "V",      "",                      "Single Diode Model",      "?"                         "",                      "" },

	{ SSC_OUTPUT,       SSC_NUMBER,      "V",                       "Output voltage",                "V",      "",                      "Single Diode Model",       "*",                        "",                      "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "I",                       "Output current",                "A",      "",                      "Single Diode Model",       "*",                        "",                      "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "Voc",                     "Open circuit voltage",          "V",      "",                      "Single Diode Model",       "*",                        "",                      "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "Isc",                     "Short circuit current",         "A",      "",                      "Single Diode Model",       "*",                        "",                      "" },

var_info_invalid };


class cm_singlediode : public compute_module
{
private:
public:
	cm_singlediode()
	{
		add_var_info( _cm_vtab_singlediode );
	}

	void exec( ) throw( general_error )
	{
		double a = as_double( "a" );
		double Il = as_double( "Il" );
		double Io = as_double( "Io" );
		double Rs = as_double( "Rs" );
		double Rsh = as_double( "Rsh" );
		double Vop = -1.0;
		if ( is_assigned( "Vop" ) )
			Vop = as_double( "Vop" );

		double V, I;
		if ( Vop < 0 )
		{
			// use 100 volts as upper bound of Voc
			maxpower_5par( 100, a, Il, Io, Rs, Rsh, &V, &I );
		}
		else
		{
			V = Vop;
			I = current_5par( V, 0.9*Il, a, Il, Io, Rs, Rsh );
		}

		assign( "V", var_data( V ) );
		assign( "I", var_data( I ) );

		double Voc = openvoltage_5par( V, a, Il, Io, Rsh );
		double Isc = current_5par( 0.0, Il, a, Il, Io, Rs, Rsh );

		assign( "Voc", var_data( Voc ) );
		assign( "Isc", var_data( Isc ) );
	}
};

DEFINE_MODULE_ENTRY( singlediode, "Single diode model function.", 1 )


static var_info _cm_vtab_singlediodeparams[] = {

	/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP                  REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/

	{ SSC_INPUT,        SSC_NUMBER,      "I",                       "Irradiance",                    "W/m2",      "",                    "Single Diode Model",      "*",                       "",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "T",                       "Temperature",                   "C",         "",                    "Single Diode Model",      "*",                       "",              "" },

	{ SSC_INPUT,        SSC_NUMBER,      "alpha_isc",               "Temp coeff of current at SC",    "A/'C",    "",                     "Single Diode Model",      "*",                       "",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Adj_ref",                 "OC SC temp coeff adjustment",    "%",       "",                     "Single Diode Model",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "a_ref",                   "Modified nonideality factor",    "1/V",     "",                     "Single Diode Model",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Il_ref",                  "Light current",                  "A",       "",                     "Single Diode Model",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Io_ref",                  "Saturation current",             "A",       "",                     "Single Diode Model",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Rs_ref",                  "Series resistance",              "ohm",     "",                     "Single Diode Model",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Rsh_ref",                 "Shunt resistance",               "ohm",     "",                     "Single Diode Model",      "*",                        "",                      "" },


	{ SSC_OUTPUT,       SSC_NUMBER,      "a",                       "Modified nonideality factor",    "1/V",    "",                      "Single Diode Model",      "*",                        "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "Il",                      "Light current",                  "A",      "",                      "Single Diode Model",      "*",                        "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "Io",                      "Saturation current",             "A",      "",                      "Single Diode Model",      "*",                        "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "Rs",                      "Series resistance",              "ohm",    "",                      "Single Diode Model",      "*",                        "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "Rsh",                     "Shunt resistance",               "ohm",    "",                      "Single Diode Model",      "*",                        "",                              "" },
	
var_info_invalid };

class cm_singlediodeparams : public compute_module
{
private:
public:
	cm_singlediodeparams()
	{
		add_var_info( _cm_vtab_singlediodeparams );
	}
		
	void exec( ) throw( general_error )
	{
#define I_ref 1000.0
#define Tc_ref 298.15
#define Eg_ref 1.12
#define KB 8.618e-5
		
		double I = as_double( "I" );	
		double T = as_double( "T" ) + 273.15; // want cell temp in kelvin
		
		double alpha_isc = as_double( "alpha_isc" );
		double Adj = as_double( "Adj_ref" );
		double Il = as_double( "Il_ref" );
		double Io = as_double( "Io_ref" );
		double a = as_double( "a_ref" );
		double Rs = as_double( "Rs_ref" );
		double Rsh = as_double( "Rsh_ref" );

		
		double muIsc = alpha_isc * (1-Adj/100.0);
		// calculation of IL and IO at operating conditions
		double IL_oper = I/I_ref *( Il + muIsc*(T-Tc_ref) );
		if (IL_oper < 0.0) IL_oper = 0.0;
		
		double EG = Eg_ref * (1-0.0002677*(T-Tc_ref));
		double IO_oper = Io * pow(T/Tc_ref, 3) * exp( 1/KB*(Eg_ref/Tc_ref - EG/T) );
		double A_oper = a * T / Tc_ref;
		double Rsh_oper = Rsh*(I_ref/I);
			

		assign( "Rs", var_data(Rs) );
		assign( "Rsh", var_data( Rsh_oper ) );
		assign( "a", var_data(A_oper) );
		assign( "Io", var_data( IO_oper ) );
		assign( "Il", var_data( IL_oper ) );		
	}
};

DEFINE_MODULE_ENTRY( singlediodeparams, "Single diode model parameter calculation.", 1 )
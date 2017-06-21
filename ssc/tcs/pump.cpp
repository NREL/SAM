#define _TCSTYPEINTERFACE_
#include "tcstype.h"

enum {	I_T1,
		I_T2,
		I_TDIFF,
		I_MDOTSET,

		O_MDOT,
		O_CTRL,
		O_ENTHALPY,

		N_MAX };

tcsvarinfo pump_variables[] = {

	{ TCS_INPUT,   TCS_NUMBER,   I_T1,       "t1",       "Test temperature 1",                        "'C",     "",      "",     "" },
	{ TCS_INPUT,   TCS_NUMBER,   I_T2,       "t2",       "Test temperature 2",                        "'C",     "",      "",     "" },
	{ TCS_INPUT,   TCS_NUMBER,   I_TDIFF,    "tdiff",    "Difference setpoint (ON if T2-T1 > Tdiff)", "'C",     "",      "",     "" },
	{ TCS_INPUT,   TCS_NUMBER,   I_MDOTSET,  "mdotset",  "Design flow rate",                          "L/s",    "",      "",     "" },

	{ TCS_OUTPUT,  TCS_NUMBER,   O_MDOT,     "mdot",     "Flow rate",                                 "L/s",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_CTRL,     "ctrl",     "Control signal (1/0)",                      "0/1",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_ENTHALPY, "enthalpy", "Enthalpy",                                  "kJ/kg",  "",      "",     "" },
	
	{ TCS_INVALID, TCS_INVALID,  N_MAX,       0,            0, 0, 0, 0, 0 }
};

//#include "waterprop.h"
#include "water_properties.h"

class pump : public tcstypeinterface
{
private:
public:
	pump( tcscontext *cxt, tcstypeinfo *ti )
		: tcstypeinterface( cxt, ti )
	{
	}

	virtual ~pump()
	{
	}

	virtual int init()
	{
		return 0;
	}

	virtual int call( double time, double step, int ncall )
	{
		if ( value(I_T2) - value(I_T1) > value(I_TDIFF) )
		{
			value( O_CTRL, 1 );
			value( O_MDOT, value( I_MDOTSET ) );
		}
		else
		{
			value( O_CTRL, 0.0 );
			value( O_MDOT, 0.0 );
		}
		
		water_state wp;
		water_PS( 600, 5.5, &wp );

		value( O_ENTHALPY, wp.enth );

		return 0;
	}
};

TCS_IMPLEMENT_TYPE( pump, "Basic pump unit", "Aron Dobos", 1, pump_variables, NULL, 0 )

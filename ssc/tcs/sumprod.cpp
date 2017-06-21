#define _TCSTYPEINTERFACE_
#include "tcstype.h"

enum {
	I_A,
	I_B,

	O_S,
	O_P,

	N_MAX };

tcsvarinfo sumprod_variables[] = {
	// vartype    datatype    index   name     label    units   meta   group   default_value
	{ TCS_INPUT,  TCS_NUMBER, I_A,    "a",     "Data 1",   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_B,    "b",     "Data 2",   "",      "",      "",     "" },

	{ TCS_OUTPUT, TCS_NUMBER, O_S,    "sum",      "Result of A+B",    "",     "",      "",     "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_P,    "product",  "Result of A*B",    "",     "",      "",     "" },
	
	{ TCS_INVALID, TCS_INVALID,  N_MAX,       0,            0, 0, 0, 0, 0 }
};

class sumprod : public tcstypeinterface
{
private:
public:
	sumprod( tcscontext *cxt, tcstypeinfo *ti )
		: tcstypeinterface( cxt, ti )
	{
	}

	virtual ~sumprod()
	{
		// free any memory
	}

	virtual int init()
	{
		return 0;
	}
	
	virtual int call( double time, double step, int ncall )
	{
		double a = value( I_A );
		double b = value( I_B );
		
		if ( a < -999 )
		{
			message( TCS_ERROR, "invalid value for a: %lg", a);
			return -1;
		}

		value( O_S, a+b );
		value( O_P, a*b );

		return 0;
	}
};

TCS_IMPLEMENT_TYPE( sumprod, "Sums and Products", "Aron", 123, sumprod_variables, NULL, 0 )
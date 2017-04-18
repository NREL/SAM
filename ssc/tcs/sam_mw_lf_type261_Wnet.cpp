#define _TCSTYPEINTERFACE_
#include "tcstype.h"

using namespace std;

enum{
	I_W_CYCLE_GROSS,
	I_W_PAR_SF_TOT,
	I_W_PAR_COOLING,

	O_W_NET,

	//Include N_max
	N_MAX
};

tcsvarinfo sam_mw_lf_type261_Wnet_variables[] = {
	{ TCS_INPUT,          TCS_NUMBER,     I_W_CYCLE_GROSS,          "W_cycle_gross",                                            "Electrical source - Power cycle gross output",           "MW",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,     I_W_PAR_SF_TOT,           "W_par_sf_tot",                                             "Total solar field parasitics"                            "MW",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,     I_W_PAR_COOLING,          "W_par_cooling",                                            "Parasitics from power cycle cooling",                    "MW",             "",             "",             "" },

	{ TCS_OUTPUT,         TCS_NUMBER,     O_W_NET,                  "W_net",												    "Net electricity generation (or usage) by the plant",           "MW",             "",             "",             "" },
	
	{ TCS_INVALID,    TCS_INVALID,    N_MAX,                0,                    0,                                                        0,                0,        0,        0 }
};

class sam_mw_lf_type261_Wnet : public tcstypeinterface
{
private:

public:

	sam_mw_lf_type261_Wnet(tcscontext *cxt, tcstypeinfo *ti)
		: tcstypeinterface(cxt, ti)
	{
		
	}

	virtual ~sam_mw_lf_type261_Wnet()
	{
	}

	virtual int init()
	{		
		// --Initialization call-- 
		return 0;
	}

	virtual int call(double time, double step, int ncall)
	{

		double W_dot_pb_gross = value( I_W_CYCLE_GROSS );
		double W_dot_sf_par = value( I_W_PAR_SF_TOT );
		double W_dot_cooling_par = value( I_W_PAR_COOLING );

		value( O_W_NET, W_dot_pb_gross - W_dot_sf_par - W_dot_cooling_par );
		
		return 0;
	}

	virtual int converged(double time)
	{

		return 0;
	}
	
	
};


TCS_IMPLEMENT_TYPE( sam_mw_lf_type261_Wnet, "Net electricity calculator for the Physical Trough", "Mike Wagner", 1, sam_mw_lf_type261_Wnet_variables, NULL, 1 );
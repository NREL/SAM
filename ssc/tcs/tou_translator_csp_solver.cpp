#define _TCSTYPEINTERFACE_
#include "tcstype.h"

#include "csp_solver_tou_block_schedules.h"
#include "csp_solver_util.h"
#include "csp_solver_core.h"

enum {	
	P_WEEKDAY_SCHEDULE,
	P_WEEKEND_SCHEDULE,

	O_TOU_VALUE,

	N_MAX };

tcsvarinfo tou_translator_variables[] = {
	// vartype            datatype              index                       name                        label                                          units           meta            group    default_value
	{ TCS_PARAM,          TCS_MATRIX,           P_WEEKDAY_SCHEDULE,         "weekday_schedule",         "12x24 matrix of values for weekdays",         "",             "",             "",          "" },
	{ TCS_PARAM,          TCS_MATRIX,           P_WEEKEND_SCHEDULE,         "weekend_schedule",         "12x24 matrix of values for weekend days",     "",             "",             "",          "" },
	
	{ TCS_OUTPUT,         TCS_NUMBER,           O_TOU_VALUE,                "tou_value",                "Value during time step",                      "",             "",             "",          "" },

	{ TCS_INVALID, TCS_INVALID,  N_MAX,       0,            0, 0, 0, 0, 0 }
};


class tou_translator : public tcstypeinterface
{
private:
C_csp_tou_block_schedules mc_tou;
C_csp_tou::S_csp_tou_outputs ms_outputs;

public:
	tou_translator( tcscontext *cxt, tcstypeinfo *ti )
		: tcstypeinterface( cxt, ti )
	{

	}

	virtual ~tou_translator()
	{
	}

	virtual int init()
	{
		int nrows, ncols;
		double *weekdays = value( P_WEEKDAY_SCHEDULE, &nrows, &ncols );
		mc_tou.ms_params.mc_csp_ops.mc_weekdays.resize(nrows,ncols);
		for( int r = 0; r < nrows; r++ )
			for( int c = 0; c < ncols; c++ )
				mc_tou.ms_params.mc_csp_ops.mc_weekdays(r, c) = TCS_MATRIX_INDEX(var(P_WEEKDAY_SCHEDULE), r, c);

		nrows = ncols = 0;
		double *weekends = value( P_WEEKEND_SCHEDULE, &nrows, &ncols );		
		mc_tou.ms_params.mc_csp_ops.mc_weekends.resize(nrows, ncols);
		for( int r = 0; r < nrows; r++ )
			for( int c = 0; c < ncols; c++ )
				mc_tou.ms_params.mc_csp_ops.mc_weekends(r, c) = TCS_MATRIX_INDEX(var(P_WEEKEND_SCHEDULE), r, c);

		// Hardcode member data that TCS models don't use but class requires
		mc_tou.ms_params.mc_pricing.mc_weekdays = mc_tou.ms_params.mc_csp_ops.mc_weekdays;
		mc_tou.ms_params.mc_pricing.mc_weekends = mc_tou.ms_params.mc_csp_ops.mc_weekends;

		std::vector<double> turbine_fracs;
		turbine_fracs.resize(9,0.0);

		mc_tou.ms_params.mc_csp_ops.mvv_tou_arrays[C_block_schedule_csp_ops::TURB_FRAC] = turbine_fracs;
		mc_tou.ms_params.mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE] = turbine_fracs;

		int out_type = -1;
		std::string out_msg = "";

		try
		{
			mc_tou.init();
		}
		catch(C_csp_exception &csp_exception)
		{
			// Report warning before exiting with error
			while( mc_tou.mc_csp_messages.get_message(&out_type, &out_msg))
			{
				if( out_type == C_csp_messages::NOTICE )
					message(TCS_NOTICE, out_msg.c_str());
				else if( out_type == C_csp_messages::WARNING )
					message(TCS_WARNING, out_msg.c_str());
			}
			message(TCS_ERROR, csp_exception.m_error_message.c_str());
			return -1;
		}

		// If no exception, then report messages and move on
		while( mc_tou.mc_csp_messages.get_message(&out_type, &out_msg) )
		{
			if( out_type == C_csp_messages::NOTICE )
				message(TCS_NOTICE, out_msg.c_str());
			else if( out_type == C_csp_messages::WARNING )
				message(TCS_WARNING, out_msg.c_str());
		}

		

		return 0;
	}

	virtual int call( double time, double step, int ncall )
	{
		
		int out_type = -1;
		std::string out_msg = "";
		
		try
		{
			mc_tou.call(time, ms_outputs);
		}

		
		catch(C_csp_exception &csp_exception)
		{
			// Report warning before exiting with error
			while( mc_tou.mc_csp_messages.get_message(&out_type, &out_msg) )
			{
				if( out_type == C_csp_messages::NOTICE )
					message(TCS_NOTICE, out_msg.c_str());
				else if( out_type == C_csp_messages::WARNING )
					message(TCS_WARNING, out_msg.c_str());
			}
			message(TCS_ERROR, csp_exception.m_error_message.c_str());
			return -1;
		}

		// If no exception, then report messages and move on
		while( mc_tou.mc_csp_messages.get_message(&out_type, &out_msg) )
		{
			if( out_type == C_csp_messages::NOTICE )
				message(TCS_NOTICE, out_msg.c_str());
			else if( out_type == C_csp_messages::WARNING )
				message(TCS_WARNING, out_msg.c_str());
		}

		value(O_TOU_VALUE, ms_outputs.m_csp_op_tou);

		return 0;
	}
};


TCS_IMPLEMENT_TYPE( tou_translator, "Time of Use translator", "Tom Ferguson", 1, tou_translator_variables, NULL, 0 )

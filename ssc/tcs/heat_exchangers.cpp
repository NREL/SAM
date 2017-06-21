#include "heat_exchangers.h"
#include "csp_solver_util.h"
#include "sam_csp_util.h"
#include <algorithm>
#include "numeric_solvers.h"

double NS_HX_counterflow_eqs::calc_max_q_dot_enth(int hot_fl_code /*-*/, HTFProperties & hot_htf_class,
	int cold_fl_code /*-*/, HTFProperties & cold_htf_class,
	double h_h_in /*kJ/kg*/, double P_h_in /*kPa*/, double P_h_out /*kPa*/, double m_dot_h /*kg/s*/,
	double h_c_in /*kJ/kg*/, double P_c_in /*kPa*/, double P_c_out /*kPa*/, double m_dot_c /*kg/s*/)
{
	int prop_error_code = 0;

	// Calculate the hot and cold temperatures at the node
	double T_h_in = std::numeric_limits<double>::quiet_NaN();
	if (hot_fl_code == NS_HX_counterflow_eqs::CO2)
	{
		CO2_state ms_co2_props;
		prop_error_code = CO2_PH(P_h_in, h_h_in, &ms_co2_props);
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::design",
				"Hot side CO2 inlet enthalpy calculations failed", 12));
		}
		T_h_in = ms_co2_props.temp;		//[K]
	}
	else if (hot_fl_code == NS_HX_counterflow_eqs::WATER)
	{
		water_state ms_water_props;
		prop_error_code = water_PH(P_h_in, h_h_in, &ms_water_props);
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::calc_max_q_dot_enth",
				"Hot side water/steam inlet enthalpy calculations failed", 12));
		}
		T_h_in = ms_water_props.temp;	//[K]
	}
	else
	{
		T_h_in = hot_htf_class.temp_lookup(h_h_in); //[K]
	}

	double T_c_in = std::numeric_limits<double>::quiet_NaN();
	if (cold_fl_code == NS_HX_counterflow_eqs::CO2)
	{
		CO2_state ms_co2_props;
		prop_error_code = CO2_PH(P_c_in, h_c_in, &ms_co2_props);
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::design",
				"Cold side inlet enthalpy calculations failed", 13));
		}
		T_c_in = ms_co2_props.temp;		//[K]
	}
	else if (cold_fl_code == NS_HX_counterflow_eqs::WATER)
	{
		water_state ms_water_props;
		prop_error_code = water_PH(P_c_in, h_c_in, &ms_water_props);
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::calc_max_q_dot_enth",
				"Cold side water/steam inlet enthalpy calculations failed", 12));
		}
		T_c_in = ms_water_props.temp;	//[K]
	}
	else
	{
		T_c_in = cold_htf_class.temp_lookup(h_c_in);	//[K]
	}

	// **********************************************************************************
	// **********************************************************************************
	double Q_dot_cold_max = std::numeric_limits<double>::quiet_NaN();
	if (cold_fl_code == NS_HX_counterflow_eqs::CO2)
	{
		CO2_state ms_co2_props;
		prop_error_code = CO2_TP(T_h_in, P_c_out, &ms_co2_props);
		if (prop_error_code == 205)
		{
			prop_error_code = CO2_TQ(T_h_in, 0.0, &ms_co2_props);
		}
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::calc_max_q_dot",
				"Cold side inlet enthalpy calculations at effectiveness calc failed", 12));
		}
		double h_c_out_max = max(ms_co2_props.enth, h_c_in);	//[kJ/kg]
		Q_dot_cold_max = m_dot_c*(h_c_out_max - h_c_in);	//[kWt]
	}
	else if (cold_fl_code == NS_HX_counterflow_eqs::WATER)
	{
		water_state ms_water_props;
		prop_error_code = water_TP(T_h_in, P_c_out, &ms_water_props);
		if (prop_error_code == 205)
		{
			prop_error_code = water_TQ(T_h_in, 0.0, &ms_water_props);
		}
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::calc_max_q_dot_enth",
				"Cold side water/steam enthalpy calcs at max eff failed", 12));
		}
		double h_c_out_max = max(ms_water_props.enth, h_c_in);	//[kJ/kg]
		Q_dot_cold_max = m_dot_c*(h_c_out_max - h_c_in);
	}
	else
	{
		double h_c_in = cold_htf_class.enth_lookup(T_c_in);		//[kJ/kg]
		double h_c_out_max = cold_htf_class.enth_lookup(T_h_in);//[kJ/kg]
		Q_dot_cold_max = m_dot_c*(h_c_out_max - h_c_in);	//[kWt]
	}

	double Q_dot_hot_max = std::numeric_limits<double>::quiet_NaN();
	if (hot_fl_code == NS_HX_counterflow_eqs::CO2)
	{
		CO2_state ms_co2_props;
		prop_error_code = CO2_TP(T_c_in, P_h_out, &ms_co2_props);
		if (prop_error_code == 205)
		{
			prop_error_code = CO2_TQ(T_c_in, 1.0, &ms_co2_props);
		}
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::calc_max_q_dot",
				"Hot side inlet enthalpy calculations at effectiveness calc failed", 12));
		}
		double h_h_out_min = min(h_h_in, ms_co2_props.enth);	//[kJ/kg]
		Q_dot_hot_max = m_dot_h*(h_h_in - h_h_out_min);	//[kWt]
	}
	else if (hot_fl_code == NS_HX_counterflow_eqs::WATER)
	{
		water_state ms_water_props;
		prop_error_code = water_TP(T_c_in, P_h_out, &ms_water_props);
		if (prop_error_code == 205)
		{
			prop_error_code = water_TQ(T_c_in, 1.0, &ms_water_props);
		}
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::calc_max_q_dot_enth",
				"Hot side water/stream inlet enthalpy calcs at max eff failed", 12));
		}
		double h_h_out_min = min(h_h_in, ms_water_props.enth);	//[kJ/kg]
		Q_dot_hot_max = m_dot_h*(h_h_in - h_h_out_min);		//[kWt]
	}
	else
	{
		double h_h_in = hot_htf_class.enth_lookup(T_h_in);		//[kJ/kg]
		double h_h_out_min = hot_htf_class.enth_lookup(T_c_in);	//[kJ/kg]
		Q_dot_hot_max = m_dot_h*(h_h_in - h_h_out_min);		//[kWt]
	}

	return min(Q_dot_hot_max, Q_dot_cold_max);
}

double NS_HX_counterflow_eqs::calc_max_q_dot(int hot_fl_code /*-*/, HTFProperties & hot_htf_class,
	int cold_fl_code /*-*/, HTFProperties & cold_htf_class,
	double T_h_in, double P_h_in, double P_h_out, double m_dot_h,
	double T_c_in, double P_c_in, double P_c_out, double m_dot_c)
{
	int prop_error_code = 0;

	double h_c_in = std::numeric_limits<double>::quiet_NaN();
	if (cold_fl_code == NS_HX_counterflow_eqs::CO2)
	{
		CO2_state ms_co2_props;
		prop_error_code = CO2_TP(T_c_in, P_c_in, &ms_co2_props);
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::calc_max_q_dot",
				"Cold side inlet enthalpy calculations at effectiveness calc failed", 12));
		}
		h_c_in = ms_co2_props.enth;
	}
	else if (cold_fl_code == NS_HX_counterflow_eqs::WATER)
	{
		water_state ms_water_props;
		prop_error_code = water_TP(T_c_in, P_c_in, &ms_water_props);
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::calc_max_q_dot",
				"Cold side water/steam inlet enthalpy calculations at effectiveness calc failed", 12));
		}
		h_c_in = ms_water_props.enth;	//[kJ/kg]
	}
	else
	{
		h_c_in = cold_htf_class.enth_lookup(T_c_in);	//[kJ/kg]
	}

	double h_h_in = std::numeric_limits<double>::quiet_NaN();
	if (hot_fl_code == NS_HX_counterflow_eqs::CO2)
	{
		CO2_state ms_co2_props;
		prop_error_code = CO2_TP(T_h_in, P_h_in, &ms_co2_props);
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::calc_max_q_dot",
				"Hot side inlet enthalpy calculations at effectiveness calc failed", 12));
		}
		h_h_in = ms_co2_props.enth;
	}
	else if (hot_fl_code == NS_HX_counterflow_eqs::WATER)
	{
		water_state ms_water_props;
		prop_error_code = water_TP(T_h_in, P_h_in, &ms_water_props);
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::calc_max_q_dot",
				"Hot side water/steam inlet enthalpy calculations at effectiveness calc failed", 12));
		}
		h_h_in = ms_water_props.enth;	//[kJ/kg]
	}
	else
	{
		h_h_in = hot_htf_class.enth_lookup(T_h_in);	//[kJ/kg]
	}

	return NS_HX_counterflow_eqs::calc_max_q_dot_enth(hot_fl_code, hot_htf_class,
											cold_fl_code, cold_htf_class,
											h_h_in, P_h_in, P_h_out, m_dot_h,
											h_c_in, P_c_in, P_c_out, m_dot_c);
}

void NS_HX_counterflow_eqs::calc_req_UA(int hot_fl_code /*-*/, HTFProperties & hot_htf_class,
	int cold_fl_code /*-*/, HTFProperties & cold_htf_class,
	int N_sub_hx /*-*/,
	double q_dot /*kWt*/, double m_dot_c /*kg/s*/, double m_dot_h /*kg/s*/,
	double T_c_in /*K*/, double T_h_in /*K*/, double P_c_in /*kPa*/, double P_c_out /*kPa*/, double P_h_in /*kPa*/, double P_h_out /*kPa*/,
	double & UA /*kW/K*/, double & min_DT /*C*/, double & eff /*-*/, double & NTU /*-*/, double & T_h_out /*K*/, double & T_c_out /*K*/, double & q_dot_calc /*kWt*/)
{
	// Check inputs
	if (q_dot < 0.0)
	{
		throw(C_csp_exception("C_HX_counterflow::design",
			"Input heat transfer rate is less than 0.0. It must be >= 0.0", 4));
	}
	if (m_dot_c < 1.E-14)
	{
		throw(C_csp_exception("C_HX_counterflow::design",
			"The cold mass flow rate must be a positive value"));
	}
	if (m_dot_h < 1.E-14)
	{
		throw(C_csp_exception("C_HX_counterflow::design",
			"The hot mass flow rate must be a positive value"));
	}
	if (T_h_in < T_c_in)
	{
		throw(C_csp_exception("C_HX_counterflow::design",
			"Inlet hot temperature is colder than the cold inlet temperature", 5));
	}
	if (P_h_in < P_h_out)
	{
		throw(C_csp_exception("C_HX_counterflow::design",
			"Hot side outlet pressure is greater than hot side inlet pressure", 6));
	}
	if (P_c_in < P_c_out)
	{
		throw(C_csp_exception("C_HX_counterflow::design",
			"Cold side outlet pressure is greater than cold side inlet pressure", 7));
	}
	if (q_dot <= 1.E-14)	// very low Q_dot; assume it is zero
	{
		// Set outputs, and S_des_solved members		
		UA = 0.0;					//[kW/K]
		NTU = 0.0;					//[-]
		q_dot_calc = 0.0;			//[kW]
		min_DT = T_h_in - T_c_in;	//[K]
		eff = 0.0;			//[-]
		T_h_out = T_h_in;	//[K]
		T_c_out = T_c_in;	//[K]

		return;
	}

	// Calculate inlet enthalpies from known state points
	double h_c_in = std::numeric_limits<double>::quiet_NaN();
	double h_h_in = std::numeric_limits<double>::quiet_NaN();
	int prop_error_code = 0;

	if (cold_fl_code == NS_HX_counterflow_eqs::CO2)
	{
		CO2_state ms_co2_props;
		prop_error_code = CO2_TP(T_c_in, P_c_in, &ms_co2_props);
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::design",
				"Cold side inlet enthalpy calculations failed", 8));
		}
		h_c_in = ms_co2_props.enth;		//[kJ/kg]
	}
	else if (cold_fl_code == NS_HX_counterflow_eqs::WATER)
	{
		water_state ms_water_props;
		prop_error_code = water_TP(T_c_in, P_c_in, &ms_water_props);
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::design",
				"Cold side water inlet enthalpy calculations failed", 8));
		}
		h_c_in = ms_water_props.enth;	//[kJ/kg]
	}
	else
	{
		h_c_in = cold_htf_class.enth_lookup(T_c_in);	//[kJ/kg]
	}

	if (hot_fl_code == NS_HX_counterflow_eqs::CO2)
	{
		CO2_state ms_co2_props;
		prop_error_code = CO2_TP(T_h_in, P_h_in, &ms_co2_props);
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::design",
				"Hot side inlet enthalpy calculations failed", 9));
		}
		h_h_in = ms_co2_props.enth;			//[kJ/kg]
	}
	else if (hot_fl_code == NS_HX_counterflow_eqs::WATER)
	{
		water_state ms_water_props;
		prop_error_code = water_TP(T_h_in, P_h_in, &ms_water_props);
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::design",
				"Hot side water/steam inlet enthalpy calculations failed", 9));
		}
		h_h_in = ms_water_props.enth;		//[kJ/kg]
	}
	else
	{
		h_h_in = hot_htf_class.enth_lookup(T_h_in); //[kJ/kg]
	}

	// Know h_h_in and h_c_in, so can call 'calc_req_UA_enth' here
	double h_c_out = std::numeric_limits<double>::quiet_NaN();
	double h_h_out = std::numeric_limits<double>::quiet_NaN();
	NS_HX_counterflow_eqs::calc_req_UA_enth(hot_fl_code, hot_htf_class,
		cold_fl_code, cold_htf_class,
		N_sub_hx,
		q_dot, m_dot_c, m_dot_h, 
		h_c_in, h_h_in, P_c_in, P_c_out, P_h_in, P_h_out, 
		h_h_out, T_h_out, h_c_out, T_c_out,
		UA, min_DT, eff, NTU, q_dot_calc);

	return;
}

void NS_HX_counterflow_eqs::calc_req_UA_enth(int hot_fl_code /*-*/, HTFProperties & hot_htf_class,
	int cold_fl_code /*-*/, HTFProperties & cold_htf_class,
	int N_sub_hx /*-*/,
	double q_dot /*kWt*/, double m_dot_c /*kg/s*/, double m_dot_h /*kg/s*/,
	double h_c_in /*kJ/kg*/, double h_h_in /*kJ/kg*/, double P_c_in /*kPa*/, double P_c_out /*kPa*/, double P_h_in /*kPa*/, double P_h_out /*kPa*/,
	double & h_h_out /*kJ/kg*/, double & T_h_out /*K*/, double & h_c_out /*kJ/kg*/, double & T_c_out /*K*/,
	double & UA /*kW/K*/, double & min_DT /*C*/, double & eff /*-*/, double & NTU /*-*/, double & q_dot_calc /*kWt*/)
{
	// Check inputs
	if (q_dot < 0.0)
	{
		throw(C_csp_exception("C_HX_counterflow::design",
			"Input heat transfer rate is less than 0.0. It must be >= 0.0", 4));
	}
	if (m_dot_c < 1.E-14)
	{
		throw(C_csp_exception("C_HX_counterflow::design",
			"The cold mass flow rate must be a positive value"));
	}
	if (m_dot_h < 1.E-14)
	{
		throw(C_csp_exception("C_HX_counterflow::design",
			"The hot mass flow rate must be a positive value"));
	}
	if (P_h_in < P_h_out)
	{
		throw(C_csp_exception("C_HX_counterflow::design",
			"Hot side outlet pressure is greater than hot side inlet pressure", 6));
	}
	if (P_c_in < P_c_out)
	{
		throw(C_csp_exception("C_HX_counterflow::design",
			"Cold side outlet pressure is greater than cold side inlet pressure", 7));
	}

	// Calculate outlet stream states
	CO2_state ms_co2_props;
	water_state ms_water_props;
	int prop_error_code = 0;

	h_h_out = h_h_in - q_dot / m_dot_h;		//[kJ/kg]
	h_c_out = h_c_in + q_dot / m_dot_c;		//[kJ/kg]	

	int N_nodes = N_sub_hx + 1;
	double h_h_prev = 0.0;
	double T_h_prev = 0.0;
	double h_c_prev = 0.0;
	double T_c_prev = 0.0;
	double T_c_in = std::numeric_limits<double>::quiet_NaN();	//[K]
	double T_h_in = std::numeric_limits<double>::quiet_NaN();	//[K]

	// Loop through the sub-heat exchangers
	UA = 0.0;
	min_DT = T_h_in;
	for (int i = 0; i < N_nodes; i++)
	{
		// Assume pressure varies linearly through heat exchanger
		double P_c = P_c_out + i*(P_c_in - P_c_out) / (N_nodes - 1);
		double P_h = P_h_in - i*(P_h_in - P_h_out) / (N_nodes - 1);

		// Calculate the entahlpy at the node
		double h_c = h_c_out + i*(h_c_in - h_c_out) / (N_nodes - 1);
		double h_h = h_h_in - i*(h_h_in - h_h_out) / (N_nodes - 1);

		// ****************************************************
		// Calculate the hot and cold temperatures at the node
		double T_h = std::numeric_limits<double>::quiet_NaN();
		if (hot_fl_code == NS_HX_counterflow_eqs::CO2)
		{
			prop_error_code = CO2_PH(P_h, h_h, &ms_co2_props);
			if (prop_error_code != 0)
			{
				throw(C_csp_exception("C_HX_counterflow::design",
					"Cold side inlet enthalpy calculations failed", 12));
			}
			T_h = ms_co2_props.temp;		//[K]
		}
		else if (hot_fl_code == NS_HX_counterflow_eqs::WATER)
		{
			prop_error_code = water_PH(P_h, h_h, &ms_water_props);
			if (prop_error_code != 0)
			{
				throw(C_csp_exception("C_HX_counterflow::calc_req_UA_enth",
					"Cold side inlet enthalpy calculations failed", 12));
			}
			T_h = ms_water_props.temp;		//[K]
		}
		else
		{
			T_h = hot_htf_class.temp_lookup(h_h);	//[K]
		}

		double T_c = std::numeric_limits<double>::quiet_NaN();
		if (cold_fl_code == NS_HX_counterflow_eqs::CO2)
		{
			prop_error_code = CO2_PH(P_c, h_c, &ms_co2_props);
			if (prop_error_code != 0)
			{
				throw(C_csp_exception("C_HX_counterflow::design",
					"Cold side inlet enthalpy calculations failed", 13));
			}
			T_c = ms_co2_props.temp;		//[K]
		}
		else if (cold_fl_code == NS_HX_counterflow_eqs::WATER)
		{
			prop_error_code = water_PH(P_c, h_c, &ms_water_props);
			if (prop_error_code != 0)
			{
				throw(C_csp_exception("C_HX_counterflow::calc_req_UA_enth",
					"Cold side water/steam inlet enthalpy calculations failed", 13));
			}
			T_c = ms_water_props.temp;		//[K]
		}
		else
		{
			T_c = cold_htf_class.temp_lookup(h_c);	//[K]
		}

		if (i == 0)
		{
			T_h_in = T_h;
			T_c_out = T_c;
		}
		if (i == N_nodes - 1)
		{
			T_h_out = T_h;
			T_c_in = T_c;
		}

		// ****************************************************
		// ****************************************************
		// ****************************************************

		// Check that 2nd law is not violated
		if (T_c >= T_h)
		{
			throw(C_csp_exception("C_HX_counterflow::design",
				"Cold temperature is hotter than hot temperature.", 11));
		}

		// Track the minimum temperature difference in the heat exchanger
		min_DT = fmin(min_DT, T_h - T_c);

		// Perform effectiveness-NTU and UA calculations 
		if (i > 0)
		{
			bool is_h_2phase = false;
			if (fabs(T_h_prev - T_h) < 0.001)
			{
				is_h_2phase = true;
			}
			bool is_c_2phase = false;
			if (fabs(T_c_prev - T_c) < 0.001)
			{
				is_c_2phase = true;
			}

			double C_dot_min, C_R;
			C_dot_min = C_R = std::numeric_limits<double>::quiet_NaN();

			if (is_h_2phase && !is_c_2phase)
			{
				C_dot_min = m_dot_c*(h_c_prev - h_c) / (T_c_prev - T_c);			// [kW/K] cold stream capacitance rate
				C_R = 0.0;
			}
			else if (!is_c_2phase && is_h_2phase)
			{
				C_dot_min = m_dot_h*(h_h_prev - h_h) / (T_h_prev - T_h);			// [kW/K] hot stream capacitance rate
				C_R = 0.0;
			}
			else if (is_c_2phase && is_h_2phase)
			{
				C_dot_min = q_dot / (double)N_sub_hx * 1.E10 * (T_h_prev - T_c);
				C_R = 1.0;
			}
			else
			{
				double C_dot_h = m_dot_h*(h_h_prev - h_h) / (T_h_prev - T_h);			// [kW/K] hot stream capacitance rate
				double C_dot_c = m_dot_c*(h_c_prev - h_c) / (T_c_prev - T_c);			// [kW/K] cold stream capacitance rate
				C_dot_min = fmin(C_dot_h, C_dot_c);						// [kW/K] Minimum capacitance stream
				double C_dot_max = fmax(C_dot_h, C_dot_c);				// [kW/K] Maximum capacitance stream
				C_R = C_dot_min / C_dot_max;						// [-] Capacitance ratio of sub-heat exchanger
			}

			double eff = min(0.99999, (q_dot / (double)N_sub_hx) / (C_dot_min*(T_h_prev - T_c)));	// [-] Effectiveness of each sub-heat exchanger
			double NTU = 0.0;
			if (C_R != 1.0)
				NTU = log((1.0 - eff*C_R) / (1.0 - eff)) / (1.0 - C_R);		// [-] NTU if C_R does not equal 1
			else
				NTU = eff / (1.0 - eff);
			UA += NTU*C_dot_min;								//[kW/K] Sum UAs for each hx section
		}
		h_h_prev = h_h;
		T_h_prev = T_h;
		h_c_prev = h_c;
		T_c_prev = T_c;
	}

	// Check for NaNs in UA
	if (UA != UA)
	{
		throw(C_csp_exception("C_HX_counterflow::design",
			"NaN found for total heat exchanger UA", 14));
	}

	q_dot_calc = q_dot;

	// **************************************************************
	// Calculate the HX effectiveness

	double q_dot_max = NS_HX_counterflow_eqs::calc_max_q_dot_enth(hot_fl_code, hot_htf_class,
		cold_fl_code, cold_htf_class,
		h_h_in, P_h_in, P_h_out, m_dot_h,
		h_c_in, P_c_in, P_c_out, m_dot_c);

	eff = q_dot / q_dot_max;

	bool is_h_2phase = false;
	if (fabs(T_h_in - T_h_out) < 0.001)
	{
		is_h_2phase = true;
	}
	bool is_c_2phase = false;
	if (fabs(T_c_out - T_c_in) < 0.001)
	{
		is_c_2phase = true;
	}

	double C_dot_min, C_R;
	C_dot_min = C_R = std::numeric_limits<double>::quiet_NaN();

	if (is_h_2phase && !is_c_2phase)
	{
		C_dot_min = m_dot_c*(h_c_out - h_c_in) / (T_c_out - T_c_in);			// [kW/K] cold stream capacitance rate
		C_R = 0.0;
	}
	else if (!is_c_2phase && is_h_2phase)
	{
		C_dot_min = m_dot_h*(h_h_in - h_h_out) / (T_h_in - T_h_out);			// [kW/K] hot stream capacitance rate
		C_R = 0.0;
	}
	else if (is_c_2phase && is_h_2phase)
	{
		C_dot_min = q_dot / (double)N_sub_hx * 1.E10 * (T_h_in - T_c_in);
		C_R = 1.0;
	}
	else
	{
		double C_dot_h = m_dot_h*(h_h_in - h_h_out) / (T_h_in - T_h_out);			// [kW/K] hot stream capacitance rate
		double C_dot_c = m_dot_c*(h_c_out - h_c_in) / (T_c_out - T_c_in);			// [kW/K] cold stream capacitance rate
		C_dot_min = fmin(C_dot_h, C_dot_c);						// [kW/K] Minimum capacitance stream
		double C_dot_max = fmax(C_dot_h, C_dot_c);				// [kW/K] Maximum capacitance stream
		C_R = C_dot_min / C_dot_max;						// [-] Capacitance ratio of sub-heat exchanger
	}

	if (C_R != 1.0)
		NTU = log((1.0 - eff*C_R) / (1.0 - eff)) / (1.0 - C_R);		// [-] NTU if C_R does not equal 1
	else
		NTU = eff / (1.0 - eff);

	return;
}

int NS_HX_counterflow_eqs::C_mono_eq_UA_v_q_enth::operator()(double q_dot /*kWt*/, double *UA_calc /*kW/K*/)
{
	double q_dot_calc = std::numeric_limits<double>::quiet_NaN();

	try
	{
		NS_HX_counterflow_eqs::calc_req_UA_enth(m_hot_fl_code, mc_hot_htf_class,
			m_cold_fl_code, mc_cold_htf_class,
			m_N_sub_hx,
			q_dot, m_m_dot_c, m_m_dot_h, 
			m_h_c_in, m_h_h_in, m_P_c_in, m_P_c_out, m_P_h_in, m_P_h_out, 
			m_h_h_out, m_T_h_out, m_h_c_out, m_T_c_out,
			m_UA_calc, m_min_DT, m_eff, m_NTU, q_dot_calc);
	}
	catch (C_csp_exception &csp_except)
	{
		int hx_error_code = csp_except.m_error_code;

		// Reset solved OD parameters to NaN
		m_T_c_out = m_T_h_out = std::numeric_limits<double>::quiet_NaN();

		// reset 'UA_calc' to NaN
		*UA_calc = std::numeric_limits<double>::quiet_NaN();		//[kW/K]

		return -1;
	}

	*UA_calc = m_UA_calc;		//[kW/K]

	return 0;
}

void NS_HX_counterflow_eqs::solve_q_dot_for_fixed_UA_enth(int hot_fl_code /*-*/, HTFProperties & hot_htf_class,
	int cold_fl_code /*-*/, HTFProperties & cold_htf_class,
	int N_sub_hx /*-*/,
	double h_c_in /*K*/, double P_c_in /*kPa*/, double m_dot_c /*kg/s*/, double P_c_out /*kPa*/,
	double h_h_in /*K*/, double P_h_in /*kPa*/, double m_dot_h /*kg/s*/, double P_h_out /*kPa*/,
	double UA_target /*kW/K*/, double eff_limit /*-*/, double eff_guess /*-*/,
	double & T_c_out  /*K*/, double & h_c_out /*kJ/kg*/,
	double & T_h_out /*K*/, double & h_h_out /*kJ/kg*/,
	double & q_dot /*kWt*/, double & eff_calc /*-*/, double & min_DT /*K*/, double & NTU /*-*/, double & UA_calc)
{
	if (UA_target < 1.E-10)
	{
		q_dot = 0.0;

		double q_dot_calc = std::numeric_limits<double>::quiet_NaN();

		try
		{
			NS_HX_counterflow_eqs::calc_req_UA_enth(hot_fl_code, hot_htf_class,
				cold_fl_code, cold_htf_class,
				N_sub_hx,
				q_dot, m_dot_c, m_dot_h,
				h_c_in, h_h_in, P_c_in, P_c_out, P_h_in, P_h_out,
				h_h_out, T_h_out, h_c_out, T_c_out,
				UA_calc, min_DT, eff_calc, NTU, q_dot_calc);
		}
		catch (C_csp_exception &csp_except)
		{
			throw(C_csp_exception("C_HX_counterflow::hx_solution(...) failed with UA_target < 1.E-10"));
		}

		q_dot = q_dot_calc;			//[kWt]

		return;
	}

	double q_dot_max = NS_HX_counterflow_eqs::calc_max_q_dot_enth(hot_fl_code, hot_htf_class,
		cold_fl_code, cold_htf_class,
		h_h_in, P_h_in, P_h_out, m_dot_h,
		h_c_in, P_c_in, P_c_out, m_dot_c);

	double q_dot_upper = eff_limit*q_dot_max;

	// Use design point effectiveness to generate 2 guess values
	double q_dot_mult = max(0.99, min(0.95, eff_limit) / eff_limit);
	if (eff_guess == eff_guess)
	{
		q_dot_mult = max(0.99, min(0.1, eff_guess));
	}

	double q_dot_guess_upper = q_dot_mult*q_dot_upper;
	double q_dot_guess_lower = 0.85*q_dot_guess_upper;

	// Complete solver settings
	double tol = 0.001;
	double q_dot_lower = 1.E-10;		//[kWt]

	NS_HX_counterflow_eqs::C_mono_eq_UA_v_q_enth od_hx_eq(hot_fl_code, hot_htf_class,
		cold_fl_code, cold_htf_class,
		N_sub_hx,
		P_c_out, P_h_out,
		h_c_in, P_c_in, m_dot_c,
		h_h_in, P_h_in, m_dot_h);
	C_monotonic_eq_solver od_hx_solver(od_hx_eq);

	// First, test at q_dot_upper
	double UA_max_eff = std::numeric_limits<double>::quiet_NaN();
	int test_code = od_hx_solver.test_member_function(q_dot_upper, &UA_max_eff);

	double q_dot_solved = std::numeric_limits<double>::quiet_NaN();
	if (test_code != 0 || UA_max_eff > UA_target)
	{
		// Set solver settings
		od_hx_solver.settings(tol, 1000, q_dot_lower, q_dot_upper, true);

		// Solve
		double tol_solved;
		q_dot_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
		int iter_solved = -1;

		int od_hx_code = od_hx_solver.solve(q_dot_guess_lower, q_dot_guess_upper, UA_target,
			q_dot_solved, tol_solved, iter_solved);

		// UA vs. q_dot is very nonlinear, with very large increases of UA as q_dot approaches q_dot_max
		// As such, may not reach convergence on UA while the uncertainty on q_dot is very small, which should be ok
		if (!(od_hx_code == C_monotonic_eq_solver::CONVERGED || od_hx_code == C_monotonic_eq_solver::SLOPE_POS_NO_POS_ERR || od_hx_code == C_monotonic_eq_solver::SLOPE_POS_BOTH_ERRS))
		{
			throw(C_csp_exception("Off-design heat exchanger method failed"));
		}
	}
	else
	{
		double hit_max_eff = 1.23;
	}

	T_c_out = od_hx_eq.m_T_c_out;	//[K]
	h_c_out = od_hx_eq.m_h_c_out;	//[kJ/kg]
	T_h_out = od_hx_eq.m_T_h_out;	//[K]
	h_h_out = od_hx_eq.m_h_h_out;	//[kJ/kg]

	q_dot = q_dot_solved;
	eff_calc = od_hx_eq.m_eff;		//[-]
	min_DT = od_hx_eq.m_min_DT;		//[K]
	NTU = od_hx_eq.m_NTU;			//[-]
	UA_calc = od_hx_eq.m_UA_calc;
}

void NS_HX_counterflow_eqs::solve_q_dot_for_fixed_UA(int hot_fl_code /*-*/, HTFProperties & hot_htf_class,
	int cold_fl_code /*-*/, HTFProperties & cold_htf_class,
	int N_sub_hx /*-*/,
	double T_c_in /*K*/, double P_c_in /*kPa*/, double m_dot_c /*kg/s*/, double P_c_out /*kPa*/,
	double T_h_in /*K*/, double P_h_in /*kPa*/, double m_dot_h /*kg/s*/, double P_h_out /*kPa*/,
	double UA_target /*kW/K*/, double eff_limit /*-*/, double eff_guess /*-*/,
	double & q_dot /*kWt*/, double & T_c_out /*K*/, double & T_h_out /*K*/,
	double & eff_calc /*-*/, double & min_DT /*K*/, double & NTU /*-*/, double & UA_calc)
{
	// Calculate inlet enthalpies from known state points
	double h_c_in = std::numeric_limits<double>::quiet_NaN();
	double h_h_in = std::numeric_limits<double>::quiet_NaN();
	int prop_error_code = 0;

	if (cold_fl_code == NS_HX_counterflow_eqs::CO2)
	{
		CO2_state ms_co2_props;
		prop_error_code = CO2_TP(T_c_in, P_c_in, &ms_co2_props);
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::design",
				"Cold side inlet enthalpy calculations failed", 8));
		}
		h_c_in = ms_co2_props.enth;		//[kJ/kg]
	}
	else if (cold_fl_code == NS_HX_counterflow_eqs::WATER)
	{
		water_state ms_water_props;
		prop_error_code = water_TP(T_c_in, P_c_in, &ms_water_props);
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::design",
				"Cold side water inlet enthalpy calculations failed", 8));
		}
		h_c_in = ms_water_props.enth;	//[kJ/kg]
	}
	else
	{
		h_c_in = cold_htf_class.enth_lookup(T_c_in);	//[kJ/kg]
	}

	if (hot_fl_code == NS_HX_counterflow_eqs::CO2)
	{
		CO2_state ms_co2_props;
		prop_error_code = CO2_TP(T_h_in, P_h_in, &ms_co2_props);
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::design",
				"Hot side inlet enthalpy calculations failed", 9));
		}
		h_h_in = ms_co2_props.enth;			//[kJ/kg]
	}
	else if (hot_fl_code == NS_HX_counterflow_eqs::WATER)
	{
		water_state ms_water_props;
		prop_error_code = water_TP(T_h_in, P_h_in, &ms_water_props);
		if (prop_error_code != 0)
		{
			throw(C_csp_exception("C_HX_counterflow::design",
				"Hot side water/steam inlet enthalpy calculations failed", 9));
		}
		h_h_in = ms_water_props.enth;		//[kJ/kg]
	}
	else
	{
		h_h_in = hot_htf_class.enth_lookup(T_h_in); //[kJ/kg]
	}	

	double h_c_out = std::numeric_limits<double>::quiet_NaN();
	double h_h_out = std::numeric_limits<double>::quiet_NaN();
	NS_HX_counterflow_eqs::solve_q_dot_for_fixed_UA_enth(hot_fl_code, hot_htf_class,
		cold_fl_code, cold_htf_class,
		N_sub_hx,
		h_c_in, P_c_in, m_dot_c, P_c_out,
		h_h_in, P_h_in, m_dot_h, P_h_out,
		UA_target, eff_limit, eff_guess,
		T_c_out, h_c_out,
		T_h_out, h_h_out,
		q_dot, eff_calc, min_DT, NTU, UA_calc);

	return;
}

C_HX_counterflow::C_HX_counterflow()
{
	m_is_HX_initialized = false;
	m_is_HX_designed = false;
}

void C_HX_counterflow::initialize(const S_init_par & init_par_in)
{
	// Set member structure
	ms_init_par = init_par_in;

	// Set up HTFProperties for the hot fluid
	if( ms_init_par.m_hot_fl != NS_HX_counterflow_eqs::CO2 && ms_init_par.m_hot_fl != NS_HX_counterflow_eqs::WATER )
	{
		if( ms_init_par.m_hot_fl != HTFProperties::User_defined && ms_init_par.m_hot_fl < HTFProperties::End_Library_Fluids )
		{
			if( !mc_hot_fl.SetFluid(ms_init_par.m_hot_fl, true) )
			{
				throw(C_csp_exception("Hot fluid code is not recognized", "Counter flow heat exchanger initialization"));
			}
		}
		else if( ms_init_par.m_hot_fl == HTFProperties::User_defined )
		{
			int n_rows = ms_init_par.mc_hot_fl_props.nrows();
			int n_cols = ms_init_par.mc_hot_fl_props.ncols();
			if( n_rows > 2 && n_cols == 7 )
			{
				if( !mc_hot_fl.SetUserDefinedFluid(ms_init_par.mc_hot_fl_props, true) )
				{
					std::string error_msg = util::format(mc_hot_fl.UserFluidErrMessage(), n_rows, n_cols);
					throw(C_csp_exception(error_msg, "Counter flow heat exchanger initialization"));
				}
			}
			else
			{
				std::string error_msg = util::format("The user defined hot fluid table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", n_rows, n_cols);
				throw(C_csp_exception(error_msg, "Counter flow heat exchanger initialization"));
			}
		}
		else
		{
			throw(C_csp_exception("Hot fluid code is not recognized", "Counter flow heat exchanger initialization"));
		}
	}

	// Set up HTFProperties for the cold fluid
	if (ms_init_par.m_cold_fl != NS_HX_counterflow_eqs::CO2 && ms_init_par.m_cold_fl != NS_HX_counterflow_eqs::WATER)
	{
		if( ms_init_par.m_cold_fl != HTFProperties::User_defined && ms_init_par.m_cold_fl < HTFProperties::End_Library_Fluids )
		{
			if( !mc_cold_fl.SetFluid(ms_init_par.m_cold_fl, true) )
			{
				throw(C_csp_exception("Cold fluid code is not recognized", "Counter flow heat exchanger initialization"));
			}
		}
		else if( ms_init_par.m_cold_fl == HTFProperties::User_defined )
		{
			int n_rows = ms_init_par.mc_cold_fl_props.nrows();
			int n_cols = ms_init_par.mc_hot_fl_props.ncols();
			if( n_rows > 2 && n_cols == 7 )
			{
				if( !mc_cold_fl.SetUserDefinedFluid(ms_init_par.mc_cold_fl_props, true) )
				{
					std::string error_msg = util::format(mc_cold_fl.UserFluidErrMessage(), n_rows, n_cols);
					throw(C_csp_exception(error_msg, "Counter flow heat exchanger initialization"));
				}
			}
			else
			{
				std::string error_msg = util::format("The user defined cold fluid table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", n_rows, n_cols);
				throw(C_csp_exception(error_msg, "Counter flow heat exchanger initialization"));
			}
		}
		else
		{
			throw(C_csp_exception("Cold fluid code is not recognized", "Counter flow heat exchanger initialization"));
		}
	}

	// Class is initialized
	m_is_HX_initialized = true;

	return;
}

void C_HX_counterflow::calc_req_UA(double q_dot /*kWt*/, double m_dot_c /*kg/s*/, double m_dot_h /*kg/s*/,
	double T_c_in /*K*/, double T_h_in /*K*/, double P_c_in /*kPa*/, double P_c_out /*kPa*/, double P_h_in /*kPa*/, double P_h_out /*kPa*/,
	double & UA /*kW/K*/, double & min_DT /*C*/, double & eff /*-*/, double & NTU /*-*/, double & T_h_out /*K*/, double & T_c_out /*K*/, double & q_dot_calc /*kWt*/)
{
	NS_HX_counterflow_eqs::calc_req_UA(ms_init_par.m_hot_fl, mc_hot_fl,
		ms_init_par.m_cold_fl, mc_cold_fl,
		ms_init_par.m_N_sub_hx,
		q_dot, m_dot_c, m_dot_h,
		T_c_in, T_h_in, P_c_in, P_c_out, P_h_in, P_h_out,
		UA, min_DT, eff, NTU, T_h_out, T_c_out, q_dot_calc);
}

void C_HX_counterflow::calc_req_UA_enth(double q_dot /*kWt*/, double m_dot_c /*kg/s*/, double m_dot_h /*kg/s*/,
	double h_c_in /*kJ/kg*/, double h_h_in /*kJ/kg*/, double P_c_in /*kPa*/, double P_c_out /*kPa*/, double P_h_in /*kPa*/, double P_h_out /*kPa*/,
	double & UA /*kW/K*/, double & min_DT /*C*/, double & eff /*-*/, double & NTU /*-*/, double & h_h_out /*K*/, double & h_c_out /*K*/, double & q_dot_calc /*kWt*/)
{
	double T_h_out = std::numeric_limits<double>::quiet_NaN();
	double T_c_out = std::numeric_limits<double>::quiet_NaN();
	NS_HX_counterflow_eqs::calc_req_UA_enth(ms_init_par.m_hot_fl, mc_hot_fl,
		ms_init_par.m_cold_fl, mc_cold_fl,
		ms_init_par.m_N_sub_hx,
		q_dot, m_dot_c, m_dot_h,
		h_c_in, h_h_in, P_c_in, P_c_out, P_h_in, P_h_out,
		h_h_out, T_h_out, h_c_out, T_c_out,
		UA, min_DT, eff, NTU, q_dot_calc);
}

double C_HX_counterflow::calc_max_q_dot_enth(double h_h_in /*kJ/kg*/, double P_h_in /*kPa*/, double P_h_out /*kPa*/, double m_dot_h /*kg/s*/,
	double h_c_in /*kJ/kg*/, double P_c_in /*kPa*/, double P_c_out /*kPa*/, double m_dot_c /*kg/s*/)
{
	return NS_HX_counterflow_eqs::calc_max_q_dot_enth(ms_init_par.m_hot_fl, mc_hot_fl,
			ms_init_par.m_cold_fl, mc_cold_fl,
			h_h_in, P_h_in, P_h_out, m_dot_h,
			h_c_in, P_c_in, P_c_out, m_dot_c);
}

void C_HX_counterflow::design_calc_UA(C_HX_counterflow::S_des_calc_UA_par des_par,
	double q_dot_design /*kWt*/, C_HX_counterflow::S_des_solved &des_solved)
{
	/*Designs heat exchanger given its mass flow rates, inlet temperatures, and a heat transfer rate.
	Note: the heat transfer rate must be positive.*/
	ms_des_calc_UA_par = des_par;
	ms_des_solved.m_DP_cold_des = des_par.m_P_c_in - des_par.m_P_c_out;		//[kPa]
	ms_des_solved.m_DP_hot_des = des_par.m_P_h_in - des_par.m_P_h_out;		//[kPa]

	// Trying to solve design point, so set boolean to false until method solves successfully
	m_is_HX_designed = false;

	// Check that design parameters are set
	if( !m_is_HX_initialized )
	{
		throw(C_csp_exception("C_HX_counterflow::design",
			"Design parameters are not initialized!"));
	}

	double UA_calc, min_DT_calc, eff_calc, NTU_calc, T_h_out_calc, T_c_out_calc, q_dot_calc;
	UA_calc = min_DT_calc = eff_calc = NTU_calc = T_h_out_calc = T_c_out_calc = q_dot_calc = std::numeric_limits<double>::quiet_NaN();
	
	NS_HX_counterflow_eqs::calc_req_UA(ms_init_par.m_hot_fl, mc_hot_fl,
		ms_init_par.m_cold_fl, mc_cold_fl,
		ms_init_par.m_N_sub_hx,
		q_dot_design, ms_des_calc_UA_par.m_m_dot_cold_des, ms_des_calc_UA_par.m_m_dot_hot_des,
		ms_des_calc_UA_par.m_T_c_in, ms_des_calc_UA_par.m_T_h_in, ms_des_calc_UA_par.m_P_c_in, ms_des_calc_UA_par.m_P_c_out, ms_des_calc_UA_par.m_P_h_in, ms_des_calc_UA_par.m_P_h_out,
		UA_calc, min_DT_calc, eff_calc, NTU_calc, T_h_out_calc, T_c_out_calc, q_dot_calc);

	// Check that calculated effectiveness is less than limit
	if (eff_calc > ms_des_calc_UA_par.m_eff_max)
	{
		std::string msg = util::format("Calculated design effectiveness, %lg [-] is greater than the specified maximum effectiveness, %lg [-].", eff_calc, ms_des_calc_UA_par.m_eff_max);

		throw(C_csp_exception("C_HX_counterflow::design",
			"Calculated design effectiveness, %lg [-] is greater than the specified maximum effectiveness, %lg [-]."));
	}

	ms_des_solved.m_UA_design_total = UA_calc;
	ms_des_solved.m_min_DT_design = min_DT_calc;
	ms_des_solved.m_eff_design = eff_calc;
	ms_des_solved.m_NTU_design = NTU_calc;
	ms_des_solved.m_T_h_out = T_h_out_calc;
	ms_des_solved.m_T_c_out = T_c_out_calc;

	// Specify that method solved successfully
	m_is_HX_designed = true;

	des_solved = ms_des_solved;

	return;
}

void C_HX_co2_to_htf::design_and_calc_m_dot_htf(C_HX_counterflow::S_des_calc_UA_par &des_par, 
			double q_dot_design /*kWt*/, double dt_cold_approach /*C/K*/, C_HX_counterflow::S_des_solved &des_solved)
{
	double T_htf_cold = des_par.m_T_c_in + dt_cold_approach;	//[C]

	double h_h_in = mc_hot_fl.enth_lookup(des_par.m_T_h_in);	//[kJ/kg]
	double h_c_in = mc_hot_fl.enth_lookup(T_htf_cold);			//[kJ/kg]

	des_par.m_m_dot_hot_des = q_dot_design/(h_h_in - h_c_in);

	design_calc_UA(des_par, q_dot_design, des_solved);
}

void C_HX_counterflow::design_fix_UA_calc_outlet(double UA_target /*kW/K*/, double eff_target /*-*/, double T_c_in /*K*/, double P_c_in /*kPa*/, double m_dot_c /*kg/s*/, double P_c_out /*kPa*/,
	double T_h_in /*K*/, double P_h_in /*kPa*/, double m_dot_h /*kg/s*/, double P_h_out /*kPa*/,
	double & q_dot /*kWt*/, double & T_c_out /*K*/, double & T_h_out /*K*/)
{
	ms_des_solved.m_UA_design_total = ms_des_solved.m_min_DT_design = ms_des_solved.m_eff_design = ms_des_solved.m_NTU_design =
		ms_des_solved.m_T_h_out = ms_des_solved.m_T_c_out = ms_des_solved.m_DP_cold_des = ms_des_solved.m_DP_hot_des = std::numeric_limits<double>::quiet_NaN();

	double eff_calc, min_DT, NTU, UA_calc;
	eff_calc = min_DT = NTU = UA_calc = std::numeric_limits<double>::quiet_NaN();
	
	NS_HX_counterflow_eqs::solve_q_dot_for_fixed_UA(ms_init_par.m_hot_fl, mc_hot_fl,
		ms_init_par.m_cold_fl, mc_cold_fl,
		ms_init_par.m_N_sub_hx,
		T_c_in, P_c_in, m_dot_c, P_c_out, 
		T_h_in, P_h_in, m_dot_h, P_h_out, 
		UA_target, eff_target, ms_des_solved.m_eff_design,
		q_dot, T_c_out, T_h_out,
		eff_calc, min_DT, NTU, UA_calc);

	ms_des_calc_UA_par.m_T_h_in = T_h_in;			//[K]
	ms_des_calc_UA_par.m_P_h_in = P_h_in;			//[kPa]
	ms_des_calc_UA_par.m_P_h_out = P_h_out;			//[kPa]
	ms_des_calc_UA_par.m_m_dot_hot_des = m_dot_h;	//[kg/s]
	ms_des_calc_UA_par.m_T_c_in = T_c_in;			//[K]
	ms_des_calc_UA_par.m_P_c_in = P_c_in;			//[kPa]
	ms_des_calc_UA_par.m_P_c_out = P_c_out;			//[kPa]
	ms_des_calc_UA_par.m_m_dot_cold_des = m_dot_c;	//[kg/s]
	ms_des_calc_UA_par.m_eff_max = eff_target;		//[-]

	ms_des_solved.m_Q_dot_design = q_dot;			//[kWt]
	ms_des_solved.m_UA_design_total = UA_calc;		//[kW/K]
	ms_des_solved.m_min_DT_design = min_DT;			//[K]
	ms_des_solved.m_eff_design = eff_calc;			//[-]
	ms_des_solved.m_NTU_design = NTU;				//[-]
	ms_des_solved.m_T_h_out = T_h_out;				//[K]
	ms_des_solved.m_T_c_out = T_c_out;				//[K]
	ms_des_solved.m_DP_cold_des = P_c_in - P_c_out;		//[kPa]
	ms_des_solved.m_DP_hot_des = P_h_in - P_h_out;		//[kPa]
}

void C_HX_counterflow::off_design_solution(double T_c_in /*K*/, double P_c_in /*kPa*/, double m_dot_c /*kg/s*/, double P_c_out /*kPa*/,
	double T_h_in /*K*/, double P_h_in /*kPa*/, double m_dot_h /*kg/s*/, double P_h_out /*kPa*/,
	double & q_dot /*kWt*/, double & T_c_out /*K*/, double & T_h_out /*K*/)
{
	// Set o.d. UA as solver target
	double UA_target = od_UA(m_dot_c, m_dot_h);	//[kW/K]
	double eff_target = ms_des_calc_UA_par.m_eff_max;

	ms_od_solved.m_q_dot = ms_od_solved.m_T_c_out = ms_od_solved.m_P_c_out = 
		ms_od_solved.m_T_h_out = ms_od_solved.m_P_h_out = ms_od_solved.m_UA_total = 
		ms_od_solved.m_min_DT = ms_od_solved.m_eff = ms_od_solved.m_NTU = std::numeric_limits<double>::quiet_NaN();

	double eff_calc, min_DT, NTU, UA_calc;
	eff_calc = min_DT = NTU = UA_calc = std::numeric_limits<double>::quiet_NaN();
	
	NS_HX_counterflow_eqs::solve_q_dot_for_fixed_UA(ms_init_par.m_hot_fl, mc_hot_fl,
		ms_init_par.m_cold_fl, mc_cold_fl,
		ms_init_par.m_N_sub_hx,
		T_c_in, P_c_in, m_dot_c, P_c_out,
		T_h_in, P_h_in, m_dot_h, P_h_out,
		UA_target, eff_target, ms_des_solved.m_eff_design,
		q_dot, T_c_out, T_h_out,
		eff_calc, min_DT, NTU, UA_calc);

	ms_od_solved.m_eff = eff_calc;			//[-]
	ms_od_solved.m_min_DT = min_DT;		//[K]
	ms_od_solved.m_NTU = NTU;			//[-]
	ms_od_solved.m_P_c_out = P_c_out;	//[kPa]
	ms_od_solved.m_P_h_out = P_h_out;	//[kPa]
	ms_od_solved.m_q_dot = q_dot;		//[kWt]
	ms_od_solved.m_T_c_out = T_c_out;	//[K]
	ms_od_solved.m_T_h_out = T_h_out;	//[K]
	ms_od_solved.m_UA_total = UA_calc;	//[kW/K]
}

void C_HX_counterflow::off_design_solution_calc_dP(double T_c_in /*K*/, double P_c_in /*kPa*/, double m_dot_c /*kg/s*/,
	double T_h_in /*K*/, double P_h_in /*kPa*/, double m_dot_h /*kg/s*/,
	double & q_dot /*kWt*/, double & T_c_out /*K*/, double & P_c_out /*kPa*/, double & T_h_out /*K*/, double & P_h_out /*kPa*/)
{
	// Calculate pressure drops
	P_c_out = P_c_in - ms_des_solved.m_DP_cold_des*od_delta_p_cold_frac(m_dot_c);
	P_h_out = P_h_in - ms_des_solved.m_DP_hot_des*od_delta_p_hot_frac(m_dot_h);

	off_design_solution(T_c_in, P_c_in, m_dot_c, P_c_out, T_h_in, P_h_in, m_dot_h, P_h_out, q_dot, T_c_out, T_h_out);
}

double C_HX_counterflow::od_delta_p_cold_frac(double m_dot_c /*kg/s*/)
{
	return pow(m_dot_c/ms_des_calc_UA_par.m_m_dot_cold_des, 1.75);
}

double C_HX_counterflow::od_delta_p_cold(double m_dot_c /*kg/s*/)
{
	return ms_des_solved.m_DP_cold_des*od_delta_p_cold_frac(m_dot_c);
}

double C_HX_counterflow::od_delta_p_hot_frac(double m_dot_h /*kg/s*/)
{
	return pow(m_dot_h/ms_des_calc_UA_par.m_m_dot_hot_des, 1.75);
}

double C_HX_counterflow::od_delta_p_hot(double m_dot_h /*kg/s*/)
{
	return ms_des_solved.m_DP_hot_des*od_delta_p_hot_frac(m_dot_h);
}

double C_HX_counterflow::od_UA(double m_dot_c /*kg/s*/, double m_dot_h /*kg/s*/)
{
	return ms_des_solved.m_UA_design_total*od_UA_frac(m_dot_c, m_dot_h);	//[kW/K]
}

double C_HX_counterflow::od_UA_frac(double m_dot_c /*kg/s*/, double m_dot_h /*kg/s*/)
{
	double m_dot_ratio = 0.5*(m_dot_c/ms_des_calc_UA_par.m_m_dot_cold_des + m_dot_h/ms_des_calc_UA_par.m_m_dot_hot_des);
	return pow(m_dot_ratio, 0.8);
}

void C_HX_co2_to_co2::initialize()
{
	// If number of sub-heat exchangers is not specified, default to 10
	int N_sub_hx = 10;

	initialize(N_sub_hx);
}

void C_HX_co2_to_co2::initialize(int N_sub_hx)
{
	// Set design parameters member structure
	ms_init_par.m_N_sub_hx = N_sub_hx;
	ms_init_par.m_cold_fl = NS_HX_counterflow_eqs::CO2;
	ms_init_par.m_hot_fl = NS_HX_counterflow_eqs::CO2;
	m_is_HX_initialized = true;
}

void C_HX_co2_to_htf::initialize(int hot_fl, util::matrix_t<double> hot_fl_props)
{
	// Hard-code some of the design parameters
	ms_init_par.m_N_sub_hx = 5;
	ms_init_par.m_cold_fl = NS_HX_counterflow_eqs::CO2;

	// Read-in hot side HTF props
	ms_init_par.m_hot_fl = hot_fl;
	ms_init_par.mc_hot_fl_props = hot_fl_props;

	// Set up HTFProperties for the hot fluid
	if( ms_init_par.m_hot_fl != HTFProperties::User_defined && ms_init_par.m_hot_fl < HTFProperties::End_Library_Fluids )
	{
		if( !mc_hot_fl.SetFluid(ms_init_par.m_hot_fl, true) )
		{
			throw(C_csp_exception("Hot fluid code is not recognized", "C_HX_co2_to_htf::initialization"));
		}
	}
	else if( ms_init_par.m_hot_fl == HTFProperties::User_defined )
	{
		int n_rows = ms_init_par.mc_hot_fl_props.nrows();
		int n_cols = ms_init_par.mc_hot_fl_props.ncols();
		if( n_rows > 2 && n_cols == 7 )
		{
			if( !mc_hot_fl.SetUserDefinedFluid(ms_init_par.mc_hot_fl_props, true) )
			{
				std::string error_msg = util::format(mc_hot_fl.UserFluidErrMessage(), n_rows, n_cols);
				throw(C_csp_exception(error_msg, "C_HX_co2_to_htf::initialization"));
			}
		}
		else
		{
			std::string error_msg = util::format("The user defined hot fluid table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", n_rows, n_cols);
			throw(C_csp_exception(error_msg, "C_HX_co2_to_htf::initialization"));
		}
	}
	else
	{
		throw(C_csp_exception("Hot fluid code is not recognized", "C_HX_co2_to_htf::initialization"));
	}

	// Class is initialized
	m_is_HX_initialized = true;

}

void C_HX_co2_to_htf::initialize(int hot_fl)
{
	util::matrix_t<double> null_fluid_props;

	initialize(hot_fl, null_fluid_props);
}

bool N_compact_hx::get_compact_hx_geom(int enum_compact_hx_config, double & d_out, double & fin_pitch, double & D_h,
	double & fin_thk, double & sigma, double & alpha, double & A_fin_to_surf,
	double & s_h, double & s_v, double & fin_V_per_m)
{

	switch( enum_compact_hx_config )
	{
	case fc_tubes_s80_38T:
		d_out = 0.0102;		//[m] Outer tube diameter
		fin_pitch = 315;	//[1/m] Number of fins per meter of tube
		D_h = 0.003632;		//[m] Hydraulic diameter of air side
		fin_thk = 0.0003302;//[m] Fin thickness
		sigma = 0.534;		//[-] Ratio of free-flow to frontal area
		alpha = 587;		//[1/m] Ratio of gas-side heat transfer area to core volume
		A_fin_to_surf = 0.913;	//[-] Ratio of finned to total surface area on gas-side
		s_h = 0.022;		//[m] Distance between tubes in air flow direction
		s_v = 0.0254;		//[m] Distance between tubes perpendicular to air flow direction
		fin_V_per_m = (s_h*s_v - 0.25*CSP::pi*pow(d_out, 2))*fin_thk*fin_pitch;

		return true;

	case fc_tubes_sCF_88_10Jb:
		d_out = 0.02601;		//[m] Outer tube diameter
		fin_pitch = 346;	//[1/m] Number of fins per meter of tube
		D_h = 0.01321;		//[m] Hydraulic diameter of air side
		fin_thk = 0.000305;//[m] Fin thickness
		sigma = 0.642;		//[-] Ratio of free-flow to frontal area
		alpha = 191;		//[1/m] Ratio of gas-side heat transfer area to core volume
		A_fin_to_surf = 0.825;	//[-] Ratio of finned to total surface area on gas-side
		s_h = 0.0524;		//[m] Distance between tubes in air flow direction
		s_v = 0.07818;		//[m] Distance between tubes perpendicular to air flow direction
		fin_V_per_m = 0.25*CSP::pi*(pow(0.04412, 2) - pow(d_out, 2))*fin_thk*fin_pitch;

		return true;

	default:
		return false;
	}

};

bool N_compact_hx::get_compact_hx_f_j(int enum_compact_hx_config, double Re, double & f, double & j_H)
{
	double Re_mm = fmax(0.001, Re*1e-3);

	switch( enum_compact_hx_config )
	{
	case fc_tubes_s80_38T:
		f = 0.02949346*pow(Re_mm, -0.208110211);
		j_H = 0.0105331507*pow(Re_mm, -0.400092073);
		return true;

	case fc_tubes_sCF_88_10Jb:
		f = 0.0606753986*pow(Re_mm, -0.256298233);
		j_H = 0.0148711552*pow(Re_mm, -0.382144871);
		return true;

	default:
		return false;
	}

};

C_CO2_to_air_cooler::C_CO2_to_air_cooler()
{
	m_th = m_eta_fan = m_roughness =
		m_A_cs = m_relRough = 
		m_L_path = m_A_surf_total = m_V_footprint =
		m_V_material_tubes = m_V_material_fins = m_P_amb_des =
		m_m_dot_air_des = m_Q_dot_des = m_P_hot_out_des =
		m_fin_pitch = m_D_h = m_fin_thk = m_sigma = m_alpha = m_A_fin_to_surf = m_s_h = m_s_v = m_fin_V_per_m = numeric_limits<double>::quiet_NaN();

	m_N_nodes = m_enum_compact_hx_config = -1;

	mc_air.SetFluid(mc_air.Air);
}

bool C_CO2_to_air_cooler::design_hx(S_des_par_ind des_par_ind, S_des_par_cycle_dep des_par_cycle_dep)
{
	// Set member structures
	ms_des_par_ind = des_par_ind;
	ms_des_par_cycle_dep = des_par_cycle_dep;

	// Calculate ambient pressure
	m_P_amb_des = 101325.0*pow(1 - 2.25577E-5*ms_des_par_ind.m_elev, 5.25588);	//[Pa] http://www.engineeringtoolbox.com/air-altitude-pressure-d_462.html	
	
	//m_enum_compact_hx_config = fc_tubes_s80_38T;
	m_enum_compact_hx_config = N_compact_hx::fc_tubes_sCF_88_10Jb;

	// Get HX Geometry
	N_compact_hx::get_compact_hx_geom(m_enum_compact_hx_config, ms_hx_des_sol.m_d_out, m_fin_pitch, m_D_h, m_fin_thk,
		m_sigma, m_alpha, m_A_fin_to_surf, m_s_h, m_s_v, m_fin_V_per_m);

	// Thickness should really be tied to HX config
	//m_th = 0.001;		//fc_tubes_s80-38T
	m_th = 0.0024;		//fc_tubes_sCF-88-10Jb

	// Get Remaining Design Info: hardcode for now, but eventually will be inputs
	// Air-Cooler Specs
	ms_hx_des_sol.m_N_passes = 3;
	m_N_nodes = 5;
	m_eta_fan = 0.5;
	ms_hx_des_sol.m_d_in = ms_hx_des_sol.m_d_out - 2.0*m_th;
	m_roughness = 4.5E-5;					//[m] absolute roughness of material
	m_A_cs = 0.25*CSP::pi*pow(ms_hx_des_sol.m_d_in, 2);	//[m2] flow cross-section area
	m_relRough = m_roughness / ms_hx_des_sol.m_d_in;		//[-] Relative Roughness

	m_P_hot_out_des = ms_des_par_cycle_dep.m_P_hot_in_des - ms_des_par_cycle_dep.m_delta_P_des;		//[kPa]
	//double P_hot_ave = 0.5*(m_P_hot_out_des + m_P_hot_in_des);
	double P_hot_ave = ms_des_par_cycle_dep.m_P_hot_in_des;			//[kPa]
	// Set up 'matrix_t's for temperature and pressure
	// Using index 1 for m_N_nodes, so 0 index remains undefined
	// Also, each node requires inlet&outlet temp, so in total, m_N_nodes + 2 required
	mm_T_co2.resize_fill(m_N_nodes + 2, ms_hx_des_sol.m_N_passes + 1, std::numeric_limits<double>::quiet_NaN());
	mm_P_co2.resize_fill(m_N_nodes + 2, ms_hx_des_sol.m_N_passes + 1, std::numeric_limits<double>::quiet_NaN());
	mm_T_air.resize_fill(m_N_nodes + 2, ms_hx_des_sol.m_N_passes + 1, std::numeric_limits<double>::quiet_NaN());

	// index that gives outlet temperatur and pressure: depends on whether odd or even loops
	m_final_outlet_index = ((ms_hx_des_sol.m_N_passes + 2) % 2)*m_N_nodes + 1;

	// Assume air props don't change significantly in air cooler
	double mu_air = mc_air.visc(ms_des_par_ind.m_T_amb_des);		//[kg/m-s] dynamic viscosity
	double v_air = 1.0 / mc_air.dens(ms_des_par_ind.m_T_amb_des, m_P_amb_des);	//[1/m3] specific volume
	double cp_air = mc_air.Cp(ms_des_par_ind.m_T_amb_des)*1000.0;	//[J/kg-K] specific heat convert from kJ/kg-K
	double k_air = mc_air.cond(ms_des_par_ind.m_T_amb_des);			//[W/m-K] conductivity
	double Pr_air = (cp_air*mu_air / k_air);						//[-] Prandtl number

	// Calculate the required heat rejection
	CO2_TP(ms_des_par_cycle_dep.m_T_hot_in_des, P_hot_ave, &mc_co2_props);
	double h_in_des = mc_co2_props.enth*1000.0;					//[J/kg]
	CO2_TP(ms_des_par_cycle_dep.m_T_hot_out_des, P_hot_ave, &mc_co2_props);
	double h_out_des = mc_co2_props.enth*1000.0;				//[J/kg]

	if (ms_des_par_cycle_dep.m_m_dot_total > 0.0)
	{
		m_Q_dot_des = ms_des_par_cycle_dep.m_m_dot_total*(h_in_des - h_out_des);	//[Wt]
	}
	else if (ms_des_par_cycle_dep.m_Q_dot_des > 0.0)
	{
		m_Q_dot_des = ms_des_par_cycle_dep.m_Q_dot_des*1.E6;		//[Wt] convert from MWt
		ms_des_par_cycle_dep.m_m_dot_total = m_Q_dot_des / (h_in_des - h_out_des);	//[kg/s]
	}
	else
	{
		throw(C_csp_exception("Air cooler design parameters need to specify either m_Q_dot_des or m_m_dot_total as positive numbers"));
	}
	
	double deltaT_hot = ms_des_par_cycle_dep.m_T_hot_in_des - ms_des_par_cycle_dep.m_T_hot_out_des;	//[K,C] Hot side temperature difference

	ms_hx_des_sol.m_Depth = m_s_h * ms_hx_des_sol.m_N_passes;	//[m] Dimension parallel to air flow

	// 1) Guess dimension perpendicular to air AND hot fluid flow
	// (basically the number of parallel flow paths)

	// ********************************************************************************
	// ** Set up guesses and control for bisection and false-position **
	// ** Try to get better guess by estimating length required to hit pressure drop **
	// ********************************************************************************
	double T_co2_deltaP_eval = 0.75*ms_des_par_cycle_dep.m_T_hot_in_des + 0.25*ms_des_par_cycle_dep.m_T_hot_out_des;
	CO2_TP(T_co2_deltaP_eval, ms_des_par_cycle_dep.m_P_hot_in_des, &mc_co2_props);
	double visc_dyn_co2_g = CO2_visc(mc_co2_props.dens, mc_co2_props.temp)*1.E-6;

	// Just try hitting a "reasonable" Reynolds number?
	// This sets the mass flow rate in the tube, which then sets the number of required tubes
	//  to contain the defined mass flow rate
	//double Re_co2_g = m_dot_tube*m_d_in / (m_A_cs*visc_dyn_co2_g);
	double Re_g = 5.E6;		//[-] A "reasonable" Reynolds number
	double m_dot_tube_g1 = Re_g*m_A_cs*visc_dyn_co2_g / ms_hx_des_sol.m_d_in;	//[kg/s] Mass flow rate to achieve Reynolds number

	double N_par_g = ms_des_par_cycle_dep.m_m_dot_total / m_dot_tube_g1;	//[-] Number of parallel flow paths required to contain all mass flow

	double W_par_guess = N_par_g * m_s_v;		//[m] Dimension perpendicular to air AND hot fluid flow... parallel paths dimension

	double tol = 1.E-3;		//[-] Relative tolerance for convergence

	C_MEQ_target_T_hot__width_parallel c_eq(this,
									mu_air, v_air,
									cp_air, Pr_air,
									T_co2_deltaP_eval, P_hot_ave,
									tol);

	C_monotonic_eq_solver c_solver(c_eq);

	c_solver.settings(tol, 50, 0.01, std::numeric_limits<double>::quiet_NaN(), true);

	double W_par_solved, tol_solved;
	W_par_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_solved = -1;

	int solver_code = 0;
	try
	{
		solver_code = c_solver.solve(W_par_guess, W_par_guess*2.0, ms_des_par_cycle_dep.m_T_hot_in_des,
			W_par_solved, tol_solved, iter_solved);
	}
	catch (C_csp_exception)
	{
		throw(C_csp_exception("Air cooler iteration on the parallel width received exception from mono equation solver"));
	}
	if (solver_code != C_monotonic_eq_solver::CONVERGED)
	{
		if (solver_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) <= 0.1)
		{
			std::string error_msg = util::format("Air cooler iteration on the parallel width only reached a convergence "
				"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
				tol_solved);
			mc_messages.add_message(C_csp_messages::WARNING, error_msg);
		}
		else
		{
			throw(C_csp_exception("Air cooler iteration on the parallel width did not converge"));
		}
	}

	// Probably have a non-integer number of parallel paths, so round up and recalculate geometry
	// C++ (int) rounds down
	int N_par = (int)c_eq.m_N_par + 1;

	// Final reporting metrics
	ms_hx_des_sol.m_W_par = W_par_solved;			//[m] Dimension perpendicular to loop/air flow direction
	ms_hx_des_sol.m_N_par = c_eq.m_N_par;			//[-] Number of parallel flow paths
	ms_hx_des_sol.m_L_tube = c_eq.m_L_tube;			//[m] Tube length
	m_L_path = ms_hx_des_sol.m_L_tube*ms_hx_des_sol.m_N_passes;	//[m] Total flow path length
	ms_hx_des_sol.m_N_tubes = c_eq.m_N_tubes;		//[-] Number of tubes
	m_V_footprint = c_eq.m_V_total;		//[m^3] Total HX "footprint" Volume
	m_A_surf_total = m_V_footprint*m_alpha;			//[-] Air-side surface area of node
	ms_hx_des_sol.m_UA_total = m_A_surf_total*c_eq.m_h_conv_air;	//[W/K] Total HX Conductance
	m_m_dot_air_des = c_eq.m_m_dot_air_total;		//[kg/s] Total air mass flow rate
	m_A_surf_node = c_eq.m_A_surf_node;		//[m^2] Air-side surface area of node

	double L_tube_total = ms_hx_des_sol.m_L_tube*ms_hx_des_sol.m_N_tubes;		//[m] Cumulative length of tubes
	m_V_material_tubes = 0.25*CSP::pi*(pow(ms_hx_des_sol.m_d_out, 2) - pow(ms_hx_des_sol.m_d_in, 2))*L_tube_total;	//[m3] Total material required for tubing
	m_V_material_fins = m_fin_V_per_m*L_tube_total;		//[m3] Total material required for fins
	ms_hx_des_sol.m_V_material_total = m_V_material_tubes + m_V_material_fins;	//[m3] Total material in HX

	return true;
};

int C_CO2_to_air_cooler::C_MEQ_target_W_dot_fan__m_dot_air::operator()(double m_dot_air /*kg/s*/, double *W_dot_fan /*MWe*/)
{
	m_h_conv_air = std::numeric_limits<double>::quiet_NaN();

	double G_air = m_dot_air / (mpc_ac->m_sigma*m_L_tube*m_W_par);
	double Re_air = G_air*mpc_ac->m_D_h / m_mu_air;
	
	double f_air, j_H_air;
	f_air, j_H_air = numeric_limits<double>::quiet_NaN();

	if (!N_compact_hx::get_compact_hx_f_j(mpc_ac->m_enum_compact_hx_config, Re_air, f_air, j_H_air))
		return -1;

	double deltaP_air = pow(G_air, 2.0)*m_v_air*0.5*f_air*mpc_ac->m_alpha*m_V_total / (mpc_ac->m_sigma*m_L_tube*m_W_par);
	m_h_conv_air = j_H_air*G_air*m_cp_air / pow(m_Pr_air, (2.0 / 3.0));	//[W/m^2-K]

	double V_dot_air_total = m_dot_air*m_v_air;
	*W_dot_fan = deltaP_air*V_dot_air_total / mpc_ac->m_eta_fan / 1.E6;

	return 0;
}

int C_CO2_to_air_cooler::C_MEQ_node_energy_balance__T_co2_out::operator()(double T_co2_hot_in /*K*/, double *diff_T_co2_cold /*-*/)
{
	if (m_T_co2_cold_out <= m_T_air_cold_in)
	{
		return -1;
	}
	
	m_Q_dot_node = std::numeric_limits<double>::quiet_NaN();

	double T_co2_ave = 0.5*(T_co2_hot_in + m_T_co2_cold_out);		//[K]

	int co2_prop_error = CO2_TP(T_co2_ave, m_P_co2_ave, &mpc_ac->mc_co2_props);
	if (co2_prop_error != 0)
	{
		return -2;
	}
	double cp_co2_ave = mpc_ac->mc_co2_props.cp*1000.0;		//[J/kg-K]

	// Capacitance rates
	double C_dot_co2 = cp_co2_ave*m_m_dot_co2_tube;			//[W/K]
	double C_dot_min = fmin(m_C_dot_air, C_dot_co2);		//[W/K]
	double C_dot_max = fmax(m_C_dot_air, C_dot_co2);		//[W/K]
	double CR = C_dot_min / C_dot_max;			//[-]
	double Q_dot_max = C_dot_min*(T_co2_hot_in - m_T_air_cold_in);	//[W]
	double NTU = m_UA_node / C_dot_min;
	// Unmixed cross-flow
	double epsilon = 1 - exp(pow(NTU, 0.22) / CR*(exp(-CR*pow(NTU, 0.78)) - 1));
	m_Q_dot_node = epsilon*Q_dot_max;
	
	double T_co2_cold_calc = T_co2_hot_in - m_Q_dot_node / C_dot_co2;
	
	*diff_T_co2_cold = (T_co2_cold_calc - m_T_co2_cold_out) / m_T_co2_cold_out;		//[K]

	return 0;
}

int C_CO2_to_air_cooler::C_MEQ_target_CO2_dP__L_tube_pass::operator()(double L_tube /*m*/, double *delta_P_co2 /*kPa*/)
{
	double L_total = L_tube*mpc_ac->ms_hx_des_sol.m_N_passes;	//[m] Total length of flow path including loops
	double L_node = L_tube/mpc_ac->m_N_nodes;	//[m] Length of one node
	double V_node = L_node*mpc_ac->m_s_v*mpc_ac->m_s_h;	//[m^3] Volume of one node
	m_V_total = L_tube*mpc_ac->ms_hx_des_sol.m_Depth*m_W_par;		//[m^3] Total HX footprint volume

	m_h_conv_air = std::numeric_limits<double>::quiet_NaN();		//[W/m2-K]
	m_m_dot_air_total = std::numeric_limits<double>::quiet_NaN();	//[kg/s]
	m_A_surf_node = std::numeric_limits<double>::quiet_NaN();		//[m2]

	// Iterate to find air mass flow rate resulting in target fan power
	C_MEQ_target_W_dot_fan__m_dot_air c_m_dot_air_eq(mpc_ac,
		L_tube, m_W_par, m_V_total,
		m_mu_air, m_v_air, m_cp_air, m_Pr_air);
	C_monotonic_eq_solver c_m_dot_air_solver(c_m_dot_air_eq);

	double tol_m_dot = m_tol_upper / 2.0;					//[-] Relative tolerance for convergence
	c_m_dot_air_solver.settings(tol_m_dot, 50, 1.E-10, std::numeric_limits<double>::quiet_NaN(), true);

	double m_dot_air_guess = mpc_ac->m_Q_dot_des / (5.0*m_cp_air);	//[kg/s] Guess assuming 5k temp rise
	double m_dot_air_guess2 = 1.05*m_dot_air_guess;		//[kg/s] Another guess...

	double m_dot_air_solved, m_dot_air_tol_solved;
	m_dot_air_solved = m_dot_air_tol_solved = std::numeric_limits<double>::quiet_NaN();
	int m_dot_air_iter_solved = -1;

	int m_dot_air_solver_code = 0;
	try
	{
		m_dot_air_solver_code = c_m_dot_air_solver.solve(m_dot_air_guess, m_dot_air_guess2,
			mpc_ac->ms_des_par_cycle_dep.m_W_dot_fan_des, m_dot_air_solved, m_dot_air_tol_solved, m_dot_air_iter_solved);
	}
	catch (C_csp_exception)
	{
		throw(C_csp_exception("Air cooler iteration on air mass flow rate received exception from mono equation solver"));
	}
	if (m_dot_air_solver_code != C_monotonic_eq_solver::CONVERGED)
	{
		if (m_dot_air_solver_code > C_monotonic_eq_solver::CONVERGED && fabs(m_dot_air_tol_solved) <= 0.1)
		{
			std::string error_msg = util::format("Air cooler iteration on air mass flow rate only reached a convergence "
				"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
				m_dot_air_tol_solved);
			mpc_ac->mc_messages.add_message(C_csp_messages::WARNING, error_msg);
		}
		else
		{
			return -1;
			//throw(C_csp_exception("Air cooler iteration on air mass flow rate did not converge"));
		}
	}

	m_m_dot_air_total = m_dot_air_solved;			//[kg/s]
	m_h_conv_air = c_m_dot_air_eq.m_h_conv_air;		//[W/m2-K]

	m_A_surf_node = V_node*mpc_ac->m_alpha;			//[m2] Air-side surface area of node
	double UA_node = m_A_surf_node*m_h_conv_air;	//[W/K] Conductance of node - assuming air convective heat transfer is governing resistance

	// Set known inlet conditions: iteration thru # of loops needs previous loop info
	mpc_ac->mm_T_co2(1, 0) = mpc_ac->ms_des_par_cycle_dep.m_T_hot_out_des;
	mpc_ac->mm_P_co2(1, 0) = mpc_ac->ms_des_par_cycle_dep.m_P_hot_in_des - mpc_ac->ms_des_par_cycle_dep.m_delta_P_des;	//[kPa]
	for (int i = 1; i < mpc_ac->m_N_nodes + 2; i++)
		mpc_ac->mm_T_air(i, 0) = mpc_ac->ms_des_par_ind.m_T_amb_des;

	// Assuming constant air props, so can set those
	double m_dot_air_tube = m_m_dot_air_total / (m_N_par*mpc_ac->m_N_nodes);
	double C_dot_air = m_cp_air*m_dot_air_tube;	//[W/K]

	for (int j = 1; j < mpc_ac->ms_hx_des_sol.m_N_passes + 1; j++)
	{
		// Set up constants and multipliers to switch direction of flow
		double mult_const = (j + 1) % 2;
		double constant = mpc_ac->m_N_nodes + 2;
		double mult_index = 1.0 - 2.0*mult_const;
		double out_const = mult_index;

		// Set inlet temperatures & pressures of current row
		double mult_inlet = (j + 1) % 2;
		double const_inlet = mpc_ac->m_N_nodes;
		double inlet = mult_inlet*const_inlet + 1;

		// Set loop inlet conditions
		mpc_ac->mm_T_co2(inlet, j) = mpc_ac->mm_T_co2(inlet, j - 1);		//[K]
		mpc_ac->mm_P_co2(inlet, j) = mpc_ac->mm_P_co2(inlet, j - 1);		//[kPa]

		//double deltaT_prev = numeric_limits<double>::quiet_NaN();

		for (int i = 1; i < mpc_ac->m_N_nodes + 1; i++)
		{
			double in = mult_const*constant + mult_index*i;
			double out = in + out_const;
			double air_in = fmin(in, out);

			// Get CO2 and Air inlet temperatures to node
			double T_co2_cold_local = mpc_ac->mm_T_co2(in, j);			//[K]
			double T_air_cold_in_local = mpc_ac->mm_T_air(air_in, j - 1);	//[K]

			// Set max allowable CO2 temp here, for now
			double T_co2_hot_max = 700.0 + 273.15;		//[K]

			C_MEQ_node_energy_balance__T_co2_out c_node_bal_eq(mpc_ac,
				T_co2_cold_local, m_P_hot_ave,
				m_m_dot_tube,
				T_air_cold_in_local, C_dot_air,
				UA_node);
			C_monotonic_eq_solver c_node_bal_solver(c_node_bal_eq);

			double tol_T_in = tol_m_dot / 5.0;		//[-] Relative tolerance for convergence
			c_node_bal_solver.settings(tol_T_in, 50, T_co2_cold_local, T_co2_hot_max, false);

			double diff_T_co2_cold_calc = std::numeric_limits<double>::quiet_NaN();
			int diff_T_co2_cold_calc_code = c_node_bal_solver.test_member_function(T_co2_hot_max, &diff_T_co2_cold_calc);
			if (diff_T_co2_cold_calc_code != 0)
			{
				throw(C_csp_exception("Air cooler UA calculation failed at maximum CO2 temperature"));
			}

			double T_co2_hot_solved = std::numeric_limits<double>::quiet_NaN();
			if (diff_T_co2_cold_calc >= 0.0)
			{	// Use monotonic equation solver to fine T_co2_hot
				double T_co2_hot_local_guess = T_co2_cold_local + 0.02;			//[K]
				double T_co2_hot_local_guess_2 = T_co2_hot_local_guess + 1.5;	//[K]

				double T_co2_hot_tol_solved;
				T_co2_hot_tol_solved = std::numeric_limits<double>::quiet_NaN();
				int T_co2_hot_iter_solved = -1;

				int T_co2_hot_solver_code = 0;
				try
				{
					T_co2_hot_solver_code = c_node_bal_solver.solve(T_co2_hot_local_guess, T_co2_hot_local_guess_2, 0.0, T_co2_hot_solved, T_co2_hot_tol_solved, T_co2_hot_iter_solved);
				}
				catch (C_csp_exception)
				{
					throw(C_csp_exception("Air cooler calculation to find T_co2_hot that matched node UA returned an exception"));
				}

				if (T_co2_hot_solver_code != C_monotonic_eq_solver::CONVERGED)
				{
					if (T_co2_hot_solver_code > C_monotonic_eq_solver::CONVERGED && fabs(T_co2_hot_tol_solved) <= 0.1)
					{
						std::string error_msg = util::format("Air cooler iteration on nodal hot co2 temperature rate only reached a convergence "
							"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
							T_co2_hot_tol_solved);
						mpc_ac->mc_messages.add_message(C_csp_messages::WARNING, error_msg);
					}
					else
					{
						return -2;
						//throw(C_csp_exception("Air cooler iteration on air mass flow rate did not converge"));
					}
				}
			}
			else
			{	// CO2 is unrealistically hot. stop here before property routine gets weird
				//    (although, generally, returning a unique value is useful...)
				//    (which is why we let it get this hot instead of say using the target outlet temp)
				T_co2_hot_solved = T_co2_hot_max;
			}

			double Q_dot_node = c_node_bal_eq.m_Q_dot_node;		//[W]
			mpc_ac->mm_T_co2(out, j) = T_co2_hot_solved;		//[K]
			mpc_ac->mm_T_air(air_in, j) = mpc_ac->mm_T_air(air_in, j - 1) + Q_dot_node / C_dot_air;	//[K]

			// Add pressure drop calcs (co2_props is up-to-date)
			// ** Could also move this to a function if also called to guess length
			double visc_dyn_co2 = CO2_visc(mpc_ac->mc_co2_props.dens, mpc_ac->mc_co2_props.temp)*1.E-6;	//[Pa-s] convert from (uPa-s)
			double Re_co2 = m_m_dot_tube*mpc_ac->ms_hx_des_sol.m_d_in / (mpc_ac->m_A_cs*visc_dyn_co2);		//[-]

			double rho_co2 = mpc_ac->mc_co2_props.dens;				//[kg/s]
			double visc_kin_co2 = visc_dyn_co2 / rho_co2;	//[m2/s]
			double cond_co2 = CO2_cond(mpc_ac->mc_co2_props.dens, mpc_ac->mc_co2_props.temp);	//[W/m-K]
			double specheat_co2 = mpc_ac->mc_co2_props.cp*1000.0;		//[J/kg-K] convert from kJ/kg-K
			double alpha_co2 = cond_co2 / (specheat_co2*rho_co2);	//[m2/s]
			double Pr_co2 = visc_kin_co2 / alpha_co2;		//[-]

			double Nusselt_co2 = -999.9;	//[-]
			double f_co2 = -999.9;			//[-]

			// Specifying the length over diameter = 1000 sets the problem as Fully Developed Flow
			// CSP::PipeFlow(Re_co2, Pr_co2, 1000.0, m_relRough, Nusselt_co2, f_co2);
			CSP::PipeFlow(Re_co2, Pr_co2, L_node / mpc_ac->ms_hx_des_sol.m_d_in, mpc_ac->m_relRough, Nusselt_co2, f_co2);

			double u_m = m_m_dot_tube / (rho_co2*mpc_ac->m_A_cs);		//[m/s]
			mpc_ac->mm_P_co2(out, j) = mpc_ac->mm_P_co2(in, j) + f_co2*L_node*rho_co2*pow(u_m, 2) / (2.0*mpc_ac->ms_hx_des_sol.m_d_in) / 1000.0;	//[kPa]

			double deltaP_node = mpc_ac->mm_P_co2(out, j) - mpc_ac->mm_P_co2(in, j);	//[kPa]

			mpc_ac->mm_P_co2(out, j) = fmin(25000.0, fmax(1000.0, mpc_ac->mm_P_co2(out, j)));

		}	// End iteration through nodes in flow path		

	}	// End iteration through loop in flow path

	*delta_P_co2 = mpc_ac->mm_P_co2(mpc_ac->m_final_outlet_index, mpc_ac->ms_hx_des_sol.m_N_passes) - mpc_ac->m_P_hot_out_des;	//[kPa]

	return 0;
}

int C_CO2_to_air_cooler::C_MEQ_target_T_hot__width_parallel::operator()(double W_par /*m*/, double *T_co2_hot /*K*/)
{
	m_L_tube = std::numeric_limits<double>::quiet_NaN();		//[m]
	m_V_total = std::numeric_limits<double>::quiet_NaN();		//[m3]
	m_h_conv_air = std::numeric_limits<double>::quiet_NaN();	//[W/m2-K]
	m_A_surf_node = std::numeric_limits<double>::quiet_NaN();	//[m2]
	
	// Divide by the distance between tubes to get number of parallel units
	// This is number of tube connected to the inlet headers
	// Can be any positive rational number so a continuous solution space is available
	m_N_par = W_par / mpc_ac->m_s_v;
	m_N_tubes = m_N_par*mpc_ac->ms_hx_des_sol.m_N_passes;
	/// Can now calculate the mass flow rate per tube
	double m_dot_tube = mpc_ac->ms_des_par_cycle_dep.m_m_dot_total / m_N_par;

	// 2) Guess the length of the hot side tube for one pass/loop
	// ********************************************************************************
	// ** Try to estimate length required to hit pressure drop **
	// ********************************************************************************
	CO2_TP(m_T_co2_deltaP_eval, m_P_hot_ave, &mpc_ac->mc_co2_props);
	double visc_dyn_co2_g = CO2_visc(mpc_ac->mc_co2_props.dens, mpc_ac->mc_co2_props.temp)*1.E-6;
	double Re_co2_g = m_dot_tube*mpc_ac->ms_hx_des_sol.m_d_in / (mpc_ac->m_A_cs*visc_dyn_co2_g);

	double rho_co2_g = mpc_ac->mc_co2_props.dens;
	double visc_kin_co2_g = visc_dyn_co2_g / rho_co2_g;
	double cond_co2_g = CO2_cond(mpc_ac->mc_co2_props.dens, mpc_ac->mc_co2_props.temp);
	double specheat_co2_g = mpc_ac->mc_co2_props.cp*1000.0;
	double alpha_co2_g = cond_co2_g / (specheat_co2_g*rho_co2_g);
	double Pr_co2 = visc_kin_co2_g / alpha_co2_g;

	double Nusselt_co2_g = -999.9;
	double f_co2_g = -999.9;

	double tol_L_tube = m_tol / 5.0;

	// Specifying the length over diameter = 1000 sets the problem as Fully Developed Flow
	CSP::PipeFlow(Re_co2_g, Pr_co2, 1000.0, mpc_ac->m_relRough, Nusselt_co2_g, f_co2_g);

	double u_m = m_dot_tube / (rho_co2_g*mpc_ac->m_A_cs);
	//m_delta_P_des*1000.0 = f_co2_g*L_node*rho_co2_g*pow(u_m,2)/(2.0*m_d_in)
	double L_tube_guess = mpc_ac->ms_des_par_cycle_dep.m_delta_P_des*1000.0*(2.0*mpc_ac->ms_hx_des_sol.m_d_in) / (f_co2_g*rho_co2_g*pow(u_m, 2)) / mpc_ac->ms_hx_des_sol.m_N_passes;

	C_MEQ_target_CO2_dP__L_tube_pass c_eq(mpc_ac,
		W_par, m_N_par,
		m_P_hot_ave, m_dot_tube,
		m_mu_air, m_v_air,
		m_cp_air, m_Pr_air,
		tol_L_tube);

	C_monotonic_eq_solver c_solver(c_eq);

	c_solver.settings(1.E-3, 50, 0.01, std::numeric_limits<double>::quiet_NaN(), true);

	double L_tube_solved, tol_solved;
	L_tube_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_solved = -1;

	int solver_code = 0;
	try
	{
		solver_code = c_solver.solve(L_tube_guess, L_tube_guess*1.1, mpc_ac->ms_des_par_cycle_dep.m_delta_P_des,
			L_tube_solved, tol_solved, iter_solved);
	}
	catch (C_csp_exception)
	{
		throw(C_csp_exception("Air cooler iteration on tube length received exception from mono equation solver"));
	}
	if (solver_code != C_monotonic_eq_solver::CONVERGED)
	{
		if (solver_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) <= 0.1)
		{
			std::string error_msg = util::format("Air cooler iteration on tube length only reached a convergence "
				"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
				tol_solved);
			mpc_ac->mc_messages.add_message(C_csp_messages::WARNING, error_msg);
		}
		else
		{
			return -1;
			//throw(C_csp_exception("Air cooler iteration on air mass flow rate did not converge"));
		}
	}

	m_L_tube = L_tube_solved;		//[m]
	m_V_total = c_eq.m_V_total;		//[m3]
	m_h_conv_air = c_eq.m_h_conv_air;	//[W/m2-K]
	m_m_dot_air_total = c_eq.m_m_dot_air_total;	//[kg/s]
	m_A_surf_node = c_eq.m_A_surf_node;	//[m2]

	*T_co2_hot = mpc_ac->mm_T_co2(mpc_ac->m_final_outlet_index, mpc_ac->ms_hx_des_sol.m_N_passes);		//[K]

	return 0;
}

void C_CO2_to_air_cooler::off_design_hx(double T_amb_K, double P_amb_Pa, double T_hot_in_K, double P_hot_in_kPa,
	double m_dot_hot_kg_s, double T_hot_out_K, double & W_dot_fan_MW, int & error_code)
{
	double T_amb = T_amb_K;
	double P_amb = P_amb_Pa;
	double T_hot_in = T_hot_in_K;
	double P_hot_in = P_hot_in_kPa;
	double m_dot_hot = m_dot_hot_kg_s;
	double T_hot_out = T_hot_out_K;

	// Assume air props don't change significantly in air cooler
	double mu_air = mc_air.visc(T_amb);
	double v_air = 1.0 / mc_air.dens(T_amb, P_amb);
	double cp_air = mc_air.Cp(T_amb)*1000.0;
	double k_air = mc_air.cond(T_amb);
	double Pr_air = (cp_air*mu_air / k_air);

	// Calculate the required heat rejection
	CO2_state co2_props;
	CO2_TP(T_hot_in, P_hot_in, &co2_props);			// Assumes no pressure drop...
	double h_in = co2_props.enth*1000.0;					//[J/kg]
	CO2_TP(T_hot_out, P_hot_in, &co2_props);
	double h_out = co2_props.enth*1000.0;					//[J/kg]
	double Q_dot = m_dot_hot*(h_in - h_out);				//[W]
	double deltaT_hot = T_hot_in - T_hot_out;				//[K,C] Hot side temperature difference

	// Set up matrices for HX
	util::matrix_t<double>    T_co2(m_N_nodes + 2, ms_hx_des_sol.m_N_passes + 1);
	util::matrix_t<double>    P_co2(m_N_nodes + 2, ms_hx_des_sol.m_N_passes + 1);
	util::matrix_t<double>    T_air(m_N_nodes + 2, ms_hx_des_sol.m_N_passes + 1);
	T_co2.fill(numeric_limits<double>::quiet_NaN());
	P_co2.fill(numeric_limits<double>::quiet_NaN());
	T_air.fill(numeric_limits<double>::quiet_NaN());

	// Set known inlet conditions: iteration thru # of loops needs previous loop info
	T_co2(1, 0) = T_hot_out;
	P_co2(1, 0) = P_hot_in;			// Again, neglecting pressure drops
	for( int i = 1; i < m_N_nodes + 2; i++ )
		T_air(i, 0) = T_amb;

	double T_hot_tol = 0.001;		//[-] Relative tolerance for convergence
	double diff_T_hot_in = 2.0*T_hot_tol;

	int iter_T_hot = -1;

	double y_upper, y_lower, x_upper, x_lower;
	y_upper = y_lower = x_upper = x_lower = std::numeric_limits<double>::quiet_NaN();
	bool is_upper = false;
	bool is_lower = false;

	double m_dot_tube = m_dot_hot / ms_hx_des_sol.m_N_par;

	// Guess air mass flow rate through heat exchanger
	double m_dot_air = (Q_dot / m_Q_dot_des)*m_m_dot_air_des;

	double W_dot_fan = std::numeric_limits<double>::quiet_NaN();

	// ********************************
	// Iteration on air mass flow rate
	// ********************************

	while( fabs(diff_T_hot_in) > T_hot_tol )
	{
		iter_T_hot++;

		if( iter_T_hot > 25 )
		{
			if( fabs(W_dot_fan - ms_des_par_cycle_dep.m_W_dot_fan_des) / ms_des_par_cycle_dep.m_W_dot_fan_des < 2.0 )		// value "close enough" to be "reasonable"
			{
				W_dot_fan_MW = W_dot_fan;
				error_code = 2;
				return;
			}
			else
			{
				W_dot_fan_MW = -999.9;
				error_code = 1;
				return;
			}
		}

		if( iter_T_hot > 0 )
		{
			if( diff_T_hot_in > 0.0 )			// diff_T_hot_in = (T_co2(m_final_outlet_index, m_N_loops) - T_hot_in) / T_hot_in; ---> mass flow rate too high
			{
				y_upper = diff_T_hot_in;
				x_upper = m_dot_air;
				is_upper = true;
				if( is_lower )
					m_dot_air = -y_upper*(x_lower - x_upper) / (y_lower - y_upper) + x_upper;
				else
					m_dot_air *= 0.5;
			}
			else						// UA_diff = (UA_calc - UA_PHX_od) / UA_PHX_od;  -> Q_dot_PHX too large	 
			{
				y_lower = diff_T_hot_in;
				x_lower = m_dot_air;
				is_lower = true;
				if( is_upper )
					m_dot_air = -y_upper*(x_lower - x_upper) / (y_lower - y_upper) + x_upper;
				else
					m_dot_air *= 1.5;
			}
		}

		// Calculate air-side heat transfer coefficient and UA
		double G_air = m_dot_air / (m_sigma*ms_hx_des_sol.m_L_tube*ms_hx_des_sol.m_W_par);
		double Re_air = G_air*m_D_h / mu_air;
		double f_air, j_H_air;
		f_air, j_H_air = numeric_limits<double>::quiet_NaN();

		if( !N_compact_hx::get_compact_hx_f_j(m_enum_compact_hx_config, Re_air, f_air, j_H_air) )
		{
			W_dot_fan_MW = -999.9;
			error_code = 1;
			return;
		}


		double deltaP_air = pow(G_air, 2.0)*v_air*0.5*f_air*m_alpha*m_V_footprint / (m_sigma*ms_hx_des_sol.m_L_tube*ms_hx_des_sol.m_W_par);
		double h_conv_air = j_H_air*G_air*cp_air / pow(Pr_air, (2.0 / 3.0));	//[W/m^2-K]

		double V_dot_air = m_dot_air*v_air;
		W_dot_fan = deltaP_air*V_dot_air / m_eta_fan / 1.E6;				//[MW]

		double UA_node = m_A_surf_node*h_conv_air;	//[W/K] Conductance of node - assuming air convective heat transfer is governing resistance

		for (int j = 1; j < ms_hx_des_sol.m_N_passes + 1; j++)
		{
			// Assuming constant air props, so can set those
			double m_dot_air_tube = m_dot_air / (ms_hx_des_sol.m_N_par*m_N_nodes);
			double C_dot_air = cp_air*m_dot_air_tube;

			// Set up constants and multipliers to switch direction of flow
			double mult_const = (j + 1) % 2;
			double constant = m_N_nodes + 2;
			double mult_index = 1.0 - 2.0*mult_const;
			double out_const = mult_index;

			// Set inlet temperatures & pressures of current row
			double mult_inlet = (j + 1) % 2;
			double const_inlet = m_N_nodes;
			double inlet = mult_inlet*const_inlet + 1;

			// Set loop inlet conditions
			T_co2(inlet, j) = T_co2(inlet, j - 1);
			P_co2(inlet, j) = P_co2(inlet, j - 1);

			double deltaT_prev = numeric_limits<double>::quiet_NaN();

			for( int i = 1; i < m_N_nodes + 1; i++ )
			{
				double in = mult_const*constant + mult_index*i;
				double out = in + out_const;
				double air_in = fmin(in, out);

				// Guess outlet temperature
				double T_out_guess = T_co2(in, j) + 1.0;

				double tol_T_in = T_hot_tol / 50.0;		//[-] Relative tolerance for convergence
				double diff_T_in = 2.0*tol_T_in;		//[-] Set diff > tol
				int iter_T_in = 0;

				bool is_lowbound_T_out = false;
				double x_lower_T_out = numeric_limits<double>::quiet_NaN();
				double y_lower_T_out = numeric_limits<double>::quiet_NaN();

				bool is_upbound_T_out = false;
				double x_upper_T_out = numeric_limits<double>::quiet_NaN();
				double y_upper_T_out = numeric_limits<double>::quiet_NaN();

				// Values solved in inner nest required later in code
				double Q_dot_node = numeric_limits<double>::quiet_NaN();

				while( fabs(diff_T_in) > tol_T_in )
				{
					iter_T_in++;

					if( iter_T_in > 1 )
					{
						if( diff_T_in > 0.0 )		// Guessed temperature too high - reduce guess
						{
							is_upbound_T_out = true;
							x_upper_T_out = T_out_guess;
							y_upper_T_out = diff_T_in;
							if( is_lowbound_T_out )
							{
								T_out_guess = -y_upper_T_out*(x_lower_T_out - x_upper_T_out) / (y_lower_T_out - y_upper_T_out) + x_upper_T_out;
							}
							else
							{
								T_out_guess = 0.5*(T_out_guess + T_co2(in, j));
							}
						}
						else						// Calculated fan power too low - increase m_dot
						{
							is_lowbound_T_out = true;
							x_lower_T_out = T_out_guess;
							y_lower_T_out = diff_T_in;
							if( is_upbound_T_out )
							{
								T_out_guess = -y_upper_T_out*(x_lower_T_out - x_upper_T_out) / (y_lower_T_out - y_upper_T_out) + x_upper_T_out;
							}
							else
							{
								T_out_guess = T_out_guess + (T_out_guess - T_co2(in, j));
							}
						}

					}
					T_out_guess = fmin(700.0 + 273.15, T_out_guess);

					if( x_lower_T_out >= 700.0 + 273.15 )
					{
						break;
					}

					double T_out_ave = 0.5*(T_out_guess + T_co2(in, j));

					// Check this error?
					int co2_prop_error = CO2_TP(T_out_ave, P_hot_in, &co2_props);
					double cp_co2_ave = co2_props.cp*1000.0;

					// Capacitance rates
					double C_dot_co2 = cp_co2_ave*m_dot_tube;
					double C_dot_min = fmin(C_dot_air, C_dot_co2);
					double C_dot_max = fmax(C_dot_air, C_dot_co2);
					double Q_dot_max = C_dot_min*(T_out_guess - T_air(air_in, j - 1));
					double NTU = UA_node / C_dot_min;
					double CR = C_dot_min / C_dot_max;
					// Unmixed cross-flow
					double epsilon = 1 - exp(pow(NTU, 0.22) / CR*(exp(-CR*pow(NTU, 0.78)) - 1));
					Q_dot_node = epsilon*Q_dot_max;

					double T_in_check = T_out_guess - Q_dot_node / C_dot_co2;

					deltaT_prev = T_co2(out, j) - T_co2(in, j);

					double T_last = T_co2(in, j);

					diff_T_in = (T_in_check - T_co2(in, j)) / T_co2(in, j);

				}	// **** End T_out (node) iteration ***********************

				T_co2(out, j) = fmin(700.0 + 273.15, T_out_guess);
				T_air(air_in, j) = T_air(air_in, j - 1) + Q_dot_node / C_dot_air;

			}	// **** End one row of nodes iteration		
		}	// **** End number of passes iteration

		double T_last = T_co2(m_final_outlet_index, ms_hx_des_sol.m_N_passes);

		diff_T_hot_in = (T_co2(m_final_outlet_index, ms_hx_des_sol.m_N_passes) - T_hot_in) / T_hot_in;

	}	// End air mass flow rate iteration

	W_dot_fan_MW = W_dot_fan;
	error_code = 0;
	return;
}

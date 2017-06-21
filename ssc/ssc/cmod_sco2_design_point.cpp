#include "core.h"
#include "sco2_pc_core.h"
#include "sco2_pc_csp_int.h"

#include "heat_exchangers.h"
#include "numeric_solvers.h"

// This compute module finds the optimal cycle design that meets the user-input design point cycle efficiency
//    and calculates the required recuperator UA

static var_info _cm_vtab_sco2_design_point[] = {

	/*   VARTYPE   DATATYPE         NAME               LABEL                                                  UNITS     META  GROUP REQUIRED_IF CONSTRAINTS         UI_HINTS*/
	{ SSC_INPUT,  SSC_NUMBER,  "W_dot_net_des",   "Design cycle power output",                              "MW",         "",    "",      "*",     "",                "" },
	{ SSC_INPUT,  SSC_NUMBER,  "eta_c",           "Design compressor(s) isentropic efficiency",             "-",          "",    "",      "*",     "",                "" },
	{ SSC_INPUT,  SSC_NUMBER,  "eta_t",           "Design turbine isentropic efficiency",                   "-",          "",    "",      "*",     "",                "" },
	{ SSC_INPUT,  SSC_NUMBER,  "P_high_limit",    "High pressure limit in cycle",                           "MPa",        "",    "",      "*",     "",                "" },
	{ SSC_INPUT,  SSC_NUMBER,  "deltaT_PHX",      "Temp diff btw hot HTF and turbine inlet",                "C",          "",    "",      "*",     "",                "" },
																								           
	{ SSC_INPUT,  SSC_NUMBER,  "deltaT_ACC",      "Temp diff btw ambient air and compressor inlet",         "C",          "",    "",      "*",     "",                "" },
	{ SSC_INPUT,  SSC_NUMBER,  "T_amb_des",       "Design: Ambient temperature for air cooler",             "C",          "",    "",      "*",     "",                "" },
																								           
	{ SSC_INPUT,  SSC_NUMBER,  "T_htf_hot_des",   "Tower design outlet temp",                               "C",          "",    "",      "*",     "",                "" },
	{ SSC_INPUT,  SSC_NUMBER,  "eta_des",         "Power cycle thermal efficiency",                         "",           "",    "",      "*",     "",                "" },
																								           
	{ SSC_INPUT,  SSC_NUMBER,  "run_off_des_study", "1 = yes, 0/other = no",                                "",           "",    "",      "*",      "",               "" },
	{ SSC_INPUT,  SSC_ARRAY,   "part_load_fracs", "Array of part load q_dot_in fractions for off-design parametric", "",  "",    "",      "run_off_des_study=1", "",  "" },
	{ SSC_INPUT,  SSC_ARRAY,   "T_amb_array",     "Array of ambient temperatures for off-design parametric","C",          "",    "",      "run_off_des_study=1", "",  "" },

	{ SSC_OUTPUT, SSC_NUMBER,  "eta_thermal_calc","Calculated cycle thermal efficiency",                    "-",          "",    "",      "*",     "",                "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "UA_total",        "Total recuperator UA",                                   "kW/K",       "",    "",      "*",     "",                "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "recomp_frac",     "Recompression fraction",                                 "-",          "",    "",      "*",     "",                "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "P_comp_in",       "Compressor inlet pressure",                              "MPa",        "",    "",      "*",     "",                "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "P_comp_out",      "Compressor outlet pressure",                             "MPa",        "",    "",      "*",     "",                "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "T_htf_cold",      "Calculated cold HTF temp",                               "C",          "",    "",      "*",     "",                "" },

	{ SSC_OUTPUT, SSC_ARRAY,   "part_load_fracs_out", "Array of part load fractions that SOLVED at off design", "-",      "",    "",      "run_off_des_study=1", "",  "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "part_load_eta",   "Matrix of power cycle efficiency results for q_dot_in part load", "-", "",    "",      "run_off_des_study=1", "",  "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "part_load_coefs", "Part load polynomial coefficients",                      "-",          "",    "",      "run_off_des_study=1", "",  "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "part_load_r_squared", "Part load curve fit R squared",                      "-",          "",    "",      "run_off_des_study=1", "",  "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "T_amb_array_out", "Array of ambient temps that SOLVED at off design",       "C",          "",    "",      "run_off_des_study=1", "",  "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "T_amb_eta",       "Matrix of ambient temps and power cycle efficiency",     "-",          "",    "",      "run_off_des_study=1", "",  "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "T_amb_coefs",     "Part load polynomial coefficients",                      "-",          "",    "",      "run_off_des_study=1", "",  "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "T_amb_r_squared", "T amb curve fit R squared",                              "-",          "",    "",      "run_off_des_study=1", "",  "" },

	var_info_invalid };

int test_mono_function(double x, double *y);

class cm_sco2_design_point : public compute_module
{
public:

	cm_sco2_design_point()
	{
		add_var_info(_cm_vtab_sco2_design_point);
	}

	void exec() throw(general_error)
	{
		// Hot sCO2 to water heat exchanger

		double W_dot_net = 10.0*1.E3;	//[KWe]
		double eta = 0.492;				//[-]
		double q_dot_reject = W_dot_net / eta - W_dot_net;	//[kWt]

		double P_co2 = 7.661*1.E3;		//[kPa]
		double T_co2_hot = 45.0;		//[C]
		double T_co2_cold = 32.0;		//[C]
		double m_dot_co2_full_q_dot = 55.8065;	//[kg/s]
		bool is_partial_med = true;

		double T_water_cold = 17.0;		//[C] Groundwater temperature
		double T_water_hot = 39.0;		//[C] Groundwater outlet
		double x_water = -1;			//[-]

		CO2_state co2_props;
		int prop_err_code = CO2_TP(T_co2_hot+273.15, P_co2, &co2_props);
		if (prop_err_code != 0)
		{
			log("CO2 hot props failed", SSC_ERROR, -1.0);
			return;
		}
		double h_co2_hot = co2_props.enth;	//[kJ/kg]

		prop_err_code = CO2_TP(T_co2_cold + 273.15, P_co2, &co2_props);
		if (prop_err_code != 0)
		{
			log("CO2 cold props failed", SSC_ERROR, -1.0);
			return;
		}
		double h_co2_cold = co2_props.enth;	//[kJ/kg]
		
		double m_dot_co2 = std::numeric_limits<double>::quiet_NaN();
		if (!is_partial_med)
		{
			m_dot_co2 = (q_dot_reject) / (h_co2_hot - h_co2_cold);
		}
		else
		{
			m_dot_co2 = m_dot_co2_full_q_dot;
			q_dot_reject = m_dot_co2*(h_co2_hot - h_co2_cold);
		}		

		water_state water_props;
		double P_water = std::numeric_limits<double>::quiet_NaN();
		if (x_water > 0.0)
		{			
			prop_err_code = water_TQ(T_water_hot + 273.15, x_water, &water_props);
			if (prop_err_code != 0)
			{
				log("Water hot props failed at inlet", SSC_ERROR, -1.0);

				return;
			}
			P_water = water_props.pres;	//[kPa]
		}
		else
		{
			P_water = 101.0;			//[kPa]

			prop_err_code = water_TP(T_water_hot + 273.15, P_water, &water_props);
			if (prop_err_code != 0)
			{
				log("Water hot props failed at inlet", SSC_ERROR, -1.0);

				return;
			}
		}		
		double h_water_hot = water_props.enth;	//[kJ/kg]

		prop_err_code = water_TP(T_water_cold + 273.15, P_water, &water_props);
		if (prop_err_code != 0)
		{
			log("Water hot props failed at inlet", SSC_ERROR, -1.0);

			return;
		}
		double h_water_cold = water_props.enth;	//[kJ/kg]

		double m_dot_water = (q_dot_reject) / (h_water_hot - h_water_cold);

		// Test C_HX_counterflow model as a sCO2-water heat exchanger
		C_HX_counterflow mc_sco2_water_hx;
		C_HX_counterflow::S_init_par ms_hx_init;
		ms_hx_init.m_N_sub_hx = 20;
		ms_hx_init.m_hot_fl = NS_HX_counterflow_eqs::CO2;
		ms_hx_init.m_cold_fl = NS_HX_counterflow_eqs::WATER;
		// Initialize
		mc_sco2_water_hx.initialize(ms_hx_init);

		double UA_cooler, min_DT_cooler, eff_cooler, NTU_cooler, h_co2_cold_calc, h_water_hot_calc, q_dot_reject_calc;
		try
		{
		mc_sco2_water_hx.calc_req_UA_enth(q_dot_reject, m_dot_water, m_dot_co2,
			h_water_cold, h_co2_hot, P_water, P_water, P_co2, P_co2,
			UA_cooler, min_DT_cooler, eff_cooler, NTU_cooler, h_co2_cold_calc, h_water_hot_calc, q_dot_reject_calc);
		}
		catch (C_csp_exception csp_except)
		{
			double blah_for_now = 1.23;
			//throw exec_error("sco2-water hx", "failed");
		}


		// Get user-defined parameters
		double W_dot_net_des = as_double("W_dot_net_des")*1.E3;
		double eta_c = as_double("eta_c");
		double eta_t = as_double("eta_t");
		double P_high_limit = as_double("P_high_limit")*1.E3;		//[kPa], convert from MPa
		double delta_T_t = as_double("deltaT_PHX");

		double delta_T_acc = as_double("deltaT_ACC");
		double T_amb_cycle_des = as_double("T_amb_des") + 273.15;

		double T_htf_hot = as_double("T_htf_hot_des") + 273.15;
		double eta_thermal_des = as_double("eta_des");

		// Define hardcoded sco2 design point parameters
		std::vector<double> DP_LT(2);
		/*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/
		DP_LT[0] = 0;
		DP_LT[1] = 0;
		/*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/
		std::vector<double> DP_HT(2);
		DP_HT[0] = 0;
		DP_HT[1] = 0;
		/*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/
		std::vector<double> DP_PC(2);
		DP_PC[0] = 0;
		DP_PC[1] = 0;
		/*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/
		std::vector<double> DP_PHX(2);
		DP_PHX[0] = 0;
		DP_PHX[1] = 0;
		int N_sub_hxrs = 10;
		double N_t_des = 3600.0;
		double tol = 1.E-3;
		double opt_tol = 1.E-3;

		// Test C_HX_counterflow model as a sCO2-water heat exchanger
		//C_HX_counterflow mc_sco2_water_hx;
		//C_HX_counterflow::S_init_par ms_hx_init;
		//ms_hx_init.m_N_sub_hx = 20;
		//ms_hx_init.m_hot_fl = NS_HX_counterflow_eqs::CO2;
		//ms_hx_init.m_cold_fl = NS_HX_counterflow_eqs::WATER;
		//// Initialize
		//mc_sco2_water_hx.initialize(ms_hx_init);

		std::vector<double> v_P_water_in;		//[kPa]
		std::vector<double> v_T_sco2_cold;		//[C]

		double T_water_amb = 20.0 + 273.15;		//[C]

		double P_water_in = 50.0;				//[kPa]
		int iter_P_water_in = 0;

		while (P_water_in < 110.0)
		{
			P_water_in = 10.0 + 50.0*iter_P_water_in;

			double delta_T_pc = 5.0;				//[C]
			int iter_deltaT_pc = 0;

			while (true)
			{
				delta_T_pc = 5.0 + 1.0*iter_deltaT_pc;

				// For now, pick outlet conditions
				// CO2
				// in
				double P_co2_in = 9.4E3;		//[kPa]
				double T_co2_in = 128.1 + 273.15;	//[K]
				
				prop_err_code = CO2_TP(T_co2_in, P_co2_in, &co2_props);
				if (prop_err_code != 0)
				{
					log("CO2 props failed at inlet", SSC_ERROR, -1.0);

					return;
				}
				double h_co2_in = co2_props.enth;	//[kJ/kg]
				// out
				double P_co2_out = P_co2_in;		//[MPa]
				double T_co2_out = T_water_amb + delta_T_pc;	//[K]
				prop_err_code = CO2_TP(T_co2_out, P_co2_out, &co2_props);
				if (prop_err_code != 0)
				{
					log("CO2 props failed at outlet", SSC_ERROR, -1.0);

					return;
				}
				double h_co2_out = co2_props.enth;	//[kJ/kg]
				double q_dot_hx = 10.E3;		//[kWt]
				double m_dot_co2 = q_dot_hx / (h_co2_in - h_co2_out);
				// Water
				// in
				double T_water_in = T_water_amb;	//[K]
				
				prop_err_code = water_TP(T_water_in, P_water_in, &water_props);
				if (prop_err_code != 0)
				{
					log("Water props failed at inlet", SSC_ERROR, -1.0);

					return;
				}
				double h_water_in = water_props.enth;	//[kJ/kg]
				// out
				double P_water_out = P_water_in;		//[kPa]
				double x_water_out = 1.0;				//[-]
				prop_err_code = water_PQ(P_water_out, x_water_out, &water_props);
				if (prop_err_code != 0)
				{
					log("Water props failed at outlet", SSC_ERROR, -1.0);

					return;
				}
				double h_water_out = water_props.enth;	//[kJ/kg]
				double m_dot_water = q_dot_hx / (h_water_out - h_water_in);

				double UA_calc, min_DT_calc, eff_calc, NTU_calc, T_co2_out_calc, T_water_out_calc, q_dot_calc;
				UA_calc = min_DT_calc = eff_calc = NTU_calc = T_co2_out_calc = T_water_out_calc = q_dot_calc = std::numeric_limits<double>::quiet_NaN();
				try
				{
					mc_sco2_water_hx.calc_req_UA(q_dot_hx, m_dot_water, m_dot_co2,
						T_water_in, T_co2_in, P_water_in, P_water_out, P_co2_in, P_co2_out,
						UA_calc, min_DT_calc, eff_calc, NTU_calc, T_co2_out_calc, T_water_out_calc, q_dot_calc);
				}
				catch (C_csp_exception csp_except)
				{
					iter_deltaT_pc++;
					continue;
				}

				break;
			}
			iter_P_water_in++;

		}
		double blajalahfla = 1.23;

//		// Test C_HX_counterflow model as a sCO2 recuperator
//		C_HX_counterflow mc_sco2_recup;
//		C_HX_counterflow::S_init_par ms_recup_init;
//		ms_recup_init.m_N_sub_hx = 10;
//		ms_recup_init.m_hot_fl = C_HX_counterflow::CO2;
//		ms_recup_init.m_cold_fl = C_HX_counterflow::CO2;
//
//		// Initialize recuperator
//		mc_sco2_recup.initialize(ms_recup_init);
//		C_HX_counterflow::S_des_par recup_par;
//		recup_par.m_Q_dot_design = 53131.3;
//		recup_par.m_m_dot_cold_des = 213.59;
//		recup_par.m_m_dot_hot_des = 305.13;
//		recup_par.m_T_c_in = 331.78;
//		recup_par.m_T_h_in = 633.65;
//		recup_par.m_P_c_in = 12639.0;
//		recup_par.m_P_c_out = 12639.0;
//		recup_par.m_P_h_in = 9694.0;
//		recup_par.m_P_h_out = 9694.0;
//		C_HX_counterflow::S_des_solved recup_des_solved;
//		mc_sco2_recup.design_calc_UA(recup_par, recup_des_solved);
//
//		C_HX_co2_to_co2 c_co2_to_co2;
//		c_co2_to_co2.initialize(10);
//
//		C_HX_counterflow::S_des_solved co2_to_co2_des_solved;
//		c_co2_to_co2.design_calc_UA(recup_par, co2_to_co2_des_solved);
//
//		//*************************************************************
//		//*************************************************************
//		// Set up PHX model from Type 424
//		//*************************************************************
//		// First, need to know something about fluids
//		HTFProperties mc_htf;
//		mc_htf.SetFluid(HTFProperties::Salt_60_NaNO3_40_KNO3,true);
//		CO2_state mc_co2_props;
//
//		double T_t_in_des = T_htf_hot - delta_T_t;	//[K]
//		// For now, estimate T_PHX_co2_in - will get from design point cycle optimization in future
//		double T_PHX_co2_in = T_t_in_des - 200.0;	//[K]
//		double T_htf_cold = T_PHX_co2_in + delta_T_t;	//[K]
//
//		// Receiver/hot side
//		double h_htf_hot = mc_htf.enth_lookup(T_htf_hot);	//[kJ/kg]
//		double h_htf_cold = mc_htf.enth_lookup(T_htf_cold);	//[kJ/kg]
//		double eta_thermal = 0.5;		//[-] target power cycle efficiency
//		double q_dot_htf = (W_dot_net_des / eta_thermal);	//[kWt] target thermal power to power cycle
//		double m_dot_htf_des = q_dot_htf / (h_htf_hot - h_htf_cold);
//
//		// Because C_dot_c = C_dot_h, q_dot_max = 
//		double h_htf_cold_min = mc_htf.enth_lookup(T_PHX_co2_in);
//		double q_dot_htf_max = m_dot_htf_des*(h_htf_hot - h_htf_cold_min);		//[kWt]
//
//		// Effectiveness & NTU
//		double eff_des = q_dot_htf / q_dot_htf_max;
//		double NTU = eff_des / (1.0 - eff_des);
//
//		// UA estimate with bulk calculation
//		double UA_PHX_des = NTU*m_dot_htf_des*(h_htf_hot-h_htf_cold)/(T_htf_hot-T_htf_cold);
//
//		// Calculate CO2 mass flow rate for HX model
//		double P_CO2 = 20000.0;										//[kPa]
//		int co2_error = CO2_TP(T_t_in_des, P_CO2, &mc_co2_props);
//		double h_out = mc_co2_props.enth;
//		co2_error = CO2_TP(T_PHX_co2_in, P_CO2, &mc_co2_props);
//		double h_in = mc_co2_props.enth;
//		double m_dot_CO2 = (W_dot_net_des / eta_thermal) / (h_out - h_in);	//[kg/s]
//
//		C_HX_co2_to_htf mc_phx;
//		mc_phx.initialize(HTFProperties::Salt_60_NaNO3_40_KNO3);
//		double UA_PHX_calc = std::numeric_limits<double>::quiet_NaN();
//		double min_DT_calc = std::numeric_limits<double>::quiet_NaN();
//
//		// ****************************************************************************
//		// ****************************************************************************
//		// ****************************************************************************
//		C_HX_counterflow::S_des_par ms_phx_des_par;
//		ms_phx_des_par.m_Q_dot_design = q_dot_htf;
//		ms_phx_des_par.m_T_h_in = T_htf_hot;
//		ms_phx_des_par.m_P_h_in = 1.0;
//		ms_phx_des_par.m_P_h_out = 1.0;
//		ms_phx_des_par.m_m_dot_hot_des = m_dot_htf_des;
//		ms_phx_des_par.m_T_c_in = T_PHX_co2_in;
//		ms_phx_des_par.m_P_c_in = P_CO2;
//		ms_phx_des_par.m_P_c_out = P_CO2;
//		ms_phx_des_par.m_m_dot_cold_des = m_dot_CO2;
//
//		C_HX_counterflow::S_des_solved phx_des_solved;
//		
//		mc_phx.design_calc_UA(ms_phx_des_par, phx_des_solved);
//
//		double q_dot_od, T_c_out_od, T_h_out_od;
//		q_dot_od = T_c_out_od = T_h_out_od = std::numeric_limits<double>::quiet_NaN();
//
//		// Need to check what is happening when one mass flow rate decreases
//		// Solver *should* return a notice...
//
///*		mc_phx.od_performance(T_PHX_co2_in, P_CO2, 0.5*m_dot_CO2,
//			T_htf_hot, 1.0, m_dot_htf_des,
//			q_dot_od, T_c_out_od, T_h_out_od);
//*/
//		mc_phx.off_design_solution(T_PHX_co2_in, P_CO2, 0.5*m_dot_CO2, P_CO2,
//			T_htf_hot, 1.0, m_dot_htf_des, 1.0,
//			q_dot_od, T_c_out_od, T_h_out_od);

		//od_performance(double T_c_in /*K*/, double P_c_in /*kPa*/, double m_dot_c /*kg/s*/,
		//	double T_h_in /*K*/, double P_h_in /*kPa*/, double m_dot_h /*kg/s*/,
		//	double & q_dot /*kWt*/, double & T_c_out /*K*/, double & T_h_out /*K*/)

		//double q_dot_od, T_PHX_co2_out_od, T_htf_cold_od;
		//mc_phx.od_performance(T_PHX_co2_in, m_dot_CO2, T_htf_hot, m_dot_htf_des,
		//	q_dot_od, T_PHX_co2_out_od, T_htf_cold_od);

		// ****************************************************************************
		// ****************************************************************************
		// Test out monotonic function solver
		// ****************************************************************************

		/*HX_object da_solver;
		double result;
		int int_success = da_solver.myfunction(result);*/

		C_import_mono_eq ty_mono_eq(&test_mono_function);
		
		C_monotonic_eq_solver eq_solv(ty_mono_eq);

		double result;
		int int_success = eq_solv.test_member_function(2.0, &result);

		double x_low = std::numeric_limits<double>::quiet_NaN();
		double x_high = std::numeric_limits<double>::quiet_NaN();
		int iter_limit = 50.0;
		eq_solv.settings(0.001, iter_limit, x_low, x_high, true);

		double x_solved, tol_solved;
		x_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
		int iter_solved = -1;
		double x_guess_1 = -100.0;
		double x_guess_2 = -99.0;
		double y_target = -2.0;
		eq_solv.solve(x_guess_1, x_guess_2, y_target, x_solved, tol_solved, iter_solved);

		// Initialize cycle here, so can use 'get_design_limits()'
			// Also define error and warning message strings
		std::string error_msg;
		error_msg[0] = NULL;
		int error_code = 0;
		C_RecompCycle rc_cycle;

		int run_off_des_study = as_integer("run_off_des_study");

		if( run_off_des_study == 1 && eta_thermal_des < 0.0 )
		{
			// Find optimal design at maximum allowable recuperator UA
			// This will result in the maximum allowable cycle efficiency
			// Subtract abs(eta_thermal_des) from max allowable efficiency to get "reasonable" efficiency to use in subsequent analysis

			C_RecompCycle::S_auto_opt_design_parameters rc_params_max_eta;

			rc_params_max_eta.m_W_dot_net = W_dot_net_des;					//[kW]
			rc_params_max_eta.m_T_mc_in = T_amb_cycle_des + delta_T_acc;	//[K]
			rc_params_max_eta.m_T_t_in = T_htf_hot - delta_T_t;				//[K]
			rc_params_max_eta.m_DP_LT = DP_LT;
			rc_params_max_eta.m_DP_HT = DP_HT;
			rc_params_max_eta.m_DP_PC = DP_PC;
			rc_params_max_eta.m_DP_PHX = DP_PHX;

			// Get max recuperator UA!!!
			rc_params_max_eta.m_UA_rec_total = rc_cycle.get_design_limits().m_UA_net_power_ratio_max*rc_params_max_eta.m_W_dot_net;		//[kW/K]

			rc_params_max_eta.m_eta_mc = eta_c;
			rc_params_max_eta.m_eta_rc = eta_c;
			rc_params_max_eta.m_eta_t = eta_t;
			rc_params_max_eta.m_N_sub_hxrs = N_sub_hxrs;
			rc_params_max_eta.m_P_high_limit = P_high_limit;
			rc_params_max_eta.m_tol = tol;
			rc_params_max_eta.m_opt_tol = opt_tol;
			rc_params_max_eta.m_N_turbine = N_t_des;

			rc_cycle.auto_opt_design(rc_params_max_eta, error_code);

			if(error_code != 0)
			{
				throw exec_error("sCO2 maximum efficiency calculations failed","");
			}

			// Get solved maximum cycle efficiency and subtract delta_eta to get "reasonable" efficiency
			eta_thermal_des = rc_cycle.get_design_solved()->m_eta_thermal - fabs(eta_thermal_des);		//[-]
		}		

		C_RecompCycle::S_auto_opt_design_hit_eta_parameters rc_params;
		rc_params.m_W_dot_net = W_dot_net_des;					//[kW]
		rc_params.m_eta_thermal = eta_thermal_des;
		rc_params.m_T_mc_in = T_amb_cycle_des + delta_T_acc;	//[K]
		rc_params.m_T_t_in = T_htf_hot - delta_T_t;				//[K]
		rc_params.m_DP_LT = DP_LT;
		rc_params.m_DP_HT = DP_HT;
		rc_params.m_DP_PC = DP_PC;
		rc_params.m_DP_PHX = DP_PHX;
		rc_params.m_eta_mc = eta_c;
		rc_params.m_eta_rc = eta_c;
		rc_params.m_eta_t = eta_t;
		rc_params.m_N_sub_hxrs = N_sub_hxrs;
		rc_params.m_P_high_limit = P_high_limit;
		rc_params.m_tol = tol;
		rc_params.m_opt_tol = opt_tol;
		rc_params.m_N_turbine = N_t_des;
		
		int hot_fl_code = HTFProperties::Salt_60_NaNO3_40_KNO3;
		
		C_sco2_recomp_csp::S_des_par sco2_rc_des_par;
		double elevation = 300.0;		//[m] Elevation
			// System design parameters
		sco2_rc_des_par.m_hot_fl_code = HTFProperties::Salt_60_NaNO3_40_KNO3;
		sco2_rc_des_par.m_T_htf_hot_in = T_htf_hot;
		sco2_rc_des_par.m_phx_dt_hot_approach = delta_T_t;
		sco2_rc_des_par.m_T_amb_des = T_amb_cycle_des;
		sco2_rc_des_par.m_dt_mc_approach = delta_T_acc;
		sco2_rc_des_par.m_elevation = elevation;
		sco2_rc_des_par.m_W_dot_net = W_dot_net_des;
		sco2_rc_des_par.m_eta_thermal = eta_thermal_des;
		sco2_rc_des_par.m_is_recomp_ok = 1;
			// Cycle design parameters
		sco2_rc_des_par.m_DP_LT = DP_LT;
		sco2_rc_des_par.m_DP_HT = DP_HT;
		sco2_rc_des_par.m_DP_PC = DP_PC;
		sco2_rc_des_par.m_DP_PHX = DP_PHX;
		sco2_rc_des_par.m_eta_mc = eta_c;
		sco2_rc_des_par.m_eta_rc = eta_c;
		sco2_rc_des_par.m_eta_t = eta_t;
		sco2_rc_des_par.m_N_sub_hxrs = N_sub_hxrs;
		sco2_rc_des_par.m_P_high_limit = P_high_limit;
		sco2_rc_des_par.m_tol = tol;
		sco2_rc_des_par.m_opt_tol = opt_tol;
		sco2_rc_des_par.m_N_turbine = N_t_des;
			// PHX design parameters
		sco2_rc_des_par.m_phx_dt_cold_approach = delta_T_t;
			// Air cooler parameters
		sco2_rc_des_par.m_frac_fan_power = 0.01;
		sco2_rc_des_par.m_deltaP_cooler_frac = 0.002;

		// So, there are some useful outputs we probably want here...
		C_sco2_recomp_csp sco2_recomp_csp;
		sco2_recomp_csp.design(sco2_rc_des_par);
		double m_dot_htf = sco2_recomp_csp.get_phx_des_par()->m_m_dot_hot_des;	//[kg/s]
		double T_htf_cold_calc = sco2_recomp_csp.get_design_solved()->ms_phx_des_solved.m_T_c_out;		//[K]
		
		// Try calling off-design model with design parameters
		C_sco2_recomp_csp::S_od_par sco2_rc_od_par;
		sco2_rc_od_par.m_T_htf_hot = sco2_rc_des_par.m_T_htf_hot_in;
		sco2_rc_od_par.m_m_dot_htf = m_dot_htf;
		sco2_rc_od_par.m_T_amb = T_amb_cycle_des;
		int od_strategy = C_sco2_recomp_csp::E_MAX_ETA;
		sco2_recomp_csp.off_design_opt(sco2_rc_od_par, od_strategy);



		rc_cycle.auto_opt_design_hit_eta(rc_params, error_code, error_msg);

		if(error_code != 0)
		{
			throw exec_error("sco2 design point calcs", error_msg);
		}

		double eta_thermal_calc = rc_cycle.get_design_solved()->m_eta_thermal;
		double UA_total = rc_cycle.get_design_solved()->m_UA_HT + rc_cycle.get_design_solved()->m_UA_LT;
		double recomp_frac = rc_cycle.get_design_solved()->m_recomp_frac;
		double P_comp_in = rc_cycle.get_design_solved()->m_pres[0] / 1.E3;
		double P_comp_out = rc_cycle.get_design_solved()->m_pres[1] / 1.E3;
		double T_htf_cold = rc_cycle.get_design_solved()->m_temp[5 - 1] + delta_T_t - 273.15;	//[C]

		// Assign SSC outputs
		assign("eta_thermal_calc", eta_thermal_calc);
		assign("UA_total", UA_total);
		assign("recomp_frac", recomp_frac);
		assign("P_comp_in", P_comp_in);
		assign("P_comp_out", P_comp_out);
		assign("T_htf_cold", T_htf_cold); 

		if( error_msg[0] == NULL )
			log("Design point optimization was successful!");
		else
		{
			log("The sCO2 design point optimization solved with the following warning(s):\n" + error_msg);
		}

		//int run_off_des_study = as_integer("run_off_des_study");

		if( run_off_des_study != 1)
		{
			return;
		}
		else	// Run off-design parametrics
		{
			C_RecompCycle::S_opt_target_od_parameters opt_target_od_params;
			
			// opt_target_od_params.m_T_mc_in      ** Set compressor inlet temperature below **
			opt_target_od_params.m_T_t_in = rc_params.m_T_t_in;			//[K]
			
			// opt_target_od_params.m_target       ** Set target q_dot below **
			opt_target_od_params.m_is_target_Q = true;

			opt_target_od_params.m_N_sub_hxrs = rc_params.m_N_sub_hxrs;
			opt_target_od_params.m_lowest_pressure = 1000.0;
			opt_target_od_params.m_highest_pressure = 17000.0;

			opt_target_od_params.m_recomp_frac_guess = rc_cycle.get_design_solved()->m_recomp_frac;
			opt_target_od_params.m_fixed_recomp_frac = false;

			opt_target_od_params.m_N_mc_guess = rc_cycle.get_design_solved()->ms_mc_des_solved.m_N_design;
			opt_target_od_params.m_fixed_N_mc = false;

			opt_target_od_params.m_N_t_guess = rc_cycle.get_design_solved()->ms_t_des_solved.m_N_design;
			opt_target_od_params.m_fixed_N_t = true;

			opt_target_od_params.m_tol = rc_params.m_tol;
			opt_target_od_params.m_opt_tol = rc_params.m_opt_tol;

			opt_target_od_params.m_use_default_res = false;

			double q_dot_in_des = rc_cycle.get_design_solved()->m_W_dot_net / rc_cycle.get_design_solved()->m_eta_thermal;	//[kWt]

			size_t n_f_pl = -1;
			ssc_number_t * f_pl = as_array("part_load_fracs", &n_f_pl);

			int n_solved = 0;

			std::vector<double> part_load_fracs_out(0);
			std::vector<double> part_load_eta(0);

			if(n_f_pl > 0)	// At least one part-load simulation is required
			{
				opt_target_od_params.m_T_mc_in = rc_params.m_T_mc_in;		//[K]

				for( int i = 0; i < n_f_pl; i++ )
				{
					opt_target_od_params.m_target = (double)f_pl[i] * q_dot_in_des;				//[kWt]
					log(util::format("Off design simulation at part load = %lg", f_pl[i]));
					int od_error_code = 0;
					rc_cycle.optimal_target_off_design(opt_target_od_params, od_error_code);
					if(od_error_code == 0)
					{
						part_load_fracs_out.push_back((double)f_pl[i]);
						part_load_eta.push_back(rc_cycle.get_od_solved()->m_eta_thermal/eta_thermal_calc);
					}
				}

				n_solved = part_load_fracs_out.size();
			}
						
			ssc_number_t * f_pl_out = allocate("part_load_fracs_out", n_solved);
			ssc_number_t * eta_f_pl = allocate("part_load_eta", n_solved);

			for( int i = 0; i < n_solved; i++ )
			{
				f_pl_out[i] = part_load_fracs_out[i];
				eta_f_pl[i] = part_load_eta[i];
			}

			// Find and write polynomial coefficients for part load
			std::vector<double> pl_coefs;
			double pl_r_squared = std::numeric_limits<double>::quiet_NaN();
			bool pl_success = find_polynomial_coefs(part_load_fracs_out, part_load_eta, 5, pl_coefs, pl_r_squared);
			assign("Part_load_r_squared", pl_r_squared);

			ssc_number_t * p_pl_coefs = allocate("part_load_coefs", 5);
			if(pl_success)
			{
				for( int i = 0; i < 5; i++ )
					p_pl_coefs[i] = pl_coefs[i];
			}
			else
			{
				log("Part load coefficient calcuations failed");
				for( int i = 0; i < 5; i++ )
					p_pl_coefs[i] = 0.0;
			}
			// ********************************************

						
			size_t n_T_amb_od = -1;
			ssc_number_t * T_amb_od = as_array("T_amb_array", &n_T_amb_od);

			n_solved = 0;

			std::vector<double> T_amb_out(0);
			std::vector<double> T_amb_eta(0);

			if(n_T_amb_od > 0)	// At least one off-design ambient temperature simulation is required
			{
				opt_target_od_params.m_target = q_dot_in_des;

				for( int i = 0; i < n_T_amb_od; i++ )
				{
					opt_target_od_params.m_T_mc_in = max(rc_cycle.get_design_limits().m_T_mc_in_min, (double)T_amb_od[i] + delta_T_acc + 273.15);	//[K] convert from C and add air cooler deltaT
					log(util::format("Off design simulation at ambient temperature = %lg", T_amb_od[i]));
					log(util::format("Corresponding compressor inlet temperature = %lg", opt_target_od_params.m_T_mc_in - 273.15));
					int od_error_code = 0;
					rc_cycle.optimal_target_off_design(opt_target_od_params, od_error_code);
					if(od_error_code == 0)
					{
						T_amb_out.push_back((double)T_amb_od[i]);
						T_amb_eta.push_back(rc_cycle.get_od_solved()->m_eta_thermal / eta_thermal_calc);
					}
				}

				n_solved = T_amb_out.size();
			}						

			ssc_number_t * T_amb_od_out = allocate("T_amb_array_out", n_solved);
			ssc_number_t * eta_T_amb = allocate("T_amb_eta", n_solved);

			for( int i = 0; i < n_solved; i++ )
			{
				T_amb_od_out[i] = T_amb_out[i];
				eta_T_amb[i] = T_amb_eta[i];
			}


			// Find polynomial coefficients for part load
			std::vector<double> T_amb_coefs;
			std::vector<double> T_amb_od_less_des;
			double T_amb_r_squared = std::numeric_limits<double>::quiet_NaN();

			T_amb_od_less_des.resize(n_solved);
			for( int i = 0; i < n_solved; i++ )
			{
				T_amb_od_less_des[i] = T_amb_od[i] - (T_amb_cycle_des - 273.15);
			}
			bool T_amb_success = find_polynomial_coefs(T_amb_od_less_des, T_amb_eta, 5, T_amb_coefs, T_amb_r_squared);
			assign("T_amb_r_squared", T_amb_r_squared);

			ssc_number_t * p_T_amb_coefs = allocate("T_amb_coefs", 5);
			if( T_amb_success )
			{
				for( int i = 0; i < 5; i++ )
					p_T_amb_coefs[i] = T_amb_coefs[i];
			}
			else
			{
				log("Ambient temperature coefficient calcuations failed");
				for( int i = 0; i < 5; i++ )
					p_T_amb_coefs[i] = 0.0;
			}

			// ******************************************************
		}

	}


};

int test_mono_function(double x, double *y)
{
	*y = -(x*x);

	return 0;
}

DEFINE_MODULE_ENTRY(sco2_design_point, "Returns optimized sco2 cycle parameters given inputs", 0)

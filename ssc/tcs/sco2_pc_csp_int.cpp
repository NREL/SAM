#include "sco2_pc_csp_int.h"
#include "sco2_pc_core.h"
#include "csp_solver_util.h"
#include "CO2_properties.h"
#include <cmath>
#include <string>

#include "nlopt.hpp"

#include "fmin.h"

C_sco2_recomp_csp::C_sco2_recomp_csp()
{
	// Get CO2 critical temperature
	CO2_info co2_fluid_info;
	get_CO2_info(&co2_fluid_info);
	m_T_co2_crit = co2_fluid_info.T_critical;		//[K]
	m_P_co2_crit = co2_fluid_info.P_critical;		//[kPa]

	m_T_mc_in_min = mc_rc_cycle.get_design_limits().m_T_mc_in_min;		//[K]

	// Set default optimization strategy
	m_od_opt_objective = E_MAX_ETA;		//[-]
	m_is_phi_optimized = true;			//[-]
	m_od_opt_ftol = 1.E-4;				//[-] Relative tolerance for od optimization: objective function convergence
	m_od_opt_xtol = 1.E-4;				//[-] Relative tolerance for od optimization: independent variable convergence
	m_true_nlopt_false_fmin = false;	//[-]
	m_eta_max_eta = 0.0;				//[-]
	// **************************************

	// Default off-design turbomachinery operation is:
	//	Can choose main compressor shaft speed
	//	Can choose recompressor shaft speed
	//	Turbine is fixed
	//m_off_design_turbo_operation = E_VFD_MC_VFD_RC_FIXED_T;
	m_off_design_turbo_operation = E_FIXED_MC_FIXED_RC_FIXED_T;

	mf_callback = 0;		// NULL
	m_cdata = 0;			// NULL

	m_is_write_mc_out_file = false;
	m_is_only_write_frecomp_opt_iters = false;

	//sco2_od_opt_file.open("C:/Users/tneises/Documents/Projects/ssc_trunk/examples/sco2_od_opt_file.csv");

	//sco2_od_opt_file << "P_mc_in_MPa,f_recomp,phi_mc,eta_ND,W_dot_MW\n";
}

void C_sco2_recomp_csp::design(C_sco2_recomp_csp::S_des_par des_par)
{
	ms_des_par = des_par;

	design_core();
}

void C_sco2_recomp_csp::design_core()
{
	// using -> C_RecompCycle::S_auto_opt_design_hit_eta_parameters
	std::string error_msg;
	error_msg[0] = NULL;
	int auto_err_code = 0;

	if(ms_des_par.m_design_method == 1)
	{
		// Design the recompression cycle to hit a specified efficiency
			// Define sCO2 cycle design parameter structure
		ms_rc_cycle_des_par.m_W_dot_net = ms_des_par.m_W_dot_net;		//[kWe]
		ms_rc_cycle_des_par.m_eta_thermal = ms_des_par.m_eta_thermal;	//[-]
		ms_rc_cycle_des_par.m_T_mc_in = ms_des_par.m_T_amb_des+ms_des_par.m_dt_mc_approach;	//[K]
		if(ms_rc_cycle_des_par.m_T_mc_in < m_T_mc_in_min)
		{
			std::string msg = util::format("The input design main compressor inlet temperature is %lg [C]." 
			" The sCO2 cycle design code reset it to the minimum allowable design main compressor inlet temperature: %lg [C].",
			ms_rc_cycle_des_par.m_T_mc_in-273.15,
			m_T_mc_in_min-273.15);
		}
		ms_rc_cycle_des_par.m_T_t_in = ms_des_par.m_T_htf_hot_in-ms_des_par.m_phx_dt_hot_approach;	//[K]
		ms_rc_cycle_des_par.m_DP_LT = ms_des_par.m_DP_LT;
		ms_rc_cycle_des_par.m_DP_HT = ms_des_par.m_DP_HT;
		ms_rc_cycle_des_par.m_DP_PC = ms_des_par.m_DP_PC;
		ms_rc_cycle_des_par.m_DP_PHX = ms_des_par.m_DP_PHX;
		ms_rc_cycle_des_par.m_LT_eff_max = ms_des_par.m_LT_eff_max;
		ms_rc_cycle_des_par.m_HT_eff_max = ms_des_par.m_HT_eff_max;
		ms_rc_cycle_des_par.m_eta_mc = ms_des_par.m_eta_mc;
		ms_rc_cycle_des_par.m_eta_rc = ms_des_par.m_eta_rc;
		ms_rc_cycle_des_par.m_eta_t = ms_des_par.m_eta_t;
		ms_rc_cycle_des_par.m_N_sub_hxrs = ms_des_par.m_N_sub_hxrs;
		ms_rc_cycle_des_par.m_P_high_limit = ms_des_par.m_P_high_limit;
		ms_rc_cycle_des_par.m_tol = ms_des_par.m_tol;
		ms_rc_cycle_des_par.m_opt_tol = ms_des_par.m_opt_tol;
		ms_rc_cycle_des_par.m_N_turbine = ms_des_par.m_N_turbine;
		ms_rc_cycle_des_par.m_is_recomp_ok = ms_des_par.m_is_recomp_ok;	

		mc_rc_cycle.auto_opt_design_hit_eta(ms_rc_cycle_des_par, auto_err_code, error_msg);
	}
	else if(ms_des_par.m_design_method == 2)
	{
		if(ms_des_par.m_UA_recup_tot_des < 0.0)
		{
			throw("sCO2 recompression cycle and CSP integration design, design method 2, conductance must be > 0");
		}

		// Design the recompression cycle using a specified total recuperator conductance
		C_RecompCycle::S_auto_opt_design_parameters s_rc_auto_opt_des_par;
		s_rc_auto_opt_des_par.m_W_dot_net = ms_des_par.m_W_dot_net;		//[kWe]
		s_rc_auto_opt_des_par.m_T_mc_in = ms_des_par.m_T_amb_des + ms_des_par.m_dt_mc_approach;		//[K]
		s_rc_auto_opt_des_par.m_T_t_in = ms_des_par.m_T_htf_hot_in - ms_des_par.m_phx_dt_hot_approach;	//[K]
		s_rc_auto_opt_des_par.m_DP_LT = ms_des_par.m_DP_LT;
		s_rc_auto_opt_des_par.m_DP_HT = ms_des_par.m_DP_HT;
		s_rc_auto_opt_des_par.m_DP_PC = ms_des_par.m_DP_PC;
		s_rc_auto_opt_des_par.m_DP_PHX = ms_des_par.m_DP_PHX;
		s_rc_auto_opt_des_par.m_UA_rec_total = ms_des_par.m_UA_recup_tot_des;	//[kW/K]
		s_rc_auto_opt_des_par.m_LT_eff_max = ms_des_par.m_LT_eff_max;
		s_rc_auto_opt_des_par.m_HT_eff_max = ms_des_par.m_HT_eff_max;
		s_rc_auto_opt_des_par.m_eta_mc = ms_des_par.m_eta_mc;
		s_rc_auto_opt_des_par.m_eta_rc = ms_des_par.m_eta_rc;
		s_rc_auto_opt_des_par.m_eta_t = ms_des_par.m_eta_t;
		s_rc_auto_opt_des_par.m_N_sub_hxrs = ms_des_par.m_N_sub_hxrs;
		s_rc_auto_opt_des_par.m_P_high_limit = ms_des_par.m_P_high_limit;
		s_rc_auto_opt_des_par.m_tol = ms_des_par.m_tol;
		s_rc_auto_opt_des_par.m_opt_tol = ms_des_par.m_opt_tol;
		s_rc_auto_opt_des_par.m_N_turbine = ms_des_par.m_N_turbine;

		s_rc_auto_opt_des_par.m_PR_mc_guess = ms_des_par.m_PR_mc_guess;		//[-]
		s_rc_auto_opt_des_par.m_fixed_PR_mc = ms_des_par.m_fixed_PR_mc;		//[-]

		s_rc_auto_opt_des_par.m_is_recomp_ok = ms_des_par.m_is_recomp_ok;
	
		mc_rc_cycle.auto_opt_design(s_rc_auto_opt_des_par, auto_err_code);
	}
	else
	{
		throw(C_csp_exception("sCO2 recompression cycle and CSP integration design, design method must be either 1 or 2\n"));
	}

	ms_des_solved.ms_rc_cycle_solved = *mc_rc_cycle.get_design_solved();

	if(auto_err_code != 0)
	{
		throw(C_csp_exception(error_msg.c_str()));
	}

	if( error_msg[0] == NULL )
	{
		mc_messages.add_notice("The recompression cycle design optimization was successful");
	}
	else
	{
		string out_msg = "The sCO2 cycle design optimization solved with the following warning(s):\n" + error_msg;
		mc_messages.add_notice(out_msg);
	}
	//*************************************************************************************
	//*************************************************************************************

	// Set air cooler design parameters that are dependent on the cycle design solution
	ms_air_cooler_des_par_dep.m_T_hot_in_des = ms_des_solved.ms_rc_cycle_solved.m_temp[C_RecompCycle::LTR_LP_OUT];
	ms_air_cooler_des_par_dep.m_P_hot_in_des = ms_des_solved.ms_rc_cycle_solved.m_pres[C_RecompCycle::LTR_LP_OUT];
	ms_air_cooler_des_par_dep.m_m_dot_total = ms_des_solved.ms_rc_cycle_solved.m_m_dot_mc;		//[kg/s]
		// This pressure drop is currently uncoupled from the cycle design
	ms_air_cooler_des_par_dep.m_delta_P_des = ms_des_par.m_deltaP_cooler_frac*ms_des_solved.ms_rc_cycle_solved.m_pres[C_RecompCycle::MC_OUT];
	ms_air_cooler_des_par_dep.m_T_hot_out_des = ms_des_solved.ms_rc_cycle_solved.m_temp[C_RecompCycle::MC_IN];
	ms_air_cooler_des_par_dep.m_W_dot_fan_des = ms_des_par.m_frac_fan_power*ms_des_par.m_W_dot_net/1000.0;		//[MWe]

	// Initialize the PHX
	mc_phx.initialize(ms_des_par.m_hot_fl_code, ms_des_par.mc_hot_fl_props);

	// Design the PHX
	double q_dot_des_phx = ms_des_solved.ms_rc_cycle_solved.m_W_dot_net / ms_des_solved.ms_rc_cycle_solved.m_eta_thermal;
	//ms_phx_des_par.m_Q_dot_design = ms_des_solved.ms_rc_cycle_solved.m_W_dot_net / ms_des_solved.ms_rc_cycle_solved.m_eta_thermal;		//[kWt]
	ms_phx_des_par.m_T_h_in = ms_des_par.m_T_htf_hot_in;	//[K] HTF hot inlet temperature 
		// Okay, but CO2-HTF HX is assumed here. How does "structure inheritance" work?
	ms_phx_des_par.m_P_h_in = 1.0;							// Assuming HTF is incompressible...
	ms_phx_des_par.m_P_h_out = 1.0;						// Assuming HTF is incompressible...
		// .................................................................................
	ms_phx_des_par.m_T_c_in = ms_des_solved.ms_rc_cycle_solved.m_temp[C_RecompCycle::HTR_HP_OUT];		//[K]
	ms_phx_des_par.m_P_c_in = ms_des_solved.ms_rc_cycle_solved.m_pres[C_RecompCycle::HTR_HP_OUT];		//[K]
	ms_phx_des_par.m_P_c_out = ms_des_solved.ms_rc_cycle_solved.m_pres[C_RecompCycle::TURB_IN];		//[K]
	ms_phx_des_par.m_m_dot_cold_des = ms_des_solved.ms_rc_cycle_solved.m_m_dot_t;	//[kg/s]
		// Calculating the HTF mass flow rate in 'design_and_calc_m_dot_htf'
	ms_phx_des_par.m_m_dot_hot_des = std::numeric_limits<double>::quiet_NaN();
		// Set maximum effectiveness
	ms_phx_des_par.m_eff_max = 1.0;
	
	mc_phx.design_and_calc_m_dot_htf(ms_phx_des_par, q_dot_des_phx, ms_des_par.m_phx_dt_cold_approach, ms_des_solved.ms_phx_des_solved);

	// Design the air cooler
		// Define Independent Air Cooler Design Parameters
	ms_air_cooler_des_par_ind.m_T_amb_des = ms_des_par.m_T_amb_des;		//[K]
	ms_air_cooler_des_par_ind.m_elev = ms_des_par.m_elevation;			//[m]
		// Add checks from Type 424 to the air cooler design code?
	
	
	
	
	
	
	
	// 4.12.2017 twn: comment this out for now
	//mc_air_cooler.design_hx(ms_air_cooler_des_par_ind, ms_air_cooler_des_par_dep);






	return;
}

int C_sco2_recomp_csp::off_design_nested_opt(C_sco2_recomp_csp::S_od_par od_par, int off_design_strategy, double od_opt_tol)
{
	ms_od_par = od_par;	

	// Optimization variables:
	m_od_opt_objective = off_design_strategy;
	if( m_od_opt_objective == E_MAX_ETA_FIX_PHI ||
		m_od_opt_objective == E_MAX_POWER_FIX_PHI ||
		m_od_opt_objective == E_MOO_ETA_0p1Wnd_FIX_PHI ||
		m_od_opt_objective == E_MOO_ETA_T_T_IN_FIX_PHI )
	{
		m_is_phi_optimized = false;
	}
	else
	{
		m_is_phi_optimized = true;
	}
	m_od_opt_ftol = od_opt_tol;
	// ****************************************



	//if (m_od_opt_objective == E_MAX_POWER_FIX_PHI ||
	//	m_od_opt_objective == E_MAX_POWER)
	//{
	//	m_od_opt_objective = E_TARGET_POWER_ETA_MAX;
	//}




	// Define ms_rc_cycle_od_par
	// Defined now
	ms_rc_cycle_od_phi_par.m_T_mc_in = ms_od_par.m_T_amb + ms_des_par.m_dt_mc_approach;		//[K]
	if( ms_rc_cycle_od_phi_par.m_T_mc_in < m_T_mc_in_min )
	{
		std::string msg = util::format("The off-design main compressor inlet temperature is %lg [C]."
			" The sCO2 cycle off-design code reset it to the minimum allowable main compressor inlet temperature: %lg [C].",
			ms_rc_cycle_od_phi_par.m_T_mc_in - 273.15,
			m_T_mc_in_min - 273.15);
		ms_rc_cycle_od_phi_par.m_T_mc_in = m_T_mc_in_min;
	}

	ms_rc_cycle_od_phi_par.m_N_sub_hxrs = ms_des_par.m_N_sub_hxrs;			//[-]
	ms_rc_cycle_od_phi_par.m_tol = ms_des_par.m_tol;						//[-]
	ms_rc_cycle_od_phi_par.m_N_t = ms_des_solved.ms_rc_cycle_solved.ms_t_des_solved.m_N_design;	//[rpm]
	// Defined downstream
	ms_rc_cycle_od_phi_par.m_T_t_in = std::numeric_limits<double>::quiet_NaN();			//[K]			
	ms_rc_cycle_od_phi_par.m_P_mc_in = std::numeric_limits<double>::quiet_NaN();		//[kPa]
	ms_rc_cycle_od_phi_par.m_recomp_frac = std::numeric_limits<double>::quiet_NaN();	//[-]
	ms_rc_cycle_od_phi_par.m_phi_mc = std::numeric_limits<double>::quiet_NaN();			//[-]
	// Define ms_phx_od_par
	// Defined now
	ms_phx_od_par.m_T_h_in = ms_od_par.m_T_htf_hot;			//[K]
	ms_phx_od_par.m_P_h_in = ms_phx_des_par.m_P_h_in;		//[kPa] Assuming fluid is incompressible in that pressure doesn't affect its properties
	ms_phx_od_par.m_m_dot_h = ms_od_par.m_m_dot_htf;		//[kg/s]
	// Defined downstream
	ms_phx_od_par.m_T_c_in = std::numeric_limits<double>::quiet_NaN();		//[K]
	ms_phx_od_par.m_P_c_in = std::numeric_limits<double>::quiet_NaN();		//[kPa]
	ms_phx_od_par.m_m_dot_c = std::numeric_limits<double>::quiet_NaN();		//[kg/s]

	m_is_write_mc_out_file = true;
	m_is_only_write_frecomp_opt_iters = true;

	mstr_base_name = "C:/Users/tneises/Documents/Brayton-Rankine/APOLLO/Off_design_turbo_balance/fixed_N_recomp/";

	ms_rc_cycle_od_phi_par.m_phi_mc = mc_rc_cycle.get_design_solved()->ms_mc_des_solved.m_phi_des;	//[-]

	//double P_mc_in_min = 5000.0;		//[kPa]
	//double P_mc_in_max = 8000.0;		//[kPa]
	//double P_mc_in_inc = 25.0;			//[kPa]
	//
	//off_design_P_mc_in_parameteric(P_mc_in_min, P_mc_in_max, P_mc_in_inc);
	//return -1;

	//double P_mc_in_parametric = 9279.7;		//[kPa]
	//double f_recomp_start = 0.21;			//[-]
	//double f_recomp_end = 0.26;				//[-]
	//double f_recomp_inc = 1.e-4;			//[-]
	//
	//off_design_fix_P_mc_in_parametric_f_recomp(P_mc_in_parametric, f_recomp_start, f_recomp_end, f_recomp_inc);	

	//m_eta_max_eta = 0.0;
	//m_od_opt_objective = E_MAX_ETA_FIX_PHI;
	
	//m_od_opt_objective = E_MAX_POWER_FIX_PHI;
	//m_od_opt_ftol = 1.E-7;
	//m_od_opt_objective = E_MAX_POWER_FIX_PHI;
	bool opt_success_2_par = opt_P_mc_in_nest_f_recomp_max_eta_core();
	if( !opt_success_2_par )
	{
		throw(C_csp_exception("2D nested optimization to maximize efficiency failed"));
	}

	// Set max efficiency
	/*
	m_eta_max_eta = ms_od_solved.ms_rc_cycle_od_solved.m_eta_thermal;		//[-]
	m_od_opt_ftol = 1.E-7;

	m_od_opt_objective = E_MAX_POWER_IN_ETA_MAX_BAND;
	opt_success_2_par = opt_P_mc_in_nest_f_recomp_max_eta_core();
	if( !opt_success_2_par )
	{
		throw(C_csp_exception("2D nested optimization to maximize efficiency failed"));
	}
	*/

	return 0;
}

bool C_sco2_recomp_csp::opt_P_mc_in_nest_f_recomp_max_eta_core()
{
	// Prior to calling, need to set :
	//	*ms_od_par, ms_rc_cycle_od_phi_par, ms_phx_od_par, ms_od_op_inputs(will set P_mc_in here and f_recomp downstream)
	
	// Get density at design point
	double mc_dens_in_des = ms_des_solved.ms_rc_cycle_solved.m_dens[C_RecompCycle::MC_IN];		//[kg/m^3]
	CO2_state co2_props;
	// Then calculate the compressor inlet pressure that achieves this density at the off-design ambient temperature
	int co2_code = CO2_TD(ms_rc_cycle_od_phi_par.m_T_mc_in, mc_dens_in_des, &co2_props);
	double mc_pres_dens_des_od = co2_props.pres;	//[kPa]
	ms_rc_cycle_od_phi_par.m_P_mc_in = mc_pres_dens_des_od;	//[kPa]

	double P_mc_in_guess = -1.23;
	double P_mc_in_upper = -1.23;
	double P_mc_in_lower = -1.23;

	int od_core_error_code_dens = 0;
	if (m_off_design_turbo_operation == E_FIXED_MC_FIXED_RC_FIXED_T)
	{
		double eta_od_core = std::numeric_limits<double>::quiet_NaN();
		od_core_error_code_dens = off_design_core(eta_od_core);
		if (od_core_error_code_dens == 0)
		{	// Have found one feasible compressor inlet pressure
			P_mc_in_guess = ms_rc_cycle_od_phi_par.m_P_mc_in;	//[kPa]

			P_mc_in_upper = P_mc_in_guess;
			// Increase compressor inlet temperature until off design returns error code
			int iter_P_mc_in_upper = 0;
			while (true)
			{
				iter_P_mc_in_upper++;
				P_mc_in_upper = 0.95*P_mc_in_upper + 0.05*ms_des_par.m_P_high_limit;	//[kPa]
				ms_rc_cycle_od_phi_par.m_P_mc_in = P_mc_in_upper;	//[kPa]

				int od_core_error_code = off_design_core(eta_od_core);

				if (od_core_error_code == 0)
				{
					if (iter_P_mc_in_upper > 20 || (ms_des_par.m_P_high_limit - P_mc_in_upper) / ms_des_par.m_P_high_limit < 0.01)
					{
						P_mc_in_upper = ms_des_par.m_P_high_limit;	//[kPa]
					}
				}
				else
				{
					break;
				}				
			}

			// Calculate P_mc_in_lower such that first guess in fmin is P_mc_in_guess
			double r = (3.0 - sqrt(5.0)) / 2.0;		// Gold section ratio
			P_mc_in_lower = (P_mc_in_guess - (r*P_mc_in_upper)) / (1.0 - r);
		}
		else if (od_core_error_code_dens == -14)
		{	// Compressor outlet pressure is too high; decrease compressor inlet pressure

			while (true)
			{
				P_mc_in_upper = ms_rc_cycle_od_phi_par.m_P_mc_in;	//[kPa]
				P_mc_in_guess = 0.98*P_mc_in_upper;					//[kPa]

				ms_rc_cycle_od_phi_par.m_P_mc_in = P_mc_in_guess;	//[kPa]

				int od_core_error_code = off_design_core(eta_od_core);

				if (od_core_error_code == 0)
				{
					break;
				}
				else if (od_core_error_code != -14)
				{	// So we've gone from error = -14 to error = something else, and this is a problem
					// Could try bisecting P_mc_in_upper and P_mc_in_guess, but there's not much room there, given the step size...
					throw(C_csp_exception("Failed to find a feasible guess value for compressor inlet pressure"));
				}
			}
			
			// Calculate P_mc_in_lower such that first guess in fmin is P_mc_in_guess
			double r = (3.0 - sqrt(5.0)) / 2.0;		// Gold section ratio
			P_mc_in_lower = (P_mc_in_guess - (r*P_mc_in_upper)) / (1.0 - r);
		}
		else if (od_core_error_code_dens == 4 || od_core_error_code_dens == 5)
		{	// Inlet state is too dense, resulting in a small recompression fraction, and the recompressor can't reach target pressure

			while (true)
			{
				P_mc_in_upper = ms_rc_cycle_od_phi_par.m_P_mc_in;	//[kPa]
				P_mc_in_guess = 0.98*P_mc_in_upper;					//[kPa]

				ms_rc_cycle_od_phi_par.m_P_mc_in = P_mc_in_guess;	//[kPa]

				int od_core_error_code = off_design_core(eta_od_core);

				if (od_core_error_code == 0)
				{
					break;
				}
				else if (od_core_error_code != 4 && od_core_error_code != 5)
				{	// So we've gone from error = -14 to error = something else, and this is a problem
					// Could try bisecting P_mc_in_upper and P_mc_in_guess, but there's not much room there, given the step size...
					throw(C_csp_exception("Failed to find a feasible guess value for compressor inlet pressure"));
				}
			}

			// Calculate P_mc_in_lower such that first guess in fmin is P_mc_in_guess
			double r = (3.0 - sqrt(5.0)) / 2.0;		// Gold section ratio
			P_mc_in_lower = (P_mc_in_guess - (r*P_mc_in_upper)) / (1.0 - r);
		}
		else
		{
			std::string err_msg = util::format("Off design error code %d current not accounted for", od_core_error_code_dens);
			throw(C_csp_exception(err_msg,""));
		}

	}
	else if ( m_off_design_turbo_operation == E_VFD_MC_VFD_RC_FIXED_T )
	{
		throw(C_csp_exception("Off design operation mode E_VFD_MC_VFD_RC_FIXED_T not supported here"));
	}
	else
	{
		throw(C_csp_exception("Off design operation mode not recognized"));
	}

	// At this point, should have calculated above in this method:
	// * P_mc_in_lower
	// * P_mc_in_guess
	// * P_mc_in_upper

	bool is_P_opt_success = false;
	int opt_iter = 0;
	while (true)
	{
		opt_iter++;

		// Optimize compressor inlet pressure using fmin
		double P_mc_in_opt = fminbr(
			P_mc_in_lower, P_mc_in_upper,
			&fmin_opt_P_mc_in_nest_f_recomp_max_eta, this, m_od_opt_ftol);

		// Now, call off-design with the optimized compressor inlet pressure		
		ms_rc_cycle_od_phi_par.m_P_mc_in = P_mc_in_opt;	//[kPa]
		double eta_od_core_1st = std::numeric_limits<double>::quiet_NaN();
		int od_core_error_code = off_design_core(eta_od_core_1st);
		if (od_core_error_code == 0)
		{
			is_P_opt_success = true;
			if ( (P_mc_in_opt - P_mc_in_lower)/P_mc_in_lower < 0.05 )
			{
				// Need to check for an optimum pressure that is less than P_mc_in_lower...
				double r = (3. - sqrt(5.0)) / 2;       /* Gold section ratio           */

				P_mc_in_guess = P_mc_in_opt;
				P_mc_in_lower = 0.8*P_mc_in_opt;
				P_mc_in_upper = (P_mc_in_guess - (1.0 - r)*P_mc_in_lower) / r;

				continue;

				//throw(C_csp_exception("Don't have code to handle optimal pressure equaling miminum pressure"));
			}
			else if (P_mc_in_opt == P_mc_in_upper && P_mc_in_opt < ms_des_par.m_P_high_limit)
			{
				throw(C_csp_exception("Don't have code to handle optimal pressure equaling maximum pressure"));
			}
			else
			{
				break;
			}
		}
		else
		{
			throw(C_csp_exception("Calculated optimal pressure returned an error"));
		}
	}

	if (m_is_write_mc_out_file)
	{
		mc_P_mc_vary_f_recomp_opt_file.close();
	}

	if (!is_P_opt_success)
	{
		throw(C_csp_exception("Off-design optimization on compressor inlet pressure failed",
			"C_sco2_recomp_csp::opt_P_mc_in_nest_f_recomp_max_eta_core"));
	}

	double eta_max = mc_rc_cycle.get_od_solved()->m_eta_thermal;
	ms_od_solved.ms_rc_cycle_od_solved = *mc_rc_cycle.get_od_solved();
	ms_od_solved.ms_phx_od_solved = mc_phx.ms_od_solved;

	return true;
}

bool C_sco2_recomp_csp::opt_P_mc_in_nest_f_recomp_max_eta_core_old_but_working()
{
	// Prior to calling, need to set :
	//	*ms_od_par, ms_rc_cycle_od_phi_par, ms_phx_od_par, ms_od_op_inputs(will set P_mc_in here and f_recomp downstream)

	// Get density at design point
	double mc_dens_in_des = ms_des_solved.ms_rc_cycle_solved.m_dens[C_RecompCycle::MC_IN];		//[kg/m^3]
	CO2_state co2_props;
	int co2_code = CO2_TD(ms_rc_cycle_od_phi_par.m_T_mc_in, mc_dens_in_des, &co2_props);

	// And calculate the compressor inlet temperature that achieves this pressure at a new ambient temperature
	double P_mc_in_feasible = co2_props.pres;		//[kPa]
	ms_rc_cycle_od_phi_par.m_P_mc_in = P_mc_in_feasible;

	bool is_P_mc_in_feasible = false;
	
	int od_core_error_code_dens = 0;
	if( m_off_design_turbo_operation == E_FIXED_MC_FIXED_RC_FIXED_T )
	{
		double eta_od_core = std::numeric_limits<double>::quiet_NaN();
		od_core_error_code_dens = off_design_core(eta_od_core);
		if( od_core_error_code_dens != 0 )
		{
			is_P_mc_in_feasible = false;
		}
		else
		{
			is_P_mc_in_feasible = true;
		}
	}
	else if( m_off_design_turbo_operation == E_VFD_MC_VFD_RC_FIXED_T )
	{
		is_P_mc_in_feasible = opt_f_recomp_fix_P_mc_in_max_eta_core();
	}


	double eta_max_P_at_dens_des = mc_rc_cycle.get_od_solved()->m_eta_thermal;	//[-]

	double P_mc_in_lower = std::numeric_limits<double>::quiet_NaN();
	double P_mc_in_upper = std::numeric_limits<double>::quiet_NaN();

	double sign_mult = 1.0;
	double mag_mult = 0.05;
	while( !is_P_mc_in_feasible )
	{
		// Try to find an inlet pressure that solves
			// First, try a lower pressure
		double P_mc_in_guess = (1.0 - sign_mult*mag_mult)*P_mc_in_feasible;	//[kPa]
		ms_rc_cycle_od_phi_par.m_P_mc_in = P_mc_in_guess;		//[kPa]
		
		
		bool is_P_mc_in_guess = false;
		int od_core_error_code_iter = 0;

		if( m_off_design_turbo_operation == E_FIXED_MC_FIXED_RC_FIXED_T )
		{
			double eta_od_core = std::numeric_limits<double>::quiet_NaN();
			od_core_error_code_iter = off_design_core(eta_od_core);
			if( od_core_error_code_iter != 0 )
			{
				is_P_mc_in_guess = false;
			}
			else
			{
				is_P_mc_in_guess = true;
			}
		}
		else if( m_off_design_turbo_operation == E_VFD_MC_VFD_RC_FIXED_T )
		{
			is_P_mc_in_guess = opt_f_recomp_fix_P_mc_in_max_eta_core();
		}


		if( is_P_mc_in_guess )
		{
			P_mc_in_feasible = P_mc_in_guess;		//[kPa]
			break;
		}
		else if( mc_rc_cycle.get_od_solved()->m_eta_thermal > eta_max_P_at_dens_des )
		{
			bool is_feasible_found = false;
			while(true)
			{				
				P_mc_in_guess = (1.0 - sign_mult*0.1)*P_mc_in_guess;	//[kPa]
				ms_rc_cycle_od_phi_par.m_P_mc_in = P_mc_in_guess;		//[kPa]
				
				
				bool is_P_mc_in_guess = false;
				if( m_off_design_turbo_operation == E_FIXED_MC_FIXED_RC_FIXED_T )
				{
					double eta_od_core = std::numeric_limits<double>::quiet_NaN();
					int od_core_error_code = off_design_core(eta_od_core);
					if( od_core_error_code != 0 )
					{
						is_P_mc_in_guess = false;
					}
					else
					{
						is_P_mc_in_guess = true;
					}
				}
				else if( m_off_design_turbo_operation == E_VFD_MC_VFD_RC_FIXED_T )
				{
					is_P_mc_in_guess = opt_f_recomp_fix_P_mc_in_max_eta_core();
				}
				
				
				if( is_P_mc_in_guess )
				{
					P_mc_in_feasible = P_mc_in_guess;		//[kPa]
					is_feasible_found = true;
				}
				if( mc_rc_cycle.get_od_solved()->m_eta_thermal == 0.0 )
				{
					break;
				}
			}
			if( is_feasible_found )
			{
				break;
			}
		}
		else if( od_core_error_code_iter != od_core_error_code_dens && mag_mult > 0.002 )
		{
			mag_mult = 0.8*mag_mult;
			continue;
		}
		else if( od_core_error_code_iter == -14 && mag_mult > 0.002 )
		{
			mag_mult *= 1.05;
			continue;
		}		
		if( true )
		{		// If decreasing pressure is not promising, try increasing
			mag_mult = 0.05;
			sign_mult = -1.0;
			
			//throw("C_sco2_recomp_csp::opt_P_mc_in_nest_f_recomp_max_eta_core can't find inlet pressure guess values");	
		}

	}

	// Should have a P_mc_in_feasible that returns a feasible solution
	// Now find bounds for the optimization routine

	P_mc_in_upper = P_mc_in_feasible;
	int iter_P_mc_in_upper = 0;
		
	while(true)
	{
		iter_P_mc_in_upper++;
		P_mc_in_upper = 0.95*P_mc_in_upper + 0.05*ms_des_par.m_P_high_limit;	//[kPa]
		ms_rc_cycle_od_phi_par.m_P_mc_in = P_mc_in_upper;		//[kPa]
		
		
		bool is_P_mc_in_upper = false;
		if( m_off_design_turbo_operation == E_FIXED_MC_FIXED_RC_FIXED_T )
		{
			double eta_od_core = std::numeric_limits<double>::quiet_NaN();
			int od_core_error_code = off_design_core(eta_od_core);
			if( od_core_error_code != 0 )
			{
				is_P_mc_in_upper = false;
			}
			else
			{
				is_P_mc_in_upper = true;
			}
		}
		else if( m_off_design_turbo_operation == E_VFD_MC_VFD_RC_FIXED_T )
		{
			is_P_mc_in_upper = opt_f_recomp_fix_P_mc_in_max_eta_core();
		}
		

		if( !is_P_mc_in_upper )
		{
			break;
		}
		else
		{
			if( eta_max_P_at_dens_des - mc_rc_cycle.get_od_solved()->m_eta_thermal > 0.02 )
			{
				break;
			}
		}
		if( iter_P_mc_in_upper > 20 )
		{
			P_mc_in_upper = ms_des_par.m_P_high_limit;		//[kPa]
		}
	}
		
	P_mc_in_lower = P_mc_in_feasible;
	int iter_P_mc_in_lower = 0;

	while(true)
	{
		iter_P_mc_in_lower++;
		P_mc_in_lower *= 0.95;						//[kPa]
		ms_rc_cycle_od_phi_par.m_P_mc_in = P_mc_in_lower;	//[kPa]
		
		
		bool is_P_mc_in_lower = false;
		if( m_off_design_turbo_operation == E_FIXED_MC_FIXED_RC_FIXED_T )
		{
			double eta_od_core = std::numeric_limits<double>::quiet_NaN();
			int od_core_error_code = off_design_core(eta_od_core);
			if( od_core_error_code != 0 )
			{
				is_P_mc_in_lower = false;
			}
			else
			{
				is_P_mc_in_lower = true;
			}
		}
		else if( m_off_design_turbo_operation == E_VFD_MC_VFD_RC_FIXED_T )
		{
			is_P_mc_in_lower = opt_f_recomp_fix_P_mc_in_max_eta_core();
		}
		
		
		if( !is_P_mc_in_lower )
		{
			break;
		}
		else
		{
			if( eta_max_P_at_dens_des - mc_rc_cycle.get_od_solved()->m_eta_thermal > 0.02 )
			{
				break;
			}
		}
		if( iter_P_mc_in_lower > 20 )
		{
			P_mc_in_lower = 200;	//[kPa]
		}
	}


	if( m_is_write_mc_out_file )
	{
		std::string case_name = util::format("%.2f_", ms_od_par.m_T_amb - 273.15) +
			util::format("%.2f_", ms_od_par.m_m_dot_htf / ms_phx_des_par.m_m_dot_hot_des) +
			util::format("%.2f_", ms_od_par.m_T_htf_hot - 273.15);

		std::string file_name = mstr_base_name + case_name + ".csv";
		mc_P_mc_vary_f_recomp_opt_file.open(file_name.c_str());

		mc_P_mc_vary_f_recomp_opt_file << "T_amb,m_dot_ND,T_htf_hot,P_mc_in,deltaP,P_mc_out,m_dot_mc,m_dot_t,N_mc,phi_mc,mc_tip_ratio,f_recomp,m_dot_rc,rc_phi,rc_tip_ratio,eta_thermal,W_dot_net,Q_dot_in,T_htf_cold,is_error_code\n";
	}


	bool use_nlopt = m_true_nlopt_false_fmin;

	double P_mc_in_opt = std::numeric_limits<double>::quiet_NaN();

	if( use_nlopt )
	{
		std::vector<double> x;
		std::vector<double> lb;
		std::vector<double> ub;
		std::vector<double> scale;

		x.resize(1);
		double P_mc_in_guess = 0.38*P_mc_in_upper + 0.68*P_mc_in_lower;		//[kPa]
		x[0] = P_mc_in_guess;		//[kPa]

		lb.resize(1);
		lb[0] = P_mc_in_lower;		//[kPa]

		ub.resize(1);
		ub[0] = P_mc_in_upper;		//[kPa]

		scale.resize(1);
		scale[0] = (0.5*P_mc_in_upper + 0.5*P_mc_in_lower) - x[0];	//[kPa]

		// Set up instance of nlopt class and set optimization parameters
		nlopt::opt  nlopt_P_mc_in_opt_max_of(nlopt::LN_NELDERMEAD, 1);
		nlopt_P_mc_in_opt_max_of.set_lower_bounds(lb);
		nlopt_P_mc_in_opt_max_of.set_upper_bounds(ub);
		nlopt_P_mc_in_opt_max_of.set_initial_step(scale);
		nlopt_P_mc_in_opt_max_of.set_xtol_rel(m_od_opt_xtol);
		nlopt_P_mc_in_opt_max_of.set_ftol_rel(m_od_opt_ftol);

		// Set max objective function
		nlopt_P_mc_in_opt_max_of.set_max_objective(nlopt_max_opt_P_mc_in_nest_f_recomp, this);

		double nlopt_max_eta = std::numeric_limits<double>::quiet_NaN();
		nlopt::result    nlopt_result = nlopt_P_mc_in_opt_max_of.optimize(x, nlopt_max_eta);

		P_mc_in_opt = x[0];

		if( nlopt_max_eta != nlopt_max_eta )
		{
			P_mc_in_opt = P_mc_in_guess;
		}
	}
	else
	{
		// Optimize compressor inlet pressure
		P_mc_in_opt = fminbr(
			P_mc_in_lower, P_mc_in_upper, &fmin_opt_P_mc_in_nest_f_recomp_max_eta, this, m_od_opt_ftol);
	}


	// Would be nice to have the corresponding optimal recompression fraction
	// ... so that we don't have to re-optimize it at the optimal pressure
	ms_rc_cycle_od_phi_par.m_P_mc_in = P_mc_in_opt;	//[kPa]
	
	bool is_P_opt_success = false;
	if( m_off_design_turbo_operation == E_FIXED_MC_FIXED_RC_FIXED_T )
	{
		bool fmin_rerun = true;
		if( use_nlopt )
			fmin_rerun = false;
		while( true )
		{
			double eta_od_core = std::numeric_limits<double>::quiet_NaN();
			int od_core_error_code = off_design_core(eta_od_core);
			if( od_core_error_code != 0 )
			{
				is_P_opt_success = false;
				if( fmin_rerun )
				{
					double r = (3. - sqrt(5.0)) / 2;       /* Gold section ratio           */

					double prev_1st_guess = P_mc_in_lower + r * (P_mc_in_upper - P_mc_in_lower);

					P_mc_in_upper = prev_1st_guess;

					// Optimize compressor inlet pressure
					P_mc_in_opt = fminbr(
						P_mc_in_lower, P_mc_in_upper, &fmin_opt_P_mc_in_nest_f_recomp_max_eta, this, m_od_opt_ftol);

					ms_rc_cycle_od_phi_par.m_P_mc_in = P_mc_in_opt;	//[kPa]
					continue;
					fmin_rerun = false;
				}
				else
				{
					break;
				}
			}
			else
			{
				is_P_opt_success = true;
				break;
			}
		}
	}
	else if( m_off_design_turbo_operation == E_VFD_MC_VFD_RC_FIXED_T )
	{
		is_P_opt_success = opt_f_recomp_fix_P_mc_in_max_eta_core();
	}

	if( m_is_write_mc_out_file )
	{
		mc_P_mc_vary_f_recomp_opt_file.close();
	}

	if( !is_P_opt_success )
	{
		throw(C_csp_exception("Off-design optimization on compressor inlet pressure failed",
			"C_sco2_recomp_csp::opt_P_mc_in_nest_f_recomp_max_eta_core"));
	}

	double eta_max = mc_rc_cycle.get_od_solved()->m_eta_thermal;
	ms_od_solved.ms_rc_cycle_od_solved = *mc_rc_cycle.get_od_solved();
	ms_od_solved.ms_phx_od_solved = mc_phx.ms_od_solved;

	return true;
}

bool C_sco2_recomp_csp::opt_f_recomp_fix_P_mc_in_max_eta_core()
{
	//Prior to calling, need to set :
	//	*ms_od_par, ms_rc_cycle_od_phi_par (will set f_recomp here), ms_phx_od_par

	if( m_is_write_mc_out_file )
	{
		std::string case_name = util::format("%.2f_", ms_od_par.m_T_amb - 273.15) +
			util::format("%.2f_", ms_od_par.m_m_dot_htf / ms_phx_des_par.m_m_dot_hot_des) +
			util::format("%.2f_", ms_od_par.m_T_htf_hot - 273.15) +
			util::format("%.1f", ms_rc_cycle_od_phi_par.m_P_mc_in);

		std::string file_name = mstr_base_name + case_name + ".csv";
		mc_P_mc_in_fixed_f_recomp_vary_file.open(file_name.c_str());
	}

	// Estimate the turbine inlet temperature for the turbomachinery balance
	double T_t_in_est = ms_od_par.m_T_htf_hot - ms_des_par.m_phx_dt_hot_approach;	//[K]

	int mc_error_code, rc_error_code;
	double mc_w_tip_ratio, P_mc_out, rc_w_tip_ratio, rc_phi;

	double f_recomp_step = 0.01;

	bool is_f_recomp_min_found = false;
	double f_recomp_min = std::numeric_limits<double>::quiet_NaN();

	bool is_f_recomp_max_found = false;
	double f_recomp_max = std::numeric_limits<double>::quiet_NaN();

	if(m_is_write_mc_out_file && !m_is_only_write_frecomp_opt_iters)
	{
		mc_P_mc_in_fixed_f_recomp_vary_file << "P_mc_in,deltaP,P_mc_out,m_dot_mc,m_dot_t,N_mc,mc_tip_ratio,f_recomp,m_dot_rc,rc_error_code,rc_phi,rc_tip_ratio\n";
		mc_P_mc_in_fixed_f_recomp_vary_file << "kPa,kPa,kPa,kg/s,kg/s,rpm,-,-,kg/s,-,-,-\n";
	}

	double f_recomp_guess = 0.0;

	for( f_recomp_guess = 0.01; f_recomp_guess < 1.0; f_recomp_guess = f_recomp_guess + f_recomp_step )
	{
		mc_error_code = rc_error_code = 0;
		mc_w_tip_ratio = P_mc_out = rc_w_tip_ratio = rc_phi = std::numeric_limits<double>::quiet_NaN();

		mc_rc_cycle.estimate_od_turbo_operation(ms_rc_cycle_od_phi_par.m_T_mc_in, ms_rc_cycle_od_phi_par.m_P_mc_in, f_recomp_guess, T_t_in_est, ms_rc_cycle_od_phi_par.m_phi_mc,
			mc_error_code, mc_w_tip_ratio, P_mc_out,
			rc_error_code, rc_w_tip_ratio, rc_phi,
			m_is_write_mc_out_file);

		if(m_is_write_mc_out_file && !m_is_only_write_frecomp_opt_iters)
		{
			double deltaP = 0.0;
			double P_mc_out_of = 0.0;
			double m_dot_mc = 0.0;
			double m_dot_t = 0.0;
			double N_mc = 0.0;
			double mc_tip_ratio_of = 0.0;
			double f_recomp_of = 0.0;
			double m_dot_rc = 0.0;
			double rc_phi_of = 0.0;
			double rc_tip_ratio_of = 0.0; 

			if(mc_error_code == 0)
			{
				deltaP = P_mc_out - ms_rc_cycle_od_phi_par.m_P_mc_in;
				P_mc_out_of = P_mc_out;
				m_dot_mc = mc_rc_cycle.get_od_solved()->m_m_dot_mc;
				m_dot_t = mc_rc_cycle.get_od_solved()->m_m_dot_t;
				m_dot_rc = mc_rc_cycle.get_od_solved()->m_m_dot_rc;
				N_mc = mc_rc_cycle.get_od_solved()->ms_mc_od_solved.m_N;
				mc_tip_ratio_of = mc_w_tip_ratio;
				f_recomp_of = f_recomp_guess;

				if(rc_error_code == 0)
				{
					rc_phi_of = rc_phi;
					rc_tip_ratio_of = rc_w_tip_ratio;
				}
			}

			mc_P_mc_in_fixed_f_recomp_vary_file << ms_rc_cycle_od_phi_par.m_P_mc_in << ","
						<< deltaP << ","
						<< P_mc_out_of << ","
						<< m_dot_mc << ","
						<< m_dot_t << ","
						<< N_mc << ","
						<< mc_tip_ratio_of << ","
						<< f_recomp_of << ","
						<< m_dot_rc << ","
						<< rc_error_code << ","
						<< rc_phi_of << ","
						<< rc_tip_ratio_of << "\n";
		}

		if( !is_f_recomp_min_found )
		{	// Looking for smallest recompression fraction where the turbomachinery balance succeeds
			// Okay to be over pressure, too fast tip speed, etc.
			if( rc_error_code == 0 && mc_error_code == 0 )
			{
				f_recomp_min = f_recomp_guess;
				is_f_recomp_min_found = true;

				if( mc_w_tip_ratio > 1.1 || P_mc_out > 1.1*ms_des_par.m_P_high_limit )
				{	// At small recompression fractions, already exceeding tip speed and over pressure conditions
					// So don't try to find f_recomp_max
					ms_rc_cycle_od_phi_par.m_recomp_frac = f_recomp_guess;	//[-]

					mc_P_mc_in_fixed_f_recomp_vary_file << "T_amb,m_dot_ND,T_htf_hot,P_mc_in,deltaP,P_mc_out,m_dot_mc,m_dot_t,N_mc,mc_tip_ratio,f_recomp,m_dot_rc,rc_phi,rc_tip_ratio,eta_thermal,W_dot_net,Q_dot_in,T_htf_cold,is_error_code\n";
					
					double eta_f_recomp_local = opt_f_recomp_max_eta(f_recomp_guess);

					ms_od_solved.ms_rc_cycle_od_solved = *mc_rc_cycle.get_od_solved();
					ms_od_solved.ms_phx_od_solved = mc_phx.ms_od_solved;

					if( eta_f_recomp_local != eta_f_recomp_local )
					{
						eta_f_recomp_local = 0.0;
					}

					if( m_is_write_mc_out_file && !m_is_only_write_frecomp_opt_iters )
					{
						mc_P_mc_in_fixed_f_recomp_vary_file << "f_recomp_opt,eta_max\n";
						mc_P_mc_in_fixed_f_recomp_vary_file << f_recomp_guess << "," << eta_f_recomp_local << "\n";
					}

					if( m_is_write_mc_out_file )
					{
						mc_P_mc_in_fixed_f_recomp_vary_file.close();
					}

					if( eta_f_recomp_local > 0.0 )
						return true;
					else
						return false;
				}
			}
			else
			{
				continue;
			}
		}
		else
		{	// Looking for the smallest recompression fraction where either:
			// * method fails (mc or rc error code)
			// * high side pressure exceeds limit
			// * mc tip ratio exceeds 1.05 (build in some tolerance)
			// (... so basically it's okay here if the RC tip speed is too fast)
			double P_high_mult_overshoot = 1.25;
			if(m_is_write_mc_out_file && !m_is_only_write_frecomp_opt_iters)
				P_high_mult_overshoot = 1.5;

			if( rc_error_code == 0 && mc_error_code == 0
				&& mc_w_tip_ratio < 1.05 && P_mc_out < P_high_mult_overshoot*ms_des_par.m_P_high_limit )
			{
				continue;
			}
			else
			{
				f_recomp_max = f_recomp_guess;
				is_f_recomp_max_found = true;
				break;
			}
		}	// End if/then of recompresion max/min logic	

	}	// End loop on recompression fraction

	// Check that max and min recompression fractions found
	if( !is_f_recomp_min_found || !is_f_recomp_max_found )
	{
		// Can't find a solution at this compressor inlet pressure
		throw(C_csp_exception("Estimates of off-design turbomachinery balance can't find a workable recompression fraction",
			"C_sco2_recomp_csp::off_design_nested_opt"));
	}

	if(m_is_write_mc_out_file && !m_is_only_write_frecomp_opt_iters)
	{
		mc_P_mc_in_fixed_f_recomp_vary_file << "f_recomp_iter_min,f_recomp_iter_max\n";
		mc_P_mc_in_fixed_f_recomp_vary_file << f_recomp_min << "," << f_recomp_max << "\n";
		mc_P_mc_in_fixed_f_recomp_vary_file << "T_amb,m_dot_ND,T_htf_hot,P_mc_in,deltaP,P_mc_out,m_dot_mc,m_dot_t,N_mc,mc_tip_ratio,f_recomp,m_dot_rc,rc_phi,rc_tip_ratio,eta_thermal,W_dot_net,Q_dot_in,T_htf_cold,is_error_code\n";
		mc_P_mc_in_fixed_f_recomp_vary_file << "kPa,kPa,kPa,kg/s,kg/s,rpm,-,-,kg/s,-,-,-,-\n";
	}
	else if(m_is_write_mc_out_file)
	{
		mc_P_mc_in_fixed_f_recomp_vary_file << "T_amb,m_dot_ND,T_htf_hot,P_mc_in,deltaP,P_mc_out,m_dot_mc,m_dot_t,N_mc,mc_tip_ratio,f_recomp,m_dot_rc,rc_phi,rc_tip_ratio,eta_thermal,W_dot_net,Q_dot_in,T_htf_cold,is_error_code\n";
	}

	// Set up recompression fraction optimization (at constant mc inlet pressure) options
	bool use_nlopt = m_true_nlopt_false_fmin;
	double f_recomp_opt = std::numeric_limits<double>::quiet_NaN();

	if( use_nlopt )
	{
		std::vector<double> x;
		std::vector<double> lb;
		std::vector<double> ub;
		std::vector<double> scale;

		x.resize(1);
		double nlopt_f_recomp_guess = 0.75*f_recomp_max + 0.25*f_recomp_min;	//[-]
		x[0] = nlopt_f_recomp_guess;	//[-]

		lb.resize(1);
		lb[0] = f_recomp_min;		//[-]

		ub.resize(1);
		ub[0] = f_recomp_max;		//[-]

		scale.resize(1);
		scale[0] = (0.65*f_recomp_max + 0.35*f_recomp_min) - x[0];

		// Set up instance of nlopt class and set optimization parameters
		nlopt::opt          f_recomp_opt_max_eta(nlopt::LN_NELDERMEAD, 1);
		f_recomp_opt_max_eta.set_lower_bounds(lb);
		f_recomp_opt_max_eta.set_upper_bounds(ub);
		f_recomp_opt_max_eta.set_initial_step(scale);
		f_recomp_opt_max_eta.set_xtol_rel(m_od_opt_xtol);
		f_recomp_opt_max_eta.set_ftol_rel(m_od_opt_ftol);

		// Set max objective function
		f_recomp_opt_max_eta.set_max_objective(nlopt_max_f_recomp_cycle_eta, this);

		double nlopt_max_eta = std::numeric_limits<double>::quiet_NaN();
		nlopt::result       nlopt_result = f_recomp_opt_max_eta.optimize(x, nlopt_max_eta);

		f_recomp_opt = x[0];

		if( nlopt_max_eta != nlopt_max_eta )
		{
			f_recomp_opt = nlopt_f_recomp_guess;
		}

	}
	else
	{
		f_recomp_opt = fminbr(
			f_recomp_min, f_recomp_max, &fmin_f_recomp_cycle_eta, this, m_od_opt_ftol);
	}

	// Call final time with optimized recompression fraction
	ms_rc_cycle_od_phi_par.m_recomp_frac = f_recomp_opt;

	double eta_f_recomp_max = std::numeric_limits<double>::quiet_NaN();
	int sco2_od_code = off_design_core(eta_f_recomp_max);

	if(eta_f_recomp_max != eta_f_recomp_max)
	{
		eta_f_recomp_max = 0.0;
	}

	if( m_is_write_mc_out_file && !m_is_only_write_frecomp_opt_iters )
	{
		mc_P_mc_in_fixed_f_recomp_vary_file << "f_recomp_opt,eta_max\n";
		mc_P_mc_in_fixed_f_recomp_vary_file << f_recomp_opt << "," << eta_f_recomp_max << "\n";
	}

	if( m_is_write_mc_out_file )
	{
		mc_P_mc_in_fixed_f_recomp_vary_file.close();
	}

	if( sco2_od_code != 0 )
		return false;

	ms_od_solved.ms_rc_cycle_od_solved = *mc_rc_cycle.get_od_solved();
	ms_od_solved.ms_phx_od_solved = mc_phx.ms_od_solved;

	return true;
}

int C_sco2_recomp_csp::off_design_opt(S_od_par od_par, int off_design_strategy, double od_opt_tol)
{
	ms_od_par = od_par;

	// Optimization variables:
	m_od_opt_objective = off_design_strategy;

	if( m_od_opt_objective == E_MAX_ETA_FIX_PHI || 
		m_od_opt_objective == E_MAX_POWER_FIX_PHI || 
		m_od_opt_objective == E_MOO_ETA_0p1Wnd_FIX_PHI ||
		m_od_opt_objective == E_MOO_ETA_T_T_IN_FIX_PHI )
	{
		m_is_phi_optimized = false;
	}
	else
	{
		m_is_phi_optimized = true;
	}
	
	//m_od_opt_ftol = od_opt_tol;
	// ****************************************

	// Define ms_rc_cycle_od_par
		// Defined now
	ms_rc_cycle_od_phi_par.m_T_mc_in = ms_od_par.m_T_amb + ms_des_par.m_dt_mc_approach;		//[K]
	if( ms_rc_cycle_od_phi_par.m_T_mc_in < m_T_mc_in_min )
	{
		std::string msg = util::format("The off-design main compressor inlet temperature is %lg [C]."
		" The sCO2 cycle off-design code reset it to the minimum allowable main compressor inlet temperature: %lg [C].",
		ms_rc_cycle_od_phi_par.m_T_mc_in - 273.15,
		m_T_mc_in_min - 273.15);
		ms_rc_cycle_od_phi_par.m_T_mc_in = m_T_mc_in_min;
	}		

	ms_rc_cycle_od_phi_par.m_N_sub_hxrs = ms_des_par.m_N_sub_hxrs;			//[-]
	ms_rc_cycle_od_phi_par.m_tol = ms_des_par.m_tol;						//[-]
	ms_rc_cycle_od_phi_par.m_N_t = ms_des_solved.ms_rc_cycle_solved.ms_t_des_solved.m_N_design;	//[rpm]
		// Defined downstream
	ms_rc_cycle_od_phi_par.m_T_t_in = std::numeric_limits<double>::quiet_NaN();			//[K]			
	ms_rc_cycle_od_phi_par.m_P_mc_in = std::numeric_limits<double>::quiet_NaN();		//[kPa]
	ms_rc_cycle_od_phi_par.m_recomp_frac = std::numeric_limits<double>::quiet_NaN();	//[-]
	ms_rc_cycle_od_phi_par.m_phi_mc = std::numeric_limits<double>::quiet_NaN();			//[-]
	// Define ms_phx_od_par
		// Defined now
	ms_phx_od_par.m_T_h_in = ms_od_par.m_T_htf_hot;			//[K]
	ms_phx_od_par.m_P_h_in = ms_phx_des_par.m_P_h_in;		//[kPa] Assuming fluid is incompressible in that pressure doesn't affect its properties
	ms_phx_od_par.m_m_dot_h = ms_od_par.m_m_dot_htf;		//[kg/s]
		// Defined downstream
	ms_phx_od_par.m_T_c_in = std::numeric_limits<double>::quiet_NaN();		//[K]
	ms_phx_od_par.m_P_c_in = std::numeric_limits<double>::quiet_NaN();		//[kPa]
	ms_phx_od_par.m_m_dot_c = std::numeric_limits<double>::quiet_NaN();		//[kg/s]

	int opt_eta_code = od_fix_T_mc__nl_opt_shell__opt_eta();

	ms_od_solved.ms_rc_cycle_od_solved = *mc_rc_cycle.get_od_solved();
	ms_od_solved.ms_phx_od_solved = mc_phx.ms_od_solved;

	return opt_eta_code;
}

int C_sco2_recomp_csp::find_a_feasible_off_design_solution(S_od_par od_par, double T_mc_in /*K*/,
	S_od_operation_inputs & od_op_inputs)
{
	// Set-up off-design operation inputs
	//C_sco2_recomp_csp::S_od_operation_inputs sco2_rc_od_op_par;
	
	// Get density at design point
	double mc_dens_in_des = ms_des_solved.ms_rc_cycle_solved.m_dens[C_RecompCycle::MC_IN];		//[kg/m^3]
	CO2_state co2_props;
	int co2_code = CO2_TD(T_mc_in, mc_dens_in_des, &co2_props);

	// And calculate the compressor inlet temperature that achieves this pressure at a new ambient temperature
	double P_mc_in_dens_des = co2_props.pres;		//[kPa]
	od_op_inputs.m_P_mc_in = P_mc_in_dens_des;

	if( ms_des_solved.ms_rc_cycle_solved.m_is_rc )
	{	// Recompression Fraction
		od_op_inputs.m_recomp_frac = ms_des_solved.ms_rc_cycle_solved.m_recomp_frac;	//[-]
	}
	od_op_inputs.m_phi_mc = ms_des_solved.ms_rc_cycle_solved.ms_mc_des_solved.m_phi_des;	//[-]

	int sco2_od_code = off_design(od_par, od_op_inputs);

	while(sco2_od_code != 0)
	{		
		for(int i = 0; i < 6; i++)
		{
			od_op_inputs.m_P_mc_in = od_op_inputs.m_P_mc_in*0.95;
		
			for(int j = 0; j < 6; j++)
			{
				od_op_inputs.m_recomp_frac = ms_des_solved.ms_rc_cycle_solved.m_recomp_frac*(1.0 - 0.5*(j/5.0));
			
				sco2_od_code = off_design(od_par, od_op_inputs);

				if(sco2_od_code == 0)
					break;
			}
			
			if( sco2_od_code == 0 )
				break;
		}

		if(sco2_od_code != 0)
		{
			std::string err_msg = util::format("Can't find a feasible solution at T_htf = %lg [C], "
				"m_dot_htf_ND = %lg [-], "
				"and T_amb = %lg [C]",
				od_par.m_T_htf_hot - 273.15,
				od_par.m_m_dot_htf / ms_phx_des_par.m_m_dot_hot_des,
				od_par.m_T_amb - 273.15);

			throw(C_csp_exception(err_msg,"Find feasible solution"));
		}
	}

	return sco2_od_code;
}

void C_sco2_recomp_csp::reset_S_od_opt_eta_tracking()
{
	ms_od_opt_eta_tracking.m_is_opt_found = false;

	ms_od_opt_eta_tracking.m_eta_max = 0.0;

	ms_od_opt_eta_tracking.ms_od_op_in_max.m_P_mc_in = 
		ms_od_opt_eta_tracking.ms_od_op_in_max.m_recomp_frac = 
		ms_od_opt_eta_tracking.ms_od_op_in_max.m_phi_mc = 
		ms_od_opt_eta_tracking.m_over_T_t_in_at_eta_max = 
		ms_od_opt_eta_tracking.m_over_P_high_at_eta_max = std::numeric_limits<double>::quiet_NaN();
}

double C_sco2_recomp_csp::adjust_P_mc_in_away_2phase(double T_co2 /*K*/, double P_mc_in /*kPa*/)
{	
	double P_mc_in_restricted = std::numeric_limits<double>::quiet_NaN();	//[kPa]
	CO2_state co2_props;
	// Is T_co2 < the critical temperature
	if (T_co2 < m_T_co2_crit)
	{
		CO2_TQ(T_co2, 0.0, &co2_props);
		P_mc_in_restricted = co2_props.pres;	//[kPa]
	}
	else if (T_co2 < 1.001*m_T_co2_crit)
	{	// Else, T_co2 > the critical temperature
		P_mc_in_restricted = m_P_co2_crit;		//[kPa]
	}
	else
	{
		return P_mc_in;	//[kPa]
	}
	
	if (P_mc_in >= P_mc_in_restricted)
	{
		double P_upper = 1.01*P_mc_in_restricted;
		if (P_mc_in < P_upper)
		{
			double P_mid = 1.005*P_mc_in_restricted;
			return P_upper - (P_upper - P_mc_in)/(P_upper - P_mc_in_restricted)*(P_upper-P_mid);
		}
		else
		{
			return P_mc_in;		//[kPa]
		}
	}
	else
	{
		double P_lower = 0.99*P_mc_in_restricted;
		double P_mid = 0.995*P_mc_in_restricted;
		if (P_mc_in > P_lower)
		{
			return P_lower + (P_mc_in - P_lower)/(P_mc_in_restricted - P_lower)*(P_mc_in-P_lower);
		}
		else
		{
			return P_mc_in;		//[kPa]
		}
	}

}

int C_sco2_recomp_csp::off_design_core(double & eta_solved)
{
	ms_rc_cycle_od_phi_par.m_P_mc_in = adjust_P_mc_in_away_2phase(ms_rc_cycle_od_phi_par.m_T_mc_in, ms_rc_cycle_od_phi_par.m_P_mc_in);

	// Apply 1 var solver to find the turbine inlet temperature that results in a "converged" PHX
	C_mono_eq_T_t_in c_phx_cycle(this);
	C_monotonic_eq_solver c_phx_cycle_solver(c_phx_cycle);

	// Set upper and lower bounds
	double T_t_upper = ms_phx_od_par.m_T_h_in;		//[K] Upper CO2 limit is HTF hot temperature
	double T_t_lower = 373.15;						//[K] Lower CO2 limit is something fairly low, I guess

	// Generate guess values
	double T_t_guess_upper = ms_phx_od_par.m_T_h_in - ms_des_par.m_phx_dt_hot_approach;	//[K] One reasonable guess might be to apply the design approach
	double T_t_guess_lower = T_t_guess_upper - 20.0;		//[K] This might be another reasonable guess...

	// Set solver settings
	// Because this application of solver is trying to get outlet to match guess, need to calculate error in function
	// So it's already relative, and solver is looking at an absolute value
	c_phx_cycle_solver.settings(ms_des_par.m_tol/10.0, 50, T_t_lower, T_t_upper, false);

	// Now, solve for the turbine inlet temperature
	double T_t_solved, tol_solved;
	T_t_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_solved = -1;

	int phx_cycle_code = 0;
	try
	{
		phx_cycle_code = c_phx_cycle_solver.solve(T_t_guess_lower, T_t_guess_upper, 0.0, T_t_solved, tol_solved, iter_solved);
	}
	catch( C_csp_exception )
	{
		eta_solved = 0.0;
		ms_od_solved.m_od_error_code = -1;
		return ms_od_solved.m_od_error_code;
	}

	if( phx_cycle_code != C_monotonic_eq_solver::CONVERGED )
	{
		int n_call_history = c_phx_cycle_solver.get_solver_call_history()->size();

		eta_solved = 0.0;
		
		int nested_error_code = (*(c_phx_cycle_solver.get_solver_call_history()))[n_call_history - 1].err_code;

		if(nested_error_code == 0)
		{
			nested_error_code = phx_cycle_code;
		}

		ms_od_solved.m_od_error_code = nested_error_code;
		return nested_error_code;
	}

	// Now, need to filter results that exceed temperature/pressure/other limitations
	// 1) Don't let the turbine inlet temperature exceed the design inlet temperature
	double over_T_t_in = 0.0;
	// double over_T_t_in = max(0.0, T_t_solved - mc_rc_cycle.get_design_solved()->m_temp[6-1]);

	// 2) Don't let the upper pressure in the system exceed the specified max (typically also = design point P_high)
	double over_P_high = max(0.0, (mc_rc_cycle.get_od_solved()->m_pres[C_RecompCycle::MC_OUT] - 0.9999*ms_des_par.m_P_high_limit) / 1.E3);

	// 3) Check compressor(s) tip ratio?
	double mc_w_tip_ratio = mc_rc_cycle.get_od_solved()->ms_mc_od_solved.m_w_tip_ratio;
	// Recompressor has multiple stages, it's reporting the fastest tip speed
	double rc_w_tip_ratio = 0.0;
	if( ms_des_solved.ms_rc_cycle_solved.m_is_rc )
	{
		rc_w_tip_ratio = mc_rc_cycle.get_od_solved()->ms_rc_od_solved.m_w_tip_ratio;
	}
	double comp_tip_ratio = max(mc_w_tip_ratio, rc_w_tip_ratio);
	double over_tip_ratio = max(0.0, 10.0*(comp_tip_ratio - 0.999));

	// 4) Check for compressor(s) surge?
	// Main compressor
	double mc_phi = mc_rc_cycle.get_od_solved()->ms_mc_od_solved.m_phi;
	double over_surge_mc = max(0.0, C_compressor::m_snl_phi_min - mc_phi);
	// Recompressor
	double rc_phi_s1, rc_phi_s2;
	rc_phi_s1 = rc_phi_s2 = 0.0;
	double over_surge_rc = 0.0;
	if( ms_des_solved.ms_rc_cycle_solved.m_is_rc )
	{
		rc_phi_s1 = mc_rc_cycle.get_od_solved()->ms_rc_od_solved.m_phi;
		rc_phi_s2 = mc_rc_cycle.get_od_solved()->ms_rc_od_solved.m_phi_2;
		double rc_phi_min = min(rc_phi_s1, rc_phi_s2);
		over_surge_rc = max(0.0, (C_recompressor::m_snl_phi_min - rc_phi_min)/C_recompressor::m_snl_phi_min*100.0);
	}


	// 5) Constrain HTF temperature difference?

	// Want thermal efficiency gradient, not step change, as turbine inlet temperature exceeds design
	// ... to help the solver
	double eta_T_t_in_scale = exp(-over_T_t_in);
	double eta_P_high_scale = exp(-over_P_high);
	double eta_tip_ratio_scale = exp(-over_tip_ratio);
	double eta_surge_mc_scale = exp(-over_surge_mc);
	double eta_surge_rc_scale = exp(-over_surge_rc);

	int od_solve_code = 0;

	// If a problem with the solved operation
	//  then overwrite integer that code returns to calling program
	if(over_T_t_in != 0.0)
		od_solve_code = E_TURBINE_INLET_OVER_TEMP;
	else if( mc_rc_cycle.get_od_solved()->m_pres[C_RecompCycle::MC_OUT] > ms_des_par.m_P_high_limit )
		od_solve_code = E_OVER_PRESSURE;
	else if (over_tip_ratio >= 1.0)
		od_solve_code = E_TIP_RATIO;
	else if(over_surge_mc != 0.0)
		od_solve_code = E_MC_SURGE;
	else if(over_surge_rc != 0.0)
		od_solve_code = E_RC_SURGE;

	double scale_product = eta_T_t_in_scale*
		eta_P_high_scale*
		eta_tip_ratio_scale*
		eta_surge_mc_scale*
		eta_surge_rc_scale;

	switch( m_od_opt_objective )
	{
	case E_MAX_ETA:
	case E_MAX_ETA_FIX_PHI:
		eta_solved = mc_rc_cycle.get_od_solved()->m_eta_thermal*scale_product;
		break;
	case E_MAX_POWER:
	case E_MAX_POWER_FIX_PHI:
		eta_solved = mc_rc_cycle.get_od_solved()->m_W_dot_net / 1.E3*scale_product;		//[MWe]
		break;
	case E_MOO_ETA_0p1Wnd:
	case E_MOO_ETA_0p1Wnd_FIX_PHI:
		eta_solved = (mc_rc_cycle.get_od_solved()->m_eta_thermal + 0.05*mc_rc_cycle.get_od_solved()->m_W_dot_net/mc_rc_cycle.get_design_solved()->m_W_dot_net)*scale_product;
		break;
	case E_MOO_ETA_T_T_IN:
	case E_MOO_ETA_T_T_IN_FIX_PHI:
		eta_solved = (mc_rc_cycle.get_od_solved()->m_eta_thermal - 0.01*max(0.0, mc_rc_cycle.get_od_solved()->m_temp[C_RecompCycle::TURB_IN] - mc_rc_cycle.get_design_solved()->m_temp[C_RecompCycle::TURB_IN]))*scale_product;
		break;
	case E_MAX_POWER_IN_ETA_MAX_BAND:
	{
		double eta_thermal = mc_rc_cycle.get_od_solved()->m_eta_thermal;					//[-]
		double under_eta_max = max(0.0, (0.99*m_eta_max_eta - eta_thermal)*1.E3);			//[-]
		eta_solved = mc_rc_cycle.get_od_solved()->m_W_dot_net/1.E3*scale_product*exp(-under_eta_max);	//[MWe]
	}
	case E_TARGET_POWER_ETA_MAX:
	{
		double W_dot_target = (ms_od_par.m_m_dot_htf / ms_phx_des_par.m_m_dot_hot_des) * ms_des_par.m_W_dot_net;	//[kWe]
		double W_dot_nd = min(1.0, fabs((mc_rc_cycle.get_od_solved()->m_W_dot_net - W_dot_target) / W_dot_target));	//[-]
		if (W_dot_nd > 0.001)
		{
			eta_solved = (1.0 - W_dot_nd)*scale_product;
		}
		else
		{
			eta_solved = (1.0 - W_dot_nd + exp(-W_dot_nd)*mc_rc_cycle.get_od_solved()->m_eta_thermal*1.E-3)*scale_product;
		}
		eta_solved = eta_solved*1.E3;
	}
		break;

	default:
		std::string err_msg = util::format("The off-design optimization objective code, %d, is not recognized.", m_od_opt_objective);
		throw(C_csp_exception(err_msg, "C_sco2_recomp_csp::off_design_core"));
		
	}


	// Weight delta_T_HTF with power cycle efficiency
	//double eta_mult = 1.E3;
	//double eta_solved = scale_product*(eta_mult*mc_rc_cycle.get_od_solved()->m_eta_thermal + 20.0) - std::fabs(mc_phx.ms_od_solved.m_T_h_out - mc_phx.ms_des_solved.m_T_h_out);

	// Minimize deltaT (need 1000.0 in here for optimization to work correctly (compared to 100, at least)
	//double eta_solved = scale_product*(1000.0 - std::fabs(mc_phx.ms_od_solved.m_T_h_out - mc_phx.ms_des_solved.m_T_h_out));

	// Weight delta_T_HTF with power cycle output
	//double work_mult = 1000.0;
	//double eta_solved = scale_product*(work_mult*mc_rc_cycle.get_od_solved()->m_W_dot_net/mc_rc_cycle.get_design_solved()->m_W_dot_net + 20.0) - std::fabs(mc_phx.ms_od_solved.m_T_h_out - mc_phx.ms_des_solved.m_T_h_out);

	// BUT, need to inform upstream code that a solution in this gradient is not acceptable
	//if( over_T_t_in == 0.0 && over_P_high == 0.0 && over_tip_ratio == 0.0 && over_surge_mc == 0.0 && over_surge_rc == 0.0 )
	//{
	//	//sco2_od_opt_file << mc_rc_cycle.get_od_solved()->m_pres[C_RecompCycle::MC_IN]/1.E3 << ","
	//	//				<< mc_rc_cycle.get_od_solved()->m_recomp_frac << ","
	//	//				<< mc_rc_cycle.get_od_solved()->ms_mc_od_solved.m_phi << ","
	//	//				<< mc_rc_cycle.get_od_solved()->m_eta_thermal << "," 
	//	//				<< mc_rc_cycle.get_od_solved()->m_W_dot_net/1.E3 << "\n";
	//
	//	ms_od_opt_eta_tracking.m_is_opt_found = true;
	//}

	if(eta_solved != eta_solved)
	{
		eta_solved = 0.0;
	}

	ms_od_solved.m_od_error_code = od_solve_code;

	// Want to make an efficiency value available to the optimization although it may be decreased by system operation constraints
	if( !(od_solve_code == 0 || od_solve_code == E_TURBINE_INLET_OVER_TEMP || od_solve_code == E_OVER_PRESSURE ||
		od_solve_code == E_TIP_RATIO || od_solve_code == E_MC_SURGE || od_solve_code == E_RC_SURGE) )
		return 0.0;

	if( eta_solved > ms_od_opt_eta_tracking.m_eta_max )
	{
		if( scale_product < 1.0 )
		{	// Scaled eta could still be the current max that NLOPT sees, so need to communicate upstream that it's breaking limits
			ms_od_opt_eta_tracking.m_is_opt_found = false;
		}
		else
		{
			ms_od_opt_eta_tracking.m_is_opt_found = true;
			ms_od_opt_eta_tracking.m_eta_max = eta_solved;
			ms_od_opt_eta_tracking.ms_od_op_in_max.m_P_mc_in = ms_rc_cycle_od_phi_par.m_P_mc_in;
			ms_od_opt_eta_tracking.ms_od_op_in_max.m_recomp_frac = ms_rc_cycle_od_phi_par.m_recomp_frac;
			ms_od_opt_eta_tracking.ms_od_op_in_max.m_phi_mc = ms_rc_cycle_od_phi_par.m_phi_mc;
		}
		
		ms_od_opt_eta_tracking.m_over_T_t_in_at_eta_max = over_T_t_in;
		ms_od_opt_eta_tracking.m_over_P_high_at_eta_max = over_P_high;
	}

	return od_solve_code;
}

int C_sco2_recomp_csp::off_design(S_od_par od_par, S_od_operation_inputs od_op_inputs)
{
	ms_od_par = od_par;

	// Define ms_rc_cycle_od_par
		// Defined here
	ms_rc_cycle_od_phi_par.m_T_mc_in = ms_od_par.m_T_amb + ms_des_par.m_dt_mc_approach;		//[K]
	if( ms_rc_cycle_od_phi_par.m_T_mc_in < m_T_mc_in_min )
	{
		std::string msg = util::format("The off-design main compressor inlet temperature is %lg [C]."
			" The sCO2 cycle off-design code reset it to the minimum allowable main compressor inlet temperature: %lg [C].",
			ms_rc_cycle_od_phi_par.m_T_mc_in - 273.15,
			m_T_mc_in_min - 273.15);
		ms_rc_cycle_od_phi_par.m_T_mc_in = m_T_mc_in_min;
	}
	ms_rc_cycle_od_phi_par.m_N_sub_hxrs = ms_des_par.m_N_sub_hxrs;			//[-]
	ms_rc_cycle_od_phi_par.m_tol = ms_des_par.m_tol;						//[-]
	ms_rc_cycle_od_phi_par.m_N_t = ms_des_solved.ms_rc_cycle_solved.ms_t_des_solved.m_N_design;	//[rpm]
		// Operational Inputs
	ms_rc_cycle_od_phi_par.m_P_mc_in = od_op_inputs.m_P_mc_in;			//[kPa]
	ms_rc_cycle_od_phi_par.m_recomp_frac = od_op_inputs.m_recomp_frac;	//[-]
	ms_rc_cycle_od_phi_par.m_phi_mc = od_op_inputs.m_phi_mc;			//[-]
		// Defined downstream
	ms_rc_cycle_od_phi_par.m_T_t_in = std::numeric_limits<double>::quiet_NaN();			//[K]
	
	// Define ms_phx_od_par
		// Defined here
	ms_phx_od_par.m_T_h_in = ms_od_par.m_T_htf_hot;			//[K]
	ms_phx_od_par.m_P_h_in = ms_phx_des_par.m_P_h_in;		//[kPa] Assuming fluid is incompressible in that pressure doesn't affect its properties
	ms_phx_od_par.m_m_dot_h = ms_od_par.m_m_dot_htf;		//[kg/s]
		// Defined downstream
	ms_phx_od_par.m_T_c_in = std::numeric_limits<double>::quiet_NaN();		//[K]
	ms_phx_od_par.m_P_c_in = std::numeric_limits<double>::quiet_NaN();		//[kPa]
	ms_phx_od_par.m_m_dot_c = std::numeric_limits<double>::quiet_NaN();		//[kg/s]

	double eta_solved = std::numeric_limits<double>::quiet_NaN();

	// Don't care about the objective for a single off design call, but need to set it to something...
	int od_code = off_design_core(eta_solved);
	
	ms_od_solved.ms_rc_cycle_od_solved = *mc_rc_cycle.get_od_solved();
	ms_od_solved.ms_phx_od_solved = mc_phx.ms_od_solved;

	return od_code;
}

int C_sco2_recomp_csp::od_fix_T_mc__nl_opt_shell__opt_eta()
{
	// Set up 3 variable optimization in NLOPT
	std::vector<double> x(0);
	std::vector<double> lb(0);
	std::vector<double> ub(0);
	std::vector<double> scale(0);
	int index = 0;

	// Try finding a feasible off-design solution
	S_od_operation_inputs od_op_inputs;
	int feas_code = find_a_feasible_off_design_solution(ms_od_par, ms_rc_cycle_od_phi_par.m_T_mc_in, od_op_inputs);

	// Inlet pressure
	// Might think of ways to generate better values here...
	x.push_back(ms_des_solved.ms_rc_cycle_solved.m_pres[1 - 1]);
	lb.push_back(1000.0);
	ub.push_back(17000.0);
	scale.push_back(-0.1*ms_des_solved.ms_rc_cycle_solved.m_pres[1 - 1]);	// Solution is probably less than design pressure
	index++;

	// Recompression Fraction
	if( ms_des_solved.ms_rc_cycle_solved.m_is_rc && m_off_design_turbo_operation != E_FIXED_MC_FIXED_RC_FIXED_T )
	{
		x.push_back(ms_des_solved.ms_rc_cycle_solved.m_recomp_frac);
		lb.push_back(0.0);
		ub.push_back(1.0);
		scale.push_back(-.02);
		index++;
	}

	// Main Compressor Flow Coefficient
	if(m_is_phi_optimized)
	{
		x.push_back(ms_des_solved.ms_rc_cycle_solved.ms_mc_des_solved.m_phi_des);
		lb.push_back(ms_des_solved.ms_rc_cycle_solved.ms_mc_des_solved.m_phi_surge);
		ub.push_back(ms_des_solved.ms_rc_cycle_solved.ms_mc_des_solved.m_phi_max);
		scale.push_back(ms_des_solved.ms_rc_cycle_solved.ms_mc_des_solved.m_phi_des +
			0.05*(ms_des_solved.ms_rc_cycle_solved.ms_mc_des_solved.m_phi_max - ms_des_solved.ms_rc_cycle_solved.ms_mc_des_solved.m_phi_des));
		index++;
	}
	
	// Reset optimization tracking structure
	reset_S_od_opt_eta_tracking();

	// Save initial vectors
	std::vector<double> x_base = x;
	std::vector<double> lb_base = lb;
	std::vector<double> ub_base = ub;
	std::vector<double> sc_base = scale;

	// Set up instance of nlopt class and set optimization parameters
	nlopt::opt          opt_od_eta(nlopt::LN_SBPLX, index);
	opt_od_eta.set_lower_bounds(lb);
	opt_od_eta.set_upper_bounds(ub);
	opt_od_eta.set_initial_step(scale);
	opt_od_eta.set_xtol_rel(m_od_opt_xtol);
	opt_od_eta.set_ftol_rel(m_od_opt_ftol);

	// Set max objective function
	opt_od_eta.set_max_objective(nlopt_cb_opt_od_eta__float_phx_dt, this);

	double max_f = std::numeric_limits<double>::quiet_NaN();
	nlopt::result          result_od_cycle = opt_od_eta.optimize(x, max_f);

	if( max_f != max_f )
	{
		double blahhhh = 1.23;
	}

	int n_opts_found = 0;
	double obj_max_1 = std::numeric_limits<double>::quiet_NaN();
	double obj_max_2 = std::numeric_limits<double>::quiet_NaN();
	std::vector<double> x_opt_1;
	std::vector<double> x_opt_2;

	if( !ms_od_opt_eta_tracking.m_is_opt_found && ms_od_opt_eta_tracking.m_eta_max <= 0.0 )
	{
		if( feas_code == 0 )
		{
			// Inlet pressure
			// Might think of ways to generate better values here...
			index = 0;
			x[index] = od_op_inputs.m_P_mc_in;
			index++;

			// Recompression Fraction
			if( ms_des_solved.ms_rc_cycle_solved.m_is_rc && m_off_design_turbo_operation != E_FIXED_MC_FIXED_RC_FIXED_T )
			{
				x[index] = od_op_inputs.m_recomp_frac;
				index++;
			}

			// Main Compressor Flow Coefficient
			if( m_is_phi_optimized )
			{
				x[index] = od_op_inputs.m_phi_mc;
				index++;
			}
			
			//std::string err_msg = util::format("Can't find a feasible solution at T_htf = %lg [C], "
			//	"m_dot_htf_ND = %lg [-], "
			//	"and T_amb = %lg [C]",
			//	ms_od_par.m_T_htf_hot - 273.15,
			//	ms_od_par.m_m_dot_htf / ms_phx_des_par.m_m_dot_hot_des,
			//	ms_od_par.m_T_amb - 273.15);
			//
			//
			//throw(C_csp_exception(err_msg, "Trying 2nd optimization by finding a feasible solution - need to debug through this for first time..."));

			// Reset optimization tracking structure
			reset_S_od_opt_eta_tracking();

			opt_od_eta.set_lower_bounds(lb);
			opt_od_eta.set_upper_bounds(ub);
			opt_od_eta.set_initial_step(scale);
			opt_od_eta.set_xtol_rel(m_od_opt_xtol);
			opt_od_eta.set_ftol_rel(m_od_opt_ftol);

			// Set max objective function
			opt_od_eta.set_max_objective(nlopt_cb_opt_od_eta__float_phx_dt, this);

			max_f = std::numeric_limits<double>::quiet_NaN();
			result_od_cycle = opt_od_eta.optimize(x,max_f);			

			if( max_f != max_f )
			{
				double blahhhh = 1.23;
			}

			if( !ms_od_opt_eta_tracking.m_is_opt_found && ms_od_opt_eta_tracking.m_eta_max <= 0.0 )
			{
				return -1;
			}
			else
			{
				n_opts_found = 1;
				
				if( !ms_od_opt_eta_tracking.m_is_opt_found )
				{
					int i = 0;
					x_opt_1.push_back(ms_od_opt_eta_tracking.ms_od_op_in_max.m_P_mc_in);

					if( ms_des_solved.ms_rc_cycle_solved.m_is_rc && m_off_design_turbo_operation != E_FIXED_MC_FIXED_RC_FIXED_T )
					{
						x_opt_1.push_back(ms_od_opt_eta_tracking.ms_od_op_in_max.m_recomp_frac);
						i++;
					}

					if( m_is_phi_optimized )
					{
						x_opt_1.push_back(ms_od_opt_eta_tracking.ms_od_op_in_max.m_phi_mc);
						i++;
					}

					obj_max_1 = ms_od_opt_eta_tracking.m_eta_max;
				}
				else
				{
					x_opt_1 = x;
					obj_max_1 = max_f;
				}
			}
		}
		else
		{
			std::string err_msg = util::format("Can't find a feasible solution at T_htf = %lg [C], "
				"m_dot_htf_ND = %lg [-], "
				"and T_amb = %lg [C]",
				ms_od_par.m_T_htf_hot - 273.15,
				ms_od_par.m_m_dot_htf / ms_phx_des_par.m_m_dot_hot_des,
				ms_od_par.m_T_amb - 273.15);

			throw(C_csp_exception("Trying 2nd optimization but can't find a feasible solution..."));

			return -1;
		}

	}
	else
	{
		n_opts_found = 1;

		if( !ms_od_opt_eta_tracking.m_is_opt_found )
		{
			int i = 0;
			x_opt_1.push_back(ms_od_opt_eta_tracking.ms_od_op_in_max.m_P_mc_in);

			if( ms_des_solved.ms_rc_cycle_solved.m_is_rc && m_off_design_turbo_operation != E_FIXED_MC_FIXED_RC_FIXED_T )
			{
				x_opt_1.push_back(ms_od_opt_eta_tracking.ms_od_op_in_max.m_recomp_frac);
				i++;
			}

			if( m_is_phi_optimized )
			{
				x_opt_1.push_back(ms_od_opt_eta_tracking.ms_od_op_in_max.m_phi_mc);
				i++;
			}
			
			obj_max_1 = ms_od_opt_eta_tracking.m_eta_max;
		}
		else
		{
			x_opt_1 = x;
			obj_max_1 = max_f;
		}				

		if( feas_code == 0 )
		{
			// Inlet pressure
			// Might think of ways to generate better values here...
			index = 0;
			x[index] = od_op_inputs.m_P_mc_in;
			index++;

			// Recompression Fraction
			if( ms_des_solved.ms_rc_cycle_solved.m_is_rc && m_off_design_turbo_operation != E_FIXED_MC_FIXED_RC_FIXED_T )
			{
				x[index] = od_op_inputs.m_recomp_frac;
				index++;
			}

			// Main Compressor Flow Coefficient
			if( m_is_phi_optimized )
			{
				x[index] = od_op_inputs.m_phi_mc;
				index++;
			}

			//std::string err_msg = util::format("Can't find a feasible solution at T_htf = %lg [C], "
			//	"m_dot_htf_ND = %lg [-], "
			//	"and T_amb = %lg [C]",
			//	ms_od_par.m_T_htf_hot - 273.15,
			//	ms_od_par.m_m_dot_htf / ms_phx_des_par.m_m_dot_hot_des,
			//	ms_od_par.m_T_amb - 273.15);
			//
			//
			//throw(C_csp_exception(err_msg, "Trying 2nd optimization by finding a feasible solution - need to debug through this for first time..."));

			// Reset optimization tracking structure
			ms_od_opt_eta_tracking.m_is_opt_found = false;
			ms_od_opt_eta_tracking.m_eta_max = 0.0;
			ms_od_opt_eta_tracking.m_over_T_t_in_at_eta_max = std::numeric_limits<double>::quiet_NaN();
			ms_od_opt_eta_tracking.m_over_P_high_at_eta_max = std::numeric_limits<double>::quiet_NaN();

			opt_od_eta.set_lower_bounds(lb);
			opt_od_eta.set_upper_bounds(ub);
			opt_od_eta.set_initial_step(scale);
			opt_od_eta.set_xtol_rel(m_od_opt_xtol);
			opt_od_eta.set_ftol_rel(m_od_opt_ftol);

			// Set max objective function
			opt_od_eta.set_max_objective(nlopt_cb_opt_od_eta__float_phx_dt, this);

			max_f = std::numeric_limits<double>::quiet_NaN();
			result_od_cycle = opt_od_eta.optimize(x, max_f);

			if(max_f != max_f)
			{
				double blahhhh = 1.23;
			}

			if( ms_od_opt_eta_tracking.m_is_opt_found || ms_od_opt_eta_tracking.m_eta_max > 0.0 )
			{
				n_opts_found = 2;
				
				if( !ms_od_opt_eta_tracking.m_is_opt_found )
				{
					int i = 0;
					x_opt_2.push_back(ms_od_opt_eta_tracking.ms_od_op_in_max.m_P_mc_in);
					i++;

					if( ms_des_solved.ms_rc_cycle_solved.m_is_rc && m_off_design_turbo_operation != E_FIXED_MC_FIXED_RC_FIXED_T )
					{
						x_opt_2.push_back(ms_od_opt_eta_tracking.ms_od_op_in_max.m_recomp_frac);
						i++;
					}

					if( m_is_phi_optimized )
					{
						x_opt_2.push_back(ms_od_opt_eta_tracking.ms_od_op_in_max.m_phi_mc);
						i++;
					}

					obj_max_2 = ms_od_opt_eta_tracking.m_eta_max;
				}
				else
				{
					x_opt_2 = x;
					obj_max_2 = max_f;
				}
			}
		}
	}

	if(n_opts_found == 2)
	{
		if(obj_max_2 > obj_max_1)
		{
			x = x_opt_2;
		}
		else
		{
			x = x_opt_1;
		}
	}
	else
	{
		x = x_opt_1;
	}

	// Call a final time with optimized parameters
	double opt_metric = od_fix_T_mc_approach__nl_opt_shell(x);

	double eta_at_opt = mc_rc_cycle.get_od_solved()->m_eta_thermal;		//[-]

	if(eta_at_opt > 0.0)
	{
		return 0;
	}
	else
	{
		return -1;
	}

}

double C_sco2_recomp_csp::od_fix_T_mc_approach__nl_opt_shell(const std::vector<double> &x)
{
	// This method solves for the off-design performance of the cycle integrated with the PHX and HTF stream
	// x includes main compressor inlet pressure, recompression fraction, and main compressor speed
	// Other off-design parameters should be already stored in ms_od_par:
	//		m_T_htf_hot
	//      m_m_dot_htf
	// ... or in ms_rc_cycle_od_par
	//	    m_T_mc_in
	//	    m_N_sub_hxrs
	//	    m_tol
	//	    m_N_t

	int index = 0;

	ms_rc_cycle_od_phi_par.m_P_mc_in = x[index];		//[kPa]
	index++;

	ms_rc_cycle_od_phi_par.m_recomp_frac = 0.0;
	if( ms_des_solved.ms_rc_cycle_solved.m_is_rc && m_off_design_turbo_operation != E_FIXED_MC_FIXED_RC_FIXED_T )
	{
		ms_rc_cycle_od_phi_par.m_recomp_frac = x[index];
		index++;
	}

	if( m_is_phi_optimized )
	{
		ms_rc_cycle_od_phi_par.m_phi_mc = x[index];
		index++;
	}
	else
	{
		ms_rc_cycle_od_phi_par.m_phi_mc = mc_rc_cycle.get_design_solved()->ms_mc_des_solved.m_phi_des;
	}

	double eta_solved = std::numeric_limits<double>::quiet_NaN();
	int od_code = off_design_core(eta_solved);

	//// Want to make an efficiency value available to the optimization although it may be decreased by system operation constraints
	//if( !(od_code == 0 || od_code == E_TURBINE_INLET_OVER_TEMP || od_code == E_OVER_PRESSURE || 
	//		od_code == E_TIP_RATIO || od_code == E_MC_SURGE || od_code == E_RC_SURGE) )
	//	return 0.0;

	return eta_solved;

}

int C_sco2_recomp_csp::C_mono_eq_T_t_in::operator()(double T_t_in /*K*/, double *diff_T_t_in /*-*/)
{
	// Using:
	//	-mc_rc_cycle
	//	-ms_rc_cycle_od_par
	//	-ms_phx_od_par

	// 1) Update Turbine Inlet Temperature in sco2 cycle off design parameter structure
	mpc_sco2_rc->ms_rc_cycle_od_phi_par.m_T_t_in = T_t_in;

	// 2) Solve the off-design cycle model with off design parameter structure
	int rc_od_error_code = 0;
	
	if( mpc_sco2_rc->m_off_design_turbo_operation == E_FIXED_MC_FIXED_RC_FIXED_T )
	{
		mpc_sco2_rc->mc_rc_cycle.off_design_fix_shaft_speeds(mpc_sco2_rc->ms_rc_cycle_od_phi_par, rc_od_error_code);
	}
	else if( mpc_sco2_rc->m_off_design_turbo_operation == E_VFD_MC_VFD_RC_FIXED_T )
	{
		mpc_sco2_rc->mc_rc_cycle.off_design_phi(mpc_sco2_rc->ms_rc_cycle_od_phi_par, rc_od_error_code);
	}
	else
	{
		throw(C_csp_exception("Off design turbomachinery operation strategy not recognized"));
	}

	// If off-design cycle model did not solve, return to solver
	if( rc_od_error_code != 0 )
	{
		*diff_T_t_in = std::numeric_limits<double>::quiet_NaN();
		return rc_od_error_code;
	}

	// Solve PHX heat exchanger performance using CO2 and HTF *inlet* conditions
	mpc_sco2_rc->ms_phx_od_par.m_T_c_in = mpc_sco2_rc->mc_rc_cycle.get_od_solved()->m_temp[C_RecompCycle::HTR_HP_OUT];	//[K]
	mpc_sco2_rc->ms_phx_od_par.m_P_c_in = mpc_sco2_rc->mc_rc_cycle.get_od_solved()->m_pres[C_RecompCycle::HTR_HP_OUT];	//[kPa]
	mpc_sco2_rc->ms_phx_od_par.m_m_dot_c = mpc_sco2_rc->mc_rc_cycle.get_od_solved()->m_m_dot_t;		//[kg/s]
	double P_c_out = mpc_sco2_rc->mc_rc_cycle.get_od_solved()->m_pres[C_RecompCycle::TURB_IN];		//[kPa]
	double q_dot, T_co2_phx_out, T_htf_cold;
	q_dot = T_co2_phx_out = T_htf_cold = std::numeric_limits<double>::quiet_NaN();
	
	// Solves HX performance. 
	// If successful, this call updates 'ms_od_solved'
	try
	{
		mpc_sco2_rc->mc_phx.off_design_solution(mpc_sco2_rc->ms_phx_od_par.m_T_c_in, mpc_sco2_rc->ms_phx_od_par.m_P_c_in, mpc_sco2_rc->ms_phx_od_par.m_m_dot_c, P_c_out,
			mpc_sco2_rc->ms_phx_od_par.m_T_h_in, mpc_sco2_rc->ms_phx_od_par.m_P_h_in, mpc_sco2_rc->ms_phx_od_par.m_m_dot_h, mpc_sco2_rc->ms_phx_od_par.m_P_h_in,
			q_dot, T_co2_phx_out, T_htf_cold);
	}						
	catch( C_csp_exception )
	{
		// reset 'diff_T_t_in' to NaN
		*diff_T_t_in = std::numeric_limits<double>::quiet_NaN();
		
		return -1;
	}
	
	*diff_T_t_in = (T_co2_phx_out - T_t_in) / T_t_in;
	return 0;
}

void C_sco2_recomp_csp::sweep_turbomachinery_deltaP(double T_mc_in /*K*/, double P_mc_in /*kPa*/,
										double T_t_in /*K*/, double phi_mc /*-*/)
{
	// Set up parametric sweep of operation inputs
	double f_recomp_start = 0.0;
	double f_recomp_end = 1.0;
	int n_f_recomp = 101;
	double f_recomp_step = (f_recomp_end - f_recomp_start) / (n_f_recomp - 1);
	
	double f_recomp = 1.0;
	

	ofstream out_file;
	out_file.open("C:/Users/tneises/Documents/Brayton-Rankine/APOLLO/Off_design_turbo_balance/balance.csv");

	out_file << "deltaP,P_mc_out,m_dot_mc,m_dot_t,N_mc,mc_tip_ratio,f_recomp,m_dot_rc,rc_error_code,rc_phi,rc_tip_ratio\n";
	out_file << "kPa,kPa,kg/s,kg/s,rpm,-,-,kg/s,-,-,-\n";

	for( int n_run = 0; n_run < n_f_recomp; n_run++ )
	{
		f_recomp = f_recomp_start + f_recomp_step*n_run;

		// Initialize a few variables
		mc_rc_cycle.set_od_temp(C_RecompCycle::MC_IN, T_mc_in);
		mc_rc_cycle.set_od_pres(C_RecompCycle::MC_IN, P_mc_in);
		mc_rc_cycle.set_od_temp(C_RecompCycle::TURB_IN, T_t_in);

		C_RecompCycle::C_mono_eq_turbo_m_dot c_turbo_bal(&mc_rc_cycle, T_mc_in, P_mc_in,
										f_recomp, T_t_in, phi_mc, true);

		C_monotonic_eq_solver c_turbo_m_dot_solver(c_turbo_bal);

		// Set lower bound on mass flow rate
		double m_dot_lower = ms_des_solved.ms_rc_cycle_solved.m_m_dot_t*1.E-3;	//[kg/s]
		double m_dot_upper = std::numeric_limits<double>::quiet_NaN();

		// Set solver settings
		c_turbo_m_dot_solver.settings(1.E-3, 100, m_dot_lower, m_dot_upper, false);

		// Generate two guess values
		double m_dot_guess_upper = ms_des_solved.ms_rc_cycle_solved.m_m_dot_t;	//[kg/s]
		double m_dot_guess_lower = 0.7*m_dot_guess_upper;						//[kg/s]

		// Solve for the turbine mass flow rate
		double m_dot_t_solved, tol_solved;
		m_dot_t_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
		int iter_solved = -1;

		int m_dot_t_code = c_turbo_m_dot_solver.solve(m_dot_guess_lower, m_dot_guess_upper, 0.0,
											m_dot_t_solved, tol_solved, iter_solved);

		double P_mc_out = std::numeric_limits<double>::quiet_NaN();
		double deltaP = std::numeric_limits<double>::quiet_NaN();
		double N_mc = std::numeric_limits<double>::quiet_NaN();
		double mc_w_tip_ratio = std::numeric_limits<double>::quiet_NaN();
		double m_dot_mc = std::numeric_limits<double>::quiet_NaN();
		double m_dot_rc = std::numeric_limits<double>::quiet_NaN();
		int rc_error_code = -1;
		double T_rc_out = std::numeric_limits<double>::quiet_NaN();
		double rc_w_tip_ratio = std::numeric_limits<double>::quiet_NaN();
		double rc_phi = std::numeric_limits<double>::quiet_NaN();;

		if( m_dot_t_code != C_monotonic_eq_solver::CONVERGED )
		{
			P_mc_out = 0.0;
			deltaP = 0.0;
			N_mc = 0.0;
			mc_w_tip_ratio = 0.0;
			m_dot_mc = 0.0;
			m_dot_rc = 0.0;
			rc_error_code = 0;
			T_rc_out = 0.0;
			rc_w_tip_ratio = 0.0;
			rc_phi = 0.0;
			m_dot_t_solved = 0.0;
		}
		else
		{
			m_dot_mc = (1.0 - f_recomp)*m_dot_t_solved;
			m_dot_rc = m_dot_t_solved - m_dot_mc;
			P_mc_out = mc_rc_cycle.get_od_solved()->m_pres[C_RecompCycle::MC_OUT];
			deltaP = P_mc_out - P_mc_in;
			N_mc = mc_rc_cycle.get_od_solved()->ms_mc_od_solved.m_N;

			// Get compressor(s) tip ratio
			mc_w_tip_ratio = mc_rc_cycle.get_od_solved()->ms_mc_od_solved.m_w_tip_ratio;
		
			// Recompressor tip ratio and surge
			// But... need to solve recompressor first...
			if(true)
			{
				if( ms_des_solved.ms_rc_cycle_solved.m_is_rc && m_dot_rc > 0.0 )
				{
					rc_error_code = 0;
					mc_rc_cycle.off_design_recompressor(mc_rc_cycle.get_design_solved()->m_temp[C_RecompCycle::HTR_LP_OUT],
														P_mc_in,
														m_dot_rc,
														P_mc_out,
														rc_error_code,
														T_rc_out);					
				}
				
				if( rc_error_code == 0 )
				{
					rc_w_tip_ratio = mc_rc_cycle.get_rc_od_solved()->m_w_tip_ratio;

					double rc_phi_s1 = mc_rc_cycle.get_rc_od_solved()->m_phi;
					double rc_phi_s2 = mc_rc_cycle.get_rc_od_solved()->m_phi_2;
					rc_phi = min(rc_phi_s1, rc_phi_s2);
				}
				else
				{
					T_rc_out = 0.0;
					rc_w_tip_ratio = 0.0;
					rc_phi = 0.0;
				}
			}

			// *************************************************
			// *************************************************			
		}
		
		out_file << deltaP << "," << 
					P_mc_out << "," <<
					m_dot_mc << "," <<
					m_dot_t_solved <<  "," <<
					N_mc << "," <<
					mc_w_tip_ratio << "," <<
					f_recomp << "," <<
					m_dot_rc << "," <<
					rc_error_code << "," <<
					rc_phi << "," <<
					rc_w_tip_ratio << "\n";
	}

	out_file.close();
}

int C_sco2_recomp_csp::C_sco2_csp_od::operator()(S_f_inputs inputs, S_f_outputs & outputs)
{
	S_od_par sco2_od_par;
	sco2_od_par.m_T_htf_hot = inputs.m_T_htf_hot + 273.15;	//[K] convert from C
	sco2_od_par.m_m_dot_htf = mpc_sco2_rc->get_phx_des_par()->m_m_dot_hot_des*inputs.m_m_dot_htf_ND;	//[kg/s] scale from [-]
	sco2_od_par.m_T_amb = inputs.m_T_amb + 273.15;			//[K] convert from C

	int od_strategy = C_sco2_recomp_csp::E_TARGET_POWER_ETA_MAX;

	int off_design_code = -1;	//[-]

	off_design_code = mpc_sco2_rc->off_design_opt(sco2_od_par, od_strategy);

	//off_design_code = mpc_sco2_rc->off_design_nested_opt(sco2_od_par, od_strategy);

	// Cycle off-design may want to operate below this value, so ND value could be < 1 everywhere
	double W_dot_gross_design = mpc_sco2_rc->get_design_solved()->ms_rc_cycle_solved.m_W_dot_net;	//[kWe]
	double Q_dot_in_design = mpc_sco2_rc->get_design_solved()->ms_rc_cycle_solved.m_W_dot_net
								/ mpc_sco2_rc->get_design_solved()->ms_rc_cycle_solved.m_eta_thermal;	//[kWt]
	double W_dot_cooling_design = mpc_sco2_rc->get_design_par()->m_frac_fan_power*W_dot_gross_design;	//[kWe]
	double m_dot_water_design = 0.0;		//[kg/s]

	outputs.m_W_dot_gross_ND = mpc_sco2_rc->get_od_solved()->ms_rc_cycle_od_solved.m_W_dot_net
								/ W_dot_gross_design;

	outputs.m_Q_dot_in_ND = mpc_sco2_rc->get_od_solved()->ms_rc_cycle_od_solved.m_Q_dot
								/ Q_dot_in_design;

	outputs.m_W_dot_cooling_ND = outputs.m_W_dot_gross_ND;

	outputs.m_m_dot_water_ND = 1.0;	

	return off_design_code;
}

int C_sco2_recomp_csp::generate_ud_pc_tables(double T_htf_low /*C*/, double T_htf_high /*C*/, int n_T_htf /*-*/,
	double T_amb_low /*C*/, double T_amb_high /*C*/, int n_T_amb /*-*/,
	double m_dot_htf_ND_low /*-*/, double m_dot_htf_ND_high /*-*/, int n_m_dot_htf_ND,
	util::matrix_t<double> & T_htf_ind, util::matrix_t<double> & T_amb_ind, util::matrix_t<double> & m_dot_htf_ND_ind)
{
	C_sco2_csp_od c_sco2_csp(this);
	C_ud_pc_table_generator c_sco2_ud_pc(c_sco2_csp);

	c_sco2_ud_pc.mf_callback = mf_callback;
	c_sco2_ud_pc.m_cdata = m_cdata;

	double T_htf_ref = ms_des_par.m_T_htf_hot_in - 273.15;	//[C] convert from K
	double T_amb_ref = ms_des_par.m_T_amb_des - 273.15;		//[C] convert from K
	double m_dot_htf_ND_ref = 1.0;							//[-]

	int ud_pc_error_code = c_sco2_ud_pc.generate_tables(T_htf_ref, T_htf_low, T_htf_high, n_T_htf,
								T_amb_ref, T_amb_low, T_amb_high, n_T_amb,
								m_dot_htf_ND_ref, m_dot_htf_ND_low, m_dot_htf_ND_high, n_m_dot_htf_ND,
								T_htf_ind, T_amb_ind, m_dot_htf_ND_ind);

	return ud_pc_error_code;
}

double C_sco2_recomp_csp::opt_P_mc_in_nest_f_recomp_max_eta(double P_mc_in /*kPa*/)
{
	ms_rc_cycle_od_phi_par.m_P_mc_in = P_mc_in;	//[kPa]	

	double eta_max_f_recomp_opt = std::numeric_limits<double>::quiet_NaN();

	if( m_off_design_turbo_operation == E_FIXED_MC_FIXED_RC_FIXED_T )
	{
		int od_err_code = off_design_core(eta_max_f_recomp_opt);
	}
	else if( m_off_design_turbo_operation == E_VFD_MC_VFD_RC_FIXED_T )
	{
		bool f_opt_success = opt_f_recomp_fix_P_mc_in_max_eta_core();		
	}
	else
	{
		throw(C_csp_exception("Off design turbomachinery operation strategy not recognized"));
	}

	double f_recomp_opt = mc_rc_cycle.get_od_solved()->m_recomp_frac;	//[-]
	double eta_solved = mc_rc_cycle.get_od_solved()->m_eta_thermal;		//[-]

	if( eta_max_f_recomp_opt != eta_max_f_recomp_opt )
	{
		eta_max_f_recomp_opt = 0.0;
	}
		 
	if( m_is_write_mc_out_file )
	{
		double deltaP = 0.0;
		double P_mc_out_of = 0.0;		//[kPa]
		double m_dot_mc = 0.0;			//[kg/s]
		double m_dot_t = 0.0;			//[kg/s]
		double N_mc = 0.0;				//[rpm]
		double phi_mc = 0.0;			//[-]
		double mc_tip_ratio_of = 0.0;	//[-]
		double f_recomp_of = 0.0;		//[-]
		double m_dot_rc = 0.0;			//[kg/s]
		double rc_phi_of = 0.0;			//[-]
		double rc_tip_ratio_of = 0.0;	//[-]
		int od_error_code = -1;			//[-]

		double W_dot_net_of = 0.0;		//[MWe]
		double Q_dot_in_of = 0.0;		//[MWt]
		double T_htf_cold_of = 0.0;		//[C]

		if( eta_max_f_recomp_opt > 0.0 )
		{
			P_mc_out_of = mc_rc_cycle.get_od_solved()->m_pres[C_RecompCycle::MC_OUT];	//[kPa]
			deltaP = P_mc_out_of - P_mc_in;
			m_dot_mc = mc_rc_cycle.get_od_solved()->m_m_dot_mc;
			m_dot_t = mc_rc_cycle.get_od_solved()->m_m_dot_t;
			m_dot_rc = mc_rc_cycle.get_od_solved()->m_m_dot_rc;
			N_mc = mc_rc_cycle.get_od_solved()->ms_mc_od_solved.m_N;
			phi_mc = mc_rc_cycle.get_od_solved()->ms_mc_od_solved.m_phi;
			mc_tip_ratio_of = mc_rc_cycle.get_od_solved()->ms_mc_od_solved.m_w_tip_ratio;
			f_recomp_of = mc_rc_cycle.get_od_solved()->m_recomp_frac;	//[-]
			
			if( mc_rc_cycle.get_design_solved()->m_is_rc )
			{
				rc_phi_of = mc_rc_cycle.get_od_solved()->ms_rc_od_solved.m_w_tip_ratio;
				double rc_phi_s1 = mc_rc_cycle.get_rc_od_solved()->m_phi;
				double rc_phi_s2 = mc_rc_cycle.get_rc_od_solved()->m_phi_2;
				rc_tip_ratio_of = min(rc_phi_s1, rc_phi_s2);
			}

			W_dot_net_of = mc_rc_cycle.get_od_solved()->m_W_dot_net / 1.E3;		//[MWe]
			Q_dot_in_of = mc_rc_cycle.get_od_solved()->m_Q_dot / 1.E3;			//[MWt]
			T_htf_cold_of = mc_phx.ms_od_solved.m_T_h_out - 273.15;				//[C]

			od_error_code = ms_od_solved.m_od_error_code;	//[-]
		}

		mc_P_mc_vary_f_recomp_opt_file << ms_od_par.m_T_amb - 273.15 << ","
			<< ms_od_par.m_m_dot_htf / mc_phx.ms_des_calc_UA_par.m_m_dot_hot_des << ","
			<< ms_od_par.m_T_htf_hot - 273.15 << ","
			<< util::format("%.4f",P_mc_in) << ","
			<< deltaP << ","
			<< util::format("%.4f",P_mc_out_of) << ","
			<< m_dot_mc << ","
			<< m_dot_t << ","
			<< N_mc << ","
			<< phi_mc << ","
			<< mc_tip_ratio_of << ","
			<< util::format("%.4f",f_recomp_of) << ","
			<< m_dot_rc << ","
			<< rc_tip_ratio_of << "," 
			<< rc_phi_of << ","
			<< eta_solved << ","
			<< W_dot_net_of << ","
			<< Q_dot_in_of << ","
			<< T_htf_cold_of << ","
			<< od_error_code << "\n";
	}

	return eta_max_f_recomp_opt;
}

void C_sco2_recomp_csp::off_design_P_mc_in_parameteric(double P_mc_in_min /*kPa*/, double P_mc_in_max /*kPa*/, double P_mc_in_inc /*kPa*/)
{
	if( m_is_write_mc_out_file )
	{
		std::string case_name = util::format("%.2f_", ms_od_par.m_T_amb - 273.15) +
			util::format("%.2f_", ms_od_par.m_m_dot_htf / ms_phx_des_par.m_m_dot_hot_des) +
			util::format("%.2f_", ms_od_par.m_T_htf_hot - 273.15);

		std::string file_name = mstr_base_name + case_name + ".csv";
		mc_P_mc_vary_f_recomp_opt_file.open(file_name.c_str());

		mc_P_mc_vary_f_recomp_opt_file << "T_amb,m_dot_ND,T_htf_hot,P_mc_in,deltaP,P_mc_out,m_dot_mc,m_dot_t,N_mc,phi_mc,mc_tip_ratio,f_recomp,m_dot_rc,rc_phi,rc_tip_ratio,eta_thermal,W_dot_net,Q_dot_in,T_htf_cold,is_error_code\n";
	}

	for(double P_mc_in = P_mc_in_min; P_mc_in <= P_mc_in_max; P_mc_in += P_mc_in_inc)
	{
		opt_P_mc_in_nest_f_recomp_max_eta(P_mc_in);
	}

	if( m_is_write_mc_out_file )
	{
		mc_P_mc_vary_f_recomp_opt_file.close();
	}
}


void C_sco2_recomp_csp::off_design_fix_P_mc_in_parametric_f_recomp(double P_mc_in /*kPa*/, double f_recomp_min /*-*/, double f_recomp_max /*-*/, double f_recomp_inc /*-*/)
{
	ms_rc_cycle_od_phi_par.m_P_mc_in = P_mc_in;		//[kPa]

	if( m_is_write_mc_out_file )
	{
		std::string case_name = util::format("%.2f_", ms_od_par.m_T_amb - 273.15) +
			util::format("%.2f_", ms_od_par.m_m_dot_htf / ms_phx_des_par.m_m_dot_hot_des) +
			util::format("%.2f_", ms_od_par.m_T_htf_hot - 273.15) +
			util::format("%.1f", P_mc_in);

		std::string file_name = mstr_base_name + case_name + ".csv";
		mc_P_mc_in_fixed_f_recomp_vary_file.open(file_name.c_str());
	}

	if( m_is_write_mc_out_file )
	{
		mc_P_mc_in_fixed_f_recomp_vary_file << "T_amb,m_dot_ND,T_htf_hot,P_mc_in,deltaP,P_mc_out,m_dot_mc,m_dot_t,N_mc,mc_tip_ratio,f_recomp,m_dot_rc,rc_phi,rc_tip_ratio,eta_thermal,W_dot_net,Q_dot_in,T_htf_cold,is_error_code\n";
	}

	for(double f_recomp = f_recomp_min; f_recomp <= f_recomp_max; f_recomp = f_recomp + f_recomp_inc)
	{
		opt_f_recomp_max_eta(f_recomp);
	}

	if( m_is_write_mc_out_file )
	{
		mc_P_mc_in_fixed_f_recomp_vary_file.close();
	}
}

double C_sco2_recomp_csp::opt_f_recomp_max_eta(double f_recomp)
{
	ms_rc_cycle_od_phi_par.m_recomp_frac = f_recomp;	//[-]

	double eta_return = std::numeric_limits<double>::quiet_NaN();

	int od_error_code = off_design_core(eta_return);	//[-]

	if( m_is_write_mc_out_file )
	{
		double P_mc_in = ms_rc_cycle_od_phi_par.m_P_mc_in;		//[kPa]
		double deltaP = 0.0;
		double P_mc_out_of = 0.0;
		double m_dot_mc = 0.0;
		double m_dot_t = 0.0;
		double N_mc = 0.0;
		double mc_tip_ratio_of = 0.0;
		double f_recomp_of = f_recomp;					//[-]
		double m_dot_rc = 0.0;
		double rc_phi_of = 0.0;
		double rc_tip_ratio_of = 0.0;

		double W_dot_net_of = 0.0;		//[MWe]
		double Q_dot_in_of = 0.0;		//[MWt]
		double T_htf_cold_of = 0.0;		//[C]

		if( eta_return > 0.0 )
		{
			P_mc_out_of = mc_rc_cycle.get_od_solved()->m_pres[C_RecompCycle::MC_OUT];	//[kPa]
			deltaP = P_mc_out_of - P_mc_in;
			m_dot_mc = mc_rc_cycle.get_od_solved()->m_m_dot_mc;
			m_dot_t = mc_rc_cycle.get_od_solved()->m_m_dot_t;
			m_dot_rc = mc_rc_cycle.get_od_solved()->m_m_dot_rc;
			N_mc = mc_rc_cycle.get_od_solved()->ms_mc_od_solved.m_N;
			mc_tip_ratio_of = mc_rc_cycle.get_od_solved()->ms_mc_od_solved.m_w_tip_ratio;

			rc_phi_of = mc_rc_cycle.get_od_solved()->ms_rc_od_solved.m_w_tip_ratio;
			double rc_phi_s1 = mc_rc_cycle.get_rc_od_solved()->m_phi;
			double rc_phi_s2 = mc_rc_cycle.get_rc_od_solved()->m_phi_2;
			rc_tip_ratio_of = min(rc_phi_s1, rc_phi_s2);

			W_dot_net_of = mc_rc_cycle.get_od_solved()->m_W_dot_net/1.E3;		//[MWe]
			Q_dot_in_of = mc_rc_cycle.get_od_solved()->m_Q_dot/1.E3;			//[MWt]
			T_htf_cold_of = mc_phx.ms_od_solved.m_T_c_out - 273.15;				//[C]
		}

		mc_P_mc_in_fixed_f_recomp_vary_file << ms_od_par.m_T_amb - 273.15 << ","
			<< ms_od_par.m_m_dot_htf << ","
			<< ms_od_par.m_T_htf_hot - 273.15 << ","
			<< util::format("%.4f", P_mc_in) << "," << deltaP << ","
			<< util::format("%.4f", P_mc_out_of) << ","
			<< m_dot_mc << ","
			<< m_dot_t << ","
			<< N_mc << ","
			<< mc_tip_ratio_of << ","
			<< util::format("%.4f", f_recomp_of) << ","
			<< m_dot_rc << ","
			<< rc_phi_of << ","
			<< rc_tip_ratio_of << ","
			<< eta_return << ","
			<< W_dot_net_of << ","
			<< Q_dot_in_of << ","
			<< T_htf_cold_of << ","
			<< od_error_code << "\n";
	}

	return eta_return;
}

double nlopt_cb_opt_od_eta__float_phx_dt(const std::vector<double> &x, std::vector<double> &grad, void *data)
{
	C_sco2_recomp_csp *frame = static_cast<C_sco2_recomp_csp*>(data);
	if( frame != NULL ) return frame->od_fix_T_mc_approach__nl_opt_shell(x);
}

double nlopt_max_f_recomp_cycle_eta(const std::vector<double> &x, std::vector<double> &grad, void *data)
{
	C_sco2_recomp_csp *frame = static_cast<C_sco2_recomp_csp*>(data);
	if( frame != NULL ) return frame->opt_f_recomp_max_eta(x[0]);
}

double fmin_f_recomp_cycle_eta(double x, void *data)
{
	C_sco2_recomp_csp *frame = static_cast<C_sco2_recomp_csp*>(data);

	return -(frame->opt_f_recomp_max_eta(x));
}

double nlopt_max_opt_P_mc_in_nest_f_recomp(const std::vector<double> &x, std::vector<double> &grad, void *data)
{
	C_sco2_recomp_csp *frame = static_cast<C_sco2_recomp_csp*>(data);
	
	if( frame != NULL )  return frame->opt_P_mc_in_nest_f_recomp_max_eta(x[0]);	
}

double fmin_opt_P_mc_in_nest_f_recomp_max_eta(double x, void *data)
{
	C_sco2_recomp_csp *frame = static_cast<C_sco2_recomp_csp*>(data);

	return -(frame->opt_P_mc_in_nest_f_recomp_max_eta(x));
}

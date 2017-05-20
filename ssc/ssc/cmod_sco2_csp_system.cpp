#include "core.h"

#include <ctime>

#include "sco2_pc_csp_int.h"

static bool ssc_sco2_csp_system_log(std::string &log_msg, std::string &progress_msg, void *data);

static var_info _cm_vtab_sco2_csp_system[] = {

	/*   VARTYPE   DATATYPE         NAME               LABEL                                                    UNITS     META  GROUP REQUIRED_IF CONSTRAINTS     UI_HINTS*/
	// ** Design Parameters **
		// System Design
	{ SSC_INPUT,  SSC_NUMBER,  "htf",                  "Integer code for HTF used in PHX",                       "",           "",    "",      "*",     "",       "" },
    { SSC_INPUT,  SSC_MATRIX,  "htf_props",            "User defined HTF property data",                         "", "7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows", "", "?=[[0]]", "", "" },
	{ SSC_INPUT,  SSC_NUMBER,  "T_htf_hot_des",        "HTF design hot temperature (PHX inlet)",                 "C",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "dT_PHX_hot_approach",  "Temp diff btw hot HTF and turbine inlet",                "C",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "T_amb_des",            "Ambient temperature",                                    "C",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "dT_mc_approach",       "Temp diff btw ambient air and main compressor inlet",    "C",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "site_elevation",       "Site elevation",                                         "m",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "W_dot_net_des",        "Design cycle power output (no cooling parasitics)",      "MWe",        "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "design_method",        "1 = Specify efficiency, 2 = Specify total recup UA",     "",           "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "eta_thermal_des",      "Power cycle thermal efficiency",                         "",           "",    "",      "?=-1.0","",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "UA_recup_tot_des",     "Total recuperator conductance",                          "kW/K",       "",    "",      "?=-1.0","",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "is_recomp_ok",         "1 = Yes, 0 = simple cycle only",                         "",           "",    "",      "?=1",   "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "is_PR_fixed",          "0 = No, >0 = fixed pressure ratio",                      "",           "",    "",      "?=0",   "",       "" },
	// Cycle Design
	{ SSC_INPUT,  SSC_NUMBER,  "eta_isen_mc",          "Design main compressor isentropic efficiency",           "-",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "eta_isen_rc",          "Design re-compressor isentropic efficiency",             "-",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "eta_isen_t",           "Design turbine isentropic efficiency",                   "-",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "LT_recup_eff_max",     "Maximum allowable effectiveness in LT recuperator",      "-",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "HT_recup_eff_max",     "Maximum allowable effectiveness in LT recuperator",      "-",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "P_high_limit",         "High pressure limit in cycle",                           "MPa",        "",    "",      "*",     "",       "" },
		// PHX Design
	{ SSC_INPUT,  SSC_NUMBER,  "dT_PHX_cold_approach", "Temp diff btw cold HTF and cold CO2",                    "C",          "",    "",      "*",     "",       "" },
		// Air Cooler Design
	{ SSC_INPUT,  SSC_NUMBER,  "fan_power_frac",       "Fraction of net cycle power consumed by air cooler fan", "",           "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "deltaP_cooler_frac",   "Fraction of cycle high pressure that is design point cooler CO2 pressure drop", "", "", "", "*","",       "" },
	// ** Off-design Inputs **
	{ SSC_INPUT,  SSC_MATRIX,  "od_cases",             "Columns: T_htf_C, m_dot_htf_ND, T_amb_C, od_opt_obj (1: MAX_ETA, 2: MAX_POWER), Rows: cases",   "",           "",    "",      "",      "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "is_gen_od_polynomials","Generate off-design polynomials for Generic CSP models? 1 = Yes, 0 = No", "", "", "",  "?=0",     "",       "" },

	//{ SSC_INPUT,  SSC_NUMBER,  "is_m_dot_htf_fracs",   "0 = No analysis of HTF off design mass flow rate, 1 = Yes", "",        "",    "",      "*",     "",       "" },
	//{ SSC_INPUT,  SSC_ARRAY,   "m_dot_htf_fracs_in",   "Array of normalized mass flow rate",                     "",           "",    "",      "is_m_dot_htf_fracs=1",     "",       "" },
	//{ SSC_INPUT,  SSC_NUMBER,  "is_T_amb_od",          "0 = N analysis of off design ambient temperature, 1 = Yes", "",        "",    "",      "*",     "",       "" },
	//{ SSC_INPUT,  SSC_ARRAY,   "T_amb_od_in",          "Array of ambient temperatures for off-design parametric","C",          "",    "",      "is_T_amb_od=1",     "",       "" },
	// ** Design OUTPUTS **
		// System Design Solution
	{ SSC_OUTPUT, SSC_NUMBER,  "T_htf_cold_des",       "HTF design cold temperature (PHX outlet)",               "C",          "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "m_dot_htf_des",        "HTF mass flow rate",                                     "kg/s",       "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "eta_thermal_calc",     "Calculated cycle thermal efficiency",                    "-",          "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "m_dot_co2_full",       "CO2 mass flow rate through HTR, PHX, turbine",           "kg/s",       "",    "",      "*",     "",       "" },	
	{ SSC_OUTPUT, SSC_NUMBER,  "recomp_frac",          "Recompression fraction",                                 "-",          "",    "",      "*",     "",       "" },
		// Compressor
	{ SSC_OUTPUT, SSC_NUMBER,  "T_comp_in",            "Compressor inlet temperature",                           "C",          "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "P_comp_in",            "Compressor inlet pressure",                              "MPa",        "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "P_comp_out",           "Compressor outlet pressure",                             "MPa",        "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_phi_des",           "Compressor design flow coefficient",					 "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_tip_ratio_des",     "Compressor design tip speed ratio",                      "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_n_stages",          "Compressor stages",                                      "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_N_des",             "Compressor design shaft speed",                          "rpm",        "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_D",                 "Compressor diameter",                                    "m",          "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_phi_surge",         "Compressor flow coefficient where surge occurs",         "",           "",    "",      "*",     "",       "" },
		// Recompressor
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_phi_des",           "Recompressor design flow coefficient",                   "",           "",    "",      "*",     "",       "" },					
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_tip_ratio1_des",    "Recompressor 1st stage design tip speed ratio",          "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_tip_ratio2_des",    "Recompressor 2nd stage design tip speed ratio",          "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_n_stages",          "Recompressor stages",                                    "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_N_des",             "Recompressor design shaft speed",                        "rpm",        "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_D1",                "Recompressor first stage diameter",                      "m",          "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_D2",                "Recompressor second stage diameter",                     "m",          "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_phi_surge",         "Compressor flow coefficient where surge occurs",         "",           "",    "",      "*",     "",       "" },
		// Turbine
	{ SSC_OUTPUT, SSC_NUMBER,  "t_nu_des",             "Turbine design velocity ratio",                          "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "t_tip_ratio_des",	   "Turbine design tip speed ratio",                         "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "t_N_des",			   "Turbine design shaft speed",	                         "rpm",        "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "t_D",                  "Turbine diameter",                                       "m",          "",    "",      "*",     "",       "" },
		// Recuperators																				 
	{ SSC_OUTPUT, SSC_NUMBER,  "UA_recup_total",       "Total recuperator UA",                                   "kW/K",       "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "UA_LTR",               "Low temp recuperator UA",                                "kW/K",       "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "eff_LTR",              "Low temp recuperator effectiveness",                     "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "NTU_LTR",              "Low temp recuperator NTU",                               "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "UA_HTR",               "High temp recuperator UA",                               "kW/K",       "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "eff_HTR",              "High temp recuperator effectiveness",                    "",           "",    "",      "*",     "",       "" },	
	{ SSC_OUTPUT, SSC_NUMBER,  "NTU_HTR",              "High temp recuperator NTRU",                             "",           "",    "",      "*",     "",       "" },
		// PHX Design Solution
	{ SSC_OUTPUT, SSC_NUMBER,  "UA_PHX",               "PHX Conductance",                                        "kW/K",       "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "eff_PHX",              "PHX effectiveness",                                      "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "NTU_PHX",              "PHX NTU",                                                "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "T_co2_PHX_in",         "CO2 temperature at PHX inlet",                           "C",          "",    "",      "*",     "",       "" },	
		// Cooler
	{ SSC_OUTPUT, SSC_NUMBER,  "T_cooler_in",          "Cooler inlet temperature",                               "C",          "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "P_cooler_in",          "Compressor inlet pressure",                              "MPa",        "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "m_dot_co2_cooler",     "CO2 mass flow rate through cooler",                      "kg/s",       "",    "",      "*",     "",       "" },	
		// State Points
	{ SSC_OUTPUT, SSC_ARRAY,  "T_state_points",       "Cycle temperature state points",      "C",	   "",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "P_state_points",       "Cycle pressure state points",         "MPa",   "",   "",   "*",   "",   "" },

		// Air Cooler Design
	// ?????

	// ** Off-Design Outputs **
		// Parameters
	{ SSC_OUTPUT, SSC_ARRAY,   "m_dot_htf_fracs",      "Normalized mass flow rate",                              "",           "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "T_amb_od",             "Ambient temperatures",                                   "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "T_htf_hot_od",         "HTF hot temperatures",                                   "C",          "",    "",      "",     "",       "" },
		// Cycle control parameters
	{ SSC_OUTPUT, SSC_ARRAY,   "od_opt_obj_code",      "1: MAX_ETA, 2: MAX_POWER",                               "",           "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "od_opt_conv_tol",      "Off design optimizer convergence tolerance",             "",           "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "P_comp_in_od",         "Main compressor inlet pressures",                        "MPa",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_phi_od",            "Main compressor flow coefficient",                       "",           "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "recomp_frac_od",       "Recompression fractions",                                "",           "",    "",      "",     "",       "" },
		// Optimizer outputs
	{ SSC_OUTPUT, SSC_ARRAY,   "sim_time_od",          "Simulation time for off design optimization",            "s",          "",    "",      "",     "",       "" },
		// System solution
	{ SSC_OUTPUT, SSC_ARRAY,   "eta_thermal_od",       "Off-design cycle thermal efficiency",                    "",           "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "P_mc_out",             "Off-design high side pressure",                          "MPa",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "T_htf_cold_od",        "Off-design cold return temperature",                     "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "m_dot_co2_full_od",    "Off-design mass flow rate through turbine",              "kg/s",       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "W_dot_net_od",         "Off-design cycle net output (no cooling pars)",          "MWe",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "Q_dot_od",             "Off-design thermal input",                               "MWt",        "",    "",      "",     "",       "" },
		// Compressor
	{ SSC_OUTPUT, SSC_ARRAY,   "N_mc_od",              "Off-design main compressor speed",                       "rpm",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_tip_ratio_od",      "Off-design main compressor tip speed ratio",             "-",          "",    "",      "",     "",       "" },
		// Recompressor
	{ SSC_OUTPUT, SSC_ARRAY,   "rc_phi_1_od",          "Off-design recompressor 1st stage flow coefficient",     "-",		   "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "rc_phi_2_od",          "Off-design recompressor 2nd stage flow coefficient",     "-",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "rc_N_od",              "Off-design recompressor shaft speed",                    "rpm",		   "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "rc_tip_ratio_od",      "Off-design recompressor tip speed ratio",                "-",		   "",    "",      "",     "",       "" },
		// Turbine																											   
	{ SSC_OUTPUT, SSC_ARRAY,   "t_nu_od",              "Off-design turbine velocity ratio",	                     "-",	       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "t_N_od",               "Off-design turbine shaft speed",	                     "rpm",	       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "t_tip_ratio_od",       "Off-design turbine tip speed ratio",                     "-",          "",    "",      "",     "",       "" },
		// Recuperators
	{ SSC_OUTPUT, SSC_ARRAY,   "eff_LTR_od",           "Off-design low temp recup effectiveness",                "",           "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "eff_HTR_od",           "Off-design high temp recup effectiveness",               "",           "",    "",      "",     "",       "" },
		// PHX 
	{ SSC_OUTPUT, SSC_ARRAY,   "T_co2_PHX_in_od",      "Off-design PHX co2 inlet temperature",                   "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "T_co2_PHX_out_od",     "Off-design PHX co2 outlet temperature",                  "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "phx_eff_od",           "Off-design PHX effectiveness",                           "-",          "",    "",      "",     "",       "" },
		// Solver Metrics
	{ SSC_OUTPUT, SSC_ARRAY,   "od_code",              "Diagnostic info",                                        "-",          ""     "",      "",     "",       "" },

		// Off-design Polynomial Outputs
	{ SSC_OUTPUT, SSC_ARRAY,   "part_load_fracs_out", "Array of part load fractions that SOLVED at off design", "-",          "",    "",      "deltaP_cooler_frac=1", "",  "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "part_load_eta",       "Matrix of power cycle efficiency results for q_dot_in part load", "-", "",    "",      "deltaP_cooler_frac=1", "",  "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "part_load_coefs",     "Part load polynomial coefficients",                      "-",          "",    "",      "deltaP_cooler_frac=1", "",  "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "part_load_r_squared", "Part load curve fit R squared",							"-",          "",    "",      "deltaP_cooler_frac=1", "",  "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "T_amb_array_out",     "Array of ambient temps that SOLVED at off design",       "C",          "",    "",      "deltaP_cooler_frac=1", "",  "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "T_amb_eta",           "Matrix of ambient temps and power cycle efficiency",     "-",          "",    "",      "deltaP_cooler_frac=1", "",  "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "T_amb_coefs",         "Part load polynomial coefficients",                      "-",          "",    "",      "deltaP_cooler_frac=1", "",  "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "T_amb_r_squared",     "T amb curve fit R squared",                              "-",          "",    "",      "deltaP_cooler_frac=1", "",  "" },


	var_info_invalid };

int test_mono_function(double x, double *y);

class cm_sco2_csp_system : public compute_module
{
public:

	// Off-design parameters
	ssc_number_t *p_m_dot_htf_fracs;
	ssc_number_t *p_T_amb_od;
	ssc_number_t *p_T_htf_hot_od;
	// Optimized control parameters
	ssc_number_t *p_od_opt_obj_code;
	ssc_number_t *p_od_opt_conv_tol;
	ssc_number_t *p_P_comp_in_od;
	ssc_number_t *p_mc_phi_od;
	ssc_number_t *p_recomp_frac_od;
	// Optimizer parameters
	ssc_number_t *p_sim_time_od;
	// Systems
	ssc_number_t *p_eta_thermal_od;
	ssc_number_t *p_P_mc_out;
	ssc_number_t *p_T_htf_cold_od;
	ssc_number_t *p_m_dot_co2_full_od;
	ssc_number_t *p_W_dot_net_od;
	ssc_number_t *p_Q_dot_od;
	// Compressor
	ssc_number_t *p_N_mc_od;
	ssc_number_t *p_mc_tip_ratio_od;
	// Recompressor
	ssc_number_t *p_rc_phi_1_od;
	ssc_number_t *p_rc_phi_2_od;
	ssc_number_t *p_rc_N_od;
	ssc_number_t *p_rc_tip_ratio_od;
	// Turbine
	ssc_number_t *p_t_nu_od;
	ssc_number_t *p_t_N_od;
	ssc_number_t *p_t_tip_ratio_od;
	// Recuperator
	ssc_number_t *p_eff_LTR_od;
	ssc_number_t *p_eff_HTR_od;
	// PHX
	ssc_number_t *p_T_co2_PHX_in_od;
	ssc_number_t *p_T_co2_PHX_out_od;
	ssc_number_t *p_phx_eff_od;
	// Solver Metrics
	ssc_number_t *p_od_code;

	cm_sco2_csp_system()
	{
		add_var_info(_cm_vtab_sco2_csp_system);
	}

	void exec() throw(general_error)
	{
		C_sco2_recomp_csp::S_des_par sco2_rc_des_par;
			// System design parameters
		sco2_rc_des_par.m_hot_fl_code = as_integer("htf");							//[-] Integer code for HTF
		sco2_rc_des_par.mc_hot_fl_props = as_matrix("htf_props");					//[-] Custom HTF properties
		sco2_rc_des_par.m_T_htf_hot_in = as_double("T_htf_hot_des")+273.15;			//[K] Convert from C
		sco2_rc_des_par.m_phx_dt_hot_approach = as_double("dT_PHX_hot_approach");	//[K/C] Temperature difference between hot HTF and turbine CO2 inlet
		sco2_rc_des_par.m_T_amb_des = as_double("T_amb_des")+273.15;				//[K] Convert from C
		sco2_rc_des_par.m_dt_mc_approach = as_double("dT_mc_approach");				//[K/C] Temperature difference between ambient air and main compressor inlet
		sco2_rc_des_par.m_elevation = as_double("site_elevation");					//[m] Site elevation
		sco2_rc_des_par.m_W_dot_net = as_double("W_dot_net_des")*1000.0;			//[kWe] Convert from MWe, cycle power output w/o cooling parasitics
			
		sco2_rc_des_par.m_design_method = as_integer("design_method");			//[-] 1 = Specify efficiency, 2 = Specify total recup UA
		if( sco2_rc_des_par.m_design_method == 1 )
		{
			sco2_rc_des_par.m_eta_thermal = as_double("eta_thermal_des");				//[-] Cycle thermal efficiency
			if( sco2_rc_des_par.m_eta_thermal < 0.0 )
			{
				log("For cycle design method = 1, the input cycle thermal efficiency must be greater than 0", SSC_ERROR, -1.0);
				return;
			}
			sco2_rc_des_par.m_UA_recup_tot_des = std::numeric_limits<double>::quiet_NaN();
		}
		else if( sco2_rc_des_par.m_design_method == 2 )
		{
			sco2_rc_des_par.m_UA_recup_tot_des = as_double("UA_recup_tot");		//[kW/K] Total recuperator conductance
			if( sco2_rc_des_par.m_UA_recup_tot_des < 0.0 )
			{
				log("For cycle design method = 2, the input total recuperator conductance must be greater than 0", SSC_ERROR, -1.0);
				return;
			}
			sco2_rc_des_par.m_eta_thermal = std::numeric_limits<double>::quiet_NaN();
		}
		else
		{
			std::string err_msg = util::format("The input cycle design method, %d, is invalid. It must be 1 or 2.", sco2_rc_des_par.m_design_method);
			log(err_msg, SSC_ERROR, -1.0);
		}

		sco2_rc_des_par.m_is_recomp_ok = as_integer("is_recomp_ok");

		double mc_PR_in = as_double("is_PR_fixed");		//[-]
		if (mc_PR_in != 0.0)
		{
			if (mc_PR_in < 0.0)
			{
				sco2_rc_des_par.m_PR_mc_guess = mc_PR_in*1.E3;		//[kPa] convert from MPa
			}
			else
			{
				sco2_rc_des_par.m_PR_mc_guess = mc_PR_in;			//[-] Pressure Ratio!
			}
				sco2_rc_des_par.m_fixed_PR_mc = true;
		}
		else
		{
			sco2_rc_des_par.m_PR_mc_guess = std::numeric_limits<double>::quiet_NaN();
			sco2_rc_des_par.m_fixed_PR_mc = false;
		}

			// Cycle design parameters: hardcode pressure drops, for now
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
		sco2_rc_des_par.m_DP_LT = DP_LT;
		sco2_rc_des_par.m_DP_HT = DP_HT;
		sco2_rc_des_par.m_DP_PC = DP_PC;
		sco2_rc_des_par.m_DP_PHX = DP_PHX;
		sco2_rc_des_par.m_N_sub_hxrs = 10;

		// 1.30.17 twn: try 30k
		//sco2_rc_des_par.m_N_turbine = 3600.0;
		sco2_rc_des_par.m_N_turbine = 30000.0;

		sco2_rc_des_par.m_tol = 1.E-3;
		sco2_rc_des_par.m_opt_tol = 1.E-3;
		
			// Remaining cycle design parameters
		sco2_rc_des_par.m_LT_eff_max = as_double("LT_recup_eff_max");
		sco2_rc_des_par.m_HT_eff_max = as_double("HT_recup_eff_max");
		sco2_rc_des_par.m_eta_mc = as_double("eta_isen_mc");
		sco2_rc_des_par.m_eta_rc = as_double("eta_isen_rc");
		sco2_rc_des_par.m_eta_t = as_double("eta_isen_t");
		sco2_rc_des_par.m_P_high_limit = as_double("P_high_limit")*1000.0;		//[kPa], convert from MPa		
			
			// PHX design parameters
		sco2_rc_des_par.m_phx_dt_cold_approach = as_double("dT_PHX_cold_approach");
			
			// Air cooler parameters
		sco2_rc_des_par.m_frac_fan_power = as_double("fan_power_frac");
		sco2_rc_des_par.m_deltaP_cooler_frac = as_double("deltaP_cooler_frac");

		// For try/catch below
		int out_type = -1;
		std::string out_msg = "";

		// Construction class and design system 
		C_sco2_recomp_csp sco2_recomp_csp;

		// Pass through callback function and pointer
		sco2_recomp_csp.mf_callback_log = ssc_sco2_csp_system_log;
		sco2_recomp_csp.mp_mf_active = (void*)(this);

		try
		{
			sco2_recomp_csp.design(sco2_rc_des_par);
		}
		catch( C_csp_exception &csp_exception )
		{
			// Report warning before exiting with error
			while( sco2_recomp_csp.mc_messages.get_message(&out_type, &out_msg))
			{
				log(out_msg + "\n");
				log("\n");
			}

			throw exec_error("sco2_csp_system", csp_exception.m_error_message);
		}

		// If all calls were successful, log to SSC any messages from sco2_recomp_csp
		while( sco2_recomp_csp.mc_messages.get_message(&out_type, &out_msg) )
		{
			log(out_msg + "\n");
		}

		// Set SSC design outputs
		// System
		double m_dot_htf_design = sco2_recomp_csp.get_phx_des_par()->m_m_dot_hot_des;	//[kg/s]
		double T_htf_cold_calc = sco2_recomp_csp.get_design_solved()->ms_phx_des_solved.m_T_h_out;		//[K]
		assign("T_htf_cold_des", T_htf_cold_calc - 273.15);		//[C] convert from K
		assign("m_dot_htf_des", m_dot_htf_design);				//[kg/s]
		assign("eta_thermal_calc", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_eta_thermal);	//[-]
		assign("m_dot_co2_full", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_m_dot_t);		//[kg/s]
		assign("recomp_frac", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_recomp_frac);		//[-]
		// Compressor
		assign("T_comp_in", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_temp[C_RecompCycle::MC_IN] - 273.15);		//[C] convert from K
		assign("P_comp_in", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_pres[1 - 1] / 1000.0);		//[MPa] convert from kPa
		assign("P_comp_out", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_pres[2 - 1] / 1000.0);		//[MPa] convert from kPa
		assign("mc_phi_des", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_mc_des_solved.m_phi_des);
		assign("mc_tip_ratio_des", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_mc_des_solved.m_w_tip_ratio);		//[-]
		assign("mc_n_stages", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_mc_des_solved.m_n_stages);	//[-]
		assign("mc_N_des", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_mc_des_solved.m_N_design);	//[rpm]
		assign("mc_D", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_mc_des_solved.m_D_rotor);			//[m]
		assign("mc_phi_surge", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_mc_des_solved.m_phi_surge);	//[-]
		// Recompressor
		assign("rc_phi_des", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_rc_des_solved.m_phi_des);	//[-]
		assign("rc_tip_ratio1_des", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_rc_des_solved.m_w_tip_ratio_1);	//[-]
		assign("rc_tip_ratio2_des", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_rc_des_solved.m_w_tip_ratio_2);	//[-]
		assign("rc_n_stages", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_rc_des_solved.m_n_stages);	//[-]
		assign("rc_N_des", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_rc_des_solved.m_N_design);	//[rpm]
		assign("rc_D1", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_rc_des_solved.m_D_rotor);		//[m] 
		assign("rc_D2", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_rc_des_solved.m_D_rotor_2);		//[m]
		assign("rc_phi_surge", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_rc_des_solved.m_phi_surge);//[-]
		// Turbine
		assign("t_nu_des", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_t_des_solved.m_nu_design);           //[-]
		assign("t_tip_ratio_des", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_t_des_solved.m_w_tip_ratio);  //[-]
		assign("t_N_des", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_t_des_solved.m_N_design);			   //[rpm]
		assign("t_D", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_t_des_solved.m_D_rotor);                  //[m]
		// Recuperator
		double UA_LTR = sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_UA_LT;		//[kW/K]
		double UA_HTR = sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_UA_HT;		//[kW/K]
		assign("UA_recup_total", UA_LTR + UA_HTR);		//[kW/K]
		assign("UA_LTR", UA_LTR);						//[kW/K]
		assign("eff_LTR", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_LT_recup_des_solved.m_eff_design);		//[-]
		assign("NTU_LTR", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_LT_recup_des_solved.m_NTU_design);		//[-]
		assign("UA_HTR", UA_HTR);						//[kW/K]
		assign("eff_HTR", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_HT_recup_des_solved.m_eff_design);		//[-]
		assign("NTU_HTR", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_HT_recup_des_solved.m_NTU_design);		//[-]
			// PHX
		assign("UA_PHX",sco2_recomp_csp.get_design_solved()->ms_phx_des_solved.m_UA_design_total);			//[kW/K]
		assign("eff_PHX",sco2_recomp_csp.get_design_solved()->ms_phx_des_solved.m_eff_design);				//[-]
		assign("NTU_PHX",sco2_recomp_csp.get_design_solved()->ms_phx_des_solved.m_NTU_design);				//[-]
		assign("T_co2_PHX_in", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_temp[C_RecompCycle::HTR_HP_OUT] - 273.15);	//[C]
			// Cooler
		assign("T_cooler_in", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_temp[C_RecompCycle::LTR_LP_OUT] - 273.15);	//[C]
		assign("P_cooler_in", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_pres[C_RecompCycle::LTR_LP_OUT] / 1.E3);	//[MPa]
		assign("m_dot_co2_cooler", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_m_dot_mc);			//[kg/s]
			// State Points
		ssc_number_t *p_T_state_points = allocate("T_state_points", C_RecompCycle::RC_OUT+1);
		ssc_number_t *p_P_state_points = allocate("P_state_points", C_RecompCycle::RC_OUT+1);
		for( int i = 0; i < C_RecompCycle::RC_OUT+1; i++ )
		{
			p_T_state_points[i] = (ssc_number_t)sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_temp[i]-273.15;	//[C]
			p_P_state_points[i] = (ssc_number_t)sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_pres[i] / 1.E3;	//[MPa]
		}

		// Check that off-design model matches design-point with same operation inputs
		C_sco2_recomp_csp::S_od_par sco2_rc_od_par;
		double T_htf_hot_in_des = sco2_recomp_csp.get_design_par()->m_T_htf_hot_in;		//[K]
		double m_dot_htf_des = sco2_recomp_csp.get_phx_des_par()->m_m_dot_hot_des;		//[kg/s]
		double T_amb_des = sco2_recomp_csp.get_design_par()->m_T_amb_des;				//[K]
		
		double m_dot_htf_ND = 1.0;
		double T_htf_hot_in_offset = 0;
		sco2_rc_od_par.m_T_htf_hot = T_htf_hot_in_des - T_htf_hot_in_offset;	//[K]	
		sco2_rc_od_par.m_m_dot_htf = m_dot_htf_ND*m_dot_htf_des;				//[kg/s]
		sco2_rc_od_par.m_T_amb = 273.15;										//[K]
					
			// Set-up one-off off-design operation inputs
		// ***************************************************************************
		//sco2_rc_od_op_par.m_P_mc_in = sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_pres[C_RecompCycle::MC_IN];		//[kPa]
		//if( sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_is_rc )
		//{	// Recompression Fraction
		//	sco2_rc_od_op_par.m_recomp_frac = sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_recomp_frac;	//[-]
		//}
		//sco2_rc_od_op_par.m_phi_mc = sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_mc_des_solved.m_phi_des;	//[-]
		//	
		//sco2_rc_od_op_par.m_P_mc_in = 7534.0;		//[kPa]
		//if( sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_is_rc )
		//{	// Recompression Fraction
		//	sco2_rc_od_op_par.m_recomp_frac = 0.1556;	//[-]
		//}
		//sco2_rc_od_op_par.m_phi_mc = 0.002971;	//[-]
		//
		//	// Call off_design
		//int sco2_od_code = sco2_recomp_csp.off_design(sco2_rc_od_par, sco2_rc_od_op_par);
		// ***************************************************************************
		// ***************************************************************************


		bool is_od_input_screen = false;
		if( is_od_input_screen )
		{
			// Set up parametric sweep of operation inputs
			double f_recomp_start = 0.15;
			double f_recomp_end = 0.26;
			int n_f_recomp = 23;
			double f_recomp_step = (f_recomp_end - f_recomp_start) / (n_f_recomp - 1);

			std::vector<double> v_f_recomp(n_f_recomp);	//[-]
			
			allocate_ssc_outputs(n_f_recomp);

			// Set off-design parameters
			sco2_rc_od_par.m_T_htf_hot = sco2_recomp_csp.get_design_par()->m_T_htf_hot_in;		//[K]
			sco2_rc_od_par.m_m_dot_htf = sco2_recomp_csp.get_phx_des_par()->m_m_dot_hot_des;	//[kg/s]
			sco2_rc_od_par.m_T_amb = sco2_recomp_csp.get_design_par()->m_T_amb_des;				//[K]

			sco2_recomp_csp.off_design_nested_opt(sco2_rc_od_par, C_sco2_recomp_csp::E_MAX_ETA_FIX_PHI);

			C_sco2_recomp_csp::S_od_operation_inputs sco2_rc_od_op_par;
				// Keep these constant, for this analysis
			sco2_rc_od_op_par.m_P_mc_in = sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_pres[C_RecompCycle::MC_IN];		//[kPa]
			sco2_rc_od_op_par.m_phi_mc = sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_mc_des_solved.m_phi_des;	//[-]

			double T_mc_in_sweep = sco2_rc_od_par.m_T_amb + sco2_recomp_csp.get_design_par()->m_dt_mc_approach;	//[K]

			sco2_recomp_csp.sweep_turbomachinery_deltaP(T_mc_in_sweep, sco2_rc_od_op_par.m_P_mc_in,
											sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_temp[C_RecompCycle::TURB_IN],
											sco2_rc_od_op_par.m_phi_mc);

			for(int n_run = 0; n_run < n_f_recomp; n_run++)
			{
				p_T_htf_hot_od[n_run] = sco2_rc_od_par.m_T_htf_hot - 273.15;			//[C]
				p_m_dot_htf_fracs[n_run] = sco2_rc_od_par.m_m_dot_htf / m_dot_htf_des;	//[-]
				p_T_amb_od[n_run] = sco2_rc_od_par.m_T_amb - 273.15;					//[C]
				p_od_opt_obj_code[n_run] = 0;		//[-]
				p_od_opt_conv_tol[n_run] = 0;		//[-]
				
				double f_recomp_od = f_recomp_start + f_recomp_step*n_run;
				
				sco2_rc_od_op_par.m_recomp_frac = f_recomp_od;	//[-]

				int sco2_od_code = 0;
				std::clock_t clock_start = std::clock();

				try
				{
					sco2_od_code = sco2_recomp_csp.off_design(sco2_rc_od_par, sco2_rc_od_op_par);
				}
				catch( C_csp_exception &csp_exception )
				{
					// Report warning before exiting with error
					while( sco2_recomp_csp.mc_messages.get_message(&out_type, &out_msg) )
					{
						log(out_msg);
					}

					log(csp_exception.m_error_message, SSC_ERROR, -1.0);

					return;				
				}
				std::clock_t clock_end = std::clock();

				double od_opt_duration = (clock_end - clock_start) / (double)CLOCKS_PER_SEC;		//[s]

				p_od_code[n_run] = sco2_od_code;
				// Can we just... see what happens getting metrics from sco2_recomp_csp when off-design fails
					// Control parameters
				p_P_comp_in_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.m_pres[1 - 1] / 1000.0;	//[MPa]
				p_mc_phi_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_mc_od_solved.m_phi;	//[-]
				p_recomp_frac_od[n_run] = sco2_rc_od_op_par.m_recomp_frac;		//[-]
					// Optimizer parameters
				p_sim_time_od[n_run] = od_opt_duration;		//[s]
					// System
				p_eta_thermal_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.m_eta_thermal;		//[-]
				p_P_mc_out[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_RecompCycle::MC_OUT] / 1.E3;	//[MPa]
				p_T_htf_cold_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_phx_od_solved.m_T_h_out - 273.15;		//[C]
				p_m_dot_co2_full_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.m_m_dot_t;		//[kg/s]
				p_W_dot_net_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.m_W_dot_net / 1.E3;	//[MWe]
				p_Q_dot_od[n_run] = p_W_dot_net_od[n_run] / p_eta_thermal_od[n_run];		//[MWt]
					// Compressor
				p_N_mc_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_mc_od_solved.m_N;		//[rpm]
				p_mc_tip_ratio_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_mc_od_solved.m_w_tip_ratio;	//[-]
					// Recompressor
				p_rc_phi_1_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_od_solved.m_phi;	//[-]
				p_rc_phi_2_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_od_solved.m_phi_2;	//[-]
				p_rc_N_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_od_solved.m_N;			//[rpm]
				p_rc_tip_ratio_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_od_solved.m_w_tip_ratio;	//[-]
					// Turbine
				p_t_nu_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_t_od_solved.m_nu;		//[-]
				p_t_N_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_t_od_solved.m_N;		//[rpm]
				p_t_tip_ratio_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_t_od_solved.m_w_tip_ratio;	//[-]
					// Recuperator
				p_eff_LTR_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_LT_recup_od_solved.m_eff;	//[-]
				p_eff_HTR_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_HT_recup_od_solved.m_eff;	//[-]
					// PHX
				p_T_co2_PHX_in_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_RecompCycle::HTR_HP_OUT] - 273.15;	//[C]
				p_T_co2_PHX_out_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_RecompCycle::TURB_IN] - 273.15;		//[C]
				p_phx_eff_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_phx_od_solved.m_eff;		//[-]

			}

			return;

			//int n_P_mc_in = 5;
			//std::vector<double> v_P_mc_in(n_P_mc_in);	//[kPa]
			//v_P_mc_in[0] = sco2_rc_od_op_par.m_P_mc_in - 1000.0;	//[kPa]
			//v_P_mc_in[1] = sco2_rc_od_op_par.m_P_mc_in - 500.0;	//[kPa]
			//v_P_mc_in[2] = sco2_rc_od_op_par.m_P_mc_in;				//[kPa]
			//v_P_mc_in[3] = sco2_rc_od_op_par.m_P_mc_in + 500.0;	//[kPa]
			//v_P_mc_in[4] = sco2_rc_od_op_par.m_P_mc_in + 1000.0;	//[kPa]

			//int n_f_recomp = 101;
			//std::vector<double> v_f_recomp(n_f_recomp);	//[-]
			//v_f_recomp[0] = sco2_rc_od_op_par.m_recomp_frac - 0.05;	//[-]
			//v_f_recomp[1] = sco2_rc_od_op_par.m_recomp_frac - 0.025;	//[-]
			//v_f_recomp[2] = sco2_rc_od_op_par.m_recomp_frac;		//[-]
			//v_f_recomp[3] = sco2_rc_od_op_par.m_recomp_frac + 0.025;	//[-]
			//v_f_recomp[4] = sco2_rc_od_op_par.m_recomp_frac + 0.05;	//[-]

			//int n_phi_mc = 5;
			//std::vector<double> v_phi_mc(n_phi_mc);	//[-]
			//double phi_min = sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_mc_des_solved.m_phi_surge;
			//double phi_max = sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_mc_des_solved.m_phi_max;
			//v_phi_mc[0] = sco2_rc_od_op_par.m_phi_mc*0.8 + phi_min*0.8;
			//v_phi_mc[1] = sco2_rc_od_op_par.m_phi_mc*0.9 + phi_min*0.1;
			//v_phi_mc[2] = sco2_rc_od_op_par.m_phi_mc;
			//v_phi_mc[3] = sco2_rc_od_op_par.m_phi_mc*0.9 + phi_max*0.1;
			//v_phi_mc[4] = sco2_rc_od_op_par.m_phi_mc*0.8 * phi_max*0.2;
			//
			//for(int i = 0; i < n_P_mc_in; i++)
			//{
			//	for(int j = 0; j < n_f_recomp; j++)
			//	{
			//		for(int k = 0; k < n_phi_mc; k++)
			//		{
			//			sco2_rc_od_op_par.m_P_mc_in = v_P_mc_in[i];			//[kPa]
			//			sco2_rc_od_op_par.m_recomp_frac = v_f_recomp[j];	//[-]
			//			sco2_rc_od_op_par.m_phi_mc = v_phi_mc[k];			//[-]

			//			int od_err_code = sco2_recomp_csp.off_design(sco2_rc_od_par, sco2_rc_od_op_par);

			//			double blah = 1.23;
			//		}
			//	}		 
			//}
		}

		// ********************************************************************
		//  Generate off-design polynomials, if necessary
		bool is_gen_od_poly = as_boolean("is_gen_od_polynomials");
		if( is_gen_od_poly )
		{
			// We are never changing the HTF hot temperature and Optimization strategy & tolerance
			sco2_rc_od_par.m_T_htf_hot = T_htf_hot_in_des;			//[K]
			int od_strategy = C_sco2_recomp_csp::E_MAX_ETA_FIX_PHI;	//[-]
			double od_opt_tol = 1.E-3;			//[-]
			
			// For the part-load parameterics, we can set ambient temperature
			sco2_rc_od_par.m_T_amb = T_amb_des;

			double part_load_max = 1.1;		//[-]
			double part_load_min = 0.5;		//[-]
			double part_load_step = 0.1;	//[-]

			std::vector<double> part_load_fracs_out(0);
			std::vector<double> part_load_eta(0);

			// First, solve for part-load conditions
			for(double part_load = part_load_max; part_load >= part_load_min; part_load = part_load - part_load_step)
			{
				// Set input structure
				sco2_rc_od_par.m_m_dot_htf = m_dot_htf_design*part_load;	//[kg/s]

				int off_design_code = 0;
				try
				{
					off_design_code = sco2_recomp_csp.off_design_opt(sco2_rc_od_par, od_strategy, od_opt_tol);
				}
				catch( C_csp_exception &csp_exception )
				{
					// Report warning before exiting with error
					while( sco2_recomp_csp.mc_messages.get_message(&out_type, &out_msg) )
					{
						log(out_msg);
					}

					log(csp_exception.m_error_message, SSC_ERROR, -1.0);

					return;
				}

				if( off_design_code != 0 )
				{	// Off-design call was successful, so write outputs
					log("Part load off design calcs for polynomials failed");
					return;
				}
			
				part_load_fracs_out.push_back(part_load);
				part_load_eta.push_back(sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.m_eta_thermal / sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_eta_thermal );
			}

			// Next, solve for off-design ambient temperature conditions
				// Reset mass flow rate to design
			sco2_rc_od_par.m_m_dot_htf = m_dot_htf_design;	//[kg/s]

			double T_amb_high = 50.0;		//[C]
			double T_amb_low = 34.0 - sco2_recomp_csp.get_design_par()->m_dt_mc_approach;	//[C]
			double T_amb_step = 5.0;		//[C/K]

			std::vector<double> T_amb_out(0);
			std::vector<double> T_amb_eta(0);

			for(double T_amb_C = T_amb_high; T_amb_C >= T_amb_low; T_amb_C = T_amb_C - T_amb_step)
			{
				// Set input structure
				sco2_rc_od_par.m_T_amb = T_amb_C + 273.15;		//[K]

				int off_design_code = 0;
				try
				{
					off_design_code = sco2_recomp_csp.off_design_opt(sco2_rc_od_par, od_strategy, od_opt_tol);
				}
				catch( C_csp_exception &csp_exception )
				{
					// Report warning before exiting with error
					while( sco2_recomp_csp.mc_messages.get_message(&out_type, &out_msg) )
					{
						log(out_msg);
					}

					log(csp_exception.m_error_message, SSC_ERROR, -1.0);

					return;
				}

				if( off_design_code != 0 )
				{	// Off-design call was successful, so write outputs
					log("Part load off design calcs for polynomials failed");
					return;
				}

				T_amb_out.push_back(T_amb_C);
				T_amb_eta.push_back(sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.m_eta_thermal / sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_eta_thermal);
			}

			// Ok, if we've made it this far, let's generate polynomial coefficients
				// Part-load
			std::vector<double> pl_coefs;
			double pl_r_squared = std::numeric_limits<double>::quiet_NaN();
			int n_coefs = 4;
			bool pl_poly_success = find_polynomial_coefs(part_load_fracs_out, part_load_eta, n_coefs, pl_coefs, pl_r_squared);

			if( !pl_poly_success )
			{
				log("Part load polynomial coefficient generation failed");
				return;
			}

			int n_pl_runs = part_load_fracs_out.size();
			ssc_number_t * p_part_load_fracs_out = allocate("part_load_fracs_out", n_pl_runs);
			ssc_number_t * p_part_load_eta = allocate("part_load_eta", n_pl_runs);
			for(int i = 0; i < n_pl_runs; i++)
			{
				p_part_load_fracs_out[i] = part_load_fracs_out[i];
				p_part_load_eta[i] = part_load_eta[i];
			}

			assign("part_load_r_squared", pl_r_squared);
			ssc_number_t * p_part_load_coefs = allocate("part_load_coefs", n_coefs);
			for(int i = 0; i < n_coefs; i++)
				p_part_load_coefs[i] = pl_coefs[i];

				// Ambient temperature
			std::vector<double> T_amb_coefs;
			std::vector<double> T_amb_od_less_des;
			double T_amb_r_squared = std::numeric_limits<double>::quiet_NaN();

			int n_T_amb_runs = T_amb_out.size();
			
			T_amb_od_less_des.resize(n_T_amb_runs);
			for(int i = 0; i < n_T_amb_runs; i++)
				T_amb_od_less_des[i] = T_amb_out[i] - (T_amb_des - 273.15);		//[C]

			bool T_amb_poly_success = find_polynomial_coefs(T_amb_od_less_des, T_amb_eta, n_coefs, T_amb_coefs, T_amb_r_squared);

			if( !T_amb_poly_success )
			{
				log("Ambient temperature polynomial coefficient generation failed");
				return;
			}

			ssc_number_t * p_T_amb_array_out = allocate("T_amb_array_out", n_T_amb_runs);
			ssc_number_t * p_T_amb_eta = allocate("T_amb_eta", n_T_amb_runs);
			for(int i = 0; i < n_T_amb_runs; i++)
			{
				p_T_amb_array_out[i] = T_amb_out[i];
				p_T_amb_eta[i] = T_amb_eta[i];
			}

			assign("T_amb_r_squared", T_amb_r_squared);
			ssc_number_t * p_T_amb_coefs = allocate("T_amb_coefs", n_coefs);
			for(int i = 0; i < n_coefs; i++)
				p_T_amb_coefs[i] = T_amb_coefs[i];
		}

		
		// Set up off-design analysis
		util::matrix_t<double> od_cases = as_matrix("od_cases");

		// Check if off cases exist and correctly formatted
		int n_od_cols = od_cases.ncols();
		int n_od_runs = od_cases.nrows();

		if( n_od_cols != 5 && n_od_runs == 1 )
		{
			// No off-design cases specified
			log("No off-design cases specified");
			return;
		}
		if( n_od_cols != 5 && n_od_runs > 1 )
		{
			std::string err_msg = util::format("The matrix of off design cases requires 3 columns. The entered matrix has %d columns", n_od_cols);
			throw exec_error("sco2_csp_system", err_msg);
		}

		allocate_ssc_outputs(n_od_runs);

		for(int n_run = 0; n_run < n_od_runs; n_run++)
		{

			
			// Try calling off-design model with design parameters
				// Set outputs
		 	p_T_htf_hot_od[n_run] = od_cases(n_run, 0);			//[C]
			p_m_dot_htf_fracs[n_run] = od_cases(n_run, 1);		//[-]
			p_T_amb_od[n_run] = od_cases(n_run, 2);				//[C]
			p_od_opt_obj_code[n_run] = od_cases(n_run, 3);		//[-]
			p_od_opt_conv_tol[n_run] = od_cases(n_run, 4);		//[-]
				// Set input structure
			sco2_rc_od_par.m_T_htf_hot = p_T_htf_hot_od[n_run] + 273.15;	//[K]
			sco2_rc_od_par.m_m_dot_htf = m_dot_htf_design*p_m_dot_htf_fracs[n_run];	//[kg/s]
			sco2_rc_od_par.m_T_amb = p_T_amb_od[n_run] + 273.15;				//[K]
			int od_strategy = (int)p_od_opt_obj_code[n_run];		//[-]
			double od_opt_tol = p_od_opt_conv_tol[n_run];			//[-]

			int off_design_code = 0;
			std::clock_t clock_start = std::clock();
			try
			{
					// 2D optimization
				//off_design_code = sco2_recomp_csp.off_design_opt(sco2_rc_od_par, od_strategy);
					// Nested optimization
				off_design_code = sco2_recomp_csp.off_design_nested_opt(sco2_rc_od_par, od_strategy);
			}
			catch( C_csp_exception &csp_exception )
			{
				// Report warning before exiting with error
				while( sco2_recomp_csp.mc_messages.get_message(&out_type, &out_msg) )
				{
					log(out_msg);
				}

				log(csp_exception.m_error_message, SSC_ERROR, -1.0);
				throw exec_error("sco2_csp_system", csp_exception.m_error_message);
			}

			std::clock_t clock_end = std::clock();

			double od_opt_duration = (clock_end - clock_start)/(double) CLOCKS_PER_SEC;		//[s]

			p_od_code[n_run] = off_design_code;
			if(off_design_code == 0)
			{	// Off-design call was successful, so write outputs
					// Control parameters
				p_P_comp_in_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.m_pres[1-1]/1000.0;	//[MPa]
				p_mc_phi_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_mc_od_solved.m_phi;	//[-]
				p_recomp_frac_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.m_recomp_frac;		//[-]
					// Optimizer parameters
				p_sim_time_od[n_run] = od_opt_duration;		//[s]
					// System
				p_eta_thermal_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.m_eta_thermal;		//[-]
				p_P_mc_out[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_RecompCycle::MC_OUT]/1.E3;	//[MPa]
				p_T_htf_cold_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_phx_od_solved.m_T_h_out - 273.15;		//[C]
				p_m_dot_co2_full_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.m_m_dot_t;		//[kg/s]
				p_W_dot_net_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.m_W_dot_net/1.E3;	//[MWe]
				p_Q_dot_od[n_run] = p_W_dot_net_od[n_run] / p_eta_thermal_od[n_run];		//[MWt]
					// Compressor
				p_N_mc_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_mc_od_solved.m_N;		//[rpm]
				p_mc_tip_ratio_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_mc_od_solved.m_w_tip_ratio;	//[-]
					// Recompressor
				p_rc_phi_1_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_od_solved.m_phi;	//[-]
				p_rc_phi_2_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_od_solved.m_phi_2;	//[-]
				p_rc_N_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_od_solved.m_N;			//[rpm]
				p_rc_tip_ratio_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_od_solved.m_w_tip_ratio;	//[-]
					// Turbine
				p_t_nu_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_t_od_solved.m_nu;		//[-]
				p_t_N_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_t_od_solved.m_N;		//[rpm]
				p_t_tip_ratio_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_t_od_solved.m_w_tip_ratio;	//[-]
					// Recuperator
				p_eff_LTR_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_LT_recup_od_solved.m_eff;	//[-]
				p_eff_HTR_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_HT_recup_od_solved.m_eff;	//[-]
					// PHX
				p_T_co2_PHX_in_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_RecompCycle::HTR_HP_OUT] - 273.15;	//[C]
				p_T_co2_PHX_out_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_RecompCycle::TURB_IN] - 273.15;		//[C]
				p_phx_eff_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_phx_od_solved.m_eff;		//[-]
			}
			else
			{	// Off-design call failed, write NaN outptus
					// Control parameters
				p_P_comp_in_od[n_run] = std::numeric_limits<double>::quiet_NaN();
				p_mc_phi_od[n_run] = std::numeric_limits<double>::quiet_NaN();
				p_recomp_frac_od[n_run] = std::numeric_limits<double>::quiet_NaN();
					// System
				p_eta_thermal_od[n_run] = std::numeric_limits<double>::quiet_NaN();
				p_P_mc_out[n_run] = std::numeric_limits<double>::quiet_NaN();
				p_T_htf_cold_od[n_run] = std::numeric_limits<double>::quiet_NaN();
				p_m_dot_co2_full_od[n_run] = std::numeric_limits<double>::quiet_NaN();
				p_W_dot_net_od[n_run] = std::numeric_limits<double>::quiet_NaN();
				p_Q_dot_od[n_run] = std::numeric_limits<double>::quiet_NaN();
					// Compressor
				p_N_mc_od[n_run] = std::numeric_limits<double>::quiet_NaN();
				p_mc_tip_ratio_od[n_run] = std::numeric_limits<double>::quiet_NaN();
					// Recompressor
				p_rc_phi_1_od[n_run] = std::numeric_limits<double>::quiet_NaN();
				p_rc_phi_2_od[n_run] = std::numeric_limits<double>::quiet_NaN();
				p_rc_N_od[n_run] = std::numeric_limits<double>::quiet_NaN();
				p_rc_tip_ratio_od[n_run] = std::numeric_limits<double>::quiet_NaN();
					// Turbine
				p_t_nu_od[n_run] = std::numeric_limits<double>::quiet_NaN();
				p_t_N_od[n_run] = std::numeric_limits<double>::quiet_NaN();
				p_t_tip_ratio_od[n_run] = std::numeric_limits<double>::quiet_NaN();
					// Recuperator
				p_eff_LTR_od[n_run] = std::numeric_limits<double>::quiet_NaN();
				p_eff_HTR_od[n_run] = std::numeric_limits<double>::quiet_NaN();
					// PHX
				p_T_co2_PHX_in_od[n_run] = std::numeric_limits<double>::quiet_NaN();
				p_T_co2_PHX_out_od[n_run] = std::numeric_limits<double>::quiet_NaN();
				p_phx_eff_od[n_run] = std::numeric_limits<double>::quiet_NaN();
			}


		}


		// If all calls were successful, log to SSC any messages from sco2_recomp_csp
		while( sco2_recomp_csp.mc_messages.get_message(&out_type, &out_msg) )
		{
			log(out_msg + "\n");
		}
		
	}

	void allocate_ssc_outputs(int n_od_runs)
	{
		// Off-design parameters
		p_m_dot_htf_fracs = allocate("m_dot_htf_fracs", n_od_runs);
		p_T_amb_od = allocate("T_amb_od", n_od_runs);
		p_T_htf_hot_od = allocate("T_htf_hot_od", n_od_runs);
		// Optimized control parameters
		p_od_opt_obj_code = allocate("od_opt_obj_code", n_od_runs);
		p_od_opt_conv_tol = allocate("od_opt_conv_tol", n_od_runs);
		p_P_comp_in_od = allocate("P_comp_in_od", n_od_runs);
		p_mc_phi_od = allocate("mc_phi_od", n_od_runs);
		p_recomp_frac_od = allocate("recomp_frac_od", n_od_runs);
		// Optimizer parameters
		p_sim_time_od = allocate("sim_time_od", n_od_runs);
		// Systems
		p_eta_thermal_od = allocate("eta_thermal_od", n_od_runs);
		p_P_mc_out = allocate("P_mc_out", n_od_runs);
		p_T_htf_cold_od = allocate("T_htf_cold_od", n_od_runs);
		p_m_dot_co2_full_od = allocate("m_dot_co2_full_od", n_od_runs);
		p_W_dot_net_od = allocate("W_dot_net_od", n_od_runs);
		p_Q_dot_od = allocate("Q_dot_od", n_od_runs);
		// Compressor
		p_N_mc_od = allocate("N_mc_od", n_od_runs);
		p_mc_tip_ratio_od = allocate("mc_tip_ratio_od", n_od_runs);
		// Recompressor
		p_rc_phi_1_od = allocate("rc_phi_1_od", n_od_runs);
		p_rc_phi_2_od = allocate("rc_phi_2_od", n_od_runs);
		p_rc_N_od = allocate("rc_N_od", n_od_runs);
		p_rc_tip_ratio_od = allocate("rc_tip_ratio_od", n_od_runs);
		// Turbine
		p_t_nu_od = allocate("t_nu_od", n_od_runs);
		p_t_N_od = allocate("t_N_od", n_od_runs);
		p_t_tip_ratio_od = allocate("t_tip_ratio_od", n_od_runs);
		// Recuperator
		p_eff_LTR_od = allocate("eff_LTR_od", n_od_runs);
		p_eff_HTR_od = allocate("eff_HTR_od", n_od_runs);
		// PHX
		p_T_co2_PHX_in_od = allocate("T_co2_PHX_in_od", n_od_runs);
		p_T_co2_PHX_out_od = allocate("T_co2_PHX_out_od", n_od_runs);
		p_phx_eff_od = allocate("phx_eff_od", n_od_runs);
		// Solver Metrics
		p_od_code = allocate("od_code", n_od_runs);

		return;
	}

};

static bool ssc_sco2_csp_system_log(std::string &log_msg, std::string &progress_msg, void *data)
{
	compute_module *cm = static_cast<compute_module*> (data);
	if (!cm)
		return false;

	cm->log(log_msg);
	bool ret = cm->update(progress_msg, 0.0);

	return ret;
}

DEFINE_MODULE_ENTRY(sco2_csp_system, "...", 0)

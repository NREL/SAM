#include "core.h"

#include "sco2_pc_csp_int.h"

static var_info _cm_vtab_sco2_csp_ud_pc_tables[] = {

	/*   VARTYPE   DATATYPE         NAME               LABEL                                                    UNITS     META  GROUP REQUIRED_IF CONSTRAINTS     UI_HINTS*/
	// ** Design Parameters **
		// System Design
	{ SSC_INPUT,  SSC_NUMBER,  "htf",                  "Integer code for HTF used in PHX",                       "",           "",    "",      "*",     "",       "" },
    { SSC_INPUT,  SSC_MATRIX,  "htf_props",            "User defined HTF property data",                         "", "7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows", "", "*", "", "" },
	{ SSC_INPUT,  SSC_NUMBER,  "T_htf_hot_des",        "HTF design hot temperature (PHX inlet)",                 "C",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "dT_PHX_hot_approach",  "Temp diff btw hot HTF and turbine inlet",                "C",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "T_amb_des",            "Ambient temperature",                                    "C",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "dT_mc_approach",       "Temp diff btw ambient air and main compressor inlet",    "C",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "site_elevation",       "Site elevation",                                         "m",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "W_dot_net_des",        "Design cycle power output (no cooling parasitics)",      "MWe",        "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "design_method",        "1 = Specify efficiency, 2 = Specify total recup UA",     "",           "",    "",      "?=1",   "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "eta_thermal_des",      "Power cycle thermal efficiency",                         "",           "",    "",      "?=-1.0","",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "UA_recup_tot_des",     "Total recuperator conductance",                          "kW/K",       "",    "",      "?=-1.0","",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "is_recomp_ok",         "1 = Yes, 0 = simple cycle only",                         "",           "",    "",      "?=1",   "",       "" },
		// Cycle Design
	{ SSC_INPUT,  SSC_NUMBER,  "eta_isen_mc",          "Design main compressor isentropic efficiency",           "-",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "eta_isen_rc",          "Design re-compressor isentropic efficiency",             "-",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "eta_isen_t",           "Design turbine isentropic efficiency",                   "-",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "LT_recup_eff_max",     "Maximum allowable effectiveness in LT recuperator",      "-",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "HT_recup_eff_max",     "Maximum allowable effectiveness in HT recuperator",      "-",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "P_high_limit",         "High pressure limit in cycle",                           "MPa",        "",    "",      "*",     "",       "" },
		// PHX Design
	{ SSC_INPUT,  SSC_NUMBER,  "dT_PHX_cold_approach", "Temp diff btw cold HTF and cold CO2",                    "C",          "",    "",      "*",     "",       "" },
		// Air Cooler Design
	{ SSC_INPUT,  SSC_NUMBER,  "fan_power_frac",       "Fraction of net cycle power consumed by air cooler fan", "",           "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "deltaP_cooler_frac",   "Fraction of cycle high pressure that is design point cooler CO2 pressure drop", "", "", "", "*","",       "" },
		// User Defined Power Cycle Table Inputs
	{ SSC_INPUT,  SSC_NUMBER,  "T_htf_hot_low",        "Lower level of HTF hot temperature",					  "C",         "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "T_htf_hot_high",	   "Upper level of HTF hot temperature",					  "C",		   "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "n_T_htf_hot",		   "Number of HTF hot temperature parametric runs",			  "",		   "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "T_amb_low",			   "Lower level of ambient temperature",					  "C",		   "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "T_amb_high",		   "Upper level of ambient temperature",					  "C",		   "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "n_T_amb",			   "Number of ambient temperature parametric runs",			  "",		   "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "m_dot_htf_ND_low",	   "Lower level of normalized HTF mass flow rate",			  "",		   "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "m_dot_htf_ND_high",	   "Upper level of normalized HTF mass flow rate",			  "",		   "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "n_m_dot_htf_ND",	   "Number of normalized HTF mass flow rate parametric runs", "",		   "",    "",      "*",     "",       "" },

	// ** Design OUTPUTS **
		// System Design Solution
	{ SSC_OUTPUT, SSC_NUMBER,  "T_htf_cold_des",       "HTF design colde temperature (PHX outlet)",              "C",          "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "m_dot_htf_des",        "HTF mass flow rate",                                     "kg/s",       "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "eta_thermal_calc",     "Calculated cycle thermal efficiency",                    "-",          "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "m_dot_co2_full",       "CO2 mass flow rate through HTR, PHX, turbine",           "kg/s",       "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "recomp_frac",          "Recompression fraction",                                 "-",          "",    "",      "*",     "",       "" },
		// Compressor
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
	{ SSC_OUTPUT, SSC_NUMBER,  "UA_HTR",               "High temp recuperator UA",                               "kW/K",       "",    "",      "*",     "",       "" },
		// PHX Design Solution
	{ SSC_OUTPUT, SSC_NUMBER,  "UA_PHX",               "PHX Conductance",                                        "kW/K",       "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "eff_PHX",              "PHX effectiveness",                                      "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "NTU_PHX",              "PHX NTU",                                                "",           "",    "",      "*",     "",       "" },
		// Air Cooler Design
	// ?????
		// State Points
	{ SSC_OUTPUT, SSC_ARRAY,  "T_co2_des",             "Array of cycle CO2 state point temps",	                 "C",          "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "P_co2_des",             "Array of cycle CO2 state point pressures",               "MPa",        "",    "",      "*",     "",       "" },

	// Power Cycle Tables
	{ SSC_OUTPUT, SSC_MATRIX,  "T_htf_ind",            "Parametric of HTF temperature w/ ND HTF mass flow rate levels",     "",       "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_MATRIX,  "T_amb_ind",            "Parametric of ambient temp w/ HTF temp levels",                     "",       "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_MATRIX,  "m_dot_htf_ND_ind",     "Parametric of ND HTF mass flow rate w/ ambient temp levels",        "",       "",    "",      "*",     "",       "" },

	var_info_invalid };

int test_mono_function(double x, double *y);

class cm_sco2_csp_ud_pc_tables : public compute_module
{
public:

	cm_sco2_csp_ud_pc_tables()
	{
		add_var_info(_cm_vtab_sco2_csp_ud_pc_tables);
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
		sco2_rc_des_par.m_eta_thermal = as_double("eta_thermal_des");				//[-] Cycle thermal efficiency
			
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
		sco2_rc_des_par.m_N_turbine = 3600.0;
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
		try
		{
			sco2_recomp_csp.design(sco2_rc_des_par);
		}
		catch( C_csp_exception &csp_exception )
		{
			// Report warning before exiting with error
			while( sco2_recomp_csp.mc_messages.get_message(&out_type, &out_msg))
			{
				log(out_msg);
			}

			log(csp_exception.m_error_message, SSC_ERROR, -1.0);

			return;
		}

		// Set SSC design outputs
			// System
		double m_dot_htf_design = sco2_recomp_csp.get_phx_des_par()->m_m_dot_hot_des;	//[kg/s]
		double T_htf_cold_calc = sco2_recomp_csp.get_design_solved()->ms_phx_des_solved.m_T_h_out;		//[K]
		assign("T_htf_cold_des",T_htf_cold_calc-273.15);		//[C] convert from K
		assign("m_dot_htf_des",m_dot_htf_design);				//[kg/s]
		assign("eta_thermal_calc", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_eta_thermal);	//[-]
		assign("m_dot_co2_full", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_m_dot_t);		//[kg/s]
		assign("recomp_frac", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_recomp_frac);		//[-]
			// Compressor
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
		assign("rc_phi_surge", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_rc_des_solved.m_phi_surge );//[-]
			// Turbine
		assign("t_nu_des", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_t_des_solved.m_nu_design);           //[-]
		assign("t_tip_ratio_des", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_t_des_solved.m_w_tip_ratio);  //[-]
		assign("t_N_des", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_t_des_solved.m_N_design);			   //[rpm]
		assign("t_D", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_t_des_solved.m_D_rotor);                  //[m]
			// Recuperator
		double UA_LTR = sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_UA_LT;		//[kW/K]
		double UA_HTR = sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_UA_HT;		//[kW/K]
		assign("UA_recup_total",UA_LTR + UA_HTR);		//[kW/K]
		assign("UA_LTR",UA_LTR);						//[kW/K]
		assign("UA_HTR",UA_HTR);						//[kW/K]
			// PHX
		assign("UA_PHX",sco2_recomp_csp.get_design_solved()->ms_phx_des_solved.m_UA_design_total);			//[kW/K]
		assign("eff_PHX",sco2_recomp_csp.get_design_solved()->ms_phx_des_solved.m_eff_design);				//[-]
		assign("NTU_PHX",sco2_recomp_csp.get_design_solved()->ms_phx_des_solved.m_NTU_design);				//[-]
			// Air Cooler

			// State Points
		int n_sp = C_RecompCycle::RC_OUT + 1;
		ssc_number_t *p_T_co2_des = allocate("T_co2_des", n_sp);
		ssc_number_t *p_P_co2_des = allocate("P_co2_des", n_sp);
		for(int i = 0; i < n_sp; i++)
		{
			p_T_co2_des[i] = sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_temp[i]-273.15;		//[C]
			p_P_co2_des[i] = sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_pres[i]/1.E3;		//[MPa]
		}


		// Get user-defined power cycle parameters
		double T_htf_hot_low = as_double("T_htf_hot_low");		//[C]
		double T_htf_hot_high = as_double("T_htf_hot_high");	//[C]
		int n_T_htf_hot_in = as_integer("n_T_htf_hot");			//[-]
		double T_amb_low = as_double("T_amb_low");				//[C]
		double T_amb_high = as_double("T_amb_high");			//[C]
		int n_T_amb_in = as_integer("n_T_amb");					//[-]
		double m_dot_htf_ND_low = as_double("m_dot_htf_ND_low");	//[-]
		double m_dot_htf_ND_high = as_double("m_dot_htf_ND_high");	//[-]
		int n_m_dot_htf_ND_in = as_integer("n_m_dot_htf_ND");			//[-]


		
		//C_sco2_recomp_csp::S_od_par sco2_rc_od_par;
		//double m_dot_htf_ND = 1.2;
		//sco2_rc_od_par.m_m_dot_htf = m_dot_htf_ND*sco2_recomp_csp.get_phx_des_par()->m_m_dot_hot_des;		//[kg/s]
		//sco2_rc_od_par.m_T_amb = 30.0 + 273.15;				//[K]
		//sco2_rc_od_par.m_T_htf_hot = 670.0 + 273.15;		//[K]
		//
		//int sco2_code = sco2_recomp_csp.off_design_opt(sco2_rc_od_par, C_sco2_recomp_csp::E_MAX_ETA);



		util::matrix_t<double> T_htf_parametrics, T_amb_parametrics, m_dot_htf_ND_parametrics;

		try
		{
			sco2_recomp_csp.generate_ud_pc_tables(T_htf_hot_low, T_htf_hot_high, n_T_htf_hot_in,
							T_amb_low, T_amb_high, n_T_amb_in,
							m_dot_htf_ND_low, m_dot_htf_ND_high, n_m_dot_htf_ND_in,
							T_htf_parametrics, T_amb_parametrics, m_dot_htf_ND_parametrics);
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

		int n_T_htf_hot = T_htf_parametrics.nrows();
		int n_T_amb = T_amb_parametrics.nrows();
		int n_m_dot_htf_ND = m_dot_htf_ND_parametrics.nrows();

		int ncols = T_htf_parametrics.ncols();

		ssc_number_t *p_T_htf_ind = allocate("T_htf_ind", n_T_htf_hot, ncols);
		for(int i = 0; i < n_T_htf_hot; i++)
		{
			for(int j = 0; j < ncols; j++)
			{
				p_T_htf_ind[i*ncols + j] = T_htf_parametrics(i,j);
			}
		}

		ssc_number_t *p_T_amb_ind = allocate("T_amb_ind", n_T_amb, ncols);
		for(int i = 0; i < n_T_amb; i++)
		{
			for(int j = 0; j < ncols; j++)
			{
				p_T_amb_ind[i*ncols + j] = T_amb_parametrics(i,j);
			}
		}

		ssc_number_t *p_m_dot_htf_ND_ind = allocate("m_dot_htf_ND_ind", n_m_dot_htf_ND, ncols);
		for(int i = 0; i < n_m_dot_htf_ND; i++)
		{
			for(int j = 0; j < ncols; j++)
			{
				p_m_dot_htf_ND_ind[i*ncols + j] = m_dot_htf_ND_parametrics(i,j);
			}
		}

		// If all calls were successful, log to SSC any messages from sco2_recomp_csp
		while( sco2_recomp_csp.mc_messages.get_message(&out_type, &out_msg) )
		{
			log(out_msg);
		}
		
	}

};

DEFINE_MODULE_ENTRY(sco2_csp_ud_pc_tables, "...", 0)

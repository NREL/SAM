#ifndef __HEAT_EXCHANGERS_
#define __HEAT_EXCHANGERS_

#include "CO2_properties.h"
#include "water_properties.h"
#include "htf_props.h"
#include "lib_util.h"
#include "numeric_solvers.h"

#include "csp_solver_util.h"

namespace NS_HX_counterflow_eqs
{
	enum
	{
		CO2 = 200,
		WATER = 201
	};

	double calc_max_q_dot_enth(int hot_fl_code /*-*/, HTFProperties & hot_htf_class,
		int cold_fl_code /*-*/, HTFProperties & cold_htf_class,
		double h_h_in /*kJ/kg*/, double P_h_in /*kPa*/, double P_h_out /*kPa*/, double m_dot_h /*kg/s*/,
		double h_c_in /*kJ/kg*/, double P_c_in /*kPa*/, double P_c_out /*kPa*/, double m_dot_c /*kg/s*/);

	double calc_max_q_dot(int hot_fl_code /*-*/, HTFProperties & hot_htf_class,
		int cold_fl_code /*-*/, HTFProperties & cold_htf_class,
		double T_h_in, double P_h_in, double P_h_out, double m_dot_h,
		double T_c_in, double P_c_in, double P_c_out, double m_dot_c);

	void calc_req_UA(int hot_fl_code /*-*/, HTFProperties & hot_htf_class,
		int cold_fl_code /*-*/, HTFProperties & cold_htf_class,
		int N_sub_hx /*-*/,
		double q_dot /*kWt*/, double m_dot_c /*kg/s*/, double m_dot_h /*kg/s*/,
		double T_c_in /*K*/, double T_h_in /*K*/, double P_c_in /*kPa*/, double P_c_out /*kPa*/, double P_h_in /*kPa*/, double P_h_out /*kPa*/,
		double & UA /*kW/K*/, double & min_DT /*C*/, double & eff /*-*/, double & NTU /*-*/, double & T_h_out /*K*/, double & T_c_out /*K*/, double & q_dot_calc /*kWt*/);

	void calc_req_UA_enth(int hot_fl_code /*-*/, HTFProperties & hot_htf_class,
		int cold_fl_code /*-*/, HTFProperties & cold_htf_class, 
		int N_sub_hx /*-*/,
		double q_dot /*kWt*/, double m_dot_c /*kg/s*/, double m_dot_h /*kg/s*/,
		double h_c_in /*kJ/kg*/, double h_h_in /*kJ/kg*/, double P_c_in /*kPa*/, double P_c_out /*kPa*/, double P_h_in /*kPa*/, double P_h_out /*kPa*/,
		double & h_h_out /*kJ/kg*/, double & T_h_out /*K*/, double & h_c_out /*kJ/kg*/, double & T_c_out /*K*/,
		double & UA /*kW/K*/, double & min_DT /*C*/, double & eff /*-*/, double & NTU /*-*/, double & q_dot_calc /*kWt*/);
	
	void solve_q_dot_for_fixed_UA(int hot_fl_code /*-*/, HTFProperties & hot_htf_class,
		int cold_fl_code /*-*/, HTFProperties & cold_htf_class,
		int N_sub_hx /*-*/,
		double T_c_in /*K*/, double P_c_in /*kPa*/, double m_dot_c /*kg/s*/, double P_c_out /*kPa*/,
		double T_h_in /*K*/, double P_h_in /*kPa*/, double m_dot_h /*kg/s*/, double P_h_out /*kPa*/,
		double UA_target /*kW/K*/, double eff_limit /*-*/, double eff_guess /*-*/,
		double & q_dot /*kWt*/, double & T_c_out /*K*/, double & T_h_out /*K*/,
		double & eff_calc /*-*/, double & min_DT /*K*/, double & NTU /*-*/, double & UA_calc);

	void solve_q_dot_for_fixed_UA_enth(int hot_fl_code /*-*/, HTFProperties & hot_htf_class,
		int cold_fl_code /*-*/, HTFProperties & cold_htf_class,
		int N_sub_hx /*-*/,
		double h_c_in /*K*/, double P_c_in /*kPa*/, double m_dot_c /*kg/s*/, double P_c_out /*kPa*/,
		double h_h_in /*K*/, double P_h_in /*kPa*/, double m_dot_h /*kg/s*/, double P_h_out /*kPa*/,
		double UA_target /*kW/K*/, double eff_limit /*-*/, double eff_guess /*-*/,
		double & T_c_out  /*K*/, double & h_c_out /*kJ/kg*/,
		double & T_h_out /*K*/, double & h_h_out /*kJ/kg*/,
		double & q_dot /*kWt*/, double & eff_calc /*-*/, double & min_DT /*K*/, double & NTU /*-*/, double & UA_calc);

	class C_mono_eq_UA_v_q_enth : public C_monotonic_equation
	{
	private:

		int m_hot_fl_code;		//[-]
		HTFProperties mc_hot_htf_class;

		int m_cold_fl_code;		//[-]
		HTFProperties mc_cold_htf_class;

		int m_N_sub_hx;			//[-]

		double m_P_c_out;		//[kPa]
		double m_P_h_out;		//[kPa]

		double m_h_c_in;		//[K]
		double m_P_c_in;		//[kPa]
		double m_m_dot_c;		//[kg/s]
		double m_h_h_in;		//[K]
		double m_P_h_in;		//[kPa]
		double m_m_dot_h;		//[kg/s]

	public:
		C_mono_eq_UA_v_q_enth(int hot_fl_code /*-*/, HTFProperties hot_htf_class,
			int cold_fl_code /*-*/, HTFProperties cold_htf_class,
			int N_sub_hx /*-*/,
			double P_c_out /*kPa*/, double P_h_out /*kPa*/,
			double h_c_in /*kJ/kg*/, double P_c_in /*kPa*/, double m_dot_c /*kg/s*/,
			double h_h_in /*kJ/kg*/, double P_h_in /*kPa*/, double m_dot_h /*kg/s*/)
		{
			m_hot_fl_code = hot_fl_code;
			mc_hot_htf_class = hot_htf_class;

			m_cold_fl_code = cold_fl_code;
			mc_cold_htf_class = cold_htf_class;

			m_N_sub_hx = N_sub_hx;

			m_P_c_out = P_c_out;	//[kPa]
			m_P_h_out = P_h_out;	//[kPa]

			m_h_c_in = h_c_in;		//[kJ/kg]
			m_P_c_in = P_c_in;		//[kPa]
			m_m_dot_c = m_dot_c;	//[kg/s]

			m_h_h_in = h_h_in;		//[kJ/kg]
			m_P_h_in = P_h_in;		//[kPa]
			m_m_dot_h = m_dot_h;	//[kg/s]

			m_h_c_out = m_h_h_out = m_T_c_out = m_T_h_out = m_eff =
				m_min_DT = m_NTU = m_UA_calc = std::numeric_limits<double>::quiet_NaN();
		}

		double m_h_c_out;		//[kJ/kg]
		double m_h_h_out;		//[kJ/kg]
		double m_T_c_out;		//[K]
		double m_T_h_out;		//[K]
		double m_eff;			//[-]
		double m_min_DT;		//[K]
		double m_NTU;			//[-]
		double m_UA_calc;		//[kW/K]

		virtual int operator()(double q_dot /*kWt*/, double *UA_calc /*kW/K*/);
	};
}

class C_HX_counterflow
{

protected:
	bool m_is_HX_initialized;		//[-] True = yes!
	bool m_is_HX_designed;			//[-] True = yes!

public:

	struct S_init_par
	{
		int m_N_sub_hx;				//[-] Number of sub-heat exchangers used in the model

		int m_hot_fl;				//[-] Integer code for hot fluid - assumed be HTF in library or w/ lookup unless = CO2 enumeration
		util::matrix_t<double> mc_hot_fl_props;		//[-] If applicable, user-defined properties

		int m_cold_fl;				//[-] Integer code for cold fluid - assumed be HTF in library or w/ lookup unless = CO2 enumeration
		util::matrix_t<double> mc_cold_fl_props;	//[-] If applicable, user-defined properties

		S_init_par()
		{
			m_N_sub_hx = m_hot_fl = m_cold_fl = -1;
		}
	};

	struct S_des_calc_UA_par
	{
		double m_T_h_in;			//[K] Design-point hot inlet temperature
		double m_P_h_in;			//[kPa] Hot fluid inlet pressure
		double m_P_h_out;			//[kPa] Hot fluid outlet pressure
		double m_m_dot_hot_des;		//[kg/s] hot fluid design mass flow rate
		double m_T_c_in;			//[K] Design-point cold inlet temperature
		double m_P_c_in;			//[kPa] Cold fluid inlet temperature
		double m_P_c_out;			//[kPa] Cold fluid outlet temperature
		double m_m_dot_cold_des;	//[kg/s] cold fluid design mass flow rate

		double m_eff_max;			//[-] Max allowable effectiveness

		S_des_calc_UA_par()
		{
			m_T_h_in = m_P_h_in = m_P_h_out = m_m_dot_hot_des = 
				m_T_c_in = m_P_c_in = m_P_c_out = m_m_dot_cold_des = 
				
				m_eff_max = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_des_solved
	{
		double m_Q_dot_design;		//[kWt] Design-point heat transfer
		double m_UA_design_total;		//[kW/K] Design-point conductance
		double m_min_DT_design;			//[K] Minimum temperature difference in heat exchanger
		double m_eff_design;			//[-] Effectiveness at design
		double m_NTU_design;			//[-] NTU at design
		double m_T_h_out;				//[K] Design-point hot outlet temperature
		double m_T_c_out;				//[K] Design-point cold outlet temperature
		double m_DP_cold_des;			//[kPa] cold fluid design pressure drop
		double m_DP_hot_des;			//[kPa] hot fluid design pressure drop

		S_des_solved()
		{
			m_Q_dot_design = m_UA_design_total = m_min_DT_design = m_eff_design = m_NTU_design =
				m_T_h_out = m_T_c_out =
				m_DP_cold_des = m_DP_hot_des = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_od_par
	{
		double m_T_c_in;		//[K] Cold fluid inlet temperature
		double m_P_c_in;		//[kPa] Cold fluid inlet pressure
		double m_m_dot_c;		//[kg/s] Cold fluid design mass flow rate
		double m_T_h_in;		//[K] Hot fluid inlet temperature
		double m_P_h_in;		//[kPa] Hot fluid inlet pressure
		double m_m_dot_h;		//[kg/s] Hot fluid design mass flow rate

		S_od_par()
		{
			m_T_c_in = m_P_c_in = m_m_dot_c =
				m_T_h_in = m_P_h_in = m_m_dot_h = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_od_solved
	{
		double m_q_dot;		//[kWt] Thermal power to cold fluid
		double m_T_c_out;	//[K] Cold fluid outlet temperature
		double m_P_c_out;	//[kPa] Cold fluid outlet pressure
		double m_T_h_out;	//[K] Hot fluid outlet temperature
		double m_P_h_out;	//[kPa] Hot fluid outlet temperature
		double m_UA_total;	//[kW/K] Conductance
		double m_min_DT;	//[K] Min temp difference
		double m_eff;		//[-]
		double m_NTU;

		S_od_solved()
		{
			m_q_dot =
				m_T_c_out = m_P_c_out = m_T_h_out = m_P_h_out =
				m_UA_total = m_min_DT = m_eff = m_NTU = std::numeric_limits<double>::quiet_NaN();
		}
	};

	S_init_par ms_init_par;
	S_des_calc_UA_par ms_des_calc_UA_par;
	S_des_solved ms_des_solved;
	S_od_par ms_od_par;
	S_od_solved ms_od_solved;

	HTFProperties mc_hot_fl;
	HTFProperties mc_cold_fl;
	CO2_state mc_co2_props;
	water_state ms_water_props;

	C_HX_counterflow();

	void design_calc_UA(C_HX_counterflow::S_des_calc_UA_par des_par, 
		double q_dot_design /*kWt*/, C_HX_counterflow::S_des_solved &des_solved);

	double calc_max_q_dot_enth(double h_h_in /*kJ/kg*/, double P_h_in /*kPa*/, double P_h_out /*kPa*/, double m_dot_h /*kg/s*/,
		double h_c_in /*kJ/kg*/, double P_c_in /*kPa*/, double P_c_out /*kPa*/, double m_dot_c /*kg/s*/);

	void calc_req_UA(double q_dot /*kWt*/, double m_dot_c /*kg/s*/, double m_dot_h /*kg/s*/,
		double T_c_in /*K*/, double T_h_in /*K*/, double P_c_in /*kPa*/, double P_c_out /*kPa*/, double P_h_in /*kPa*/, double P_h_out /*kPa*/,
		double & UA /*kW/K*/, double & min_DT /*C*/, double & eff /*-*/, double & NTU /*-*/, double & T_h_out /*K*/, double & T_c_out /*K*/, double & q_dot_calc /*kWt*/);
	
	void calc_req_UA_enth(double q_dot /*kWt*/, double m_dot_c /*kg/s*/, double m_dot_h /*kg/s*/,
		double h_c_in /*kJ/kg*/, double h_h_in /*kJ/kg*/, double P_c_in /*kPa*/, double P_c_out /*kPa*/, double P_h_in /*kPa*/, double P_h_out /*kPa*/,
		double & UA /*kW/K*/, double & min_DT /*C*/, double & eff /*-*/, double & NTU /*-*/, double & h_h_out /*K*/, double & h_c_out /*K*/, double & q_dot_calc /*kWt*/);

	void design_fix_UA_calc_outlet(double UA_target /*kW/K*/, double eff_target /*-*/,
		double T_c_in /*K*/, double P_c_in /*kPa*/, double m_dot_c /*kg/s*/, double P_c_out /*kPa*/,
		double T_h_in /*K*/, double P_h_in /*kPa*/, double m_dot_h /*kg/s*/, double P_h_out /*kPa*/,
		double & q_dot /*kWt*/, double & T_c_out /*K*/, double & T_h_out /*K*/);

	void off_design_solution(double T_c_in /*K*/, double P_c_in /*kPa*/, double m_dot_c /*kg/s*/, double P_c_out /*kPa*/,
		double T_h_in /*K*/, double P_h_in /*kPa*/, double m_dot_h /*kg/s*/, double P_h_out /*kPa*/,
		double & q_dot /*kWt*/, double & T_c_out /*K*/, double & T_h_out /*K*/);

	void off_design_solution_calc_dP(double T_c_in /*K*/, double P_c_in /*kPa*/, double m_dot_c /*kg/s*/,
		double T_h_in /*K*/, double P_h_in /*kPa*/, double m_dot_h /*kg/s*/,
		double & q_dot /*kWt*/, double & T_c_out /*K*/, double & P_c_out /*kPa*/, double & T_h_out /*K*/, double & P_h_out /*kPa*/);

	double od_delta_p_cold_frac(double m_dot_c /*kg/s*/);

	double od_delta_p_cold(double m_dot_c /*kg/s*/);

	double od_delta_p_hot_frac(double m_dot_h /*kg/s*/);

	double od_delta_p_hot(double m_dot_h /*kg/s*/);

	double od_UA_frac(double m_dot_c /*kg/s*/, double m_dot_h /*kg/s*/);

	double od_UA(double m_dot_c /*kg/s*/, double m_dot_h /*kg/s*/); 

	virtual void initialize(const S_init_par & init_par);

};

class C_HX_co2_to_htf : public C_HX_counterflow
{

private:



public:

	//// This method calculates the HTF mass flow rate (m_m_dot_hot_des) that results in CR = 1
	//void design_with_m_dot(C_HX_counterflow::S_des_par &des_par, double T_htf_cold, C_HX_counterflow::S_des_solved &des_solved);

	// This method calculates the required HTF mass flow rate (m_m_dot_hot_des) given a cold side approach temperature (assuming hot HTF temp is a design parameter)
	void design_and_calc_m_dot_htf(C_HX_counterflow::S_des_calc_UA_par &des_par, 
		double q_dot_design /*kWt*/, double dt_cold_approach /*C/K*/, C_HX_counterflow::S_des_solved &des_solved);

	virtual void initialize(int hot_fl, util::matrix_t<double> hot_fl_props);

	virtual void initialize(int hot_fl);
	
};

class C_HX_co2_to_co2 : public C_HX_counterflow
{

public:

	virtual void initialize(int N_sub_hx);

	virtual void initialize();

};


namespace N_compact_hx
{
	enum
	{
		fc_tubes_s80_38T = 1,
		fc_tubes_sCF_88_10Jb
	};

	bool get_compact_hx_geom(int enum_compact_hx_config, double & d_out, double & fin_pitch, double & D_h,
		double & fin_thk, double & sigma, double & alpha, double & A_fin_to_surf,
		double & s_h, double & s_v, double & fin_V_per_m);

	bool get_compact_hx_f_j(int enum_compact_hx_config, double Re, double & f, double & j_H);

};

class C_CO2_to_air_cooler
{

public:
	
	// Class to save messages for up stream classes
	C_csp_messages mc_messages;

	// Design parameters that are independent of the cycle-side inputs
	struct S_des_par_ind
	{
		double m_T_amb_des;		//[K] Design point ambient temperature
		double m_elev;			//[m] Elevation (used to calculate ambient pressure)

		S_des_par_ind()
		{
			m_T_amb_des = m_elev = std::numeric_limits<double>::quiet_NaN();
		}
	};

	// Design parameters that are dependent on the cycle-side performance
	struct S_des_par_cycle_dep
	{
		// Either specify mass flow rate or heat rejection rate
			// If mass flow rate is > 0.0, will use that
		double m_m_dot_total;		//[kg/s] Total sCO2 mass flow into air-cooler
		double m_Q_dot_des;			//[MWt] Heat rejected over specified temperatures

		double m_T_hot_in_des;		//[K] sCO2 hot side (inlet) temperature
		double m_P_hot_in_des;		//[kPa] sCO2 hot side (inlet) pressure
		double m_delta_P_des;		//[kPa] sCO2 pressure drop
		double m_T_hot_out_des;		//[K] sCO2 cold outlet temperature
		double m_W_dot_fan_des;		//[MWe] Design point fan power

		S_des_par_cycle_dep()
		{
			// One of these needs to be > 0.
				// Will check mass flow rate first
			m_m_dot_total = m_Q_dot_des = -1;

			// All of these must be set
			m_T_hot_in_des = m_P_hot_in_des = 
				m_delta_P_des = m_T_hot_out_des = m_W_dot_fan_des = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_hx_design_solved
	{
		int m_N_passes;		//[-] Number of serial passes in flow direction
		
		double m_d_out;		//[m] CO2 tube outer diameter
		double m_d_in;		//[m] CO2 tube inner diameter
		double m_Depth;		//[m] Dimension in loop/air flow direction
		double m_W_par;		//[m] Dimension of parallel flow paths
		double m_N_par;		//[-] Number of parallel flow paths
		double m_N_tubes;	//[-] Number of tubes
		double m_L_tube;	//[m] Tube length
		double m_UA_total;	//[W/K] Total air-side conductance at design
		double m_V_material_total;	//[m^3] Total Material volume - no headers


		S_hx_design_solved()
		{
			m_N_passes = -1;

			m_d_out = m_d_in = m_Depth = m_W_par = m_N_par =
				m_N_tubes = m_L_tube = m_UA_total = m_V_material_total = std::numeric_limits<double>::quiet_NaN();
		}
	};

private:

	// Classes
	HTFProperties mc_air;				// Instance of HTFProperties class for ambient air

	// Remaining Air-Cooler Specs
	// Inputs
	int m_N_nodes;			//[-]
	double m_th;			//[m]
	double m_eta_fan;		//[-]
	double m_roughness;		//[m]
	// Calculated
	double m_A_cs;			//[m^2]
	double m_relRough;		//[-]
	double m_L_path;		//[m]		Total flow path length
	double m_A_surf_total;	//[m^2]		Total air-side surface area
	double m_V_footprint;	//[m^3]		Total HX footprint
	double m_A_surf_node;	//[m^2]
	double m_V_material_tubes;	//[m^3] Total material required for tubing
	double m_V_material_fins;	//[m^3] Total material required for fins

	// Design Ambient Conditions
	double m_P_amb_des;		//[Pa]

	// Design Performance Targets
	double m_m_dot_air_des;		//[kg/s]
	double m_Q_dot_des;			//[W]

	// Calculated Performance Target
	double m_P_hot_out_des;		//[kPa]

	// HX geometry
		// Input
	int m_enum_compact_hx_config;
		// Defined from Config
	double m_fin_pitch;	//[1/m]
	double m_D_h;		//[m]
	double m_fin_thk;	//[m]
	double m_sigma;		//[-]
	double m_alpha;		//[1/m]
	double m_A_fin_to_surf;	//[-]
	double m_s_h;		//[m]
	double m_s_v;		//[m]
	double m_fin_V_per_m;	//[1/m]

	int m_final_outlet_index;

	// Structures
		// In
	S_des_par_ind ms_des_par_ind;
	S_des_par_cycle_dep ms_des_par_cycle_dep;
		// Out
	S_hx_design_solved ms_hx_des_sol;

public:

	util::matrix_t<double> mm_T_co2;	//[K]
	util::matrix_t<double> mm_P_co2;	//[K]
	util::matrix_t<double> mm_T_air;	//[K]

	CO2_state mc_co2_props;

	C_CO2_to_air_cooler();

	~C_CO2_to_air_cooler(){};

	bool design_hx(S_des_par_ind des_par_ind, S_des_par_cycle_dep des_par_cycle_dep);

	void off_design_hx(double T_amb_K, double P_amb_Pa, double T_hot_in_K, double P_hot_in_kPa,
		double m_dot_hot_kg_s, double T_hot_out_K, double & W_dot_fan_MW, int & error_code);

	double get_total_hx_volume()
	{
		return ms_hx_des_sol.m_V_material_total;
	}

	const C_CO2_to_air_cooler::S_hx_design_solved * get_design_solved()
	{
		return &ms_hx_des_sol;
	}

	class C_MEQ_target_W_dot_fan__m_dot_air : public C_monotonic_equation
	{
	private:
		C_CO2_to_air_cooler *mpc_ac;
		double m_L_tube;	//[m] Length of tube in one pass (flow direction)
		double m_W_par;		//[m] Dimension of parallel paths
		double m_V_total;	//[m3] Total HX "footprint" volume

		double m_mu_air;	//[kg/m-s] dynamic viscosity
		double m_v_air;		//[1/m3] specific volume
		double m_cp_air;	//[J/kg-K] specific heat convert from kJ/kg-K
		double m_Pr_air;	//[-] Prandtl number

	public:
		C_MEQ_target_W_dot_fan__m_dot_air(C_CO2_to_air_cooler *pc_ac,
					double L_tube /*m*/, double W_par /*m*/, double V_total /*m3*/,
					double mu_air /*kg/m-s*/, double v_air /*1/m3*/, 
					double cp_air /*J/kg-K*/, double Pr_air /*-*/)
		{
			mpc_ac = pc_ac;

			m_L_tube = L_tube;
			m_W_par = W_par;
			m_V_total = V_total;

			m_mu_air = mu_air;
			m_v_air = v_air;
			m_cp_air = cp_air;
			m_Pr_air = Pr_air;

			m_h_conv_air = std::numeric_limits<double>::quiet_NaN();
		}

		double m_h_conv_air;	//[W/m2-K] Convective coefficient

		virtual int operator()(double m_dot_air /*kg/s*/, double *W_dot_fan /*MWe*/);
	};

	class C_MEQ_node_energy_balance__T_co2_out : public C_monotonic_equation
	{
	private:
		C_CO2_to_air_cooler *mpc_ac;
		double m_T_co2_cold_out;	//[K] CO2 inlet temperature
		double m_P_co2_ave;			//[kPa] Average CO2 pressure

		double m_m_dot_co2_tube;	//[kg/s] CO2 mass flow rate through tube
		
		double m_T_air_cold_in;		//[K] Air cold temperature
		double m_C_dot_air;		//[W/K] CO2 flow capacitance

		double m_UA_node;		//[W/K] Conductance of node - assuming air convective heat transfer is governing resistance

	public:
		C_MEQ_node_energy_balance__T_co2_out(C_CO2_to_air_cooler *pc_ac,
					double T_co2_cold_out /*K*/, double P_co2_ave /*kPa*/, 
					double m_dot_co2_tube /*kg/s*/, 
					double T_air_cold_in /*K*/, double C_dot_air /*W/K*/,
					double UA_node /*W/K*/)
		{
			mpc_ac = pc_ac;

			m_T_co2_cold_out = T_co2_cold_out;	//[K]
			m_P_co2_ave = P_co2_ave;	//[kPa]

			m_m_dot_co2_tube = m_dot_co2_tube;	//[kg/s]
			
			m_T_air_cold_in = T_air_cold_in;	//[K]
			m_C_dot_air = C_dot_air;	//[W/K]

			m_UA_node = UA_node;		//[W/K]

			m_Q_dot_node = std::numeric_limits<double>::quiet_NaN();
		}

		double m_Q_dot_node;	//[W]

		virtual int operator()(double T_co2_hot_in /*K*/, double *diff_T_co2_cold /*-*/);
	};

	class C_MEQ_target_CO2_dP__L_tube_pass : public C_monotonic_equation
	{
	private:
		C_CO2_to_air_cooler *mpc_ac;
		double m_W_par;		//[m] Dimension of parallel paths
		double m_N_par;		//[-] Number of tubes in parallel

		double m_P_hot_ave;	//[kPa] Global average hot fluid temperature
		double m_m_dot_tube;//[kg/s] Mass flow rate through one tube

		double m_mu_air;	//[kg/m-s] dynamic viscosity
		double m_v_air;		//[1/m3] specific volume
		double m_cp_air;	//[J/kg-K] specific heat convert from kJ/kg-K
		double m_Pr_air;	//[-] Prandtl number

		double m_tol_upper;	//[-] Tolerance from upper level loop
		
	public:
		C_MEQ_target_CO2_dP__L_tube_pass(C_CO2_to_air_cooler *pc_ac,
			double W_par /*m*/, double N_par /*-*/,
			double P_hot_ave /*kPa*/, double m_dot_tube /*kg/s*/,
			double mu_air /*kg/m-s*/, double v_air /*1/m3*/,
			double cp_air /*J/kg-K*/, double Pr_air /*-*/,
			double tol_upper /*-*/)
		{
			mpc_ac = pc_ac;
			m_W_par = W_par;	//[m]
			m_N_par = N_par;	//[-]

			m_P_hot_ave = P_hot_ave;
			m_m_dot_tube = m_dot_tube;

			m_mu_air = mu_air;
			m_v_air = v_air;
			m_cp_air = cp_air;
			m_Pr_air = Pr_air;

			m_tol_upper = tol_upper;	//[-]

			m_V_total = std::numeric_limits<double>::quiet_NaN();
			m_h_conv_air = std::numeric_limits<double>::quiet_NaN();
			m_m_dot_air_total = std::numeric_limits<double>::quiet_NaN();
			m_A_surf_node = std::numeric_limits<double>::quiet_NaN();
		}

		double m_V_total;		//[m^3] Total HX "footprint" Volume
		double m_h_conv_air;	//[W/m2-K] Convective coefficient
		double m_m_dot_air_total;		//[kg/s] Total air mass flow rate
		double m_A_surf_node;	//[m2] Air-side surface area of node

		virtual int operator()(double L_tube /*m*/, double *delta_P_co2 /*kPa*/);
	};

	class C_MEQ_target_T_hot__width_parallel : public C_monotonic_equation
	{
	private:
		C_CO2_to_air_cooler *mpc_ac;

		double m_mu_air;	//[kg/m-s] dynamic viscosity
		double m_v_air;		//[1/m3] specific volume
		double m_cp_air;	//[J/kg-K] specific heat convert from kJ/kg-K
		double m_Pr_air;	//[-] Prandtl number

		double m_T_co2_deltaP_eval;	//[K] Representative temperature for property evaluation
		double m_P_hot_ave;			//[kPa]

		double m_tol;		//[-] Convergence tolerance

	public:
		C_MEQ_target_T_hot__width_parallel(C_CO2_to_air_cooler *pc_ac,
			double mu_air /*kg/m-s*/, double v_air /*1/m3*/,
			double cp_air /*J/kg-K*/, double Pr_air /*-*/,
			double T_co2_deltaP_eval /*K*/, double P_hot_ave /*kPa*/,
			double tol /*-*/)
		{
			mpc_ac = pc_ac;

			m_mu_air = mu_air;
			m_v_air = v_air;
			m_cp_air = cp_air;
			m_Pr_air = Pr_air;

			m_T_co2_deltaP_eval = T_co2_deltaP_eval;
			m_P_hot_ave = P_hot_ave;

			m_tol = tol;

			m_L_tube = std::numeric_limits<double>::quiet_NaN();
			m_N_par = std::numeric_limits<double>::quiet_NaN();
			m_N_tubes = std::numeric_limits<double>::quiet_NaN();
			m_V_total = std::numeric_limits<double>::quiet_NaN();
			m_h_conv_air = std::numeric_limits<double>::quiet_NaN();
			m_m_dot_air_total = std::numeric_limits<double>::quiet_NaN();
			m_A_surf_node = std::numeric_limits<double>::quiet_NaN();
		}

		double m_L_tube;	//[m]
		double m_N_par;		//[-]
		double m_N_tubes;	//[-]
		double m_V_total;	//[m^3] Total HX "footprint" Volume
		double m_h_conv_air;	//[W/m2-K] Convective coefficient
		double m_m_dot_air_total;		//[kg/s] Total air mass flow rate
		double m_A_surf_node;	//[m2] Air-side surface area of node

		virtual int operator()(double W_par /*m*/, double *T_co2_hot /*K*/);
	};
};


#endif
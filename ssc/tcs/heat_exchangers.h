#ifndef __HEAT_EXCHANGERS_
#define __HEAT_EXCHANGERS_

#include "CO2_properties.h"
#include "water_properties.h"
#include "htf_props.h"
#include "lib_util.h"
#include "numeric_solvers.h"

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
		double m_T_hot_in_des;		//[K] sCO2 hot side (inlet) temperature
		double m_P_hot_in_des;		//[kPa] sCO2 hot side (inlet) pressure
		double m_m_dot_total;		//[kg/s] Total sCO2 mass flow into air-cooler
		double m_delta_P_des;		//[kPa] sCO2 pressure drop
		double m_T_hot_out_des;		//[K] sCO2 cold outlet temperature
		double m_W_dot_fan_des;		//[MW] Design point fan power

		S_des_par_cycle_dep()
		{
			m_T_hot_in_des = m_P_hot_in_des = m_m_dot_total =
				m_delta_P_des = m_T_hot_out_des = m_W_dot_fan_des = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_hx_design_solved
	{
		double m_material_V;	//[m^3]		Total Material volume - no headers

		S_hx_design_solved()
		{
			m_material_V = std::numeric_limits<double>::quiet_NaN();
		}
	};

private:

	// Classes
	HTFProperties mc_air;				// Instance of HTFProperties class for ambient air

	// Remaining Air-Cooler Specs
	// Inputs
	int m_N_loops;
	int m_N_nodes;
	double m_th;			//[m]
	double m_eta_fan;		//[-]
	double m_roughness;		//[m]
	// Calculated
	double m_d_in;			//[m]
	double m_A_cs;			//[m^2]
	double m_relRough;		//[-]
	double m_Depth;			//[m]		Dimension in loop/air flow direction
	double m_W_par;			//[m]		Dimension of parallel flow paths
	double m_N_par;			//[-]		Number of parallel flow paths
	double m_N_tubes;		//[-]		Number of tubes
	double m_L_tube;		//[m]		Tube length
	double m_L_path;		//[m]		Total flow path length
	double m_A_surf_total;	//[m^2]		Total air-side surface area
	double m_UA_total;		//[W/K]		Total air-side conductance at design
	double m_V_total;		//[m^3]		Total HX volume
	double m_material_V;	//[m^3]		Total Material volume - no headers
	double m_A_surf_node;	//[m^2]

	// Design Ambient Conditions
	//double m_T_amb_des;		//[K]
	double m_P_amb_des;		//[Pa]

	// Hot-side Inlet Conditions
	//double m_T_hot_in_des;		//[K]
	//double m_P_hot_in_des;		//[kPa]
	//double m_m_dot_total;		//[kg/s] Total sCO2 mass flow into air-cooler

	// Design Performance Targets
	//double m_W_dot_fan_des;		//[MW]
	//double m_delta_P_des;		//[kPa]
	//double m_T_hot_out_des;		//[K]
	double m_m_dot_air_des;		//[kg/s]
	double m_Q_dot_des;			//[W]

	// Calculated Performance Target
	double m_P_hot_out_des;		//[kPa]

	// HX geometry
	// Input
	int m_enum_compact_hx_config;
	// Defined from Config
	double m_d_out;		//[m]
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
	S_des_par_ind ms_des_par_ind;
	S_des_par_cycle_dep ms_des_par_cycle_dep;
	S_hx_design_solved m_hx_design_solved;

public:

	C_CO2_to_air_cooler();

	~C_CO2_to_air_cooler(){};

	//bool design_hx(double T_amb_K, double P_amb_Pa, double T_hot_in_K, double P_hot_in_kPa,
	//	double m_dot_hot_kg_s, double W_dot_fan_MW, double deltaP_kPa, double T_hot_out_K);

	bool design_hx(S_des_par_ind des_par_ind, S_des_par_cycle_dep des_par_cycle_dep);

	void off_design_hx(double T_amb_K, double P_amb_Pa, double T_hot_in_K, double P_hot_in_kPa,
		double m_dot_hot_kg_s, double T_hot_out_K, double & W_dot_fan_MW, int & error_code);

	const S_hx_design_solved * get_hx_design_solved()
	{
		m_hx_design_solved.m_material_V = m_material_V;

		return &m_hx_design_solved;
	}

};


#endif
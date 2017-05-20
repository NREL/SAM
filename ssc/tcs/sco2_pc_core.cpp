#include "sco2_pc_core.h"
#include "CO2_properties.h"
#include <limits>
#include <algorithm>

#include "nlopt.hpp"
#include "nlopt_callbacks.h"

#include "fmin.h"

#include "lib_util.h"

#include "csp_solver_util.h"

using namespace std;


const double C_turbine::m_nu_design = 0.7476;
const double C_compressor::m_snl_phi_design = 0.02971;		//[-] Design-point flow coef. for Sandia compressor (corresponds to max eta)
const double C_compressor::m_snl_phi_min = 0.02;				//[-] Approximate surge limit for SNL compressor
const double C_compressor::m_snl_phi_max = 0.05;				//[-] Approximate x-intercept for SNL compressor
const double C_recompressor::m_snl_phi_design = 0.02971;		//[-] Design-point flow coef. for Sandia compressor (corresponds to max eta)
const double C_recompressor::m_snl_phi_min = 0.02;				//[-] Approximate surge limit for SNL compressor
const double C_recompressor::m_snl_phi_max = 0.05;				//[-] Approximate x-intercept for SNL compressor

void calculate_turbomachinery_outlet_1(double T_in /*K*/, double P_in /*kPa*/, double P_out /*kPa*/, double eta /*-*/, bool is_comp, int & error_code, double & spec_work /*kJ/kg*/)
{
	double enth_in, entr_in, dens_in, temp_out, enth_out, entr_out, dens_out;

	calculate_turbomachinery_outlet_1(T_in, P_in, P_out, eta, is_comp, error_code, enth_in, entr_in, dens_in, temp_out, enth_out, entr_out, dens_out, spec_work);
}

void calculate_turbomachinery_outlet_1(double T_in /*K*/, double P_in /*kPa*/, double P_out /*kPa*/, double eta /*-*/, bool is_comp, int & error_code, double & enth_in /*kJ/kg*/, double & entr_in /*kJ/kg-K*/,
	double & dens_in /*kg/m3*/, double & temp_out /*K*/, double & enth_out /*kJ/kg*/, double & entr_out /*kJ/kg-K*/, double & dens_out /*kg/m3*/, double & spec_work /*kJ/kg*/)
{
	/*Calculates the outlet state of a compressor or turbine using its isentropic efficiency.
	is_comp = .true.means the turbomachine is a compressor(w = w_s / eta)
	is_comp = .false.means the turbomachine is a turbine(w = w_s * eta) */

	CO2_state co2_props;

	error_code = 0;

	int prop_error_code = CO2_TP(T_in, P_in, &co2_props);		// properties at the inlet conditions
	if( prop_error_code != 0 )
	{
		error_code = prop_error_code;
		return;
	}
	double h_in = co2_props.enth;
	double s_in = co2_props.entr;
	dens_in = co2_props.dens;

	prop_error_code = CO2_PS(P_out, s_in, &co2_props);			// outlet enthalpy if compression/expansion is isentropic
	if( prop_error_code != 0 )
	{
		error_code = prop_error_code;
		return;
	}
	double h_s_out = co2_props.enth;

	double w_s = h_in - h_s_out;			// specific work if process is isentropic (negative for compression, positive for expansion)

	double w = 0.0;
	if( is_comp )
		w = w_s / eta;						// actual specific work of compressor (negative)
	else
		w = w_s * eta;						// actual specific work of turbine (positive)

	double h_out = h_in - w;

	prop_error_code = CO2_PH(P_out, h_out, &co2_props);
	if( prop_error_code != 0 )
	{
		error_code = prop_error_code;
		return;
	}

	enth_in = h_in;
	entr_in = s_in;
	temp_out = co2_props.temp;
	enth_out = h_out;
	entr_out = co2_props.entr;
	dens_out = co2_props.dens;
	spec_work = w;

	return;
};

//void calculate_hxr_UA_1(int N_hxrs, double Q_dot /*units?*/, double m_dot_c, double m_dot_h, double T_c_in, double T_h_in, double P_c_in, double P_c_out, double P_h_in, double P_h_out,
//	int & error_code, double & UA, double & min_DT)
//{
//	/*Calculates the UA of a heat exchanger given its mass flow rates, inlet temperatures, and a heat transfer rate.
//	Note: the heat transfer rate must be positive.*/
//
//	// Check inputs
//	if( Q_dot < 0.0 )
//	{
//		error_code = 4;
//		return;
//	}
//	if( T_h_in < T_c_in )
//	{
//		error_code = 5;
//		return;
//	}
//	if( P_h_in < P_h_out )
//	{
//		error_code = 6;
//		return;
//	}
//	if( P_c_in < P_c_out )
//	{
//		error_code = 7;
//		return;
//	}
//	if( Q_dot <= 1.E-14 )		// very low Q_dot; assume it is zero
//	{
//		UA = 0.0;
//		min_DT = T_h_in - T_c_in;
//		return;
//	}
//
//	// Calculate inlet enthalpies from known state points
//	CO2_state co2_props;
//	int prop_error_code = CO2_TP(T_c_in, P_c_in, &co2_props);
//	if( prop_error_code != 0 )
//	{
//		error_code = prop_error_code;
//		return;
//	}
//	double h_c_in = co2_props.enth;
//
//	prop_error_code = CO2_TP(T_h_in, P_h_in, &co2_props);
//	if( prop_error_code != 0 )
//	{
//		error_code = 9;
//		return;
//	}
//	double h_h_in = co2_props.enth;
//
//	// Calculate outlet enthalpies from energy balance
//	double h_c_out = h_c_in + Q_dot / m_dot_c;
//	double h_h_out = h_h_in - Q_dot / m_dot_h;
//
//	int N_nodes = N_hxrs + 1;
//	double h_h_prev = 0.0;
//	double T_h_prev = 0.0;
//	double h_c_prev = 0.0;
//	double T_c_prev = 0.0;
//	UA = 0.0;
//	min_DT = T_h_in;
//	// Loop through the sub-heat exchangers
//	for( int i = 0; i < N_nodes; i++ )
//	{
//		// Assume pressure varies linearly through heat exchanger
//		double P_c = P_c_out + i*(P_c_in - P_c_out) / (N_nodes - 1);
//		double P_h = P_h_in - i*(P_h_in - P_h_out) / (N_nodes - 1);
//
//		// Calculate the entahlpy at the node
//		double h_c = h_c_out + i*(h_c_in - h_c_out) / (N_nodes - 1);
//		double h_h = h_h_in - i*(h_h_in - h_h_out) / (N_nodes - 1);
//
//		// Calculate the hot and cold temperatures at the node
//		prop_error_code = CO2_PH(P_h, h_h, &co2_props);
//		if( prop_error_code != 0 )
//		{
//			error_code = 12;
//			return;
//		}
//		double T_h = co2_props.temp;
//
//		prop_error_code = CO2_PH(P_c, h_c, &co2_props);
//		if( prop_error_code != 0 )
//		{
//			error_code = 13;
//			return;
//		}
//		double T_c = co2_props.temp;
//
//		// Check that 2nd law was not violated
//		if( T_c >= T_h )
//		{
//			error_code = 11;
//			return;
//		}
//
//		// Track the minimum temperature difference in the heat exchanger
//		min_DT = fmin(min_DT, T_h - T_c);
//
//		// Perform effectiveness-NTU and UA calculations 
//		if( i > 0 )
//		{
//			double C_dot_h = m_dot_h*(h_h_prev - h_h) / (T_h_prev - T_h);			// [kW/K] hot stream capacitance rate
//			double C_dot_c = m_dot_c*(h_c_prev - h_c) / (T_c_prev - T_c);			// [kW/K] cold stream capacitance rate
//			double C_dot_min = min(C_dot_h, C_dot_c);				// [kW/K] Minimum capacitance stream
//			double C_dot_max = max(C_dot_h, C_dot_c);				// [kW/K] Maximum capacitance stream
//			double C_R = C_dot_min / C_dot_max;						// [-] Capacitance ratio of sub-heat exchanger
//			double eff = (Q_dot / (double)N_hxrs) / (C_dot_min*(T_h_prev - T_c));	// [-] Effectiveness of each sub-heat exchanger
//			double NTU = 0.0;
//			if( C_R != 1.0 )
//				NTU = log((1.0 - eff*C_R) / (1.0 - eff)) / (1.0 - C_R);		// [-] NTU if C_R does not equal 1
//			else
//				NTU = eff / (1.0 - eff);
//			UA += NTU*C_dot_min;						// [kW/K] Sum UAs for each hx section			
//		}
//		h_h_prev = h_h;
//		T_h_prev = T_h;
//		h_c_prev = h_c;
//		T_c_prev = T_c;
//	}
//
//	// Check for NaNs that arose
//	if( UA != UA )
//	{
//		error_code = 14;
//		return;
//	}
//
//	return;
//};

void isen_eta_from_poly_eta(double T_in /*K*/, double P_in /*kPa*/, double P_out /*kPa*/, double poly_eta /*-*/, bool is_comp, int & error_code, double & isen_eta)
{
	/* 9.3.14: code written by John Dyreby, translated to C++ by Ty Neises
	! Calculate the isentropic efficiency that corresponds to a given polytropic efficiency
	! for the expansion or compression from T_in and P_in to P_out.
	!
	! Inputs:
	!   T_in -- inlet temperature (K)
	!   P_in -- inlet pressure (kPa)
	!   P_out -- outlet pressure (kPa)
	!   poly_eta -- polytropic efficiency (-)
	!   is_comp -- if .true., model a compressor (w = w_s / eta); if .false., model a turbine (w = w_s * eta)
	!
	! Outputs:
	!   error_trace -- an ErrorTrace object
	!   isen_eta -- the equivalent isentropic efficiency (-)
	!
	! Notes:
	!   1) Integration of small DP is approximated numerically by using 200 stages.
	!   2) No error checking is performed on the inlet and outlet pressures; valid pressure ratios are assumed. */

	CO2_state co2_props;
	
	// Properties at the inlet conditions
	int prop_error_code = CO2_TP(T_in, P_in, &co2_props);
	if(prop_error_code != 0)
	{
		error_code = prop_error_code;
		return;
	}
	double h_in = co2_props.enth;
	double s_in = co2_props.entr;

	// Outlet enthalpy if compression/expansion is isentropic
	prop_error_code = CO2_PS(P_out, s_in, &co2_props);
	if(prop_error_code != 0)
	{
		error_code = prop_error_code;
		return;
	}
	double h_s_out = co2_props.enth;

	double stage_P_in = P_in;		// Initialize first stage inlet pressure
	double stage_h_in = h_in;		// Initialize first stage inlet enthalpy
	double stage_s_in = s_in;		// Initialize first stage inlet entropy

	int N_stages = 200;

	double stage_DP = (P_out - P_in) / (double)N_stages;

	double stage_P_out = -999.9;
	double stage_h_out = -999.9;

	for( int i = 1; i <= N_stages; i++ )
	{
		stage_P_out = stage_P_in + stage_DP;

		// Outlet enthalpy if compression/expansion is isentropic
		prop_error_code = CO2_PS(stage_P_out, stage_s_in, &co2_props);
		if( prop_error_code != 0 )
		{
			error_code = prop_error_code;
			return;
		}
		double stage_h_s_out = co2_props.enth;

		double w_s = stage_h_in - stage_h_s_out;		// specific work if process is isentropic
		double w = numeric_limits<double>::quiet_NaN();
		if( is_comp )
			w = w_s / poly_eta;
		else
			w = w_s * poly_eta;
		stage_h_out = stage_h_in - w;

		// Reset next stage inlet values
		stage_P_in = stage_P_out;
		stage_h_in = stage_h_out;

		prop_error_code = CO2_PH(stage_P_in, stage_h_in, &co2_props);
		if( prop_error_code != 0 )
		{
			error_code = prop_error_code;
			return;
		}
		stage_s_in = co2_props.entr;
	}

	// Note: last stage outlet enthalpy is equivalent to turbomachinery outlet enthalpy
	if( is_comp )
		isen_eta = (h_s_out - h_in) / (stage_h_out - h_in);
	else
		isen_eta = (stage_h_out - h_in) / (h_s_out - h_in);
}

void C_HeatExchanger::initialize(const S_design_parameters & des_par_in)
{
	ms_des_par = des_par_in;
	return;
}

void C_HeatExchanger::hxr_pressure_drops(const std::vector<double> & m_dots, std::vector<double> & hxr_deltaP)
{
	int N = m_dots.size();
	hxr_deltaP.resize(N);
	for( int i = 0; i < N; i++ )
		hxr_deltaP[i] = ms_des_par.m_DP_design[i] * pow((m_dots[i] / ms_des_par.m_m_dot_design[i]), 1.75);
}

void C_HeatExchanger::hxr_conductance(const std::vector<double> & m_dots, double & hxr_UA)
{
	int N = m_dots.size();
	double m_dot_ratio = 0.5*(m_dots[0] / ms_des_par.m_m_dot_design[0] + m_dots[1] / ms_des_par.m_m_dot_design[1]);
	hxr_UA = ms_des_par.m_UA_design*pow(m_dot_ratio, 0.8);
}

void C_turbine::turbine_sizing(const S_design_parameters & des_par_in, int & error_code)
{
	/* 9.4.14: code from John Dyreby, converted to C++ by Ty Neises
	! Determine the turbine rotor diameter, effective nozzle area, and design-point shaft
	! speed and store values in recomp_cycle%t.
	!
	! Arguments:
	!   recomp_cycle -- a RecompCycle object that defines the simple/recompression cycle at the design point
	!   error_trace -- an ErrorTrace object
	!
	! Notes:
	!   1) The value for recomp_cycle%t%N_design is required to be set.  If it is <= 0.0 then
	!      the value for recomp_cycle%mc%N_design is used (i.e., link the compressor and turbine
	!      shafts).  For this reason, turbine_sizing must be called after compressor_sizing if
	!      the shafts are to be linked. */

	CO2_state co2_props;

	ms_des_par = des_par_in;

	// Check that a design-point shaft speed is available
	if( ms_des_par.m_N_design <= 0.0 )	// Link shafts
	{
		ms_des_solved.m_N_design = ms_des_par.m_N_comp_design_if_linked;
		if( ms_des_par.m_N_design <= 0.0 )
		{
			error_code = 7;
			return;
		}
	}
	else
		ms_des_solved.m_N_design = ms_des_par.m_N_design;

	// Get speed of sound at inlet
	int prop_error_code = CO2_TD(ms_des_par.m_T_in, ms_des_par.m_D_in, &co2_props);
	if( prop_error_code != 0 )
	{
		error_code = prop_error_code;
		return;
	}
	double ssnd_in = co2_props.ssnd;

	// Outlet specific enthalpy after isentropic expansion
	prop_error_code = CO2_PS(ms_des_par.m_P_out, ms_des_par.m_s_in, &co2_props);
	if( prop_error_code != 0 )
	{
		error_code = prop_error_code;
		return;
	}
	double h_s_out = co2_props.enth;

	// Determine necessary turbine parameters
	ms_des_solved.m_nu_design = m_nu_design;
	double w_i = ms_des_par.m_h_in - h_s_out;			//[kJ/kg] Isentropic specific work of turbine
	double C_s = sqrt(2.0*w_i*1000.0);					//[m/s] Spouting velocity
	double U_tip = ms_des_solved.m_nu_design*C_s;		//[m/s] Tip speed
	ms_des_solved.m_D_rotor = U_tip / (0.5*ms_des_solved.m_N_design*0.104719755);	//[m]
	ms_des_solved.m_A_nozzle = ms_des_par.m_m_dot / (C_s*ms_des_par.m_D_in);		//[m^2]

	// Set other turbine variables
	ms_des_solved.m_w_tip_ratio = U_tip / ssnd_in;				//[-]
	ms_des_solved.m_eta = (ms_des_par.m_h_in - ms_des_par.m_h_out) / w_i;	//[-] Isentropic efficiency
}

void C_turbine::off_design_turbine(double T_in, double P_in, double P_out, double N, int & error_code, double & m_dot, double & T_out)
{
	/* 9.4.14: code from John Dyreby, converted to C++ by Ty Neises
	! Solve for the outlet state of 'turb' given its inlet conditions, outlet pressure, and shaft speed.
	!
	! Inputs:
	!   turb -- a Turbine object, with design-point values and sizing set
	!   T_in -- turbine inlet temperature (K)
	!   P_in -- turbine inlet pressure (kPa)
	!   P_out -- turbine outlet pressure (kPa)
	!   N -- shaft speed of turbine (rpm)
	!
	! Outputs:
	!   error_trace -- an ErrorTrace object
	!   m_dot -- allowable mass flow rate through the turbine (kg/s)
	!   T_out -- turbine outlet temperature (K)
	!
	! Notes:
	!   1) This subroutine also sets the following values in 'turb': nu, eta, m_dot, w, w_tip_ratio */

	CO2_state co2_props;

	// Get properties at turbine inlet
	int prop_error_code = CO2_TP(T_in, P_in, &co2_props);
	if( prop_error_code != 0 )
	{
		error_code = prop_error_code;
		return;
	}
	double D_in = co2_props.dens;
	double h_in = co2_props.enth;
	double s_in = co2_props.entr;
	double ssnd_in = co2_props.ssnd;

	prop_error_code = CO2_PS(P_out, s_in, &co2_props);
	if( prop_error_code != 0 )
	{
		error_code = prop_error_code;
		return;
	}
	double h_s_out = co2_props.enth;

	// Apply the radial turbine equations for efficiency
	double C_s = sqrt(2.0*(h_in - h_s_out)*1000.0);				//[m/s] spouting velocity
	double U_tip = ms_des_solved.m_D_rotor*0.5*N*0.104719755;	//[m/s] tip speed
	ms_od_solved.m_nu = U_tip / C_s;							//[-] ratio of tip speed to spouting velocity

	double eta_0 = (((1.0626*ms_od_solved.m_nu - 3.0874)*ms_od_solved.m_nu + 1.3668)*ms_od_solved.m_nu + 1.3567)*ms_od_solved.m_nu + 0.179921180;
	eta_0 = max(eta_0, 0.0);
	eta_0 = min(eta_0, 1.0);
	ms_od_solved.m_eta = eta_0*ms_des_solved.m_eta;		//[-] Actual turbine efficiency

	// Calculate the outlet state and allowable mass flow rate
	double h_out = h_in - ms_od_solved.m_eta*(h_in - h_s_out);		//[kJ/kg] Enthalpy at turbine outlet
	prop_error_code = CO2_PH(P_out, h_out, &co2_props);
	if( prop_error_code != 0 )
	{
		error_code = prop_error_code;
		return;
	}
	T_out = co2_props.temp;

	m_dot = C_s*ms_des_solved.m_A_nozzle*D_in;			//[kg/s] Mass flow rate through turbine
	ms_od_solved.m_w_tip_ratio = U_tip / ssnd_in;		//[-] Ratio of the tip speed to the local (turbine inlet) speed of sound
	ms_od_solved.m_N = N;
	ms_od_solved.m_W_dot_out = m_dot*(h_in-h_out);		//[kW] Turbine power output
}

void C_turbine::od_turbine_at_N_des(double T_in, double P_in, double P_out, int & error_code, double & m_dot, double & T_out)
{
	double N = ms_des_solved.m_N_design;		//[rpm]

	off_design_turbine(T_in, P_in, P_out, N, error_code, m_dot, T_out);

	return;
}

void C_compressor::compressor_sizing(const S_design_parameters & des_par_in, int & error_code)
{
	ms_des_par = des_par_in;

	CO2_state co2_props;

	int prop_error_code = CO2_TD(ms_des_par.m_T_out, ms_des_par.m_D_out, &co2_props);
	if( prop_error_code != 0 )
	{
		error_code = prop_error_code;
		return;
	}
	double ssnd_out = co2_props.ssnd;

	prop_error_code = CO2_PS(ms_des_par.m_P_out, ms_des_par.m_s_in, &co2_props);
	if( prop_error_code != 0 )
	{
		error_code = prop_error_code;
		return;
	}
	double h_s_out = co2_props.enth;

	// Calculate psi at the design-point phi using Horner's method
	double psi_design = ((((-498626.0*m_snl_phi_design) + 53224.0) * m_snl_phi_design - 2505.0) * m_snl_phi_design + 54.6) *
		m_snl_phi_design + 0.04049;		// from dimensionless modified head curve(at design - point, psi and modified psi are equal)

	// Determine required size and speed of compressor
	double w_i = h_s_out - ms_des_par.m_h_in;		//[kJ/kg] positive isentropic specific work of compressor
	double U_tip = sqrt(1000.0*w_i / psi_design);		//[m/s]
	ms_des_solved.m_D_rotor = sqrt(ms_des_par.m_m_dot / (m_snl_phi_design*ms_des_par.m_D_in*U_tip));
	double N_rad_s = U_tip*2.0 / ms_des_solved.m_D_rotor;		//[rad/s] shaft speed
	ms_des_solved.m_N_design = N_rad_s * 9.549296590;			//[rpm] shaft speed

	// Set other compressor member data
	ms_des_solved.m_w_tip_ratio = U_tip / ssnd_out;
	ms_des_solved.m_eta_design = w_i / (ms_des_par.m_h_out - ms_des_par.m_h_in);
	ms_des_solved.m_phi_des = m_snl_phi_design;
	ms_des_solved.m_phi_surge = m_snl_phi_min;
	ms_des_solved.m_phi_max = m_snl_phi_max;

	ms_des_solved.m_n_stages = 1;
}

void C_compressor::od_comp_phi_opt(double T_in /*K*/, double P_in /*kPa*/, double m_dot /*kg/s*/,
	int &error_code, double &T_out /*K*/, double &P_out /*kPa*/)
{
	double phi = m_snl_phi_design;

	od_comp_phi(phi, T_in, P_in, m_dot, error_code, T_out, P_out);

	return;
}

void C_compressor::od_comp_phi(double phi_in /*-*/, double T_in /*K*/, double P_in /*kPa*/, double m_dot /*kg/s*/,
	int &error_code, double &T_out /*K*/, double &P_out /*kPa*/)
{
	CO2_state co2_props;

	// Get properties at the inlet state of the compressor
	int prop_error_code = CO2_TP(T_in, P_in, &co2_props);
	if( prop_error_code != 0 )
	{
		error_code = 1;
		return;
	}
	double rho_in = co2_props.dens;
	double h_in = co2_props.enth;
	double s_in = co2_props.entr;

	// Want design-point value of phi
	double U_tip = m_dot / (rho_in*pow(ms_des_solved.m_D_rotor, 2)*phi_in);	//[m/s]
	double N_mc = U_tip / (ms_des_solved.m_D_rotor*0.5*0.104719755);		//[rpm]

	// Can now call performance call that requires shaft speed
	off_design_compressor(T_in, P_in, m_dot, N_mc, error_code, T_out, P_out);

	return;
}

void C_compressor::od_comp_at_N_des(double T_in, double P_in, double m_dot, int & error_code, double & T_out, double & P_out)
{
	double N = ms_des_solved.m_N_design;	//[rpm]

	off_design_compressor(T_in, P_in, m_dot, N, error_code, T_out, P_out);
}

void C_compressor::off_design_compressor(double T_in, double P_in, double m_dot, double N_in, int & error_code, double & T_out, double & P_out)
{
	CO2_state co2_props;

	ms_od_solved.m_N = N_in;		//[rpm]

	// Fully define the inlet state of the compressor
	int prop_error_code = CO2_TP(T_in, P_in, &co2_props);
	if( prop_error_code != 0 )
	{
		error_code = 1;
		return;
	}
	double rho_in = co2_props.dens;
	double h_in = co2_props.enth;
	double s_in = co2_props.entr;

	// Calculate the modified flow and head coefficients and efficiency for the SNL compressor
	double U_tip = ms_des_solved.m_D_rotor*0.5*ms_od_solved.m_N*0.104719755;				//[m/s]
	double phi = m_dot / (rho_in*U_tip*pow(ms_des_solved.m_D_rotor, 2));	//[-]
	if( phi < m_snl_phi_min )
	{
		ms_od_solved.m_surge = true;
		phi = m_snl_phi_min;
	}
	else
		ms_od_solved.m_surge = false;

	double phi_star = phi*pow(ms_od_solved.m_N / ms_des_solved.m_N_design, 0.2);		//[-] modified flow coefficient
	double psi_star = ((((-498626.0*phi_star) + 53224.0)*phi_star - 2505.0)*phi_star + 54.6)*phi_star + 0.04049;	// from dimensionless modified head curve
	double eta_star = ((((-1.638e6*phi_star) + 182725.0)*phi_star - 8089.0)*phi_star + 168.6)*phi_star - 0.7069;	// from dimensionless modified efficiency curve
	double psi = psi_star / pow(ms_des_solved.m_N_design / ms_od_solved.m_N, pow(20.0*phi_star, 3.0));
	double eta_0 = eta_star*1.47528 / pow(ms_des_solved.m_N_design / ms_od_solved.m_N, pow(20.0*phi_star, 5.0));		// Efficiency is normalized so it equals 1.0 at snl_phi_design
	ms_od_solved.m_eta = max(eta_0*ms_des_solved.m_eta_design, 0.0);		//[-] Actual compressor efficiency, not allowed to go negative

	// Check that the specified mass flow rate is possible with the compressor's current shaft speed
	if( psi <= 0.0 )
	{
		error_code = 1;
		return;
	}

	// Calculate the compressor outlet state
	double dh_s = psi * pow(U_tip, 2.0) * 0.001;			//[kJ/kg] Ideal enthalpy rise in compressor, from definition of head coefficient
	double dh = dh_s / ms_od_solved.m_eta;					//[kJ/kg] Actual enthalpy rise in compressor
	double h_s_out = h_in + dh_s;							//[kJ/kg] Ideal enthalpy at compressor outlet
	double h_out = h_in + dh;								//[kJ/kg] Actual enthalpy at compressor outlet

	// Get the compressor outlet pressure
	prop_error_code = CO2_HS(h_s_out, s_in, &co2_props);
	if( prop_error_code != 0 )
	{
		error_code = 2;
		return;
	}
	P_out = co2_props.pres;

	// Determine compressor outlet temperature and speed of sound
	prop_error_code = CO2_PH(P_out, h_out, &co2_props);
	if( prop_error_code != 0 )
	{
		error_code = 2;
		return;
	}
	T_out = co2_props.temp;
	double ssnd_out = co2_props.ssnd;

	// Set a few compressor variables
	ms_od_solved.m_phi = phi;
	ms_od_solved.m_surge_safety = phi / m_snl_phi_min;	//[-] If > 1, then not in surge
	ms_od_solved.m_w_tip_ratio = U_tip / ssnd_out;
	ms_od_solved.m_W_dot_in = m_dot*(h_out - h_in);	//[kWe]
	ms_od_solved.m_P_in = P_in;		//[kPa]
	ms_od_solved.m_P_out = P_out;	//[kPa]
}

void C_recompressor::recompressor_sizing(const S_design_parameters & des_par_in, int & error_code)
{
	ms_des_par = des_par_in;

	CO2_state co2_props;

	// Ideal specific enthalpy after compression
	int prop_error_code = CO2_PS(ms_des_par.m_P_out, ms_des_par.m_s_in, &co2_props);
	if( prop_error_code != 0 )
	{
		error_code = prop_error_code;
		return;
	}
	double h_s_out = co2_props.enth;

	double eta_design = (h_s_out - ms_des_par.m_h_in) / (ms_des_par.m_h_out - ms_des_par.m_h_in);		//[-] overall isentropic efficiency
	double psi_design = ((((-498626.0*m_snl_phi_design) + 53224.0)*m_snl_phi_design - 2505.0)*m_snl_phi_design + 54.6)*m_snl_phi_design + 0.04049;

	// Prepare intermediate pressure iteration loop
	double last_residual = 0.0;
	double last_P_int = 1.E12;			// Ensures bisection will be used for first loop
	double lower_bound = ms_des_par.m_P_in + 1.E-6;
	double upper_bound = ms_des_par.m_P_out - 1.E-6;
	double P_int = 0.5*(lower_bound + upper_bound);
	double eta_stage = eta_design;	// First guess for stage efficiency
	int max_iter = 100;
	double tolerance = 1.0E-8;

	double D_rotor_1 = -999.9;
	double D_rotor_2 = -999.9;
	double N_design = -999.9;

	double ssnd_int = std::numeric_limits<double>::quiet_NaN();
	double U_tip_1 = std::numeric_limits<double>::quiet_NaN();
	double U_tip_2 = std::numeric_limits<double>::quiet_NaN();

	int i = -1;
	for( i = 0; i < max_iter; i++ )
	{
		// First stage
		// Ideal outlet specific enthalpy after first stage
		prop_error_code = CO2_PS(P_int, ms_des_par.m_s_in, &co2_props);
		if( prop_error_code != 0 )
		{
			error_code = prop_error_code;
			return;
		}
		double h_s_out = co2_props.enth;

		double w_i = h_s_out - ms_des_par.m_h_in;				//[kJ/kg] positive isentropic specific work of first stage
		U_tip_1 = sqrt(1000.0 * w_i / psi_design);		//[m/s]
		D_rotor_1 = sqrt(ms_des_par.m_m_dot / (m_snl_phi_design*ms_des_par.m_D_in*U_tip_1));	//[m]
		double N_rad_s = U_tip_1*2.0 / D_rotor_1;				//[rad/s]
		N_design = N_rad_s*9.549296590;					//[rpm]
		double w = w_i / eta_stage;								//[kJ/kg]
		double h_int = ms_des_par.m_h_in + w;					//[kJ/kg] Energy balance on first stage

		prop_error_code = CO2_PH(P_int, h_int, &co2_props);
		if( prop_error_code != 0 )
		{
			error_code = prop_error_code;
			return;
		}
		double D_int = co2_props.dens;
		double s_int = co2_props.entr;
		ssnd_int = co2_props.ssnd;

		// Second stage
		prop_error_code = CO2_PS(ms_des_par.m_P_out, s_int, &co2_props);
		if( prop_error_code != 0 )
		{
			error_code = prop_error_code;
			return;
		}
		h_s_out = co2_props.enth;

		w_i = h_s_out - h_int;			// positive isentropic specific work of second stage
		U_tip_2 = sqrt(1000.0*w_i / psi_design);
		D_rotor_2 = 2.0*U_tip_2 / (N_design*0.104719755);
		double phi = ms_des_par.m_m_dot / (D_int*U_tip_2*pow(D_rotor_2, 2));	//[-] Flow coefficient
		double eta_2_req = w_i / (ms_des_par.m_h_out - h_int);				//[-] Required second stage efficiency to achieve overall eta_design

		// Check convergence and update guesses
		double residual = m_snl_phi_design - phi;
		if( residual < 0.0 )		// P_int guess is too high
		{
			if( -residual <= tolerance && fabs(eta_stage - eta_2_req) <= tolerance )
				break;
			upper_bound = P_int;
		}
		else					// P_int guess is too low
		{
			if( -residual <= tolerance && fabs(eta_stage - eta_2_req) <= tolerance )
				break;
			lower_bound = P_int;
		}

		double secant_step = -residual*(last_P_int - P_int) / (last_residual - residual);
		double P_secant = P_int + secant_step;
		last_P_int = P_int;
		last_residual = residual;
		if( P_secant <= lower_bound || P_secant >= upper_bound )		// secant method overshot
			P_int = 0.5*(lower_bound + upper_bound);
		else if( fabs(secant_step) > fabs(0.5*(upper_bound - lower_bound)) )	// take the smaller step to ensure convergence
			P_int = 0.5*(lower_bound + upper_bound);
		else
			P_int = P_secant;		// Use secant guess

		eta_stage = 0.5*(eta_stage + eta_2_req);		// Update guess for stage efficiency
	}

	// Check for convergence
	if( i == max_iter )		// did  not converge
	{
		error_code = 1;
		return;
	}

	// Set recompressor variables
	ms_des_solved.m_D_rotor = D_rotor_1;
	ms_des_solved.m_D_rotor_2 = D_rotor_2;
	ms_des_solved.m_eta_design = eta_stage;
	ms_des_solved.m_N_design = N_design;
	
	// Calculate tip ratios for each stage
		// 1st stage (ssnd calculated in iteration loop above)
	ms_des_solved.m_w_tip_ratio_1 = U_tip_1 / ssnd_int;

		// 2nd stage
	prop_error_code = CO2_TD(ms_des_par.m_T_out, ms_des_par.m_D_out, &co2_props);
	if( prop_error_code != 0 )
	{
		error_code = prop_error_code;
		return;
	}
	double ssnd_out_2 = co2_props.ssnd;
	ms_des_solved.m_w_tip_ratio_2 = U_tip_2 / ssnd_out_2;
	

	ms_des_solved.m_phi_des = m_snl_phi_design;
	ms_des_solved.m_phi_surge = m_snl_phi_min;
	ms_des_solved.m_phi_max = m_snl_phi_max;
	ms_des_solved.m_n_stages = 2;

}

int C_recompressor::C_mono_eq_phi_off_design::operator()(double phi_1 /*-*/, double *P_high /*kPa*/)
{
	// Variables that we need to set as member data so the solver can call this method
	double m_dot = mpc_recompressor->ms_od_inputs.m_m_dot;	//[kg/s]
	double rho_in = mpc_recompressor->ms_od_inputs.m_rho_in;	//[kg/m^3]
	double h_in = mpc_recompressor->ms_od_inputs.m_h_in;		//[kJ/kg]
	double s_in = mpc_recompressor->ms_od_inputs.m_s_in;		//[kJ/kg-K]
	
	// First stage - dh_s and eta_stage_1
	double U_tip_1 = m_dot / (phi_1*rho_in*pow(mpc_recompressor->ms_des_solved.m_D_rotor, 2));		//[m/s]
	double N_od = (U_tip_1*2.0 / mpc_recompressor->ms_des_solved.m_D_rotor)*9.549296590;			//[rpm] shaft spped
	double phi_star = phi_1*pow((N_od / mpc_recompressor->ms_des_solved.m_N_design), 0.2);		//[-] Modified flow coefficient
	double psi_star = ((((-498626.0*phi_star) + 53224.0)*phi_star - 2505.0)*phi_star + 54.6)*phi_star + 0.04049;		//[-] from dimensionless modified head curve
	double psi = psi_star / (pow(mpc_recompressor->ms_des_solved.m_N_design / N_od, pow(20.0*phi_star, 3.0)));
	double dh_s = psi*pow(U_tip_1, 2)*0.001;										//[kJ/kg] Calculated ideal enthalpy rise in first stage of compressor
	double eta_star = ((((-1.638e6*phi_star) + 182725.0)*phi_star - 8089.0)*phi_star + 168.6)*phi_star - 0.7069;		//[-] from dimensionless modified efficiency curve
	double eta_0 = eta_star*1.47528 / pow((mpc_recompressor->ms_des_solved.m_N_design / N_od), pow(20.0*phi_star, 5));		//[-] Stage efficiency is normalized so it equals 1.0 at snl_phi_design
	double eta_stage_1 = max(eta_0*mpc_recompressor->ms_des_solved.m_eta_design, 0.0);			//[-] The actual stage efficiency, not allowed to go negative

	// Calculate first - stage outlet (second - stage inlet) state
	double dh = dh_s / eta_stage_1;		//[kJ/kg] Actual enthalpy rise in first stage
	double h_s_out = h_in + dh_s;		//[kJ/kg] Ideal enthalpy between stages
	double h_int = h_in + dh;			//[kJ/kg] Actual enthalpy between stages

	CO2_state co2_props;
	int prop_error_code = CO2_HS(h_s_out, s_in, &co2_props);
	if( prop_error_code != 0 )
	{
		*P_high = std::numeric_limits<double>::quiet_NaN();
		return prop_error_code;
	}
	double P_int = co2_props.pres;

	prop_error_code = CO2_PH(P_int, h_int, &co2_props);
	if( prop_error_code != 0 )
	{
		*P_high = std::numeric_limits<double>::quiet_NaN();
		return prop_error_code;
	}
	double D_int = co2_props.dens;
	double s_int = co2_props.entr;
	double ssnd_int = co2_props.ssnd;

	// Second stage - dh_s and eta_stage_2
	double U_tip_2 = mpc_recompressor->ms_des_solved.m_D_rotor_2*0.5*N_od*0.104719755;				//[m/s] second-stage tip speed in m/s
	double phi_2 = m_dot / (D_int*U_tip_2*pow(mpc_recompressor->ms_des_solved.m_D_rotor_2, 2));	//[-] second-stage flow coefficient
	phi_star = phi_2*pow((N_od / mpc_recompressor->ms_des_solved.m_N_design), 0.2);				//[-] modified flow coefficient
	psi_star = ((((-498626.0*phi_star) + 53224.0)*phi_star - 2505.0)*phi_star + 54.6)*phi_star + 0.04049;	// from dimensionless modified head curve
	psi = psi_star / (pow(mpc_recompressor->ms_des_solved.m_N_design / N_od, pow(20.0*phi_star, 3.0)));
	dh_s = psi*pow(U_tip_2, 2)*0.001;									// [kJ/kg] Calculated ideal enthalpy rise in second stage of compressor, from definition of head coefficient
	eta_star = ((((-1.638e6*phi_star) + 182725.0)*phi_star - 8089.0)*phi_star + 168.6)*phi_star - 0.7069;		//[-] from dimensionless modified efficiency curve
	eta_0 = eta_star*1.47528 / pow((mpc_recompressor->ms_des_solved.m_N_design / N_od), pow(20.0*phi_star, 5));	//[-] Stage efficiency is normalized so it equals 1.0 at snl_phi_design
	double eta_stage_2 = max(eta_0*mpc_recompressor->ms_des_solved.m_eta_design, 0.0);			//[-] the actual stage efficiency, not allowed to go negative

	// Calculate second-stage outlet state
	dh = dh_s / eta_stage_2;			// actual enthalpy rise in second stage
	h_s_out = h_int + dh_s;				// ideal enthalpy at compressor outlet
	double h_out = h_int + dh;			// actual enthalpy at compressor outlet

	// Get the calculated compressor outlet pressure
	prop_error_code = CO2_HS(h_s_out, s_int, &co2_props);
	if( prop_error_code != 0 )
	{
		*P_high = std::numeric_limits<double>::quiet_NaN();
		return prop_error_code;
	}
	*P_high = co2_props.pres;
	
	// Determine outlet temperature and speed of sound
	prop_error_code = CO2_PH(*P_high, h_out, &co2_props);
	if( prop_error_code != 0 )
	{
		*P_high = std::numeric_limits<double>::quiet_NaN();
		return prop_error_code;
	}
	mpc_recompressor->ms_od_solved.m_T_out = co2_props.temp;		//[K]
	double ssnd_out = co2_props.ssnd;

	// Set relevant recompressor variables
	mpc_recompressor->ms_od_solved.m_N = N_od;		//[rpm] shaft speed
	mpc_recompressor->ms_od_solved.m_eta = (h_s_out - h_in) / (h_out - h_in);		//[-] use overall isentropic efficiency
	mpc_recompressor->ms_od_solved.m_phi = phi_1;
	mpc_recompressor->ms_od_solved.m_phi_2 = phi_2;
	mpc_recompressor->ms_od_solved.m_w_tip_ratio = max(U_tip_1 / ssnd_int, U_tip_2 / ssnd_out);	//[-] maximum tip speed ratio
	mpc_recompressor->ms_od_solved.m_surge = phi_1 < m_snl_phi_min || phi_2 < m_snl_phi_min;
	mpc_recompressor->ms_od_solved.m_W_dot_in = m_dot*(h_out - h_in);		//[kWe]
	mpc_recompressor->ms_od_solved.m_surge_safety = min(phi_1, phi_2) / m_snl_phi_min;

	return 0;
}

void C_recompressor::off_design_recompressor(double T_in, double P_in, double m_dot, double P_out, int & error_code, double & T_out)
{
	/* code from John Dyreby, converted to C++ by Ty Neises
	! Solve for the outlet state (and shaft speed) of 'comp' given its inlet conditions, mass flow rate, and outlet pressure.
	!
	! Inputs:
	!   comp -- a Compressor object, with design-point values and sizing set
	!   T_in -- compressor inlet temperature (K)
	!   P_in -- compressor inlet pressure (kPa)
	!   m_dot -- mass flow rate through compressor (kg/s)
	!   P_out -- compressor outlet pressure (kPa)
	!
	! Outputs:
	!   error_trace -- an ErrorTrace object
	!   T_out -- compressor outlet temperature (K)
	!
	! Notes:
	!   1) This subroutine also sets the following values in 'comp': N, surge, eta, w, w_tip_ratio, phi
	!   2) In order to solve the compressor, the value for flow coefficient (phi) is varied until convergence.
	!   3) Surge is not allowed; if the corresponding flow coefficient is not between phi_min and phi_max an error is raised.
	!   4) Two-stage recompressor; surge is true if either stages are in surge conditions; w_tip_ratio is max of the two stages.
	*/

	CO2_state co2_props;

	// Fully define the inlet state of the compressor
	int prop_error_code = CO2_TP(T_in, P_in, &co2_props);
	if( prop_error_code != 0 )
	{
		error_code = prop_error_code;
		return;
	}
	ms_od_inputs.m_rho_in = co2_props.dens;
	ms_od_inputs.m_h_in = co2_props.enth;
	ms_od_inputs.m_s_in = co2_props.entr;
	ms_od_inputs.m_m_dot = m_dot;

	// Apply 1 var solver to find the phi that results in a converged recompressor
	C_mono_eq_phi_off_design c_rc_od(this);
	C_monotonic_eq_solver c_rd_od_solver(c_rc_od);
	
	// Set upper and lower bounds
	double phi_upper = ms_des_solved.m_phi_max;
	double phi_lower = ms_des_solved.m_phi_surge;

	// Generate first x-y pair
	double phi_guess_lower = ms_des_solved.m_phi_des;
	double P_solved_phi_guess_lower = std::numeric_limits<double>::quiet_NaN();
	int test_code = c_rd_od_solver.test_member_function(phi_guess_lower, &P_solved_phi_guess_lower);
	if( test_code != 0 )
	{
		for(int i = 1; i < 9; i++)
		{
			phi_guess_lower = ms_des_solved.m_phi_des*(10-i)/10.0 + ms_des_solved.m_phi_max*i/10.0;
			test_code = c_rd_od_solver.test_member_function(phi_guess_lower, &P_solved_phi_guess_lower);
			if(test_code == 0)
				break;
		}	
	}
	if( test_code != 0 )
	{
		// Can't find a RC phi guess value that returns an outlet pressure
		error_code = -20;
		return;
	}
	C_monotonic_eq_solver::S_xy_pair phi_pair_lower;
	phi_pair_lower.x = phi_guess_lower;
	phi_pair_lower.y = P_solved_phi_guess_lower;

	// Generate second x-y pair
	double phi_guess_upper = phi_guess_lower*0.5 + ms_des_solved.m_phi_max*0.5;
	double P_solved_phi_guess_upper = std::numeric_limits<double>::quiet_NaN();
	test_code = c_rd_od_solver.test_member_function(phi_guess_upper, &P_solved_phi_guess_upper);
	if( test_code != 0 )
	{
		for(int i = 6; i < 10; i++)
		{
			phi_guess_upper = phi_guess_lower*i/10.0 + ms_des_solved.m_phi_max*(10-i)/10.0;
			test_code = c_rd_od_solver.test_member_function(phi_guess_upper, &P_solved_phi_guess_upper);
			if(test_code == 0)
				break;
		}
		if( test_code != 0 && phi_guess_lower == ms_des_solved.m_phi_des )
		{
			for(int i = 6; i < 10; i++)
			{
				phi_guess_upper = phi_guess_lower*i/10.0 + ms_des_solved.m_phi_surge*(10-i)/10.0;
				test_code = c_rd_od_solver.test_member_function(phi_guess_upper, &P_solved_phi_guess_upper);
				if( test_code == 0 )
					break;
			}
		}
	}
	if( test_code != 0 )
	{
		// Can't find a RC 2nd guess value (which, if we've found a first, means the solution space is really small?)
		error_code = -20;
		return;
	}
	C_monotonic_eq_solver::S_xy_pair phi_pair_upper;
	phi_pair_upper.x = phi_guess_upper;
	phi_pair_upper.y = P_solved_phi_guess_upper;

	// Set solver settings
	c_rd_od_solver.settings(0.001, 50, phi_lower, phi_upper, true);

	// Now, solve for the flow coefficient
	double phi_solved, tol_solved;
	phi_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_solved = -1;

	int phi_code = 0;
	try
	{
		phi_code = c_rd_od_solver.solve(phi_pair_lower, phi_pair_upper, P_out, phi_solved, tol_solved, iter_solved);
	}
	catch( C_csp_exception )
	{
		error_code = -1;
		return;
	}

	if( phi_code != C_monotonic_eq_solver::CONVERGED )
	{
		int n_call_history = c_rd_od_solver.get_solver_call_history()->size();

		if( n_call_history > 0 )
			error_code = -(*(c_rd_od_solver.get_solver_call_history()))[n_call_history - 1].err_code;

		if( error_code == 0 )
		{
			error_code = phi_code;
		}

		return;
	}

	T_out = ms_od_solved.m_T_out;		//[K]
}

//void C_recompressor::off_design_recompressor(double T_in, double P_in, double m_dot, double P_out, int & error_code, double & T_out)
//{
//	/* code from John Dyreby, converted to C++ by Ty Neises
//	! Solve for the outlet state (and shaft speed) of 'comp' given its inlet conditions, mass flow rate, and outlet pressure.
//	!
//	! Inputs:
//	!   comp -- a Compressor object, with design-point values and sizing set
//	!   T_in -- compressor inlet temperature (K)
//	!   P_in -- compressor inlet pressure (kPa)
//	!   m_dot -- mass flow rate through compressor (kg/s)
//	!   P_out -- compressor outlet pressure (kPa)
//	!
//	! Outputs:
//	!   error_trace -- an ErrorTrace object
//	!   T_out -- compressor outlet temperature (K)
//	!
//	! Notes:
//	!   1) This subroutine also sets the following values in 'comp': N, surge, eta, w, w_tip_ratio, phi
//	!   2) In order to solve the compressor, the value for flow coefficient (phi) is varied until convergence.
//	!   3) Surge is not allowed; if the corresponding flow coefficient is not between phi_min and phi_max an error is raised.
//	!   4) Two-stage recompressor; surge is true if either stages are in surge conditions; w_tip_ratio is max of the two stages.
//	*/
//
//	CO2_state co2_props;
//
//	// Fully define the inlet state of the compressor
//	int prop_error_code = CO2_TP(T_in, P_in, &co2_props);
//	if( prop_error_code != 0 )
//	{
//		error_code = prop_error_code;
//		return;
//	}
//	double rho_in = co2_props.dens;
//	double h_in = co2_props.enth;
//	double s_in = co2_props.entr;
//
//	// Iterate on first-stage phi
//	double phi_1 = m_snl_phi_design;		// start with design-point value
//	bool first_pass = true;
//
//	int max_iter = 100;
//	double rel_tol = 1.0E-9;
//
//	double last_phi_1 = std::numeric_limits<double>::quiet_NaN();
//	double last_residual = std::numeric_limits<double>::quiet_NaN();
//	double P_out_calc = std::numeric_limits<double>::quiet_NaN();
//	double h_out = std::numeric_limits<double>::quiet_NaN();
//	double N = std::numeric_limits<double>::quiet_NaN();
//	double phi_2 = std::numeric_limits<double>::quiet_NaN();
//	double U_tip_1 = std::numeric_limits<double>::quiet_NaN();
//	double ssnd_int = std::numeric_limits<double>::quiet_NaN();
//	double U_tip_2 = std::numeric_limits<double>::quiet_NaN();
//
//	int i = -1;
//	for( i = 0; i < max_iter; i++ )
//	{
//		// First stage - dh_s and eta_stage_1
//		U_tip_1 = m_dot / (phi_1*rho_in*pow(ms_des_solved.m_D_rotor, 2));		//[m/s]
//		N = (U_tip_1*2.0 / ms_des_solved.m_D_rotor)*9.549296590;					//[rpm] shaft spped
//		double phi_star = phi_1*pow((N / ms_des_solved.m_N_design), 0.2);				//[-] Modified flow coefficient
//		double psi_star = ((((-498626.0*phi_star) + 53224.0)*phi_star - 2505.0)*phi_star + 54.6)*phi_star + 0.04049;		//[-] from dimensionless modified head curve
//		double psi = psi_star / (pow(ms_des_solved.m_N_design / N, pow(20.0*phi_star, 3.0)));
//		double dh_s = psi*pow(U_tip_1, 2)*0.001;										//[kJ/kg] Calculated ideal enthalpy rise in first stage of compressor
//		double eta_star = ((((-1.638e6*phi_star) + 182725.0)*phi_star - 8089.0)*phi_star + 168.6)*phi_star - 0.7069;		//[-] from dimensionless modified efficiency curve
//		double eta_0 = eta_star*1.47528 / pow((ms_des_solved.m_N_design / N), pow(20.0*phi_star, 5));		//[-] Stage efficiency is normalized so it equals 1.0 at snl_phi_design
//		double eta_stage_1 = max(eta_0*ms_des_solved.m_eta_design, 0.0);				//[-] The actual stage efficiency, not allowed to go negative
//
//		// Calculate first - stage outlet (second - stage inlet) state
//		double dh = dh_s / eta_stage_1;		//[kJ/kg] Actual enthalpy rise in first stage
//		double h_s_out = h_in + dh_s;		//[kJ/kg] Ideal enthalpy between stages
//		double h_int = h_in + dh;			//[kJ/kg] Actual enthalpy between stages
//
//		prop_error_code = CO2_HS(h_s_out, s_in, &co2_props);
//		if( prop_error_code != 0 )
//		{
//			error_code = prop_error_code;
//			return;
//		}
//		double P_int = co2_props.pres;
//
//		prop_error_code = CO2_PH(P_int, h_int, &co2_props);
//		if( prop_error_code != 0 )
//		{
//			error_code = prop_error_code;
//			return;
//		}
//		double D_int = co2_props.dens;
//		double s_int = co2_props.entr;
//		ssnd_int = co2_props.ssnd;
//
//		// Second stage - dh_s and eta_stage_2
//		U_tip_2 = ms_des_solved.m_D_rotor_2*0.5*N*0.104719755;				// second-stage tip speed in m/s
//		phi_2 = m_dot / (D_int*U_tip_2*pow(ms_des_solved.m_D_rotor_2, 2));	// second-stage flow coefficient
//		phi_star = phi_2*pow((N / ms_des_solved.m_N_design), 0.2);					// modified flow coefficient
//		psi_star = ((((-498626.0*phi_star) + 53224.0)*phi_star - 2505.0)*phi_star + 54.6)*phi_star + 0.04049;	// from dimensionless modified head curve
//		psi = psi_star / (pow(ms_des_solved.m_N_design / N, pow(20.0*phi_star, 3.0)));
//		dh_s = psi*pow(U_tip_2, 2)*0.001;									// [kJ/kg] Calculated ideal enthalpy rise in second stage of compressor, from definition of head coefficient
//		eta_star = ((((-1.638e6*phi_star) + 182725.0)*phi_star - 8089.0)*phi_star + 168.6)*phi_star - 0.7069;		//[-] from dimensionless modified efficiency curve
//		eta_0 = eta_star*1.47528 / pow((ms_des_solved.m_N_design / N), pow(20.0*phi_star, 5));		//[-] Stage efficiency is normalized so it equals 1.0 at snl_phi_design
//		double eta_stage_2 = max(eta_0*ms_des_solved.m_eta_design, 0.0);			// the actual stage efficiency, not allowed to go negative
//
//		// Calculate second-stage outlet state
//		dh = dh_s / eta_stage_2;			// actual enthalpy rise in second stage
//		h_s_out = h_int + dh_s;				// ideal enthalpy at compressor outlet
//		h_out = h_int + dh;					// actual enthalpy at compressor outlet
//
//		// Get the calculated compressor outlet pressure
//		prop_error_code = CO2_HS(h_s_out, s_int, &co2_props);
//		if( prop_error_code != 0 )
//		{
//			error_code = prop_error_code;
//			return;
//		}
//		P_out_calc = co2_props.pres;
//
//		// Check for convergence and adjust phi_1 guess
//		double residual = P_out - P_out_calc;
//		if( fabs(residual) / P_out <= rel_tol )
//			break;
//
//		double next_phi = std::numeric_limits<double>::quiet_NaN();
//		if( first_pass )
//		{
//			next_phi = phi_1*1.0001;		// take a small step
//			first_pass = false;
//		}
//		else
//			next_phi = phi_1 - residual*(last_phi_1 - phi_1) / (last_residual - residual);		// next guess predicted using secant method
//
//		last_phi_1 = phi_1;
//		last_residual = residual;
//		phi_1 = next_phi;
//	}
//
//	// Check for convergence
//	if( i == max_iter )		// did not converge
//	{
//		error_code = 1;
//		return;
//	}
//
//	// Determine outlet temperature and speed of sound
//	prop_error_code = CO2_PH(P_out_calc, h_out, &co2_props);
//	if( prop_error_code != 0 )
//	{
//		error_code = prop_error_code;
//		return;
//	}
//	T_out = co2_props.temp;
//	double ssnd_out = co2_props.ssnd;
//
//	// Outlet specific enthalpy after isentropic compression
//	prop_error_code = CO2_PS(P_out_calc, s_in, &co2_props);
//	if( prop_error_code != 0 )
//	{
//		error_code = prop_error_code;
//		return;
//	}
//	double h_s_out = co2_props.enth;
//
//	// Set relevant recompressor variables
//	ms_od_solved.m_N = N;
//	ms_od_solved.m_eta = (h_s_out - h_in) / (h_out - h_in);		// use overall isentropic efficiency
//	ms_od_solved.m_phi = phi_1;
//	ms_od_solved.m_phi_2 = phi_2;
//	ms_od_solved.m_w_tip_ratio = max(U_tip_1 / ssnd_int, U_tip_2 / ssnd_out);	// store ratio
//	ms_od_solved.m_surge = phi_1 < m_snl_phi_min || phi_2 < m_snl_phi_min;
//	ms_od_solved.m_W_dot_in = m_dot*(h_out - h_in);		//[kWe]
//	ms_od_solved.m_surge_safety = min(phi_1,phi_2)/m_snl_phi_min;
//}

//void C_RecompCycle::design_core_bypass(int & error_code)
//{
//	CO2_state co2_props;
//
//	int max_iter = 500;
//	double temperature_tolerance = 1.E-6;		// Temp differences below this are considered zero
//
//	int cpp_offset = 1;
//
//	// Initialize a few variables
//	double m_dot_t, m_dot_mc, m_dot_rc, Q_dot_LT, Q_dot_HT, UA_LT_calc, UA_HT_calc;
//	m_dot_t = m_dot_mc = m_dot_rc = Q_dot_LT = Q_dot_HT = UA_LT_calc = UA_HT_calc = 0.0;
//
//	m_temp_last[1 - cpp_offset] = ms_des_par.m_T_mc_in;
//	m_pres_last[1 - cpp_offset] = ms_des_par.m_P_mc_in;
//	m_pres_last[2 - cpp_offset] = ms_des_par.m_P_mc_out;
//	m_temp_last[6 - cpp_offset] = ms_des_par.m_T_t_in;
//
//	// Apply pressure drops to heat exchangers, fully defining the pressures at all states
//	if( ms_des_par.m_DP_LT[1 - cpp_offset] < 0.0 )
//		m_pres_last[3 - cpp_offset] = m_pres_last[2 - cpp_offset] - m_pres_last[2 - cpp_offset] * fabs(ms_des_par.m_DP_LT[1 - cpp_offset]);	// relative pressure drop specified for LT recuperator (cold stream)
//	else
//		m_pres_last[3 - cpp_offset] = m_pres_last[2 - cpp_offset] - ms_des_par.m_DP_LT[1 - cpp_offset];									// absolute pressure drop specified for LT recuperator (cold stream)
//
//	if( ms_des_par.m_UA_LT < 1.0E-12 )
//		m_pres_last[3 - cpp_offset] = m_pres_last[2 - cpp_offset];		// If there is no LT recuperator, there is no pressure drop
//
//	m_pres_last[4 - cpp_offset] = m_pres_last[3 - cpp_offset];			// Assume no pressure drop in mixing valve
//	m_pres_last[10 - cpp_offset] = m_pres_last[3 - cpp_offset];			// Assume no pressure drop in mixing valve
//
//	if( ms_des_par.m_DP_HT[1 - cpp_offset] < 0.0 )
//		m_pres_last[5 - cpp_offset] = m_pres_last[4 - cpp_offset] - m_pres_last[4 - cpp_offset] * fabs(ms_des_par.m_DP_HT[1 - cpp_offset]);	// relative pressure drop specified for HT recuperator (cold stream)
//	else
//		m_pres_last[5 - cpp_offset] = m_pres_last[4 - cpp_offset] - ms_des_par.m_DP_HT[1 - cpp_offset];									// absolute pressure drop specified for HT recuperator (cold stream)
//
//	if( ms_des_par.m_UA_HT < 1.0E-12 )
//		m_pres_last[5 - cpp_offset] = m_pres_last[4 - cpp_offset];		// If there is no HT recuperator, there is no pressure drop
//
//	if( ms_des_par.m_DP_PHX[1 - cpp_offset] < 0.0 )
//		m_pres_last[6 - cpp_offset] = m_pres_last[5 - cpp_offset] - m_pres_last[5 - cpp_offset] * fabs(ms_des_par.m_DP_PHX[1 - cpp_offset]);	// relative pressure drop specified for PHX
//	else
//		m_pres_last[6 - cpp_offset] = m_pres_last[5 - cpp_offset] - ms_des_par.m_DP_PHX[1 - cpp_offset];									// absolute pressure drop specified for PHX
//
//	if( ms_des_par.m_DP_PC[2 - cpp_offset] < 0.0 )
//		m_pres_last[9 - cpp_offset] = m_pres_last[1 - cpp_offset] / (1.0 - fabs(ms_des_par.m_DP_PC[2 - cpp_offset]));	// relative pressure drop specified for precooler: P1=P9-P9*rel_DP => P1=P9*(1-rel_DP)
//	else
//		m_pres_last[9 - cpp_offset] = m_pres_last[1 - cpp_offset] + ms_des_par.m_DP_PC[2 - cpp_offset];
//
//	if( ms_des_par.m_DP_LT[2 - cpp_offset] < 0.0 )
//		m_pres_last[8 - cpp_offset] = m_pres_last[9 - cpp_offset] / (1.0 - fabs(ms_des_par.m_DP_LT[2 - cpp_offset]));	// relative pressure drop specified for LT recuperator (hot stream)
//	else
//		m_pres_last[8 - cpp_offset] = m_pres_last[9 - cpp_offset] + ms_des_par.m_DP_LT[2 - cpp_offset];				// absolute pressure drop specified for LT recuperator (hot stream)
//
//	if( ms_des_par.m_UA_LT < 1.0E-12 )
//		m_pres_last[8 - cpp_offset] = m_pres_last[9 - cpp_offset];		// if there is no LT recuperator, there is no pressure drop
//
//	if( ms_des_par.m_DP_HT[2 - cpp_offset] < 0.0 )
//		m_pres_last[7 - cpp_offset] = m_pres_last[8 - cpp_offset] / (1.0 - fabs(ms_des_par.m_DP_HT[2 - cpp_offset]));	// relative pressure drop specified for HT recuperator (hot stream)
//	else
//		m_pres_last[7 - cpp_offset] = m_pres_last[8 - cpp_offset] + ms_des_par.m_DP_HT[2 - cpp_offset];				// absolute pressure drop specified for HT recuperator (hot stream)
//
//	if( ms_des_par.m_UA_HT < 1.0E-12 )
//		m_pres_last[7 - cpp_offset] = m_pres_last[8 - cpp_offset];		// if there is no HT recuperator, there is no pressure drop
//
//	// Determine equivalent isentropic efficiencies for main compressor and turbine, if necessary.
//	double eta_mc_isen = std::numeric_limits<double>::quiet_NaN();
//	double eta_t_isen = std::numeric_limits<double>::quiet_NaN();
//	if( ms_des_par.m_eta_mc < 0.0 )
//	{
//		int poly_error_code = 0;
//
//		isen_eta_from_poly_eta(m_temp_last[1 - cpp_offset], m_pres_last[1 - cpp_offset], m_pres_last[2 - cpp_offset], fabs(ms_des_par.m_eta_mc),
//			true, poly_error_code, eta_mc_isen);
//
//		if( poly_error_code != 0 )
//		{
//			error_code = poly_error_code;
//			return;
//		}
//	}
//	else
//		eta_mc_isen = ms_des_par.m_eta_mc;
//
//	if( ms_des_par.m_eta_t < 0.0 )
//	{
//		int poly_error_code = 0;
//
//		isen_eta_from_poly_eta(m_temp_last[6 - cpp_offset], m_pres_last[6 - cpp_offset], m_pres_last[7 - cpp_offset], fabs(ms_des_par.m_eta_t),
//			false, poly_error_code, eta_t_isen);
//
//		if( poly_error_code != 0 )
//		{
//			error_code = poly_error_code;
//			return;
//		}
//	}
//	else
//		eta_t_isen = ms_des_par.m_eta_t;
//
//	// Determine the outlet state and specific work for the main compressor and turbine.
//	int comp_error_code = 0;
//	double w_mc = std::numeric_limits<double>::quiet_NaN();
//	// Main compressor
//	calculate_turbomachinery_outlet_1(m_temp_last[1 - cpp_offset], m_pres_last[1 - cpp_offset], m_pres_last[2 - cpp_offset], eta_mc_isen, true,
//		comp_error_code, m_enth_last[1 - cpp_offset], m_entr_last[1 - cpp_offset], m_dens_last[1 - cpp_offset], m_temp_last[2 - cpp_offset],
//		m_enth_last[2 - cpp_offset], m_entr_last[2 - cpp_offset], m_dens_last[2 - cpp_offset], w_mc);
//
//	if( comp_error_code != 0 )
//	{
//		error_code = comp_error_code;
//		return;
//	}
//
//	int turbine_error_code = 0;
//	double w_t = std::numeric_limits<double>::quiet_NaN();
//	// Turbine
//	calculate_turbomachinery_outlet_1(m_temp_last[6 - cpp_offset], m_pres_last[6 - cpp_offset], m_pres_last[7 - cpp_offset], eta_t_isen, false,
//		turbine_error_code, m_enth_last[6 - cpp_offset], m_entr_last[6 - cpp_offset], m_dens_last[6 - cpp_offset], m_temp_last[7 - cpp_offset],
//		m_enth_last[7 - cpp_offset], m_entr_last[7 - cpp_offset], m_dens_last[7 - cpp_offset], w_t);
//
//	if( turbine_error_code != 0 )
//	{
//		error_code = turbine_error_code;
//		return;
//	}
//
//	// Check that this cycle can produce power
//	double eta_rc_isen = std::numeric_limits<double>::quiet_NaN();
//	double w_rc = std::numeric_limits<double>::quiet_NaN();
//	if( ms_des_par.m_recomp_frac >= 1.E-12 )
//	{
//		if( ms_des_par.m_eta_rc < 0.0 )		// need to convert polytropic efficiency to isentropic efficiency
//		{
//			int rc_error_code = 0;
//
//			isen_eta_from_poly_eta(m_temp_last[2 - cpp_offset], m_pres_last[9 - cpp_offset], m_pres_last[10 - cpp_offset], fabs(ms_des_par.m_eta_rc),
//				true, rc_error_code, eta_rc_isen);
//
//			if( rc_error_code != 0 )
//			{
//				error_code = rc_error_code;
//				return;
//			}
//		}
//		else
//			eta_rc_isen = ms_des_par.m_eta_rc;
//
//		int rc_error_code = 0;
//		calculate_turbomachinery_outlet_1(m_temp_last[2 - cpp_offset], m_pres_last[9 - cpp_offset], m_pres_last[10 - cpp_offset], eta_rc_isen,
//			true, rc_error_code, w_rc);
//
//		if( rc_error_code != 0 )
//		{
//			error_code = rc_error_code;
//			return;
//		}
//	}
//	else
//		w_rc = 0.0;
//
//	if( w_mc + w_rc + w_t <= 0.0 )	// positive net power is impossible; return an error
//	{
//		error_code = 25;
//		return;
//	}
//
//	// Outer iteration loop: temp(8), checking against UA_HT
//	double T8_lower_bound, T8_upper_bound, last_HT_residual, last_T8_guess;
//	T8_lower_bound = T8_upper_bound = last_HT_residual = last_T8_guess = std::numeric_limits<double>::quiet_NaN();
//	if( ms_des_par.m_UA_HT < 1.0E-12 )		// no high-temp recuperator
//	{
//		T8_lower_bound = m_temp_last[7 - cpp_offset];		// no iteration necessary
//		T8_upper_bound = m_temp_last[7 - cpp_offset];		// no iteration necessary
//		m_temp_last[8 - cpp_offset] = m_temp_last[7 - cpp_offset];
//		UA_HT_calc = 0.0;
//		last_HT_residual = 0.0;
//		last_T8_guess = m_temp_last[7 - cpp_offset];
//	}
//	else
//	{
//		T8_lower_bound = m_temp_last[2 - cpp_offset];		// the absolute lower temp(8) could be
//		T8_upper_bound = m_temp_last[7 - cpp_offset];		// the absolutely highest temp(8) could be
//		m_temp_last[8 - cpp_offset] = (T8_lower_bound + T8_upper_bound)*0.5;		// bisect bounds for first guess
//		UA_HT_calc = -1.0;
//		last_HT_residual = ms_des_par.m_UA_HT;			// know a priori that with T8 = T7, UA_calc = 0 therefore residual is UA_HT - 0.0
//		last_T8_guess = m_temp_last[7 - cpp_offset];
//	}
//
//	int prop_error_code = 0;
//
//	double T9_lower_bound, T9_upper_bound, last_LT_residual, last_T9_guess;
//	T9_lower_bound = T9_upper_bound = last_LT_residual = last_T9_guess = std::numeric_limits<double>::quiet_NaN();
//
//	double min_DT_LT = std::numeric_limits<double>::quiet_NaN();
//	double min_DT_HT = std::numeric_limits<double>::quiet_NaN();
//
//	int T8_iter = -1;
//	for( T8_iter = 0; T8_iter < max_iter; T8_iter++ )
//	{
//		// Fully define state 8
//		prop_error_code = CO2_TP(m_temp_last[8 - cpp_offset], m_pres_last[8 - cpp_offset], &co2_props);
//		if( prop_error_code != 0 )
//		{
//			error_code = prop_error_code;
//			return;
//		}
//		m_enth_last[8 - cpp_offset] = co2_props.enth;
//		m_entr_last[8 - cpp_offset] = co2_props.entr;
//		m_dens_last[8 - cpp_offset] = co2_props.dens;
//
//		// Inner iteration loop: temp(9), checking against UA_LT
//		if( ms_des_par.m_UA_LT < 1.0E-12 )	// no low-temperature recuperator
//		{
//			T9_lower_bound = m_temp_last[8 - cpp_offset];		// no iteration necessary
//			T9_upper_bound = m_temp_last[8 - cpp_offset];		// no iteration necessary
//			m_temp_last[9 - cpp_offset] = m_temp_last[8 - cpp_offset];
//			UA_LT_calc = 0.0;
//			last_LT_residual = 0.0;
//			last_T9_guess = m_temp_last[8 - cpp_offset];
//		}
//		else
//		{
//			T9_lower_bound = m_temp_last[2 - cpp_offset];		// the absolute lowest temp(9) could be
//			T9_upper_bound = m_temp_last[8 - cpp_offset];		// the absolute highest temp(9) could be
//			m_temp_last[9 - cpp_offset] = (T9_lower_bound + T9_upper_bound)*0.5;	// biset bounds for first guess
//			UA_LT_calc = -1.0;
//			last_LT_residual = ms_des_par.m_UA_LT;			// know a priori that with T9 = T8, UA_calc = 0 therefore residual is UA_LT - 0
//			last_T9_guess = m_temp_last[8 - cpp_offset];
//		}
//
//		int T9_iter = -1;
//		for( T9_iter = 0; T9_iter < max_iter; T9_iter++ )
//		{
//			// Determine the outlet state of the recompressing compressor and its specific work
//			if( ms_des_par.m_recomp_frac >= 1.E-12 )
//			{
//				if( ms_des_par.m_eta_rc < 0.0 )		// recalculate isentropic efficiency of recompressing compressor (because T9 changes)
//				{
//					int rc_error_code = 0;
//					isen_eta_from_poly_eta(m_temp_last[9 - cpp_offset], m_pres_last[9 - cpp_offset], m_pres_last[10 - cpp_offset], fabs(ms_des_par.m_eta_rc), true,
//						rc_error_code, eta_rc_isen);
//
//					if( rc_error_code != 0 )
//					{
//						error_code = rc_error_code;
//						return;
//					}
//				}
//				else
//				{
//					eta_rc_isen = ms_des_par.m_eta_rc;
//				}
//
//				int rc_error_code = 0;
//				calculate_turbomachinery_outlet_1(m_temp_last[9 - cpp_offset], m_pres_last[9 - cpp_offset], m_pres_last[10 - cpp_offset], eta_rc_isen, true, rc_error_code,
//					m_enth_last[9 - cpp_offset], m_entr_last[9 - cpp_offset], m_dens_last[9 - cpp_offset], m_temp_last[10 - cpp_offset], m_enth_last[10 - cpp_offset], m_entr_last[10 - cpp_offset],
//					m_dens_last[10 - cpp_offset], w_rc);
//
//				if( rc_error_code != 0 )
//				{
//					error_code = rc_error_code;
//					return;
//				}
//			}
//			else
//			{
//				w_rc = 0.0;		// the recompressing compressor does not exist
//				prop_error_code = CO2_TP(m_temp_last[9 - cpp_offset], m_pres_last[9 - cpp_offset], &co2_props);
//				if( prop_error_code != 0 )		// fully define state 9
//				{
//					error_code = prop_error_code;
//					return;
//				}
//				m_enth_last[9 - cpp_offset] = co2_props.enth;
//				m_entr_last[9 - cpp_offset] = co2_props.entr;
//				m_dens_last[9 - cpp_offset] = co2_props.dens;
//				m_temp_last[10 - cpp_offset] = m_temp_last[9 - cpp_offset];		// assume state 10 is the same as state 9
//				m_enth_last[10 - cpp_offset] = m_enth_last[9 - cpp_offset];
//				m_entr_last[10 - cpp_offset] = m_entr_last[9 - cpp_offset];
//				m_dens_last[10 - cpp_offset] = m_dens_last[9 - cpp_offset];
//			}
//
//			// Knowing the specific work of the recompressor, the required mass flow rate can be calculated
//			//m_dot_t = ms_des_par.m_W_dot_net / (w_mc*(1.0 - ms_des_par.m_recomp_frac) + w_rc*ms_des_par.m_recomp_frac + w_t);	// Required mass flow rate through turbine
//			m_dot_t = ms_des_par.m_W_dot_net / (w_mc + w_t);	// Required mass flow rate through turbine
//			
//			if( m_dot_t < 0.0 )		// positive power output is not possible with these inputs
//			{
//				error_code = 29;
//				return;
//			}
//			m_dot_rc = m_dot_t * ms_des_par.m_recomp_frac;		// apply definition of recompression fraction
//			m_dot_mc = m_dot_t - m_dot_rc;						// mass balance
//
//			// Calculate the UA value of the low-temperature recuperator
//			if( ms_des_par.m_UA_LT < 1.0E-12 )		// no low-temp recuperator (this check is necessary to prevent pressure drops with UA=0 from causing problems)
//				Q_dot_LT = 0.0;
//			else
//				Q_dot_LT = m_dot_t * (m_enth_last[8 - cpp_offset] - m_enth_last[9 - cpp_offset]);
//
//			min_DT_LT = std::numeric_limits<double>::quiet_NaN();
//			
//			// Define variables that method outputs that we don't use
//			double eff_LT_hx, NTU_LT_hx, T_h_out_LT_hx, T_c_out_LT_hx, q_dot_LT_hx;
//			eff_LT_hx = NTU_LT_hx = T_h_out_LT_hx = T_c_out_LT_hx = q_dot_LT_hx = std::numeric_limits<double>::quiet_NaN();
//
//			try 
//			{
//			mc_LT_recup.calc_req_UA(Q_dot_LT, m_dot_mc, m_dot_t, m_temp_last[MC_OUT], m_temp_last[HTR_LP_OUT], 
//				m_pres_last[MC_OUT], m_pres_last[LTR_HP_OUT], m_pres_last[HTR_LP_OUT], m_pres_last[LTR_LP_OUT],
//				UA_LT_calc, min_DT_LT, eff_LT_hx, NTU_LT_hx, T_h_out_LT_hx, T_c_out_LT_hx, q_dot_LT_hx);
//			}
//			catch( C_csp_exception & csp_except )
//			{
//				if( csp_except.m_error_code == 11 )		// second-law violation in hxr, therefore temp(9) is too low
//				{
//					T9_lower_bound = m_temp_last[9 - cpp_offset];
//					m_temp_last[9 - cpp_offset] = 0.5*(T9_lower_bound + T9_upper_bound);		// bisect bounds for next guess
//					continue;
//				}
//				else
//				{
//					error_code = csp_except.m_error_code;
//					return;
//				}
//			
//			}
//
//			//calculate_hxr_UA_1(ms_des_par.m_N_sub_hxrs, Q_dot_LT, m_dot_mc, m_dot_t, m_temp_last[2 - cpp_offset], m_temp_last[8 - cpp_offset],
//			//	m_pres_last[2 - cpp_offset], m_pres_last[3 - cpp_offset], m_pres_last[8 - cpp_offset], m_pres_last[9 - cpp_offset],
//			//	hx_error_code, UA_LT_calc, min_DT_LT);
//			//
//			//if( hx_error_code != 0 )
//			//{
//			//	if( hx_error_code == 11 )		// second-law violation in hxr, therefore temp(9) is too low
//			//	{
//			//		T9_lower_bound = m_temp_last[9 - cpp_offset];
//			//		m_temp_last[9 - cpp_offset] = 0.5*(T9_lower_bound + T9_upper_bound);		// bisect bounds for next guess
//			//		continue;
//			//	}
//			//	else
//			//	{
//			//		error_code = hx_error_code;
//			//		return;
//			//	}
//			//}
//
//			// Check for convergence and adjust T9 appropriately
//			double UA_LT_residual = ms_des_par.m_UA_LT - UA_LT_calc;
//
//			if( fabs(UA_LT_residual) < 1.0E-12 )		// catches no LT case
//				break;
//
//			double secant_guess = m_temp_last[9 - cpp_offset] - UA_LT_residual*(last_T9_guess - m_temp_last[9 - cpp_offset]) / (last_LT_residual - UA_LT_residual);	// next guess predicted using secant method
//
//			if( UA_LT_residual < 0.0 )		// UA_LT_calc is too big, temp(9) needs to be higher
//			{
//				if( fabs(UA_LT_residual) / ms_des_par.m_UA_LT < ms_des_par.m_tol )
//					break;
//
//				T9_lower_bound = m_temp_last[9 - cpp_offset];
//			}
//			else		// UA_LT_calc is too small, temp(9) needs to be lower
//			{
//				if( UA_LT_residual / ms_des_par.m_UA_LT < ms_des_par.m_tol )	// UA_LT converged
//					break;
//
//				if( min_DT_LT < temperature_tolerance )		// UA_calc is still too low but there isn't anywhere to go so it's ok (catches huge UA values)
//					break;
//
//				T9_upper_bound = m_temp_last[9 - cpp_offset];
//			}
//
//			last_LT_residual = UA_LT_residual;			// reset lsat stored residual value
//			last_T9_guess = m_temp_last[9 - cpp_offset];	// reset last stored guess value
//
//			// Check if the secant method overshoots and fall back to bisection if it does
//			if( secant_guess <= T9_lower_bound || secant_guess >= T9_upper_bound || secant_guess != secant_guess )	// secant method overshot (or is NaN), use bisection
//				m_temp_last[9 - cpp_offset] = 0.5*(T9_lower_bound + T9_upper_bound);
//			else
//				m_temp_last[9 - cpp_offset] = secant_guess;
//
//		}	// End T9 iteration
//
//		// Check that T9_loop converged
//		if( T9_iter >= max_iter )
//		{
//			error_code = 31;
//			return;
//		}
//
//		// State 3 can now be fully defined
//		m_enth_last[3 - cpp_offset] = m_enth_last[2 - cpp_offset] + Q_dot_LT / m_dot_mc;		// Energy balalnce on cold stream of low-temp recuperator
//		prop_error_code = CO2_PH(m_pres_last[3 - cpp_offset], m_enth_last[3 - cpp_offset], &co2_props);
//		if( prop_error_code != 0 )
//		{
//			error_code = prop_error_code;
//			return;
//		}
//		m_temp_last[3 - cpp_offset] = co2_props.temp;
//		m_entr_last[3 - cpp_offset] = co2_props.entr;
//		m_dens_last[3 - cpp_offset] = co2_props.dens;
//
//		// Go through the mixing valve
//		if( ms_des_par.m_recomp_frac >= 1.E-12 )
//		{
//			m_enth_last[4 - cpp_offset] = (1.0 - ms_des_par.m_recomp_frac)*m_enth_last[3 - cpp_offset] + ms_des_par.m_recomp_frac*m_enth_last[10 - cpp_offset];	// conservation of energy (both sides divided by m_dot_t)
//			prop_error_code = CO2_PH(m_pres_last[4 - cpp_offset], m_enth_last[4 - cpp_offset], &co2_props);
//			if( prop_error_code != 0 )
//			{
//				error_code = prop_error_code;
//				return;
//			}
//			m_temp_last[4 - cpp_offset] = co2_props.temp;
//			m_entr_last[4 - cpp_offset] = co2_props.entr;
//			m_dens_last[4 - cpp_offset] = co2_props.dens;
//		}
//		else		// no mixing valve, therefore state 4 is equal to state 3
//		{
//			m_temp_last[4 - cpp_offset] = m_temp_last[3 - cpp_offset];
//			m_enth_last[4 - cpp_offset] = m_enth_last[3 - cpp_offset];
//			m_entr_last[4 - cpp_offset] = m_entr_last[3 - cpp_offset];
//			m_dens_last[4 - cpp_offset] = m_dens_last[3 - cpp_offset];
//		}
//
//		// Check for a second law violation at the outlet of the high-temp recuperator
//		if( m_temp_last[4 - cpp_offset] >= m_temp_last[8 - cpp_offset] )		// temp(8) is not valid and it must be increased
//		{
//			T8_lower_bound = m_temp_last[8 - cpp_offset];
//			m_temp_last[8 - cpp_offset] = 0.5*(T8_lower_bound + T8_upper_bound);
//			continue;
//		}
//
//		// Calculate the UA value of the high-temp recuperator
//		if( ms_des_par.m_UA_HT < 1.E-12 )			// no high-temp recuperator
//			Q_dot_HT = 0.0;
//		else
//			Q_dot_HT = m_dot_t * (m_enth_last[7 - cpp_offset] - m_enth_last[8 - cpp_offset]);
//
//		int HT_error_code = 0;
//		min_DT_HT = std::numeric_limits<double>::quiet_NaN();
//
//		calculate_hxr_UA_1(ms_des_par.m_N_sub_hxrs, Q_dot_HT, m_dot_t, m_dot_t, m_temp_last[4 - cpp_offset], m_temp_last[7 - cpp_offset], m_pres_last[4 - cpp_offset],
//			m_pres_last[5 - cpp_offset], m_pres_last[7 - cpp_offset], m_pres_last[8 - cpp_offset], HT_error_code, UA_HT_calc, min_DT_HT);
//
//		if( HT_error_code != 0 )
//		{
//			if( HT_error_code == 11 )			// second-law violation in hxr, therefore temp(8) is too low
//			{
//				T8_lower_bound = m_temp_last[8 - cpp_offset];
//				m_temp_last[8 - cpp_offset] = 0.5*(T8_lower_bound + T8_upper_bound);	// bisect bounds for next guess
//				continue;
//			}
//			else
//			{
//				error_code = HT_error_code;
//				return;
//			}
//		}
//
//		// Check for convergence and adjust T8 appropriately
//		double UA_HT_residual = ms_des_par.m_UA_HT - UA_HT_calc;
//
//		if( fabs(UA_HT_residual) < 1.0E-12 )		// catches no HT case
//			break;
//
//		double secant_guess = m_temp_last[8 - cpp_offset] - UA_HT_residual*(last_T8_guess - m_temp_last[8 - cpp_offset]) / (last_HT_residual - UA_HT_residual);		// Next guess predicted using secant method
//
//		if( UA_HT_residual < 0.0 )	// UA_HT_calc is too big, temp(8) needs to be higher
//		{
//			if( fabs(UA_HT_residual) / ms_des_par.m_UA_HT < ms_des_par.m_tol )
//				break;
//			T8_lower_bound = m_temp_last[8 - cpp_offset];
//		}
//		else						// UA_HT_calc is too small, temp(8) needs to be lower
//		{
//			if( UA_HT_residual / ms_des_par.m_UA_HT < ms_des_par.m_tol )		// UA_HT converged
//				break;
//			if( min_DT_HT < temperature_tolerance )								// UA_calc is still too low, but there isn't anywhere to go so it's okay
//				break;
//			T8_upper_bound = m_temp_last[8 - cpp_offset];
//		}
//		last_HT_residual = UA_HT_residual;				// reset last stored residual value
//		last_T8_guess = m_temp_last[8 - cpp_offset];		// reset last stored guess value
//
//		// Check if the secant method overshoots and fall back to bisection if it does
//		if( secant_guess <= T8_lower_bound || secant_guess >= T8_upper_bound )		// secant method overshot, use bisection
//			m_temp_last[8 - cpp_offset] = 0.5*(T8_lower_bound + T8_upper_bound);
//		else
//			m_temp_last[8 - cpp_offset] = secant_guess;
//
//	}	// End T8 iteration
//
//	// Check that T8_loop converged
//	if( T8_iter >= max_iter )
//	{
//		error_code = 35;
//		return;
//	}
//
//	// State 5 can now be fully defined
//	m_enth_last[5 - cpp_offset] = m_enth_last[4 - cpp_offset] + Q_dot_HT / m_dot_t;						// Energy balance on cold stream of high-temp recuperator
//	prop_error_code = CO2_PH(m_pres_last[5 - cpp_offset], m_enth_last[5 - cpp_offset], &co2_props);
//	if( prop_error_code != 0 )
//	{
//		error_code = prop_error_code;
//		return;
//	}
//	m_temp_last[5 - cpp_offset] = co2_props.temp;
//	m_entr_last[5 - cpp_offset] = co2_props.entr;
//	m_dens_last[5 - cpp_offset] = co2_props.dens;
//
//	// Calculate performance metrics for low-temperature recuperator
//	C_HeatExchanger::S_design_parameters LT_des_par;
//	double C_dot_hot = m_dot_t*(m_enth_last[8 - cpp_offset] - m_enth_last[9 - cpp_offset]) / (m_temp_last[8 - cpp_offset] - m_temp_last[9 - cpp_offset]);		// LT recuperator hot stream capacitance rate
//	double C_dot_cold = m_dot_mc*(m_enth_last[3 - cpp_offset] - m_enth_last[2 - cpp_offset]) / (m_temp_last[3 - cpp_offset] - m_temp_last[2 - cpp_offset]);	// LT recuperator cold stream capacitance rate
//	double C_dot_min = min(C_dot_hot, C_dot_cold);
//	double Q_dot_max = C_dot_min*(m_temp_last[8 - cpp_offset] - m_temp_last[2 - cpp_offset]);
//	double hx_eff = Q_dot_LT / Q_dot_max;				// Definition of effectiveness
//	LT_des_par.m_DP_design[0] = m_pres_last[2 - cpp_offset] - m_pres_last[3 - cpp_offset];
//	LT_des_par.m_DP_design[1] = m_pres_last[8 - cpp_offset] - m_pres_last[9 - cpp_offset];
//	LT_des_par.m_eff_design = hx_eff;
//	LT_des_par.m_min_DT_design = min_DT_LT;
//	LT_des_par.m_m_dot_design[0] = m_dot_mc;
//	LT_des_par.m_m_dot_design[1] = m_dot_t;
//	LT_des_par.m_N_sub = ms_des_par.m_N_sub_hxrs;
//	LT_des_par.m_Q_dot_design = Q_dot_LT;
//	LT_des_par.m_UA_design = UA_LT_calc;
//	m_LT.initialize(LT_des_par);
//
//	// Calculate performance metrics for high-temperature recuperator
//	C_HeatExchanger::S_design_parameters HT_des_par;
//	C_dot_hot = m_dot_t*(m_enth_last[7 - cpp_offset] - m_enth_last[8 - cpp_offset]) / (m_temp_last[7 - cpp_offset] - m_temp_last[8 - cpp_offset]);			// HT recuperator hot stream capacitance rate
//	C_dot_cold = m_dot_t*(m_enth_last[5 - cpp_offset] - m_enth_last[4 - cpp_offset]) / (m_temp_last[5 - cpp_offset] - m_temp_last[4 - cpp_offset]);			// HT recuperator cold stream capacitance rate
//	C_dot_min = min(C_dot_hot, C_dot_cold);
//	Q_dot_max = C_dot_min*(m_temp_last[7 - cpp_offset] - m_temp_last[4 - cpp_offset]);
//	hx_eff = Q_dot_HT / Q_dot_max;						// Definition of effectiveness
//	HT_des_par.m_DP_design[0] = m_pres_last[4 - cpp_offset] - m_pres_last[5 - cpp_offset];
//	HT_des_par.m_DP_design[1] = m_pres_last[7 - cpp_offset] - m_pres_last[8 - cpp_offset];
//	HT_des_par.m_eff_design = hx_eff;
//	HT_des_par.m_min_DT_design = min_DT_HT;
//	HT_des_par.m_m_dot_design[0] = m_dot_t;
//	HT_des_par.m_m_dot_design[1] = m_dot_t;
//	HT_des_par.m_N_sub = ms_des_par.m_N_sub_hxrs;
//	HT_des_par.m_Q_dot_design = Q_dot_HT;
//	HT_des_par.m_UA_design = UA_HT_calc;
//	m_HT.initialize(HT_des_par);
//
//	// Set relevant values for other heat exchangers
//	C_HeatExchanger::S_design_parameters PHX_des_par;
//	PHX_des_par.m_DP_design[0] = m_pres_last[5 - cpp_offset] - m_pres_last[6 - cpp_offset];
//	PHX_des_par.m_DP_design[1] = 0.0;
//	PHX_des_par.m_m_dot_design[0] = m_dot_t;
//	PHX_des_par.m_m_dot_design[1] = 0.0;
//	PHX_des_par.m_Q_dot_design = m_dot_t*(m_enth_last[6 - cpp_offset] - m_enth_last[5 - cpp_offset]);
//	m_PHX.initialize(PHX_des_par);
//
//	C_HeatExchanger::S_design_parameters PC_des_par;
//	PC_des_par.m_DP_design[0] = 0.0;
//	PC_des_par.m_DP_design[1] = m_pres_last[9 - cpp_offset] - m_pres_last[1 - cpp_offset];
//	PC_des_par.m_m_dot_design[0] = 0.0;
//	PC_des_par.m_m_dot_design[1] = m_dot_mc;
//	PC_des_par.m_Q_dot_design = m_dot_mc*(m_enth_last[9 - cpp_offset] - m_enth_last[1 - cpp_offset]);
//	m_PC.initialize(PC_des_par);
//
//	// Calculate/set cycle performance metrics
//	// m_W_dot_net_last = w_mc*m_dot_mc + w_rc*m_dot_rc + w_t*m_dot_t;
//	m_W_dot_net_last = (w_mc + w_t)*m_dot_t;
//	
//	double Q_dot_heat_shield = m_dot_rc * (m_enth_last[10 - cpp_offset] - m_enth_last[2-cpp_offset]);
//	
//	m_eta_thermal_last = m_W_dot_net_last / (PHX_des_par.m_Q_dot_design + Q_dot_heat_shield);
//
//	//double eta_thermal = ms_od_solved.m_eta_thermal;
//
//	double deltaT_hs = m_temp_last[10-cpp_offset] - m_temp_last[2-cpp_offset];
//
//	double deltaT_low_limit = 150.0;
//	double deltaT_high_limit = 250.0;
//
//	double diff_deltaT_hs = max( max(0.0, deltaT_low_limit - deltaT_hs), max(0.0, deltaT_hs - deltaT_high_limit) );
//
//	double Q_hs_frac_target = 10.0 / 65.0;
//	double Q_hs_frac = Q_dot_heat_shield / (PHX_des_par.m_Q_dot_design+Q_dot_heat_shield);
//
//	double diff_Q_hs = max(0.0, fabs(Q_hs_frac - Q_hs_frac_target) - ms_des_par.m_tol);		
//
//	double E_bal = (PHX_des_par.m_Q_dot_design + Q_dot_heat_shield) - (m_W_dot_net_last + PC_des_par.m_Q_dot_design/m_dot_mc*(m_dot_mc + m_dot_rc));
//	
//	m_eta_thermal_last = m_eta_thermal_last*exp(-diff_deltaT_hs)*exp(-100.0*diff_Q_hs);
//
//	m_m_dot_mc = m_dot_mc;
//	m_m_dot_rc = m_dot_rc;
//	m_m_dot_t = m_dot_t;
//}

//void C_RecompCycle::design_core_bypass150C(int & error_code)
//{
//	CO2_state co2_props;
//
//	int max_iter = 500;
//	double temperature_tolerance = 1.E-6;		// Temp differences below this are considered zero
//
//	int cpp_offset = 1;
//
//	// Initialize a few variables
//	double m_dot_t, m_dot_mc, m_dot_rc, Q_dot_LT, Q_dot_HT, UA_LT_calc, UA_HT_calc;
//	m_dot_t = m_dot_mc = m_dot_rc = Q_dot_LT = Q_dot_HT = UA_LT_calc = UA_HT_calc = 0.0;
//
//	m_temp_last[1 - cpp_offset] = ms_des_par.m_T_mc_in;
//	m_pres_last[1 - cpp_offset] = ms_des_par.m_P_mc_in;
//	m_pres_last[2 - cpp_offset] = ms_des_par.m_P_mc_out;
//	m_temp_last[6 - cpp_offset] = ms_des_par.m_T_t_in;
//
//	// Apply pressure drops to heat exchangers, fully defining the pressures at all states
//	if( ms_des_par.m_DP_LT[1 - cpp_offset] < 0.0 )
//		m_pres_last[3 - cpp_offset] = m_pres_last[2 - cpp_offset] - m_pres_last[2 - cpp_offset] * fabs(ms_des_par.m_DP_LT[1 - cpp_offset]);	// relative pressure drop specified for LT recuperator (cold stream)
//	else
//		m_pres_last[3 - cpp_offset] = m_pres_last[2 - cpp_offset] - ms_des_par.m_DP_LT[1 - cpp_offset];									// absolute pressure drop specified for LT recuperator (cold stream)
//
//	if( ms_des_par.m_UA_LT < 1.0E-12 )
//		m_pres_last[3 - cpp_offset] = m_pres_last[2 - cpp_offset];		// If there is no LT recuperator, there is no pressure drop
//
//	m_pres_last[4 - cpp_offset] = m_pres_last[3 - cpp_offset];			// Assume no pressure drop in mixing valve
//	m_pres_last[10 - cpp_offset] = m_pres_last[3 - cpp_offset];			// Assume no pressure drop in mixing valve
//
//	if( ms_des_par.m_DP_HT[1 - cpp_offset] < 0.0 )
//		m_pres_last[5 - cpp_offset] = m_pres_last[4 - cpp_offset] - m_pres_last[4 - cpp_offset] * fabs(ms_des_par.m_DP_HT[1 - cpp_offset]);	// relative pressure drop specified for HT recuperator (cold stream)
//	else
//		m_pres_last[5 - cpp_offset] = m_pres_last[4 - cpp_offset] - ms_des_par.m_DP_HT[1 - cpp_offset];									// absolute pressure drop specified for HT recuperator (cold stream)
//
//	if( ms_des_par.m_UA_HT < 1.0E-12 )
//		m_pres_last[5 - cpp_offset] = m_pres_last[4 - cpp_offset];		// If there is no HT recuperator, there is no pressure drop
//
//	if( ms_des_par.m_DP_PHX[1 - cpp_offset] < 0.0 )
//		m_pres_last[6 - cpp_offset] = m_pres_last[5 - cpp_offset] - m_pres_last[5 - cpp_offset] * fabs(ms_des_par.m_DP_PHX[1 - cpp_offset]);	// relative pressure drop specified for PHX
//	else
//		m_pres_last[6 - cpp_offset] = m_pres_last[5 - cpp_offset] - ms_des_par.m_DP_PHX[1 - cpp_offset];									// absolute pressure drop specified for PHX
//
//	if( ms_des_par.m_DP_PC[2 - cpp_offset] < 0.0 )
//		m_pres_last[9 - cpp_offset] = m_pres_last[1 - cpp_offset] / (1.0 - fabs(ms_des_par.m_DP_PC[2 - cpp_offset]));	// relative pressure drop specified for precooler: P1=P9-P9*rel_DP => P1=P9*(1-rel_DP)
//	else
//		m_pres_last[9 - cpp_offset] = m_pres_last[1 - cpp_offset] + ms_des_par.m_DP_PC[2 - cpp_offset];
//
//	if( ms_des_par.m_DP_LT[2 - cpp_offset] < 0.0 )
//		m_pres_last[8 - cpp_offset] = m_pres_last[9 - cpp_offset] / (1.0 - fabs(ms_des_par.m_DP_LT[2 - cpp_offset]));	// relative pressure drop specified for LT recuperator (hot stream)
//	else
//		m_pres_last[8 - cpp_offset] = m_pres_last[9 - cpp_offset] + ms_des_par.m_DP_LT[2 - cpp_offset];				// absolute pressure drop specified for LT recuperator (hot stream)
//
//	if( ms_des_par.m_UA_LT < 1.0E-12 )
//		m_pres_last[8 - cpp_offset] = m_pres_last[9 - cpp_offset];		// if there is no LT recuperator, there is no pressure drop
//
//	if( ms_des_par.m_DP_HT[2 - cpp_offset] < 0.0 )
//		m_pres_last[7 - cpp_offset] = m_pres_last[8 - cpp_offset] / (1.0 - fabs(ms_des_par.m_DP_HT[2 - cpp_offset]));	// relative pressure drop specified for HT recuperator (hot stream)
//	else
//		m_pres_last[7 - cpp_offset] = m_pres_last[8 - cpp_offset] + ms_des_par.m_DP_HT[2 - cpp_offset];				// absolute pressure drop specified for HT recuperator (hot stream)
//
//	if( ms_des_par.m_UA_HT < 1.0E-12 )
//		m_pres_last[7 - cpp_offset] = m_pres_last[8 - cpp_offset];		// if there is no HT recuperator, there is no pressure drop
//
//	// Determine equivalent isentropic efficiencies for main compressor and turbine, if necessary.
//	double eta_mc_isen = std::numeric_limits<double>::quiet_NaN();
//	double eta_t_isen = std::numeric_limits<double>::quiet_NaN();
//	if( ms_des_par.m_eta_mc < 0.0 )
//	{
//		int poly_error_code = 0;
//
//		isen_eta_from_poly_eta(m_temp_last[1 - cpp_offset], m_pres_last[1 - cpp_offset], m_pres_last[2 - cpp_offset], fabs(ms_des_par.m_eta_mc),
//			true, poly_error_code, eta_mc_isen);
//
//		if( poly_error_code != 0 )
//		{
//			error_code = poly_error_code;
//			return;
//		}
//	}
//	else
//		eta_mc_isen = ms_des_par.m_eta_mc;
//
//	if( ms_des_par.m_eta_t < 0.0 )
//	{
//		int poly_error_code = 0;
//
//		isen_eta_from_poly_eta(m_temp_last[6 - cpp_offset], m_pres_last[6 - cpp_offset], m_pres_last[7 - cpp_offset], fabs(ms_des_par.m_eta_t),
//			false, poly_error_code, eta_t_isen);
//
//		if( poly_error_code != 0 )
//		{
//			error_code = poly_error_code;
//			return;
//		}
//	}
//	else
//		eta_t_isen = ms_des_par.m_eta_t;
//
//	// Determine the outlet state and specific work for the main compressor and turbine.
//	int comp_error_code = 0;
//	double w_mc = std::numeric_limits<double>::quiet_NaN();
//	// Main compressor
//	calculate_turbomachinery_outlet_1(m_temp_last[1 - cpp_offset], m_pres_last[1 - cpp_offset], m_pres_last[2 - cpp_offset], eta_mc_isen, true,
//		comp_error_code, m_enth_last[1 - cpp_offset], m_entr_last[1 - cpp_offset], m_dens_last[1 - cpp_offset], m_temp_last[2 - cpp_offset],
//		m_enth_last[2 - cpp_offset], m_entr_last[2 - cpp_offset], m_dens_last[2 - cpp_offset], w_mc);
//
//	if( comp_error_code != 0 )
//	{
//		error_code = comp_error_code;
//		return;
//	}
//
//	int turbine_error_code = 0;
//	double w_t = std::numeric_limits<double>::quiet_NaN();
//	// Turbine
//	calculate_turbomachinery_outlet_1(m_temp_last[6 - cpp_offset], m_pres_last[6 - cpp_offset], m_pres_last[7 - cpp_offset], eta_t_isen, false,
//		turbine_error_code, m_enth_last[6 - cpp_offset], m_entr_last[6 - cpp_offset], m_dens_last[6 - cpp_offset], m_temp_last[7 - cpp_offset],
//		m_enth_last[7 - cpp_offset], m_entr_last[7 - cpp_offset], m_dens_last[7 - cpp_offset], w_t);
//
//	if( turbine_error_code != 0 )
//	{
//		error_code = turbine_error_code;
//		return;
//	}
//
//	// Check that this cycle can produce power
//	double eta_rc_isen = std::numeric_limits<double>::quiet_NaN();
//	double w_rc = std::numeric_limits<double>::quiet_NaN();
//	if( ms_des_par.m_recomp_frac >= 1.E-12 )
//	{
//		if( ms_des_par.m_eta_rc < 0.0 )		// need to convert polytropic efficiency to isentropic efficiency
//		{
//			int rc_error_code = 0;
//
//			isen_eta_from_poly_eta(m_temp_last[2 - cpp_offset], m_pres_last[9 - cpp_offset], m_pres_last[10 - cpp_offset], fabs(ms_des_par.m_eta_rc),
//				true, rc_error_code, eta_rc_isen);
//
//			if( rc_error_code != 0 )
//			{
//				error_code = rc_error_code;
//				return;
//			}
//		}
//		else
//			eta_rc_isen = ms_des_par.m_eta_rc;
//
//		int rc_error_code = 0;
//		calculate_turbomachinery_outlet_1(m_temp_last[2 - cpp_offset], m_pres_last[9 - cpp_offset], m_pres_last[10 - cpp_offset], eta_rc_isen,
//			true, rc_error_code, w_rc);
//
//		if( rc_error_code != 0 )
//		{
//			error_code = rc_error_code;
//			return;
//		}
//	}
//	else
//		w_rc = 0.0;
//
//	if( w_mc + w_rc + w_t <= 0.0 )	// positive net power is impossible; return an error
//	{
//		error_code = 25;
//		return;
//	}
//
//	// Outer iteration loop: temp(8), checking against UA_HT
//	double T8_lower_bound, T8_upper_bound, last_HT_residual, last_T8_guess;
//	T8_lower_bound = T8_upper_bound = last_HT_residual = last_T8_guess = std::numeric_limits<double>::quiet_NaN();
//	if( ms_des_par.m_UA_HT < 1.0E-12 )		// no high-temp recuperator
//	{
//		T8_lower_bound = m_temp_last[7 - cpp_offset];		// no iteration necessary
//		T8_upper_bound = m_temp_last[7 - cpp_offset];		// no iteration necessary
//		m_temp_last[8 - cpp_offset] = m_temp_last[7 - cpp_offset];
//		UA_HT_calc = 0.0;
//		last_HT_residual = 0.0;
//		last_T8_guess = m_temp_last[7 - cpp_offset];
//	}
//	else
//	{
//		T8_lower_bound = m_temp_last[2 - cpp_offset];		// the absolute lower temp(8) could be
//		T8_upper_bound = m_temp_last[7 - cpp_offset];		// the absolutely highest temp(8) could be
//		m_temp_last[8 - cpp_offset] = (T8_lower_bound + T8_upper_bound)*0.5;		// bisect bounds for first guess
//		UA_HT_calc = -1.0;
//		last_HT_residual = ms_des_par.m_UA_HT;			// know a priori that with T8 = T7, UA_calc = 0 therefore residual is UA_HT - 0.0
//		last_T8_guess = m_temp_last[7 - cpp_offset];
//	}
//
//	int prop_error_code = 0;
//
//	double T9_lower_bound, T9_upper_bound, last_LT_residual, last_T9_guess;
//	T9_lower_bound = T9_upper_bound = last_LT_residual = last_T9_guess = std::numeric_limits<double>::quiet_NaN();
//
//	double min_DT_LT = std::numeric_limits<double>::quiet_NaN();
//	double min_DT_HT = std::numeric_limits<double>::quiet_NaN();
//
//	int T8_iter = -1;
//	for( T8_iter = 0; T8_iter < max_iter; T8_iter++ )
//	{
//		// Fully define state 8
//		prop_error_code = CO2_TP(m_temp_last[8 - cpp_offset], m_pres_last[8 - cpp_offset], &co2_props);
//		if( prop_error_code != 0 )
//		{
//			error_code = prop_error_code;
//			return;
//		}
//		m_enth_last[8 - cpp_offset] = co2_props.enth;
//		m_entr_last[8 - cpp_offset] = co2_props.entr;
//		m_dens_last[8 - cpp_offset] = co2_props.dens;
//
//		// Inner iteration loop: temp(9), checking against UA_LT
//		if( ms_des_par.m_UA_LT < 1.0E-12 )	// no low-temperature recuperator
//		{
//			T9_lower_bound = m_temp_last[8 - cpp_offset];		// no iteration necessary
//			T9_upper_bound = m_temp_last[8 - cpp_offset];		// no iteration necessary
//			m_temp_last[9 - cpp_offset] = m_temp_last[8 - cpp_offset];
//			UA_LT_calc = 0.0;
//			last_LT_residual = 0.0;
//			last_T9_guess = m_temp_last[8 - cpp_offset];
//		}
//		else
//		{
//			T9_lower_bound = m_temp_last[2 - cpp_offset];		// the absolute lowest temp(9) could be
//			T9_upper_bound = m_temp_last[8 - cpp_offset];		// the absolute highest temp(9) could be
//			m_temp_last[9 - cpp_offset] = (T9_lower_bound + T9_upper_bound)*0.5;	// biset bounds for first guess
//			UA_LT_calc = -1.0;
//			last_LT_residual = ms_des_par.m_UA_LT;			// know a priori that with T9 = T8, UA_calc = 0 therefore residual is UA_LT - 0
//			last_T9_guess = m_temp_last[8 - cpp_offset];
//		}
//
//		int T9_iter = -1;
//		for( T9_iter = 0; T9_iter < max_iter; T9_iter++ )
//		{
//			// Determine the outlet state of the recompressing compressor and its specific work
//			if( ms_des_par.m_recomp_frac >= 1.E-12 )
//			{
//				if( ms_des_par.m_eta_rc < 0.0 )		// recalculate isentropic efficiency of recompressing compressor (because T9 changes)
//				{
//					int rc_error_code = 0;
//					isen_eta_from_poly_eta(m_temp_last[9 - cpp_offset], m_pres_last[9 - cpp_offset], m_pres_last[10 - cpp_offset], fabs(ms_des_par.m_eta_rc), true,
//						rc_error_code, eta_rc_isen);
//
//					if( rc_error_code != 0 )
//					{
//						error_code = rc_error_code;
//						return;
//					}
//				}
//				else
//				{
//					eta_rc_isen = ms_des_par.m_eta_rc;
//				}
//
//				int rc_error_code = 0;
//				calculate_turbomachinery_outlet_1(m_temp_last[9 - cpp_offset], m_pres_last[9 - cpp_offset], m_pres_last[10 - cpp_offset], eta_rc_isen, true, rc_error_code,
//					m_enth_last[9 - cpp_offset], m_entr_last[9 - cpp_offset], m_dens_last[9 - cpp_offset], m_temp_last[10 - cpp_offset], m_enth_last[10 - cpp_offset], m_entr_last[10 - cpp_offset],
//					m_dens_last[10 - cpp_offset], w_rc);
//
//				if( rc_error_code != 0 )
//				{
//					error_code = rc_error_code;
//					return;
//				}
//			}
//			else
//			{
//				w_rc = 0.0;		// the recompressing compressor does not exist
//				prop_error_code = CO2_TP(m_temp_last[9 - cpp_offset], m_pres_last[9 - cpp_offset], &co2_props);
//				if( prop_error_code != 0 )		// fully define state 9
//				{
//					error_code = prop_error_code;
//					return;
//				}
//				m_enth_last[9 - cpp_offset] = co2_props.enth;
//				m_entr_last[9 - cpp_offset] = co2_props.entr;
//				m_dens_last[9 - cpp_offset] = co2_props.dens;
//				m_temp_last[10 - cpp_offset] = m_temp_last[9 - cpp_offset];		// assume state 10 is the same as state 9
//				m_enth_last[10 - cpp_offset] = m_enth_last[9 - cpp_offset];
//				m_entr_last[10 - cpp_offset] = m_entr_last[9 - cpp_offset];
//				m_dens_last[10 - cpp_offset] = m_dens_last[9 - cpp_offset];
//			}
//
//			// Knowing the specific work of the recompressor, the required mass flow rate can be calculated
//			m_dot_t = ms_des_par.m_W_dot_net / (w_mc*(1.0 - ms_des_par.m_recomp_frac) + w_rc*ms_des_par.m_recomp_frac + w_t);	// Required mass flow rate through turbine
//
//			if( m_dot_t < 0.0 )		// positive power output is not possible with these inputs
//			{
//				error_code = 29;
//				return;
//			}
//			m_dot_rc = m_dot_t * ms_des_par.m_recomp_frac;		// apply definition of recompression fraction
//			m_dot_mc = m_dot_t - m_dot_rc;						// mass balance
//
//			// Calculate the UA value of the low-temperature recuperator
//			if( ms_des_par.m_UA_LT < 1.0E-12 )		// no low-temp recuperator (this check is necessary to prevent pressure drops with UA=0 from causing problems)
//				Q_dot_LT = 0.0;
//			else
//				Q_dot_LT = m_dot_t * (m_enth_last[8 - cpp_offset] - m_enth_last[9 - cpp_offset]);
//
//			int hx_error_code = 0;
//			min_DT_LT = std::numeric_limits<double>::quiet_NaN();
//			calculate_hxr_UA_1(ms_des_par.m_N_sub_hxrs, Q_dot_LT, m_dot_mc, m_dot_t, m_temp_last[2 - cpp_offset], m_temp_last[8 - cpp_offset],
//				m_pres_last[2 - cpp_offset], m_pres_last[3 - cpp_offset], m_pres_last[8 - cpp_offset], m_pres_last[9 - cpp_offset],
//				hx_error_code, UA_LT_calc, min_DT_LT);
//
//			if( hx_error_code != 0 )
//			{
//				if( hx_error_code == 11 )		// second-law violation in hxr, therefore temp(9) is too low
//				{
//					T9_lower_bound = m_temp_last[9 - cpp_offset];
//					m_temp_last[9 - cpp_offset] = 0.5*(T9_lower_bound + T9_upper_bound);		// bisect bounds for next guess
//					continue;
//				}
//				else
//				{
//					error_code = hx_error_code;
//					return;
//				}
//			}
//
//			// Check for convergence and adjust T9 appropriately
//			double UA_LT_residual = ms_des_par.m_UA_LT - UA_LT_calc;
//
//			if( fabs(UA_LT_residual) < 1.0E-12 )		// catches no LT case
//				break;
//
//			double secant_guess = m_temp_last[9 - cpp_offset] - UA_LT_residual*(last_T9_guess - m_temp_last[9 - cpp_offset]) / (last_LT_residual - UA_LT_residual);	// next guess predicted using secant method
//
//			if( UA_LT_residual < 0.0 )		// UA_LT_calc is too big, temp(9) needs to be higher
//			{
//				if( fabs(UA_LT_residual) / ms_des_par.m_UA_LT < ms_des_par.m_tol )
//					break;
//
//				T9_lower_bound = m_temp_last[9 - cpp_offset];
//			}
//			else		// UA_LT_calc is too small, temp(9) needs to be lower
//			{
//				if( UA_LT_residual / ms_des_par.m_UA_LT < ms_des_par.m_tol )	// UA_LT converged
//					break;
//
//				if( min_DT_LT < temperature_tolerance )		// UA_calc is still too low but there isn't anywhere to go so it's ok (catches huge UA values)
//					break;
//
//				T9_upper_bound = m_temp_last[9 - cpp_offset];
//			}
//
//			last_LT_residual = UA_LT_residual;			// reset lsat stored residual value
//			last_T9_guess = m_temp_last[9 - cpp_offset];	// reset last stored guess value
//
//			// Check if the secant method overshoots and fall back to bisection if it does
//			if( secant_guess <= T9_lower_bound || secant_guess >= T9_upper_bound || secant_guess != secant_guess )	// secant method overshot (or is NaN), use bisection
//				m_temp_last[9 - cpp_offset] = 0.5*(T9_lower_bound + T9_upper_bound);
//			else
//				m_temp_last[9 - cpp_offset] = secant_guess;
//
//		}	// End T9 iteration
//
//		// Check that T9_loop converged
//		if( T9_iter >= max_iter )
//		{
//			error_code = 31;
//			return;
//		}
//
//		// State 3 can now be fully defined
//		m_enth_last[3 - cpp_offset] = m_enth_last[2 - cpp_offset] + Q_dot_LT / m_dot_mc;		// Energy balalnce on cold stream of low-temp recuperator
//		prop_error_code = CO2_PH(m_pres_last[3 - cpp_offset], m_enth_last[3 - cpp_offset], &co2_props);
//		if( prop_error_code != 0 )
//		{
//			error_code = prop_error_code;
//			return;
//		}
//		m_temp_last[3 - cpp_offset] = co2_props.temp;
//		m_entr_last[3 - cpp_offset] = co2_props.entr;
//		m_dens_last[3 - cpp_offset] = co2_props.dens;
//
//		// Go through the mixing valve
//		if( ms_des_par.m_recomp_frac >= 1.E-12 )
//		{
//			m_enth_last[4 - cpp_offset] = (1.0 - ms_des_par.m_recomp_frac)*m_enth_last[3 - cpp_offset] + ms_des_par.m_recomp_frac*m_enth_last[10 - cpp_offset];	// conservation of energy (both sides divided by m_dot_t)
//			prop_error_code = CO2_PH(m_pres_last[4 - cpp_offset], m_enth_last[4 - cpp_offset], &co2_props);
//			if( prop_error_code != 0 )
//			{
//				error_code = prop_error_code;
//				return;
//			}
//			m_temp_last[4 - cpp_offset] = co2_props.temp;
//			m_entr_last[4 - cpp_offset] = co2_props.entr;
//			m_dens_last[4 - cpp_offset] = co2_props.dens;
//		}
//		else		// no mixing valve, therefore state 4 is equal to state 3
//		{
//			m_temp_last[4 - cpp_offset] = m_temp_last[3 - cpp_offset];
//			m_enth_last[4 - cpp_offset] = m_enth_last[3 - cpp_offset];
//			m_entr_last[4 - cpp_offset] = m_entr_last[3 - cpp_offset];
//			m_dens_last[4 - cpp_offset] = m_dens_last[3 - cpp_offset];
//		}
//
//		// Check for a second law violation at the outlet of the high-temp recuperator
//		if( m_temp_last[4 - cpp_offset] >= m_temp_last[8 - cpp_offset] )		// temp(8) is not valid and it must be increased
//		{
//			T8_lower_bound = m_temp_last[8 - cpp_offset];
//			m_temp_last[8 - cpp_offset] = 0.5*(T8_lower_bound + T8_upper_bound);
//			continue;
//		}
//
//		// Calculate the UA value of the high-temp recuperator
//		if( ms_des_par.m_UA_HT < 1.E-12 )			// no high-temp recuperator
//			Q_dot_HT = 0.0;
//		else
//			Q_dot_HT = m_dot_t * (m_enth_last[7 - cpp_offset] - m_enth_last[8 - cpp_offset]);
//
//		int HT_error_code = 0;
//		min_DT_HT = std::numeric_limits<double>::quiet_NaN();
//
//		calculate_hxr_UA_1(ms_des_par.m_N_sub_hxrs, Q_dot_HT, m_dot_t, m_dot_t, m_temp_last[4 - cpp_offset], m_temp_last[7 - cpp_offset], m_pres_last[4 - cpp_offset],
//			m_pres_last[5 - cpp_offset], m_pres_last[7 - cpp_offset], m_pres_last[8 - cpp_offset], HT_error_code, UA_HT_calc, min_DT_HT);
//
//		if( HT_error_code != 0 )
//		{
//			if( HT_error_code == 11 )			// second-law violation in hxr, therefore temp(8) is too low
//			{
//				T8_lower_bound = m_temp_last[8 - cpp_offset];
//				m_temp_last[8 - cpp_offset] = 0.5*(T8_lower_bound + T8_upper_bound);	// bisect bounds for next guess
//				continue;
//			}
//			else
//			{
//				error_code = HT_error_code;
//				return;
//			}
//		}
//
//		// Check for convergence and adjust T8 appropriately
//		double UA_HT_residual = ms_des_par.m_UA_HT - UA_HT_calc;
//
//		if( fabs(UA_HT_residual) < 1.0E-12 )		// catches no HT case
//			break;
//
//		double secant_guess = m_temp_last[8 - cpp_offset] - UA_HT_residual*(last_T8_guess - m_temp_last[8 - cpp_offset]) / (last_HT_residual - UA_HT_residual);		// Next guess predicted using secant method
//
//		if( UA_HT_residual < 0.0 )	// UA_HT_calc is too big, temp(8) needs to be higher
//		{
//			if( fabs(UA_HT_residual) / ms_des_par.m_UA_HT < ms_des_par.m_tol )
//				break;
//			T8_lower_bound = m_temp_last[8 - cpp_offset];
//		}
//		else						// UA_HT_calc is too small, temp(8) needs to be lower
//		{
//			if( UA_HT_residual / ms_des_par.m_UA_HT < ms_des_par.m_tol )		// UA_HT converged
//				break;
//			if( min_DT_HT < temperature_tolerance )								// UA_calc is still too low, but there isn't anywhere to go so it's okay
//				break;
//			T8_upper_bound = m_temp_last[8 - cpp_offset];
//		}
//		last_HT_residual = UA_HT_residual;				// reset last stored residual value
//		last_T8_guess = m_temp_last[8 - cpp_offset];		// reset last stored guess value
//
//		// Check if the secant method overshoots and fall back to bisection if it does
//		if( secant_guess <= T8_lower_bound || secant_guess >= T8_upper_bound )		// secant method overshot, use bisection
//			m_temp_last[8 - cpp_offset] = 0.5*(T8_lower_bound + T8_upper_bound);
//		else
//			m_temp_last[8 - cpp_offset] = secant_guess;
//
//	}	// End T8 iteration
//
//	// Check that T8_loop converged
//	if( T8_iter >= max_iter )
//	{
//		error_code = 35;
//		return;
//	}
//
//	// State 5 can now be fully defined
//	m_enth_last[5 - cpp_offset] = m_enth_last[4 - cpp_offset] + Q_dot_HT / m_dot_t;						// Energy balance on cold stream of high-temp recuperator
//	prop_error_code = CO2_PH(m_pres_last[5 - cpp_offset], m_enth_last[5 - cpp_offset], &co2_props);
//	if( prop_error_code != 0 )
//	{
//		error_code = prop_error_code;
//		return;
//	}
//	m_temp_last[5 - cpp_offset] = co2_props.temp;
//	m_entr_last[5 - cpp_offset] = co2_props.entr;
//	m_dens_last[5 - cpp_offset] = co2_props.dens;
//
//	// Calculate performance metrics for low-temperature recuperator
//	C_HeatExchanger::S_design_parameters LT_des_par;
//	double C_dot_hot = m_dot_t*(m_enth_last[8 - cpp_offset] - m_enth_last[9 - cpp_offset]) / (m_temp_last[8 - cpp_offset] - m_temp_last[9 - cpp_offset]);		// LT recuperator hot stream capacitance rate
//	double C_dot_cold = m_dot_mc*(m_enth_last[3 - cpp_offset] - m_enth_last[2 - cpp_offset]) / (m_temp_last[3 - cpp_offset] - m_temp_last[2 - cpp_offset]);	// LT recuperator cold stream capacitance rate
//	double C_dot_min = min(C_dot_hot, C_dot_cold);
//	double Q_dot_max = C_dot_min*(m_temp_last[8 - cpp_offset] - m_temp_last[2 - cpp_offset]);
//	double hx_eff = Q_dot_LT / Q_dot_max;				// Definition of effectiveness
//	LT_des_par.m_DP_design[0] = m_pres_last[2 - cpp_offset] - m_pres_last[3 - cpp_offset];
//	LT_des_par.m_DP_design[1] = m_pres_last[8 - cpp_offset] - m_pres_last[9 - cpp_offset];
//	LT_des_par.m_eff_design = hx_eff;
//	LT_des_par.m_min_DT_design = min_DT_LT;
//	LT_des_par.m_m_dot_design[0] = m_dot_mc;
//	LT_des_par.m_m_dot_design[1] = m_dot_t;
//	LT_des_par.m_N_sub = ms_des_par.m_N_sub_hxrs;
//	LT_des_par.m_Q_dot_design = Q_dot_LT;
//	LT_des_par.m_UA_design = UA_LT_calc;
//	m_LT.initialize(LT_des_par);
//
//	// Calculate performance metrics for high-temperature recuperator
//	C_HeatExchanger::S_design_parameters HT_des_par;
//	C_dot_hot = m_dot_t*(m_enth_last[7 - cpp_offset] - m_enth_last[8 - cpp_offset]) / (m_temp_last[7 - cpp_offset] - m_temp_last[8 - cpp_offset]);			// HT recuperator hot stream capacitance rate
//	C_dot_cold = m_dot_t*(m_enth_last[5 - cpp_offset] - m_enth_last[4 - cpp_offset]) / (m_temp_last[5 - cpp_offset] - m_temp_last[4 - cpp_offset]);			// HT recuperator cold stream capacitance rate
//	C_dot_min = min(C_dot_hot, C_dot_cold);
//	Q_dot_max = C_dot_min*(m_temp_last[7 - cpp_offset] - m_temp_last[4 - cpp_offset]);
//	hx_eff = Q_dot_HT / Q_dot_max;						// Definition of effectiveness
//	HT_des_par.m_DP_design[0] = m_pres_last[4 - cpp_offset] - m_pres_last[5 - cpp_offset];
//	HT_des_par.m_DP_design[1] = m_pres_last[7 - cpp_offset] - m_pres_last[8 - cpp_offset];
//	HT_des_par.m_eff_design = hx_eff;
//	HT_des_par.m_min_DT_design = min_DT_HT;
//	HT_des_par.m_m_dot_design[0] = m_dot_t;
//	HT_des_par.m_m_dot_design[1] = m_dot_t;
//	HT_des_par.m_N_sub = ms_des_par.m_N_sub_hxrs;
//	HT_des_par.m_Q_dot_design = Q_dot_HT;
//	HT_des_par.m_UA_design = UA_HT_calc;
//	m_HT.initialize(HT_des_par);
//
//	// Set relevant values for other heat exchangers
//	C_HeatExchanger::S_design_parameters PHX_des_par;
//	PHX_des_par.m_DP_design[0] = m_pres_last[5 - cpp_offset] - m_pres_last[6 - cpp_offset];
//	PHX_des_par.m_DP_design[1] = 0.0;
//	PHX_des_par.m_m_dot_design[0] = m_dot_t;
//	PHX_des_par.m_m_dot_design[1] = 0.0;
//	PHX_des_par.m_Q_dot_design = m_dot_t*(m_enth_last[6 - cpp_offset] - m_enth_last[5 - cpp_offset]);
//	m_PHX.initialize(PHX_des_par);
//
//	C_HeatExchanger::S_design_parameters PC_des_par;
//	PC_des_par.m_DP_design[0] = 0.0;
//	PC_des_par.m_DP_design[1] = m_pres_last[9 - cpp_offset] - m_pres_last[1 - cpp_offset];
//	PC_des_par.m_m_dot_design[0] = 0.0;
//	PC_des_par.m_m_dot_design[1] = m_dot_mc;
//	PC_des_par.m_Q_dot_design = m_dot_mc*(m_enth_last[9 - cpp_offset] - m_enth_last[1 - cpp_offset]);
//	m_PC.initialize(PC_des_par);
//
//	// Calculate/set cycle performance metrics
//	m_W_dot_mc = w_mc*m_dot_mc;
//	m_W_dot_rc = w_rc*m_dot_rc;
//	m_W_dot_mc_bypass = w_mc*(m_dot_t);
//	m_W_dot_net_last = w_mc*m_dot_mc + w_rc*m_dot_rc + w_t*m_dot_t;
//
//	m_eta_thermal_last = m_W_dot_net_last / (PHX_des_par.m_Q_dot_design);
//
//	m_Q_dot_PHX = PHX_des_par.m_Q_dot_design;		//[kW]
//	m_Q_dot_bypass = m_dot_rc*(m_enth_last[3-cpp_offset] - m_enth_last[2-cpp_offset]);		//[kW]
//	m_eta_bypass = ((m_enth_last[3-cpp_offset] - m_enth_last[9-cpp_offset]) - (m_enth_last[2-cpp_offset] - m_enth_last[1-cpp_offset])) /
//						(m_enth_last[3-cpp_offset] - m_enth_last[2-cpp_offset]);
//
//
//	//double Q_dot_geo = m_dot_rc * (m_enth_last[10 - cpp_offset] - m_enth_last[2 - cpp_offset]);
//
//	//double E_bal = (PHX_des_par.m_Q_dot_design + Q_dot_heat_shield) - (m_W_dot_net_last + PC_des_par.m_Q_dot_design / m_dot_mc*(m_dot_mc + m_dot_rc));
//
//	double T_limit = 150.0+273.15;
//
//	double over_T_limit = fmax(0.0, m_temp_last[10-cpp_offset] - T_limit);
//
//	m_eta_thermal_last = m_eta_thermal_last*exp(-over_T_limit);
//
//	m_m_dot_mc = m_dot_mc;
//	m_m_dot_rc = m_dot_rc;
//	m_m_dot_t = m_dot_t;
//}

//void C_RecompCycle::design_core_HTR_hs(int & error_code)
//{
//	CO2_state co2_props;
//
//	double Q_hs_frac_target = 10.0 / 65.0;
//
//	double f_bypass = 0.25;
//
//	double f_bypass_min = 0.01;
//	double f_bypass_max = 0.8;
//
//	double f_bypass_low = f_bypass_min;
//	double f_bypass_high = f_bypass_max;
//
//	int iter_f_bypass = 0;
//
//	do
//	{		
//		iter_f_bypass++;
//
//		int max_iter = 500;
//		double temperature_tolerance = 1.E-6;		// Temp differences below this are considered zero
//
//		int cpp_offset = 1;
//
//		// Initialize a few variables
//		double m_dot_t, m_dot_mc, m_dot_rc, Q_dot_LT, Q_dot_HT, UA_LT_calc, UA_HT_calc;
//		m_dot_t = m_dot_mc = m_dot_rc = Q_dot_LT = Q_dot_HT = UA_LT_calc = UA_HT_calc = 0.0;
//
//		m_temp_last[1 - cpp_offset] = ms_des_par.m_T_mc_in;
//		m_pres_last[1 - cpp_offset] = ms_des_par.m_P_mc_in;
//		m_pres_last[2 - cpp_offset] = ms_des_par.m_P_mc_out;
//		m_temp_last[6 - cpp_offset] = ms_des_par.m_T_t_in;
//
//		// Apply pressure drops to heat exchangers, fully defining the pressures at all states
//		if( ms_des_par.m_DP_LT[1 - cpp_offset] < 0.0 )
//			m_pres_last[3 - cpp_offset] = m_pres_last[2 - cpp_offset] - m_pres_last[2 - cpp_offset] * fabs(ms_des_par.m_DP_LT[1 - cpp_offset]);	// relative pressure drop specified for LT recuperator (cold stream)
//		else
//			m_pres_last[3 - cpp_offset] = m_pres_last[2 - cpp_offset] - ms_des_par.m_DP_LT[1 - cpp_offset];									// absolute pressure drop specified for LT recuperator (cold stream)
//
//		if( ms_des_par.m_UA_LT < 1.0E-12 )
//			m_pres_last[3 - cpp_offset] = m_pres_last[2 - cpp_offset];		// If there is no LT recuperator, there is no pressure drop
//
//		m_pres_last[4 - cpp_offset] = m_pres_last[3 - cpp_offset];			// Assume no pressure drop in mixing valve
//		m_pres_last[10 - cpp_offset] = m_pres_last[3 - cpp_offset];			// Assume no pressure drop in mixing valve
//
//		if( ms_des_par.m_DP_HT[1 - cpp_offset] < 0.0 )
//			m_pres_last[5 - cpp_offset] = m_pres_last[4 - cpp_offset] - m_pres_last[4 - cpp_offset] * fabs(ms_des_par.m_DP_HT[1 - cpp_offset]);	// relative pressure drop specified for HT recuperator (cold stream)
//		else
//			m_pres_last[5 - cpp_offset] = m_pres_last[4 - cpp_offset] - ms_des_par.m_DP_HT[1 - cpp_offset];									// absolute pressure drop specified for HT recuperator (cold stream)
//
//		if( ms_des_par.m_UA_HT < 1.0E-12 )
//			m_pres_last[5 - cpp_offset] = m_pres_last[4 - cpp_offset];		// If there is no HT recuperator, there is no pressure drop
//
//		if( ms_des_par.m_DP_PHX[1 - cpp_offset] < 0.0 )
//			m_pres_last[6 - cpp_offset] = m_pres_last[5 - cpp_offset] - m_pres_last[5 - cpp_offset] * fabs(ms_des_par.m_DP_PHX[1 - cpp_offset]);	// relative pressure drop specified for PHX
//		else
//			m_pres_last[6 - cpp_offset] = m_pres_last[5 - cpp_offset] - ms_des_par.m_DP_PHX[1 - cpp_offset];									// absolute pressure drop specified for PHX
//
//		if( ms_des_par.m_DP_PC[2 - cpp_offset] < 0.0 )
//			m_pres_last[9 - cpp_offset] = m_pres_last[1 - cpp_offset] / (1.0 - fabs(ms_des_par.m_DP_PC[2 - cpp_offset]));	// relative pressure drop specified for precooler: P1=P9-P9*rel_DP => P1=P9*(1-rel_DP)
//		else
//			m_pres_last[9 - cpp_offset] = m_pres_last[1 - cpp_offset] + ms_des_par.m_DP_PC[2 - cpp_offset];
//
//		if( ms_des_par.m_DP_LT[2 - cpp_offset] < 0.0 )
//			m_pres_last[8 - cpp_offset] = m_pres_last[9 - cpp_offset] / (1.0 - fabs(ms_des_par.m_DP_LT[2 - cpp_offset]));	// relative pressure drop specified for LT recuperator (hot stream)
//		else
//			m_pres_last[8 - cpp_offset] = m_pres_last[9 - cpp_offset] + ms_des_par.m_DP_LT[2 - cpp_offset];				// absolute pressure drop specified for LT recuperator (hot stream)
//
//		if( ms_des_par.m_UA_LT < 1.0E-12 )
//			m_pres_last[8 - cpp_offset] = m_pres_last[9 - cpp_offset];		// if there is no LT recuperator, there is no pressure drop
//
//		if( ms_des_par.m_DP_HT[2 - cpp_offset] < 0.0 )
//			m_pres_last[7 - cpp_offset] = m_pres_last[8 - cpp_offset] / (1.0 - fabs(ms_des_par.m_DP_HT[2 - cpp_offset]));	// relative pressure drop specified for HT recuperator (hot stream)
//		else
//			m_pres_last[7 - cpp_offset] = m_pres_last[8 - cpp_offset] + ms_des_par.m_DP_HT[2 - cpp_offset];				// absolute pressure drop specified for HT recuperator (hot stream)
//
//		if( ms_des_par.m_UA_HT < 1.0E-12 )
//			m_pres_last[7 - cpp_offset] = m_pres_last[8 - cpp_offset];		// if there is no HT recuperator, there is no pressure drop
//
//		// Determine equivalent isentropic efficiencies for main compressor and turbine, if necessary.
//		double eta_mc_isen = std::numeric_limits<double>::quiet_NaN();
//		double eta_t_isen = std::numeric_limits<double>::quiet_NaN();
//		if( ms_des_par.m_eta_mc < 0.0 )
//		{
//			int poly_error_code = 0;
//
//			isen_eta_from_poly_eta(m_temp_last[1 - cpp_offset], m_pres_last[1 - cpp_offset], m_pres_last[2 - cpp_offset], fabs(ms_des_par.m_eta_mc),
//				true, poly_error_code, eta_mc_isen);
//
//			if( poly_error_code != 0 )
//			{
//				error_code = poly_error_code;
//				return;
//			}
//		}
//		else
//			eta_mc_isen = ms_des_par.m_eta_mc;
//
//		if( ms_des_par.m_eta_t < 0.0 )
//		{
//			int poly_error_code = 0;
//
//			isen_eta_from_poly_eta(m_temp_last[6 - cpp_offset], m_pres_last[6 - cpp_offset], m_pres_last[7 - cpp_offset], fabs(ms_des_par.m_eta_t),
//				false, poly_error_code, eta_t_isen);
//
//			if( poly_error_code != 0 )
//			{
//				error_code = poly_error_code;
//				return;
//			}
//		}
//		else
//			eta_t_isen = ms_des_par.m_eta_t;
//
//		// Determine the outlet state and specific work for the main compressor and turbine.
//		int comp_error_code = 0;
//		double w_mc = std::numeric_limits<double>::quiet_NaN();
//		// Main compressor
//		calculate_turbomachinery_outlet_1(m_temp_last[1 - cpp_offset], m_pres_last[1 - cpp_offset], m_pres_last[2 - cpp_offset], eta_mc_isen, true,
//			comp_error_code, m_enth_last[1 - cpp_offset], m_entr_last[1 - cpp_offset], m_dens_last[1 - cpp_offset], m_temp_last[2 - cpp_offset],
//			m_enth_last[2 - cpp_offset], m_entr_last[2 - cpp_offset], m_dens_last[2 - cpp_offset], w_mc);
//
//		if( comp_error_code != 0 )
//		{
//			error_code = comp_error_code;
//			return;
//		}
//
//		int turbine_error_code = 0;
//		double w_t = std::numeric_limits<double>::quiet_NaN();
//		// Turbine
//		calculate_turbomachinery_outlet_1(m_temp_last[6 - cpp_offset], m_pres_last[6 - cpp_offset], m_pres_last[7 - cpp_offset], eta_t_isen, false,
//			turbine_error_code, m_enth_last[6 - cpp_offset], m_entr_last[6 - cpp_offset], m_dens_last[6 - cpp_offset], m_temp_last[7 - cpp_offset],
//			m_enth_last[7 - cpp_offset], m_entr_last[7 - cpp_offset], m_dens_last[7 - cpp_offset], w_t);
//
//		if( turbine_error_code != 0 )
//		{
//			error_code = turbine_error_code;
//			return;
//		}
//
//		// Check that this cycle can produce power
//		double eta_rc_isen = std::numeric_limits<double>::quiet_NaN();
//		double w_rc = std::numeric_limits<double>::quiet_NaN();
//		if( ms_des_par.m_recomp_frac >= 1.E-12 )
//		{
//			if( ms_des_par.m_eta_rc < 0.0 )		// need to convert polytropic efficiency to isentropic efficiency
//			{
//				int rc_error_code = 0;
//
//				isen_eta_from_poly_eta(m_temp_last[2 - cpp_offset], m_pres_last[9 - cpp_offset], m_pres_last[10 - cpp_offset], fabs(ms_des_par.m_eta_rc),
//					true, rc_error_code, eta_rc_isen);
//
//				if( rc_error_code != 0 )
//				{
//					error_code = rc_error_code;
//					return;
//				}
//			}
//			else
//				eta_rc_isen = ms_des_par.m_eta_rc;
//
//			int rc_error_code = 0;
//			calculate_turbomachinery_outlet_1(m_temp_last[2 - cpp_offset], m_pres_last[9 - cpp_offset], m_pres_last[10 - cpp_offset], eta_rc_isen,
//				true, rc_error_code, w_rc);
//
//			if( rc_error_code != 0 )
//			{
//				error_code = rc_error_code;
//				return;
//			}
//		}
//		else
//			w_rc = 0.0;
//
//		if( w_mc + w_rc + w_t <= 0.0 )	// positive net power is impossible; return an error
//		{
//			error_code = 25;
//			return;
//		}
//
//		// Outer iteration loop: temp(8), checking against UA_HT
//		double T8_lower_bound, T8_upper_bound, last_HT_residual, last_T8_guess;
//		T8_lower_bound = T8_upper_bound = last_HT_residual = last_T8_guess = std::numeric_limits<double>::quiet_NaN();
//		if( ms_des_par.m_UA_HT < 1.0E-12 )		// no high-temp recuperator
//		{
//			T8_lower_bound = m_temp_last[7 - cpp_offset];		// no iteration necessary
//			T8_upper_bound = m_temp_last[7 - cpp_offset];		// no iteration necessary
//			m_temp_last[8 - cpp_offset] = m_temp_last[7 - cpp_offset];
//			UA_HT_calc = 0.0;
//			last_HT_residual = 0.0;
//			last_T8_guess = m_temp_last[7 - cpp_offset];
//		}
//		else
//		{
//			T8_lower_bound = m_temp_last[2 - cpp_offset];		// the absolute lower temp(8) could be
//			T8_upper_bound = m_temp_last[7 - cpp_offset];		// the absolutely highest temp(8) could be
//			m_temp_last[8 - cpp_offset] = (T8_lower_bound + T8_upper_bound)*0.5;		// bisect bounds for first guess
//			UA_HT_calc = -1.0;
//			last_HT_residual = ms_des_par.m_UA_HT;			// know a priori that with T8 = T7, UA_calc = 0 therefore residual is UA_HT - 0.0
//			last_T8_guess = m_temp_last[7 - cpp_offset];
//		}
//
//		int prop_error_code = 0;
//
//		double T9_lower_bound, T9_upper_bound, last_LT_residual, last_T9_guess;
//		T9_lower_bound = T9_upper_bound = last_LT_residual = last_T9_guess = std::numeric_limits<double>::quiet_NaN();
//
//		double min_DT_LT = std::numeric_limits<double>::quiet_NaN();
//		double min_DT_HT = std::numeric_limits<double>::quiet_NaN();
//
//		int T8_iter = -1;
//		for( T8_iter = 0; T8_iter < max_iter; T8_iter++ )
//		{
//			// Fully define state 8
//			prop_error_code = CO2_TP(m_temp_last[8 - cpp_offset], m_pres_last[8 - cpp_offset], &co2_props);
//			if( prop_error_code != 0 )
//			{
//				error_code = prop_error_code;
//				return;
//			}
//			m_enth_last[8 - cpp_offset] = co2_props.enth;
//			m_entr_last[8 - cpp_offset] = co2_props.entr;
//			m_dens_last[8 - cpp_offset] = co2_props.dens;
//
//			// Inner iteration loop: temp(9), checking against UA_LT
//			if( ms_des_par.m_UA_LT < 1.0E-12 )	// no low-temperature recuperator
//			{
//				T9_lower_bound = m_temp_last[8 - cpp_offset];		// no iteration necessary
//				T9_upper_bound = m_temp_last[8 - cpp_offset];		// no iteration necessary
//				m_temp_last[9 - cpp_offset] = m_temp_last[8 - cpp_offset];
//				UA_LT_calc = 0.0;
//				last_LT_residual = 0.0;
//				last_T9_guess = m_temp_last[8 - cpp_offset];
//			}
//			else
//			{
//				T9_lower_bound = m_temp_last[2 - cpp_offset];		// the absolute lowest temp(9) could be
//				T9_upper_bound = m_temp_last[8 - cpp_offset];		// the absolute highest temp(9) could be
//				m_temp_last[9 - cpp_offset] = (T9_lower_bound + T9_upper_bound)*0.5;	// biset bounds for first guess
//				UA_LT_calc = -1.0;
//				last_LT_residual = ms_des_par.m_UA_LT;			// know a priori that with T9 = T8, UA_calc = 0 therefore residual is UA_LT - 0
//				last_T9_guess = m_temp_last[8 - cpp_offset];
//			}
//
//			int T9_iter = -1;
//			for( T9_iter = 0; T9_iter < max_iter; T9_iter++ )
//			{
//				// Determine the outlet state of the recompressing compressor and its specific work
//				if( ms_des_par.m_recomp_frac >= 1.E-12 )
//				{
//					if( ms_des_par.m_eta_rc < 0.0 )		// recalculate isentropic efficiency of recompressing compressor (because T9 changes)
//					{
//						int rc_error_code = 0;
//						isen_eta_from_poly_eta(m_temp_last[9 - cpp_offset], m_pres_last[9 - cpp_offset], m_pres_last[10 - cpp_offset], fabs(ms_des_par.m_eta_rc), true,
//							rc_error_code, eta_rc_isen);
//
//						if( rc_error_code != 0 )
//						{
//							error_code = rc_error_code;
//							return;
//						}
//					}
//					else
//					{
//						eta_rc_isen = ms_des_par.m_eta_rc;
//					}
//
//					int rc_error_code = 0;
//					calculate_turbomachinery_outlet_1(m_temp_last[9 - cpp_offset], m_pres_last[9 - cpp_offset], m_pres_last[10 - cpp_offset], eta_rc_isen, true, rc_error_code,
//						m_enth_last[9 - cpp_offset], m_entr_last[9 - cpp_offset], m_dens_last[9 - cpp_offset], m_temp_last[10 - cpp_offset], m_enth_last[10 - cpp_offset], m_entr_last[10 - cpp_offset],
//						m_dens_last[10 - cpp_offset], w_rc);
//
//					if( rc_error_code != 0 )
//					{
//						error_code = rc_error_code;
//						return;
//					}
//				}
//				else
//				{
//					w_rc = 0.0;		// the recompressing compressor does not exist
//					prop_error_code = CO2_TP(m_temp_last[9 - cpp_offset], m_pres_last[9 - cpp_offset], &co2_props);
//					if( prop_error_code != 0 )		// fully define state 9
//					{
//						error_code = prop_error_code;
//						return;
//					}
//					m_enth_last[9 - cpp_offset] = co2_props.enth;
//					m_entr_last[9 - cpp_offset] = co2_props.entr;
//					m_dens_last[9 - cpp_offset] = co2_props.dens;
//					m_temp_last[10 - cpp_offset] = m_temp_last[9 - cpp_offset];		// assume state 10 is the same as state 9
//					m_enth_last[10 - cpp_offset] = m_enth_last[9 - cpp_offset];
//					m_entr_last[10 - cpp_offset] = m_entr_last[9 - cpp_offset];
//					m_dens_last[10 - cpp_offset] = m_dens_last[9 - cpp_offset];
//				}
//
//				// Knowing the specific work of the recompressor, the required mass flow rate can be calculated
//				m_dot_t = ms_des_par.m_W_dot_net / (w_mc*(1.0 - ms_des_par.m_recomp_frac) + w_rc*ms_des_par.m_recomp_frac + w_t);	// Required mass flow rate through turbine
//				if( m_dot_t < 0.0 )		// positive power output is not possible with these inputs
//				{
//					error_code = 29;
//					return;
//				}
//				m_dot_rc = m_dot_t * ms_des_par.m_recomp_frac;		// apply definition of recompression fraction
//				m_dot_mc = m_dot_t - m_dot_rc;						// mass balance
//
//				// Calculate the UA value of the low-temperature recuperator
//				if( ms_des_par.m_UA_LT < 1.0E-12 )		// no low-temp recuperator (this check is necessary to prevent pressure drops with UA=0 from causing problems)
//					Q_dot_LT = 0.0;
//				else
//					Q_dot_LT = m_dot_t * (m_enth_last[8 - cpp_offset] - m_enth_last[9 - cpp_offset]);
//
//				int hx_error_code = 0;
//				min_DT_LT = std::numeric_limits<double>::quiet_NaN();
//				calculate_hxr_UA_1(ms_des_par.m_N_sub_hxrs, Q_dot_LT, m_dot_mc, m_dot_t, m_temp_last[2 - cpp_offset], m_temp_last[8 - cpp_offset],
//					m_pres_last[2 - cpp_offset], m_pres_last[3 - cpp_offset], m_pres_last[8 - cpp_offset], m_pres_last[9 - cpp_offset],
//					hx_error_code, UA_LT_calc, min_DT_LT);
//
//				if( hx_error_code != 0 )
//				{
//					if( hx_error_code == 11 )		// second-law violation in hxr, therefore temp(9) is too low
//					{
//						T9_lower_bound = m_temp_last[9 - cpp_offset];
//						m_temp_last[9 - cpp_offset] = 0.5*(T9_lower_bound + T9_upper_bound);		// bisect bounds for next guess
//						continue;
//					}
//					else
//					{
//						error_code = hx_error_code;
//						return;
//					}
//				}
//
//				// Check for convergence and adjust T9 appropriately
//				double UA_LT_residual = ms_des_par.m_UA_LT - UA_LT_calc;
//
//				if( fabs(UA_LT_residual) < 1.0E-12 )		// catches no LT case
//					break;
//
//				double secant_guess = m_temp_last[9 - cpp_offset] - UA_LT_residual*(last_T9_guess - m_temp_last[9 - cpp_offset]) / (last_LT_residual - UA_LT_residual);	// next guess predicted using secant method
//
//				if( UA_LT_residual < 0.0 )		// UA_LT_calc is too big, temp(9) needs to be higher
//				{
//					if( fabs(UA_LT_residual) / ms_des_par.m_UA_LT < ms_des_par.m_tol )
//						break;
//
//					T9_lower_bound = m_temp_last[9 - cpp_offset];
//				}
//				else		// UA_LT_calc is too small, temp(9) needs to be lower
//				{
//					if( UA_LT_residual / ms_des_par.m_UA_LT < ms_des_par.m_tol )	// UA_LT converged
//						break;
//
//					if( min_DT_LT < temperature_tolerance )		// UA_calc is still too low but there isn't anywhere to go so it's ok (catches huge UA values)
//						break;
//
//					T9_upper_bound = m_temp_last[9 - cpp_offset];
//				}
//
//				last_LT_residual = UA_LT_residual;			// reset lsat stored residual value
//				last_T9_guess = m_temp_last[9 - cpp_offset];	// reset last stored guess value
//
//				// Check if the secant method overshoots and fall back to bisection if it does
//				if( secant_guess <= T9_lower_bound || secant_guess >= T9_upper_bound || secant_guess != secant_guess )	// secant method overshot (or is NaN), use bisection
//					m_temp_last[9 - cpp_offset] = 0.5*(T9_lower_bound + T9_upper_bound);
//				else
//					m_temp_last[9 - cpp_offset] = secant_guess;
//
//			}	// End T9 iteration
//
//			// Check that T9_loop converged
//			if( T9_iter >= max_iter )
//			{
//				error_code = 31;
//				return;
//			}
//
//			// State 3 can now be fully defined
//			m_enth_last[3 - cpp_offset] = m_enth_last[2 - cpp_offset] + Q_dot_LT / m_dot_mc;		// Energy balalnce on cold stream of low-temp recuperator
//			prop_error_code = CO2_PH(m_pres_last[3 - cpp_offset], m_enth_last[3 - cpp_offset], &co2_props);
//			if( prop_error_code != 0 )
//			{
//				error_code = prop_error_code;
//				return;
//			}
//			m_temp_last[3 - cpp_offset] = co2_props.temp;
//			m_entr_last[3 - cpp_offset] = co2_props.entr;
//			m_dens_last[3 - cpp_offset] = co2_props.dens;
//
//			// Go through the mixing valve
//			if( ms_des_par.m_recomp_frac >= 1.E-12 )
//			{
//				m_enth_last[4 - cpp_offset] = (1.0 - ms_des_par.m_recomp_frac)*m_enth_last[3 - cpp_offset] + ms_des_par.m_recomp_frac*m_enth_last[10 - cpp_offset];	// conservation of energy (both sides divided by m_dot_t)
//				prop_error_code = CO2_PH(m_pres_last[4 - cpp_offset], m_enth_last[4 - cpp_offset], &co2_props);
//				if( prop_error_code != 0 )
//				{
//					error_code = prop_error_code;
//					return;
//				}
//				m_temp_last[4 - cpp_offset] = co2_props.temp;
//				m_entr_last[4 - cpp_offset] = co2_props.entr;
//				m_dens_last[4 - cpp_offset] = co2_props.dens;
//			}
//			else		// no mixing valve, therefore state 4 is equal to state 3
//			{
//				m_temp_last[4 - cpp_offset] = m_temp_last[3 - cpp_offset];
//				m_enth_last[4 - cpp_offset] = m_enth_last[3 - cpp_offset];
//				m_entr_last[4 - cpp_offset] = m_entr_last[3 - cpp_offset];
//				m_dens_last[4 - cpp_offset] = m_dens_last[3 - cpp_offset];
//			}
//
//			// Check for a second law violation at the outlet of the high-temp recuperator
//			if( m_temp_last[4 - cpp_offset] >= m_temp_last[8 - cpp_offset] )		// temp(8) is not valid and it must be increased
//			{
//				T8_lower_bound = m_temp_last[8 - cpp_offset];
//				m_temp_last[8 - cpp_offset] = 0.5*(T8_lower_bound + T8_upper_bound);
//				continue;
//			}
//
//			double m_dot_bypass = f_bypass*m_dot_t;
//			double m_dot_HTR_cold = (1.0 - f_bypass)*m_dot_t;
//
//			// Calculate the UA value of the high-temp recuperator
//			if( ms_des_par.m_UA_HT < 1.E-12 )			// no high-temp recuperator
//				Q_dot_HT = 0.0;
//			else
//				Q_dot_HT = m_dot_t * (m_enth_last[7 - cpp_offset] - m_enth_last[8 - cpp_offset]);
//
//
//			int HT_error_code = 0;
//			min_DT_HT = std::numeric_limits<double>::quiet_NaN();
//
//			calculate_hxr_UA_1(ms_des_par.m_N_sub_hxrs, Q_dot_HT, m_dot_HTR_cold, m_dot_t, m_temp_last[4 - cpp_offset], m_temp_last[7 - cpp_offset], m_pres_last[4 - cpp_offset],
//				m_pres_last[5 - cpp_offset], m_pres_last[7 - cpp_offset], m_pres_last[8 - cpp_offset], HT_error_code, UA_HT_calc, min_DT_HT);
//
//			if( HT_error_code != 0 )
//			{
//				if( HT_error_code == 11 )			// second-law violation in hxr, therefore temp(8) is too low
//				{
//					T8_lower_bound = m_temp_last[8 - cpp_offset];
//					m_temp_last[8 - cpp_offset] = 0.5*(T8_lower_bound + T8_upper_bound);	// bisect bounds for next guess
//					continue;
//				}
//				else
//				{
//					error_code = HT_error_code;
//					return;
//				}
//			}
//
//			// Check for convergence and adjust T8 appropriately
//			double UA_HT_residual = ms_des_par.m_UA_HT - UA_HT_calc;
//
//			if( fabs(UA_HT_residual) < 1.0E-12 )		// catches no HT case
//				break;
//
//			double secant_guess = m_temp_last[8 - cpp_offset] - UA_HT_residual*(last_T8_guess - m_temp_last[8 - cpp_offset]) / (last_HT_residual - UA_HT_residual);		// Next guess predicted using secant method
//
//			if( UA_HT_residual < 0.0 )	// UA_HT_calc is too big, temp(8) needs to be higher
//			{
//				if( fabs(UA_HT_residual) / ms_des_par.m_UA_HT < ms_des_par.m_tol )
//					break;
//				T8_lower_bound = m_temp_last[8 - cpp_offset];
//			}
//			else						// UA_HT_calc is too small, temp(8) needs to be lower
//			{
//				if( UA_HT_residual / ms_des_par.m_UA_HT < ms_des_par.m_tol )		// UA_HT converged
//					break;
//				if( min_DT_HT < temperature_tolerance )								// UA_calc is still too low, but there isn't anywhere to go so it's okay
//					break;
//				T8_upper_bound = m_temp_last[8 - cpp_offset];
//			}
//			last_HT_residual = UA_HT_residual;				// reset last stored residual value
//			last_T8_guess = m_temp_last[8 - cpp_offset];		// reset last stored guess value
//
//			// Check if the secant method overshoots and fall back to bisection if it does
//			if( secant_guess <= T8_lower_bound || secant_guess >= T8_upper_bound )		// secant method overshot, use bisection
//				m_temp_last[8 - cpp_offset] = 0.5*(T8_lower_bound + T8_upper_bound);
//			else
//				m_temp_last[8 - cpp_offset] = secant_guess;
//
//		}	// End T8 iteration
//
//		// Check that T8_loop converged
//		if( T8_iter >= max_iter )
//		{
//			error_code = 35;
//			return;
//		}
//
//		// State 5 can now be fully defined
//		m_enth_last[5 - cpp_offset] = m_enth_last[4 - cpp_offset] + Q_dot_HT / ((1.0 - f_bypass)*m_dot_t);						// Energy balance on cold stream of high-temp recuperator
//		prop_error_code = CO2_PH(m_pres_last[5 - cpp_offset], m_enth_last[5 - cpp_offset], &co2_props);
//		if( prop_error_code != 0 )
//		{
//			error_code = prop_error_code;
//			return;
//		}
//		m_temp_last[5 - cpp_offset] = co2_props.temp;
//		m_entr_last[5 - cpp_offset] = co2_props.entr;
//		m_dens_last[5 - cpp_offset] = co2_props.dens;
//
//		// Calculate performance metrics for low-temperature recuperator
//		C_HeatExchanger::S_design_parameters LT_des_par;
//		double C_dot_hot = m_dot_t*(m_enth_last[8 - cpp_offset] - m_enth_last[9 - cpp_offset]) / (m_temp_last[8 - cpp_offset] - m_temp_last[9 - cpp_offset]);		// LT recuperator hot stream capacitance rate
//		double C_dot_cold = m_dot_mc*(m_enth_last[3 - cpp_offset] - m_enth_last[2 - cpp_offset]) / (m_temp_last[3 - cpp_offset] - m_temp_last[2 - cpp_offset]);	// LT recuperator cold stream capacitance rate
//		double C_dot_min = min(C_dot_hot, C_dot_cold);
//		double Q_dot_max = C_dot_min*(m_temp_last[8 - cpp_offset] - m_temp_last[2 - cpp_offset]);
//		double hx_eff = Q_dot_LT / Q_dot_max;				// Definition of effectiveness
//		LT_des_par.m_DP_design[0] = m_pres_last[2 - cpp_offset] - m_pres_last[3 - cpp_offset];
//		LT_des_par.m_DP_design[1] = m_pres_last[8 - cpp_offset] - m_pres_last[9 - cpp_offset];
//		LT_des_par.m_eff_design = hx_eff;
//		LT_des_par.m_min_DT_design = min_DT_LT;
//		LT_des_par.m_m_dot_design[0] = m_dot_mc;
//		LT_des_par.m_m_dot_design[1] = m_dot_t;
//		LT_des_par.m_N_sub = ms_des_par.m_N_sub_hxrs;
//		LT_des_par.m_Q_dot_design = Q_dot_LT;
//		LT_des_par.m_UA_design = UA_LT_calc;
//		m_LT.initialize(LT_des_par);
//
//		// Calculate performance metrics for high-temperature recuperator
//		C_HeatExchanger::S_design_parameters HT_des_par;
//		C_dot_hot = m_dot_t*(m_enth_last[7 - cpp_offset] - m_enth_last[8 - cpp_offset]) / (m_temp_last[7 - cpp_offset] - m_temp_last[8 - cpp_offset]);			// HT recuperator hot stream capacitance rate
//		C_dot_cold = m_dot_t*(m_enth_last[5 - cpp_offset] - m_enth_last[4 - cpp_offset]) / (m_temp_last[5 - cpp_offset] - m_temp_last[4 - cpp_offset]);			// HT recuperator cold stream capacitance rate
//		C_dot_min = min(C_dot_hot, C_dot_cold);
//		Q_dot_max = C_dot_min*(m_temp_last[7 - cpp_offset] - m_temp_last[4 - cpp_offset]);
//		hx_eff = Q_dot_HT / Q_dot_max;						// Definition of effectiveness
//		HT_des_par.m_DP_design[0] = m_pres_last[4 - cpp_offset] - m_pres_last[5 - cpp_offset];
//		HT_des_par.m_DP_design[1] = m_pres_last[7 - cpp_offset] - m_pres_last[8 - cpp_offset];
//		HT_des_par.m_eff_design = hx_eff;
//		HT_des_par.m_min_DT_design = min_DT_HT;
//		HT_des_par.m_m_dot_design[0] = m_dot_t;
//		HT_des_par.m_m_dot_design[1] = m_dot_t;
//		HT_des_par.m_N_sub = ms_des_par.m_N_sub_hxrs;
//		HT_des_par.m_Q_dot_design = Q_dot_HT;
//		HT_des_par.m_UA_design = UA_HT_calc;
//		m_HT.initialize(HT_des_par);
//
//		// Set relevant values for other heat exchangers
//		C_HeatExchanger::S_design_parameters PHX_des_par;
//		PHX_des_par.m_DP_design[0] = m_pres_last[5 - cpp_offset] - m_pres_last[6 - cpp_offset];
//		PHX_des_par.m_DP_design[1] = 0.0;
//		PHX_des_par.m_m_dot_design[0] = m_dot_t;
//		PHX_des_par.m_m_dot_design[1] = 0.0;
//		PHX_des_par.m_Q_dot_design = m_dot_t*(m_enth_last[6 - cpp_offset] - m_enth_last[5 - cpp_offset]);
//		m_PHX.initialize(PHX_des_par);
//
//		C_HeatExchanger::S_design_parameters PC_des_par;
//		PC_des_par.m_DP_design[0] = 0.0;
//		PC_des_par.m_DP_design[1] = m_pres_last[9 - cpp_offset] - m_pres_last[1 - cpp_offset];
//		PC_des_par.m_m_dot_design[0] = 0.0;
//		PC_des_par.m_m_dot_design[1] = m_dot_mc;
//		PC_des_par.m_Q_dot_design = m_dot_mc*(m_enth_last[9 - cpp_offset] - m_enth_last[1 - cpp_offset]);
//		m_PC.initialize(PC_des_par);
//
//		double Q_dot_bypass = f_bypass*m_dot_t*(m_enth_last[5 - cpp_offset] - m_enth_last[4 - cpp_offset]);
//
//		// Calculate/set cycle performance metrics
//		m_W_dot_net_last = w_mc*m_dot_mc + w_rc*m_dot_rc + w_t*m_dot_t;
//		m_eta_thermal_last = m_W_dot_net_last / (PHX_des_par.m_Q_dot_design + Q_dot_bypass);
//
//		m_m_dot_mc = m_dot_mc;
//		m_m_dot_rc = m_dot_rc;
//		m_m_dot_t = m_dot_t;
//
//		double E_bal = (PHX_des_par.m_Q_dot_design + Q_dot_bypass) - (m_W_dot_net_last + PC_des_par.m_Q_dot_design);
//
//		//double Q_dot_bypass = f_bypass*m_dot_t*(m_enth_last[5-cpp_offset] - m_enth_last[4-cpp_offset]);
//
//		double Q_hs_frac = Q_dot_bypass / (PHX_des_par.m_Q_dot_design+Q_dot_bypass);
//
//		double diff_q_hs_frac = Q_hs_frac - Q_hs_frac_target;
//
//		if( fabs(diff_q_hs_frac) > ms_des_par.m_tol )
//		{
//			if(diff_q_hs_frac > 0.0)
//			{
//				f_bypass_high = f_bypass;
//				f_bypass = 0.5*(f_bypass_high + f_bypass_low);
//			}
//			else
//			{
//				f_bypass_low = f_bypass;
//				f_bypass = 0.5*(f_bypass_high + f_bypass_low);
//			}
//			if(f_bypass_max - f_bypass_low < 0.005)
//			{
//				m_eta_thermal_last = 0.0;
//				break;
//			}
//			if(f_bypass_high - f_bypass_min < 0.005)
//			{
//				m_eta_thermal_last = 0.0;
//				break;
//			}
//		}
//		else
//		{
//			double this_solved_i_guess = 321.456;
//			break;
//		}
//
//		if(iter_f_bypass > 50)
//		{
//			m_eta_thermal_last = 0.0;
//			break;
//		}
//
//
//	} while( true );
//
//	
//}

void C_RecompCycle::design_core_standard(int & error_code)
{
	// twn 1.4.17: put reasonable lower bound on *modeled* recompression fraction
	if( ms_des_par.m_recomp_frac < 0.01 )
	{
		ms_des_par.m_recomp_frac = 0.0;
		double UA_tot = ms_des_par.m_UA_LT + ms_des_par.m_UA_HT;
		ms_des_par.m_UA_LT = UA_tot;
		ms_des_par.m_UA_HT = 0.0;
	}

	CO2_state co2_props;

	// Initialize Recuperators
		// LTR
	mc_LT_recup.initialize(ms_des_par.m_N_sub_hxrs);
		// HTR
	mc_HT_recup.initialize(ms_des_par.m_N_sub_hxrs);

	int max_iter = 500;
	double temperature_tolerance = 1.E-6;		// Temp differences below this are considered zero

	// Initialize a few variables
	double m_dot_t, m_dot_mc, m_dot_rc, Q_dot_LT, Q_dot_HT, UA_LT_calc, UA_HT_calc;
	m_dot_t = m_dot_mc = m_dot_rc = Q_dot_LT = Q_dot_HT = UA_LT_calc = UA_HT_calc = 0.0;

	m_temp_last[MC_IN] = ms_des_par.m_T_mc_in;
	m_pres_last[MC_IN] = ms_des_par.m_P_mc_in;
	m_pres_last[MC_OUT] = ms_des_par.m_P_mc_out;
	m_temp_last[TURB_IN] = ms_des_par.m_T_t_in;

	// Apply pressure drops to heat exchangers, fully defining the pressures at all states
	if( ms_des_par.m_DP_LT[0] < 0.0 )
		m_pres_last[LTR_HP_OUT] = m_pres_last[MC_OUT] - m_pres_last[MC_OUT] * fabs(ms_des_par.m_DP_LT[0]);		// relative pressure drop specified for LT recuperator (cold stream)
	else
		m_pres_last[LTR_HP_OUT] = m_pres_last[MC_OUT] - ms_des_par.m_DP_LT[0];				// absolute pressure drop specified for LT recuperator (cold stream)

	if( ms_des_par.m_UA_LT < 1.0E-12 )
		m_pres_last[LTR_HP_OUT] = m_pres_last[MC_OUT];			// If there is no LT recuperator, there is no pressure drop

	m_pres_last[MIXER_OUT] = m_pres_last[LTR_HP_OUT];			// Assume no pressure drop in mixing valve
	m_pres_last[RC_OUT] = m_pres_last[LTR_HP_OUT];				// Assume no pressure drop in mixing valve

	if( ms_des_par.m_DP_HT[0] < 0.0 )
		m_pres_last[HTR_HP_OUT] = m_pres_last[MIXER_OUT] - m_pres_last[MIXER_OUT] * fabs(ms_des_par.m_DP_HT[0]);	// relative pressure drop specified for HT recuperator (cold stream)
	else
		m_pres_last[HTR_HP_OUT] = m_pres_last[MIXER_OUT] - ms_des_par.m_DP_HT[0];				// absolute pressure drop specified for HT recuperator (cold stream)

	if( ms_des_par.m_UA_HT < 1.0E-12 )
		m_pres_last[HTR_HP_OUT] = m_pres_last[MIXER_OUT];		// If there is no HT recuperator, there is no pressure drop

	if( ms_des_par.m_DP_PHX[0] < 0.0 )
		m_pres_last[TURB_IN] = m_pres_last[HTR_HP_OUT] - m_pres_last[HTR_HP_OUT] * fabs(ms_des_par.m_DP_PHX[0]);	// relative pressure drop specified for PHX
	else
		m_pres_last[TURB_IN] = m_pres_last[HTR_HP_OUT] - ms_des_par.m_DP_PHX[0];									// absolute pressure drop specified for PHX

	if( ms_des_par.m_DP_PC[1] < 0.0 )
		m_pres_last[LTR_LP_OUT] = m_pres_last[MC_IN] / (1.0 - fabs(ms_des_par.m_DP_PC[1]));					// relative pressure drop specified for precooler: P1=P9-P9*rel_DP => P1=P9*(1-rel_DP)
	else
		m_pres_last[LTR_LP_OUT] = m_pres_last[MC_IN] + ms_des_par.m_DP_PC[1];

	if( ms_des_par.m_DP_LT[1] < 0.0 )
		m_pres_last[HTR_LP_OUT] = m_pres_last[LTR_LP_OUT] / (1.0 - fabs(ms_des_par.m_DP_LT[1]));	// relative pressure drop specified for LT recuperator (hot stream)
	else
		m_pres_last[HTR_LP_OUT] = m_pres_last[LTR_LP_OUT] + ms_des_par.m_DP_LT[1];					// absolute pressure drop specified for LT recuperator (hot stream)

	if( ms_des_par.m_UA_LT < 1.0E-12 )
		m_pres_last[HTR_LP_OUT] = m_pres_last[LTR_LP_OUT];			// if there is no LT recuperator, there is no pressure drop

	if( ms_des_par.m_DP_HT[1] < 0.0 )
		m_pres_last[TURB_OUT] = m_pres_last[HTR_LP_OUT] / (1.0 - fabs(ms_des_par.m_DP_HT[1]));	// relative pressure drop specified for HT recuperator (hot stream)
	else
		m_pres_last[TURB_OUT] = m_pres_last[HTR_LP_OUT] + ms_des_par.m_DP_HT[1];				// absolute pressure drop specified for HT recuperator (hot stream)

	if( ms_des_par.m_UA_HT < 1.0E-12 )
		m_pres_last[TURB_OUT] = m_pres_last[HTR_LP_OUT];		// if there is no HT recuperator, there is no pressure drop

	// Determine equivalent isentropic efficiencies for main compressor and turbine, if necessary.
	double eta_mc_isen = std::numeric_limits<double>::quiet_NaN();
	double eta_t_isen = std::numeric_limits<double>::quiet_NaN();
	if( ms_des_par.m_eta_mc < 0.0 )
	{
		int poly_error_code = 0;

		isen_eta_from_poly_eta(m_temp_last[MC_IN], m_pres_last[MC_IN], m_pres_last[MC_OUT], fabs(ms_des_par.m_eta_mc),
			true, poly_error_code, eta_mc_isen);

		if( poly_error_code != 0 )
		{
			error_code = poly_error_code;
			return;
		}
	}
	else
		eta_mc_isen = ms_des_par.m_eta_mc;

	if( ms_des_par.m_eta_t < 0.0 )
	{
		int poly_error_code = 0;

		isen_eta_from_poly_eta(m_temp_last[TURB_IN], m_pres_last[TURB_IN], m_pres_last[TURB_OUT], fabs(ms_des_par.m_eta_t),
			false, poly_error_code, eta_t_isen);

		if( poly_error_code != 0 )
		{
			error_code = poly_error_code;
			return;
		}
	}
	else
		eta_t_isen = ms_des_par.m_eta_t;

	// Determine the outlet state and specific work for the main compressor and turbine.
	int comp_error_code = 0;
	double w_mc = std::numeric_limits<double>::quiet_NaN();
	// Main compressor
	calculate_turbomachinery_outlet_1(m_temp_last[MC_IN], m_pres_last[MC_IN], m_pres_last[MC_OUT], eta_mc_isen, true,
		comp_error_code, m_enth_last[MC_IN], m_entr_last[MC_IN], m_dens_last[MC_IN], m_temp_last[MC_OUT],
		m_enth_last[MC_OUT], m_entr_last[MC_OUT], m_dens_last[MC_OUT], w_mc);

	if( comp_error_code != 0 )
	{
		error_code = comp_error_code;
		return;
	}

	int turbine_error_code = 0;
	double w_t = std::numeric_limits<double>::quiet_NaN();
	// Turbine
	calculate_turbomachinery_outlet_1(m_temp_last[TURB_IN], m_pres_last[TURB_IN], m_pres_last[TURB_OUT], eta_t_isen, false,
		turbine_error_code, m_enth_last[TURB_IN], m_entr_last[TURB_IN], m_dens_last[TURB_IN], m_temp_last[TURB_OUT],
		m_enth_last[TURB_OUT], m_entr_last[TURB_OUT], m_dens_last[TURB_OUT], w_t);

	if( turbine_error_code != 0 )
	{
		error_code = turbine_error_code;
		return;
	}

	// Check that this cycle can produce power
	double eta_rc_isen = std::numeric_limits<double>::quiet_NaN();
	double w_rc = std::numeric_limits<double>::quiet_NaN();
	if( ms_des_par.m_recomp_frac >= 1.E-12 )
	{
		if( ms_des_par.m_eta_rc < 0.0 )		// need to convert polytropic efficiency to isentropic efficiency
		{
			int rc_error_code = 0;

			isen_eta_from_poly_eta(m_temp_last[MC_OUT], m_pres_last[LTR_LP_OUT], m_pres_last[RC_OUT], fabs(ms_des_par.m_eta_rc),
				true, rc_error_code, eta_rc_isen);

			if( rc_error_code != 0 )
			{
				error_code = rc_error_code;
				return;
			}
		}
		else
			eta_rc_isen = ms_des_par.m_eta_rc;

		int rc_error_code = 0;
		calculate_turbomachinery_outlet_1(m_temp_last[MC_OUT], m_pres_last[LTR_LP_OUT], m_pres_last[RC_OUT], eta_rc_isen,
			true, rc_error_code, w_rc);

		if( rc_error_code != 0 )
		{
			error_code = rc_error_code;
			return;
		}
	}
	else
		w_rc = 0.0;

	if( w_mc + w_rc + w_t <= 0.0 )	// positive net power is impossible; return an error
	{
		error_code = 25;
		return;
	}


	// ****************************************************
	// ****************************************************
	// Solve the recuperators
	double T_HTR_LP_out_lower = m_temp_last[MC_OUT];		//[K] Coldest possible temperature
	double T_HTR_LP_out_upper = m_temp_last[TURB_OUT];		//[K] Hottest possible temperature
	
	double T_HTR_LP_out_guess_lower = min(T_HTR_LP_out_upper - 2.0, max(T_HTR_LP_out_lower + 15.0, 220.0 + 273.15));	//[K] There is nothing special about these guesses...
	double T_HTR_LP_out_guess_upper = min(T_HTR_LP_out_guess_lower + 20.0, T_HTR_LP_out_upper - 1.0);	//[K] There is nothing special about these guesses, either...
	
	C_mono_eq_HTR_des HTR_des_eq(this, w_mc, w_t);
	C_monotonic_eq_solver HTR_des_solver(HTR_des_eq);
	
	HTR_des_solver.settings(ms_des_par.m_tol*m_temp_last[MC_IN], 1000, T_HTR_LP_out_lower, T_HTR_LP_out_upper, false);
	
	double T_HTR_LP_out_solved, tol_T_HTR_LP_out_solved;
	T_HTR_LP_out_solved = tol_T_HTR_LP_out_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_T_HTR_LP_out = -1;
	
	int T_HTR_LP_out_code = HTR_des_solver.solve(T_HTR_LP_out_guess_lower, T_HTR_LP_out_guess_upper, 0,
								T_HTR_LP_out_solved, tol_T_HTR_LP_out_solved, iter_T_HTR_LP_out);
	
	if( T_HTR_LP_out_code != C_monotonic_eq_solver::CONVERGED )
	{
		error_code = 35;
		return;
	}

	// Get information calculated in C_mono_eq_HTR_des
	w_rc = HTR_des_eq.m_w_rc;
	m_dot_t = HTR_des_eq.m_m_dot_t;
	m_dot_rc = HTR_des_eq.m_m_dot_rc;
	m_dot_mc = HTR_des_eq.m_m_dot_mc;
	Q_dot_LT = HTR_des_eq.m_Q_dot_LT;
	Q_dot_HT = HTR_des_eq.m_Q_dot_HT;

	// State 5 can now be fully defined
	m_enth_last[HTR_HP_OUT] = m_enth_last[MIXER_OUT] + Q_dot_HT / m_dot_t;						// Energy balance on cold stream of high-temp recuperator
	int prop_error_code = CO2_PH(m_pres_last[HTR_HP_OUT], m_enth_last[HTR_HP_OUT], &co2_props);
	if( prop_error_code != 0 )
	{
		error_code = prop_error_code;
		return;
	}
	m_temp_last[HTR_HP_OUT] = co2_props.temp;
	m_entr_last[HTR_HP_OUT] = co2_props.entr;
	m_dens_last[HTR_HP_OUT] = co2_props.dens;

	// Set relevant values for other heat exchangers
	C_HeatExchanger::S_design_parameters PHX_des_par;
	PHX_des_par.m_DP_design[0] = m_pres_last[HTR_HP_OUT] - m_pres_last[TURB_IN];
	PHX_des_par.m_DP_design[1] = 0.0;
	PHX_des_par.m_m_dot_design[0] = m_dot_t;
	PHX_des_par.m_m_dot_design[1] = 0.0;
	PHX_des_par.m_Q_dot_design = m_dot_t*(m_enth_last[TURB_IN] - m_enth_last[HTR_HP_OUT]);
	m_PHX.initialize(PHX_des_par);

	C_HeatExchanger::S_design_parameters PC_des_par;
	PC_des_par.m_DP_design[0] = 0.0;
	PC_des_par.m_DP_design[1] = m_pres_last[LTR_LP_OUT] - m_pres_last[MC_IN];
	PC_des_par.m_m_dot_design[0] = 0.0;
	PC_des_par.m_m_dot_design[1] = m_dot_mc;
	PC_des_par.m_Q_dot_design = m_dot_mc*(m_enth_last[LTR_LP_OUT] - m_enth_last[MC_IN]);
	m_PC.initialize(PC_des_par);

	// Calculate/set cycle performance metrics
	m_W_dot_net_last = w_mc*m_dot_mc + w_rc*m_dot_rc + w_t*m_dot_t;
	m_eta_thermal_last = m_W_dot_net_last / PHX_des_par.m_Q_dot_design;

	m_m_dot_mc = m_dot_mc;
	m_m_dot_rc = m_dot_rc;
	m_m_dot_t = m_dot_t;
}

int C_RecompCycle::C_mono_eq_LTR_des::operator()(double T_LTR_LP_out /*K*/, double *diff_T_LTR_LP_out /*K*/)
{
	m_w_rc = m_m_dot_t = m_m_dot_rc = m_m_dot_mc = m_Q_dot_LT = std::numeric_limits<double>::quiet_NaN();
	
	mpc_rc_cycle->m_temp_last[LTR_LP_OUT] = T_LTR_LP_out;

	// First, solve the recompressor model as necessary
	if(mpc_rc_cycle->ms_des_par.m_recomp_frac >= 1.E-12)
	{
		double eta_rc_isen = std::numeric_limits<double>::quiet_NaN();

		if( mpc_rc_cycle->ms_des_par.m_eta_rc < 0.0 )		// recalculate isen. efficiency of recompressor because inlet temp changes
		{
			int rc_error_code = 0;
			isen_eta_from_poly_eta(mpc_rc_cycle->m_temp_last[LTR_LP_OUT], mpc_rc_cycle->m_pres_last[LTR_LP_OUT],
								mpc_rc_cycle->m_pres_last[RC_OUT], fabs(mpc_rc_cycle->ms_des_par.m_eta_rc), true,
								rc_error_code, eta_rc_isen);

			if( rc_error_code != 0 )
			{
				*diff_T_LTR_LP_out = std::numeric_limits<double>::quiet_NaN();
				return rc_error_code;
			}		
		}
		else
		{
			eta_rc_isen = mpc_rc_cycle->ms_des_par.m_eta_rc;
		}
	
		int rc_error_code = 0;

		calculate_turbomachinery_outlet_1(mpc_rc_cycle->m_temp_last[LTR_LP_OUT], mpc_rc_cycle->m_pres_last[LTR_LP_OUT], mpc_rc_cycle->m_pres_last[RC_OUT], eta_rc_isen, true, rc_error_code,
			mpc_rc_cycle->m_enth_last[LTR_LP_OUT], mpc_rc_cycle->m_entr_last[LTR_LP_OUT], mpc_rc_cycle->m_dens_last[LTR_LP_OUT], mpc_rc_cycle->m_temp_last[RC_OUT], mpc_rc_cycle->m_enth_last[RC_OUT],
			mpc_rc_cycle->m_entr_last[RC_OUT], mpc_rc_cycle->m_dens_last[RC_OUT], m_w_rc);

		if( rc_error_code != 0 )
		{
			*diff_T_LTR_LP_out = std::numeric_limits<double>::quiet_NaN();
			return rc_error_code;
		}
	}
	else
	{
		m_w_rc = 0.0;		// no recompressor
		int prop_error_code = CO2_TP(mpc_rc_cycle->m_temp_last[LTR_LP_OUT], mpc_rc_cycle->m_pres_last[LTR_LP_OUT], &mpc_rc_cycle->mc_co2_props);
		if( prop_error_code != 0 )
		{
			*diff_T_LTR_LP_out = std::numeric_limits<double>::quiet_NaN();
			return prop_error_code;
		}
		mpc_rc_cycle->m_enth_last[LTR_LP_OUT] = mpc_rc_cycle->mc_co2_props.enth;
		mpc_rc_cycle->m_entr_last[LTR_LP_OUT] = mpc_rc_cycle->mc_co2_props.entr;
		mpc_rc_cycle->m_dens_last[LTR_LP_OUT] = mpc_rc_cycle->mc_co2_props.dens;
		mpc_rc_cycle->m_temp_last[RC_OUT] = mpc_rc_cycle->m_temp_last[LTR_LP_OUT];
		mpc_rc_cycle->m_enth_last[RC_OUT] = mpc_rc_cycle->m_enth_last[LTR_LP_OUT];
		mpc_rc_cycle->m_entr_last[RC_OUT] = mpc_rc_cycle->m_entr_last[LTR_LP_OUT];
		mpc_rc_cycle->m_dens_last[RC_OUT] = mpc_rc_cycle->m_dens_last[LTR_LP_OUT];
	}

	// Calculate the mass flow required to hit cycle target power
	m_m_dot_t = mpc_rc_cycle->ms_des_par.m_W_dot_net / (m_w_mc*(1.0 - mpc_rc_cycle->ms_des_par.m_recomp_frac) + m_w_rc*mpc_rc_cycle->ms_des_par.m_recomp_frac + m_w_t);		//[kg/s]
	if( m_m_dot_t < 0.0 )
	{
		*diff_T_LTR_LP_out = std::numeric_limits<double>::quiet_NaN();
		return 29;
	}
	m_m_dot_rc = m_m_dot_t * mpc_rc_cycle->ms_des_par.m_recomp_frac;		//[kg/s]
	m_m_dot_mc = m_m_dot_t - m_m_dot_rc;

	double T_LTR_LP_out_calc = std::numeric_limits<double>::quiet_NaN();

	try
	{
		mpc_rc_cycle->mc_LT_recup.design_fix_UA_calc_outlet(mpc_rc_cycle->ms_des_par.m_UA_LT, mpc_rc_cycle->ms_des_par.m_LT_eff_max,
			mpc_rc_cycle->m_temp_last[MC_OUT], mpc_rc_cycle->m_pres_last[MC_OUT], m_m_dot_mc, mpc_rc_cycle->m_pres_last[LTR_HP_OUT],
			mpc_rc_cycle->m_temp_last[HTR_LP_OUT], mpc_rc_cycle->m_pres_last[HTR_LP_OUT], m_m_dot_t, mpc_rc_cycle->m_pres_last[LTR_LP_OUT],
			m_Q_dot_LT, mpc_rc_cycle->m_temp_last[LTR_HP_OUT], T_LTR_LP_out_calc);
	}
	catch( C_csp_exception & csp_except)
	{
		*diff_T_LTR_LP_out = std::numeric_limits<double>::quiet_NaN();

		return -1;
	}

	*diff_T_LTR_LP_out = T_LTR_LP_out_calc - mpc_rc_cycle->m_temp_last[LTR_LP_OUT];		//[K]

	return 0;
}

int C_RecompCycle::C_mono_eq_HTR_des::operator()(double T_HTR_LP_out /*K*/, double *diff_T_HTR_LP_out /*K*/)
{
	m_w_rc = m_m_dot_t = m_m_dot_rc = m_m_dot_mc = m_Q_dot_LT = m_Q_dot_HT = std::numeric_limits<double>::quiet_NaN();	

	mpc_rc_cycle->m_temp_last[HTR_LP_OUT] = T_HTR_LP_out;		//[K]	

	int prop_error_code = CO2_TP(mpc_rc_cycle->m_temp_last[HTR_LP_OUT], mpc_rc_cycle->m_temp_last[HTR_LP_OUT], &mpc_rc_cycle->mc_co2_props);
	if( prop_error_code != 0 )
	{
		*diff_T_HTR_LP_out = std::numeric_limits<double>::quiet_NaN();
		return prop_error_code;
	}
	mpc_rc_cycle->m_enth_last[HTR_LP_OUT] = mpc_rc_cycle->mc_co2_props.enth;
	mpc_rc_cycle->m_entr_last[HTR_LP_OUT] = mpc_rc_cycle->mc_co2_props.entr;
	mpc_rc_cycle->m_dens_last[HTR_LP_OUT] = mpc_rc_cycle->mc_co2_props.dens;

	// *********************************************************************************
	// *********************************************************************************
	// Solve for the LTR solution
	double T_LTR_LP_out_lower = mpc_rc_cycle->m_temp_last[MC_OUT];		//[K] Coldest possible outlet temperature
	double T_LTR_LP_out_upper = mpc_rc_cycle->m_temp_last[HTR_LP_OUT];	//[K] Hottest possible outlet temperature

	double T_LTR_LP_out_guess_upper = min(T_LTR_LP_out_upper, T_LTR_LP_out_lower + 15.0);	//[K] There is nothing special about using 15 here...
	double T_LTR_LP_out_guess_lower = min(T_LTR_LP_out_guess_upper*0.99, T_LTR_LP_out_lower + 2.0);	//[K] There is nothing special about using 2 here...

	C_mono_eq_LTR_des LTR_des_eq(mpc_rc_cycle, m_w_mc, m_w_t);
	C_monotonic_eq_solver LTR_des_solver(LTR_des_eq);

	LTR_des_solver.settings(mpc_rc_cycle->ms_des_par.m_tol*mpc_rc_cycle->m_temp_last[MC_IN], 1000, T_LTR_LP_out_lower,
								T_LTR_LP_out_upper, false);

	double T_LTR_LP_out_solved, tol_T_LTR_LP_out_solved;
	T_LTR_LP_out_solved = tol_T_LTR_LP_out_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_T_LTR_LP_out = -1;

	int T_LTR_LP_out_code = LTR_des_solver.solve(T_LTR_LP_out_guess_lower, T_LTR_LP_out_guess_upper, 0,
		T_LTR_LP_out_solved, tol_T_LTR_LP_out_solved, iter_T_LTR_LP_out);

	if( T_LTR_LP_out_code != C_monotonic_eq_solver::CONVERGED )
	{
		return 31;
	}
	  
	// Get information set in the Monotonic Equation class
	m_w_rc = LTR_des_eq.m_w_rc;
	m_m_dot_t = LTR_des_eq.m_m_dot_t;
	m_m_dot_rc = LTR_des_eq.m_m_dot_rc;
	m_m_dot_mc = LTR_des_eq.m_m_dot_mc;
	m_Q_dot_LT = LTR_des_eq.m_Q_dot_LT;

	// Know LTR performance so we can calculate the HP outlet
		// Energy balance on LTR HP stream
	mpc_rc_cycle->m_enth_last[LTR_HP_OUT] = mpc_rc_cycle->m_enth_last[MC_OUT] + m_Q_dot_LT/ m_m_dot_mc;		//[kJ/kg]
	prop_error_code = CO2_PH(mpc_rc_cycle->m_pres_last[LTR_HP_OUT], mpc_rc_cycle->m_enth_last[LTR_HP_OUT], &mpc_rc_cycle->mc_co2_props);
	if( prop_error_code != 0 )
	{
		*diff_T_HTR_LP_out = std::numeric_limits<double>::quiet_NaN();
		return prop_error_code;
	}
	mpc_rc_cycle->m_temp_last[LTR_HP_OUT] = mpc_rc_cycle->mc_co2_props.temp;	//[K]
	mpc_rc_cycle->m_entr_last[LTR_HP_OUT] = mpc_rc_cycle->mc_co2_props.entr;	//[kJ/kg-K]
	mpc_rc_cycle->m_dens_last[LTR_HP_OUT] = mpc_rc_cycle->mc_co2_props.dens;	//[kg/m^3]	

	// Simulate the Mixer
	if( mpc_rc_cycle->ms_des_par.m_recomp_frac >= 1.E-12 )
	{
		mpc_rc_cycle->m_enth_last[MIXER_OUT] = (1.0 - mpc_rc_cycle->ms_des_par.m_recomp_frac)*mpc_rc_cycle->m_enth_last[LTR_HP_OUT] + mpc_rc_cycle->ms_des_par.m_recomp_frac*mpc_rc_cycle->m_enth_last[RC_OUT];	//[kJ/kg]
		prop_error_code = CO2_PH(mpc_rc_cycle->m_pres_last[MIXER_OUT], mpc_rc_cycle->m_enth_last[MIXER_OUT], &mpc_rc_cycle->mc_co2_props);
		if( prop_error_code != 0 )
		{
			*diff_T_HTR_LP_out = std::numeric_limits<double>::quiet_NaN();
			return prop_error_code;
		}
		mpc_rc_cycle->m_temp_last[MIXER_OUT] = mpc_rc_cycle->mc_co2_props.temp;		//[K]
		mpc_rc_cycle->m_entr_last[MIXER_OUT] = mpc_rc_cycle->mc_co2_props.entr;		//[kJ/kg-K]
		mpc_rc_cycle->m_dens_last[MIXER_OUT] = mpc_rc_cycle->mc_co2_props.dens;		//[kg/m^3]
	}
	else
	{	// No recompressor, so no mixing required, and HTR HP inlet = LTR HP outlet
		mpc_rc_cycle->m_temp_last[MIXER_OUT] = mpc_rc_cycle->m_temp_last[LTR_HP_OUT];		//[K]
		mpc_rc_cycle->m_enth_last[MIXER_OUT] = mpc_rc_cycle->m_enth_last[LTR_HP_OUT];		//[kJ/kg]
		mpc_rc_cycle->m_entr_last[MIXER_OUT] = mpc_rc_cycle->m_entr_last[LTR_HP_OUT];		//[kJ/kg-K]
		mpc_rc_cycle->m_dens_last[MIXER_OUT] = mpc_rc_cycle->m_dens_last[LTR_HP_OUT];		//[kg/m^3]
	}

	// Find the design solution of the HTR
	double T_HTR_LP_out_calc = std::numeric_limits<double>::quiet_NaN();

	try
	{
	mpc_rc_cycle->mc_HT_recup.design_fix_UA_calc_outlet(mpc_rc_cycle->ms_des_par.m_UA_HT, mpc_rc_cycle->ms_des_par.m_HT_eff_max,
		mpc_rc_cycle->m_temp_last[MIXER_OUT], mpc_rc_cycle->m_pres_last[MIXER_OUT], m_m_dot_t, mpc_rc_cycle->m_pres_last[HTR_HP_OUT],
		mpc_rc_cycle->m_temp_last[TURB_OUT], mpc_rc_cycle->m_pres_last[TURB_OUT], m_m_dot_t, mpc_rc_cycle->m_pres_last[HTR_LP_OUT],
		m_Q_dot_HT, mpc_rc_cycle->m_temp_last[HTR_HP_OUT], T_HTR_LP_out_calc);
	}
	catch( C_csp_exception &csp_except )
	{
		*diff_T_HTR_LP_out = std::numeric_limits<double>::quiet_NaN();
		return -1;
	}
	
	*diff_T_HTR_LP_out = T_HTR_LP_out_calc - mpc_rc_cycle->m_temp_last[HTR_LP_OUT];		//[K]	

	return 0;
}


void C_RecompCycle::design_core(int & error_code)
{
	// 2.16.15 twn: choose which design point model to use

	

	
	//design_core_bypass150C(error_code);




	
	design_core_standard(error_code);

	//design_core_bypass(error_code);

	//design_core_HTR_hs(error_code);
}

void C_RecompCycle::design(S_design_parameters & des_par_in, int & error_code)
{
	ms_des_par = des_par_in;

	int design_error_code = 0;
	
	design_core(design_error_code);

	if(design_error_code != 0)
	{
		error_code = design_error_code;
		return;
	}

	finalize_design(design_error_code);

	error_code = design_error_code;
}

void C_RecompCycle::opt_design(S_opt_design_parameters & opt_des_par_in, int & error_code)
{
	ms_opt_des_par = opt_des_par_in;

	int opt_design_error_code = 0;

	opt_design_core(error_code);

	if(opt_design_error_code != 0)
	{
		error_code = opt_design_error_code;
		return;
	}

	finalize_design(opt_design_error_code);

	error_code = opt_design_error_code;
}

void C_RecompCycle::opt_design_core(int & error_code)
{
	// Map ms_opt_des_par to ms_des_par
	ms_des_par.m_W_dot_net = ms_opt_des_par.m_W_dot_net;
	ms_des_par.m_T_mc_in = ms_opt_des_par.m_T_mc_in;
	ms_des_par.m_T_t_in = ms_opt_des_par.m_T_t_in;
	ms_des_par.m_DP_LT = ms_opt_des_par.m_DP_LT;
	ms_des_par.m_DP_HT = ms_opt_des_par.m_DP_HT;
	ms_des_par.m_DP_PC = ms_opt_des_par.m_DP_PC;
	ms_des_par.m_DP_PHX = ms_opt_des_par.m_DP_PHX;
	ms_des_par.m_LT_eff_max = ms_opt_des_par.m_LT_eff_max;
	ms_des_par.m_HT_eff_max = ms_opt_des_par.m_HT_eff_max;
	ms_des_par.m_eta_mc = ms_opt_des_par.m_eta_mc;
	ms_des_par.m_eta_rc = ms_opt_des_par.m_eta_rc;
	ms_des_par.m_eta_t = ms_opt_des_par.m_eta_t;
	ms_des_par.m_N_sub_hxrs = ms_opt_des_par.m_N_sub_hxrs;
	ms_des_par.m_P_high_limit = ms_opt_des_par.m_P_high_limit;
	ms_des_par.m_tol = ms_opt_des_par.m_tol;
	ms_des_par.m_N_turbine = ms_opt_des_par.m_N_turbine;

	// ms_des_par members to be defined by optimizer and set in 'design_point_eta':
		// m_P_mc_in
		// m_P_mc_out
		// m_recomp_frac
		// m_UA_LT
		// m_UA_HT

	int index = 0;

	std::vector<double> x(0);
	std::vector<double> lb(0);
	std::vector<double> ub(0);
	std::vector<double> scale(0);

	if( !ms_opt_des_par.m_fixed_P_mc_out )
	{
		x.push_back(ms_opt_des_par.m_P_mc_out_guess);
		lb.push_back(100.0);
		ub.push_back(ms_opt_des_par.m_P_high_limit);
		scale.push_back(500.0);

		index++;
	}

	if( !ms_opt_des_par.m_fixed_PR_mc )
	{
		x.push_back(ms_opt_des_par.m_PR_mc_guess);
		lb.push_back(0.0001);
		double PR_max = ms_opt_des_par.m_P_high_limit / 100.0;
		ub.push_back(PR_max);
		scale.push_back(0.2);

		index++;
	}

	if( !ms_opt_des_par.m_fixed_recomp_frac )
	{
		x.push_back(ms_opt_des_par.m_recomp_frac_guess);
		lb.push_back(0.0);
		ub.push_back(1.0);
		scale.push_back(0.05);

		index++;
	}

	if( !ms_opt_des_par.m_fixed_LT_frac )
	{
		x.push_back(ms_opt_des_par.m_LT_frac_guess);
		lb.push_back(0.0);
		ub.push_back(1.0);
		scale.push_back(0.05);

		index++;
	}

	int no_opt_error_code = 0;
	if( index > 0 )
	{
		// Ensure thermal efficiency is initialized to 0
		m_eta_thermal_opt = 0.0;

		// Set up instance of nlopt class and set optimization parameters
		nlopt::opt		opt_des_cycle(nlopt::LN_SBPLX, index);
		opt_des_cycle.set_lower_bounds(lb);
		opt_des_cycle.set_upper_bounds(ub);
		opt_des_cycle.set_initial_step(scale);
		opt_des_cycle.set_xtol_rel(ms_opt_des_par.m_opt_tol);

		// Set max objective function
		opt_des_cycle.set_max_objective(nlopt_callback_opt_des_1, this);		// Calls wrapper/callback that calls 'design_point_eta', which optimizes design point eta through repeated calls to 'design'
		double max_f = std::numeric_limits<double>::quiet_NaN();
		nlopt::result   result_des_cycle = opt_des_cycle.optimize(x, max_f);
		
		ms_des_par = ms_des_par_optimal;

		design_core(no_opt_error_code);

		/*
		m_W_dot_net_last = m_W_dot_net_opt;
		m_eta_thermal_last = m_eta_thermal_opt;
		m_temp_last = m_temp_opt;
		m_pres_last = m_pres_opt;
		m_enth_last = m_enth_opt;
		m_entr_last = m_entr_opt;
		m_dens_last = m_dens_opt;
		*/
	}
	else
	{
		// Finish defining ms_des_par based on current 'x' values
		ms_des_par.m_P_mc_out = ms_opt_des_par.m_P_mc_out_guess;
		ms_des_par.m_P_mc_in = ms_des_par.m_P_mc_out / ms_opt_des_par.m_PR_mc_guess;
		ms_des_par.m_recomp_frac = ms_opt_des_par.m_recomp_frac_guess;
		ms_des_par.m_UA_LT = ms_opt_des_par.m_UA_rec_total*ms_opt_des_par.m_LT_frac_guess;
		ms_des_par.m_UA_HT = ms_opt_des_par.m_UA_rec_total*(1.0 - ms_opt_des_par.m_LT_frac_guess);
		
		design_core(no_opt_error_code);

		ms_des_par_optimal = ms_des_par;
	}

}

double C_RecompCycle::design_point_eta(const std::vector<double> &x)
{
	// 'x' is array of inputs either being adjusted by optimizer or set constant
	// Finish defining ms_des_par based on current 'x' values

	int index = 0;

	// Main compressor outlet pressure
	if( !ms_opt_des_par.m_fixed_P_mc_out )
	{
		ms_des_par.m_P_mc_out = x[index];
		if( ms_des_par.m_P_mc_out > ms_opt_des_par.m_P_high_limit )
			return 0.0;
		index++;
	}
	else
		ms_des_par.m_P_mc_out = ms_opt_des_par.m_P_mc_out_guess;

	// Main compressor pressure ratio
	double PR_mc_local = -999.9;
	double P_mc_in = -999.9;
	if( !ms_opt_des_par.m_fixed_PR_mc )
	{
		PR_mc_local = x[index];
		if( PR_mc_local > 50.0 )
			return 0.0;
		index++;
		P_mc_in = ms_des_par.m_P_mc_out / PR_mc_local;
	}
	else
	{
		if (ms_opt_des_par.m_PR_mc_guess >= 0.0)
		{
			PR_mc_local = ms_opt_des_par.m_PR_mc_guess;
			P_mc_in = ms_des_par.m_P_mc_out / PR_mc_local;		//[kPa]
		}
		else
		{
			P_mc_in = fabs(ms_opt_des_par.m_PR_mc_guess);		//[kPa]
		}
	}
	

	if( P_mc_in >= ms_des_par.m_P_mc_out )
		return 0.0;
	if( P_mc_in <= 100.0 )
		return 0.0;
	ms_des_par.m_P_mc_in = P_mc_in;

	// Recompression fraction
	if( !ms_opt_des_par.m_fixed_recomp_frac )
	{
		ms_des_par.m_recomp_frac = x[index];
		if( ms_des_par.m_recomp_frac < 0.0 )
			return 0.0;
		index++;
	}
	else
		ms_des_par.m_recomp_frac = ms_opt_des_par.m_recomp_frac_guess;

	// Recuperator split fraction
	double LT_frac_local = -999.9;
	if( !ms_opt_des_par.m_fixed_LT_frac )
	{
		LT_frac_local = x[index];
		if( LT_frac_local > 1.0 || LT_frac_local < 0.0 )
			return 0.0;
		index++;
	}
	else
		LT_frac_local = ms_opt_des_par.m_LT_frac_guess;
	
	ms_des_par.m_UA_LT = ms_opt_des_par.m_UA_rec_total*LT_frac_local;
	ms_des_par.m_UA_HT = ms_opt_des_par.m_UA_rec_total*(1.0 - LT_frac_local);

	int error_code = 0;

	design_core(error_code);

	double eta_thermal = 0.0;
	if( error_code == 0 )
	{
		eta_thermal = m_eta_thermal_last;

		if( m_eta_thermal_last > m_eta_thermal_opt )
		{
			ms_des_par_optimal = ms_des_par;
			m_eta_thermal_opt = m_eta_thermal_last;
		}
	}

	return eta_thermal;
}

void C_RecompCycle::auto_opt_design(S_auto_opt_design_parameters & auto_opt_des_par_in, int & error_code)
{
	ms_auto_opt_des_par = auto_opt_des_par_in;

	int auto_opt_des_error_code = 0;

	auto_opt_design_core(auto_opt_des_error_code);

	error_code = auto_opt_des_error_code;
	
	return;
}

void C_RecompCycle::auto_opt_design_core(int & error_code)
{
	// Check that simple/recomp flag is set
	if( ms_auto_opt_des_par.m_is_recomp_ok != 0 && ms_auto_opt_des_par.m_is_recomp_ok != 1 )
	{
		throw(C_csp_exception("C_RecompCycle::auto_opt_design_core(...) requires that ms_auto_opt_des_par.m_is_recomp_ok"
				"is either 0 (simple cycle only) or 1 (recomp allowed)\n"));
	}

	// map 'auto_opt_des_par_in' to 'ms_auto_opt_des_par'
	ms_opt_des_par.m_W_dot_net = ms_auto_opt_des_par.m_W_dot_net;
	ms_opt_des_par.m_T_mc_in = ms_auto_opt_des_par.m_T_mc_in;
	ms_opt_des_par.m_T_t_in = ms_auto_opt_des_par.m_T_t_in;
	ms_opt_des_par.m_DP_LT = ms_auto_opt_des_par.m_DP_LT;
	ms_opt_des_par.m_DP_HT = ms_auto_opt_des_par.m_DP_HT;
	ms_opt_des_par.m_DP_PC = ms_auto_opt_des_par.m_DP_PC;
	ms_opt_des_par.m_DP_PHX = ms_auto_opt_des_par.m_DP_PHX;
	ms_opt_des_par.m_LT_eff_max = ms_auto_opt_des_par.m_LT_eff_max;
	ms_opt_des_par.m_HT_eff_max = ms_auto_opt_des_par.m_HT_eff_max;
	ms_opt_des_par.m_UA_rec_total = ms_auto_opt_des_par.m_UA_rec_total;
	ms_opt_des_par.m_eta_mc = ms_auto_opt_des_par.m_eta_mc;
	ms_opt_des_par.m_eta_rc = ms_auto_opt_des_par.m_eta_rc;
	ms_opt_des_par.m_eta_t = ms_auto_opt_des_par.m_eta_t;
	ms_opt_des_par.m_N_sub_hxrs = ms_auto_opt_des_par.m_N_sub_hxrs;
	ms_opt_des_par.m_P_high_limit = ms_auto_opt_des_par.m_P_high_limit;
	ms_opt_des_par.m_tol = ms_auto_opt_des_par.m_tol;
	ms_opt_des_par.m_opt_tol = ms_auto_opt_des_par.m_opt_tol;
	ms_opt_des_par.m_N_turbine = ms_auto_opt_des_par.m_N_turbine;

	// Outer optimization loop
	m_eta_thermal_auto_opt = 0.0;

	double best_P_high = fminbr(
		ms_auto_opt_des_par.m_P_high_limit*0.2, ms_auto_opt_des_par.m_P_high_limit, &fmin_callback_opt_eta_1, this, 1.0);

	// Check model with P_mc_out set at P_high_limit for a recompression and simple cycle and use the better configuration
	double PR_mc_guess = ms_des_par_auto_opt.m_P_mc_out / ms_des_par_auto_opt.m_P_mc_in;

	if( ms_auto_opt_des_par.m_is_recomp_ok )
	{
		// Complete 'ms_opt_des_par' for recompression cycle
		ms_opt_des_par.m_P_mc_out_guess = ms_auto_opt_des_par.m_P_high_limit;
		ms_opt_des_par.m_fixed_P_mc_out = true;
		
		//ms_opt_des_par.m_PR_mc_guess = PR_mc_guess;
		//ms_opt_des_par.m_fixed_PR_mc = false;
		ms_opt_des_par.m_fixed_PR_mc = ms_auto_opt_des_par.m_fixed_PR_mc;	//[-]
		if (ms_opt_des_par.m_fixed_PR_mc)
		{
			ms_opt_des_par.m_PR_mc_guess = ms_auto_opt_des_par.m_PR_mc_guess;	//[-]
		}
		else
		{
			ms_opt_des_par.m_PR_mc_guess = PR_mc_guess;		//[-]
		}

		ms_opt_des_par.m_recomp_frac_guess = 0.3;
		ms_opt_des_par.m_fixed_recomp_frac = false;
		ms_opt_des_par.m_LT_frac_guess = 0.5;
		ms_opt_des_par.m_fixed_LT_frac = false;

		int rc_error_code = 0;

		opt_design_core(rc_error_code);

		if( rc_error_code == 0 && m_eta_thermal_opt > m_eta_thermal_auto_opt )
		{
			ms_des_par_auto_opt = ms_des_par_optimal;
			m_eta_thermal_auto_opt = m_eta_thermal_opt;
		}
	}

	// Complete 'ms_opt_des_par' for simple cycle
	ms_opt_des_par.m_P_mc_out_guess = ms_auto_opt_des_par.m_P_high_limit;
	ms_opt_des_par.m_fixed_P_mc_out = true;
	
	//ms_opt_des_par.m_PR_mc_guess = PR_mc_guess;
	//ms_opt_des_par.m_fixed_PR_mc = false;
	ms_opt_des_par.m_fixed_PR_mc = ms_auto_opt_des_par.m_fixed_PR_mc;	//[-]
	if (ms_opt_des_par.m_fixed_PR_mc)
	{
		ms_opt_des_par.m_PR_mc_guess = ms_auto_opt_des_par.m_PR_mc_guess;	//[-]
	}
	else
	{
		ms_opt_des_par.m_PR_mc_guess = PR_mc_guess;		//[-]
	}

	ms_opt_des_par.m_recomp_frac_guess = 0.0;
	ms_opt_des_par.m_fixed_recomp_frac = true;
	ms_opt_des_par.m_LT_frac_guess = 1.0;
	ms_opt_des_par.m_fixed_LT_frac = true;

	int s_error_code = 0;

	opt_design_core(s_error_code);

	if( s_error_code == 0 && m_eta_thermal_opt > m_eta_thermal_auto_opt )
	{
		ms_des_par_auto_opt = ms_des_par_optimal;
		m_eta_thermal_auto_opt = m_eta_thermal_opt;
	}

	ms_des_par = ms_des_par_auto_opt;

	int optimal_design_error_code = 0;
	design_core(optimal_design_error_code);

	if( optimal_design_error_code != 0 )
	{
		error_code = optimal_design_error_code;
		return;
	}

	finalize_design(optimal_design_error_code);

	error_code = optimal_design_error_code;
}

void C_RecompCycle::auto_opt_design_hit_eta(S_auto_opt_design_hit_eta_parameters & auto_opt_des_hit_eta_in, int & error_code, string & error_msg)
{
	ms_auto_opt_des_par.m_W_dot_net = auto_opt_des_hit_eta_in.m_W_dot_net;				//[kW] Target net cycle power
	ms_auto_opt_des_par.m_T_mc_in = auto_opt_des_hit_eta_in.m_T_mc_in;					//[K] Compressor inlet temperature
	ms_auto_opt_des_par.m_T_t_in = auto_opt_des_hit_eta_in.m_T_t_in;					//[K] Turbine inlet temperature
	ms_auto_opt_des_par.m_DP_LT = auto_opt_des_hit_eta_in.m_DP_LT;						//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	ms_auto_opt_des_par.m_DP_HT = auto_opt_des_hit_eta_in.m_DP_HT;						//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	ms_auto_opt_des_par.m_DP_PC = auto_opt_des_hit_eta_in.m_DP_PC;						//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	ms_auto_opt_des_par.m_DP_PHX = auto_opt_des_hit_eta_in.m_DP_PHX;					//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	ms_auto_opt_des_par.m_UA_rec_total = std::numeric_limits<double>::quiet_NaN();		// ***** This method finds the UA required to hit the input efficiency! *****
	ms_auto_opt_des_par.m_LT_eff_max = auto_opt_des_hit_eta_in.m_LT_eff_max;
	ms_auto_opt_des_par.m_HT_eff_max = auto_opt_des_hit_eta_in.m_HT_eff_max;
	ms_auto_opt_des_par.m_eta_mc = auto_opt_des_hit_eta_in.m_eta_mc;					//[-] design-point efficiency of the main compressor; isentropic if positive, polytropic if negative
	ms_auto_opt_des_par.m_eta_rc = auto_opt_des_hit_eta_in.m_eta_rc;					//[-] design-point efficiency of the recompressor; isentropic if positive, polytropic if negative
	ms_auto_opt_des_par.m_eta_t = auto_opt_des_hit_eta_in.m_eta_t;						//[-] design-point efficiency of the turbine; isentropic if positive, polytropic if negative
	ms_auto_opt_des_par.m_N_sub_hxrs = auto_opt_des_hit_eta_in.m_N_sub_hxrs;			//[-] Number of sub-heat exchangers to use when calculating UA value for a heat exchanger
	ms_auto_opt_des_par.m_P_high_limit = auto_opt_des_hit_eta_in.m_P_high_limit;		//[kPa] maximum allowable pressure in cycle
	ms_auto_opt_des_par.m_tol = auto_opt_des_hit_eta_in.m_tol;							//[-] Convergence tolerance
	ms_auto_opt_des_par.m_opt_tol = auto_opt_des_hit_eta_in.m_opt_tol;					//[-] Optimization tolerance
	ms_auto_opt_des_par.m_N_turbine = auto_opt_des_hit_eta_in.m_N_turbine;				//[rpm] Turbine shaft speed (negative values link turbine to compressor)
	ms_auto_opt_des_par.m_is_recomp_ok = auto_opt_des_hit_eta_in.m_is_recomp_ok;		//[-] 1 = yes, 0 = no, other = invalid

	ms_auto_opt_des_par.mf_callback_log = auto_opt_des_hit_eta_in.mf_callback_log;
	ms_auto_opt_des_par.mp_mf_active = auto_opt_des_hit_eta_in.mp_mf_active;

	ms_auto_opt_des_par.m_PR_mc_guess = auto_opt_des_hit_eta_in.m_PR_mc_guess;			//[-] Initial guess for ratio of P_mc_out to P_mc_in
	ms_auto_opt_des_par.m_fixed_PR_mc = auto_opt_des_hit_eta_in.m_fixed_PR_mc;			//[-] if true, ratio of P_mc_out to P_mc_in is fixed at PR_mc_guess		

	// At this point, 'auto_opt_des_hit_eta_in' should only be used to access the targer thermal efficiency: 'm_eta_thermal'

	double Q_dot_rec_des = ms_auto_opt_des_par.m_W_dot_net / auto_opt_des_hit_eta_in.m_eta_thermal;		//[kWt] Receiver thermal input at design

	error_msg = "";
	error_code = 0;

	// Check cycle parameter values are reasonable
	if( ms_auto_opt_des_par.m_is_recomp_ok != 0 && ms_auto_opt_des_par.m_is_recomp_ok != 1 )
	{
		throw(C_csp_exception("C_RecompCycle::auto_opt_design_core(...) requires that ms_auto_opt_des_par.m_is_recomp_ok"
			"is either 0 (simple cycle only) or 1 (recomp allowed)\n"));
	}
		// Can't operate compressore in 2-phase region
	if( ms_auto_opt_des_par.m_T_mc_in <= N_co2_props::T_crit )
	{
		error_msg.append( util::format("Only single phase cycle operation is allowed in this model." 
			"The compressor inlet temperature (%lg [C]) must be great than the critical temperature: %lg [C]",
			ms_auto_opt_des_par.m_T_mc_in - 273.15, ((N_co2_props::T_crit) - 273.15)));

		error_code = -1;
		return;
	}

		// "Reasonable" ceiling on compressor inlet temp
	double T_mc_in_max = 70.0 + 273.15;		//[K] Arbitrary value for max compressor inlet temperature
	if( ms_auto_opt_des_par.m_T_mc_in > T_mc_in_max )
	{
		error_msg.append( util::format("The compressor inlet temperature input was %lg [C]. This value was reset internally to the max allowable inlet temperature: %lg [C]\n",
			ms_auto_opt_des_par.m_T_mc_in - 273.15, T_mc_in_max - 273.15));

		ms_auto_opt_des_par.m_T_mc_in = T_mc_in_max;
	}

		// "Reasonable" floor on turbine inlet temp
	double T_t_in_min = 300.0 + 273.15;		//[K] Arbitrary value for min turbine inlet temperature
	if( ms_auto_opt_des_par.m_T_t_in < T_t_in_min )
	{
		error_msg.append( util::format("The turbine inlet temperature input was %lg [C]. This value was reset internally to the min allowable inlet temperature: %lg [C]\n",
			ms_auto_opt_des_par.m_T_t_in - 273.15, T_t_in_min - 273.15));

		ms_auto_opt_des_par.m_T_t_in = T_t_in_min;
	}

		// Turbine inlet temperature must be hotter than compressor outlet temperature
	if( ms_auto_opt_des_par.m_T_t_in <= ms_auto_opt_des_par.m_T_mc_in )
	{
		error_msg.append( util::format("The turbine inlet temperature, %lg [C], is colder than the specified compressor inlet temperature %lg [C]",
			ms_auto_opt_des_par.m_T_t_in - 273.15, ms_auto_opt_des_par.m_T_mc_in - 273.15));

		error_code = -1;
		return;
	}

		// Turbine inlet temperature must be colder than property limits
	if( ms_auto_opt_des_par.m_T_t_in >= N_co2_props::T_upper_limit )
	{
		error_msg.append( util::format("The turbine inlet temperature, %lg [C], is hotter than the maximum allow temperature in the CO2 property code %lg [C]",
			ms_auto_opt_des_par.m_T_t_in - 273.15, N_co2_props::T_upper_limit - 273.15));

		error_code = -1;
		return;
	}

		// Check for realistic isentropic efficiencies
	if( ms_auto_opt_des_par.m_eta_mc > 1.0 )
	{
		error_msg.append( util::format("The main compressor isentropic efficiency, %lg, was reset to theoretical maximum 1.0\n", 
			ms_auto_opt_des_par.m_eta_mc));

		ms_auto_opt_des_par.m_eta_mc = 1.0;
	}
	if( ms_auto_opt_des_par.m_eta_rc > 1.0 )
	{
		error_msg.append( util::format("The re-compressor isentropic efficiency, %lg, was reset to theoretical maximum 1.0\n",
			ms_auto_opt_des_par.m_eta_rc));

		ms_auto_opt_des_par.m_eta_rc = 1.0;
	}
	if( ms_auto_opt_des_par.m_eta_t > 1.0 )
	{
		error_msg.append( util::format("The turbine isentropic efficiency, %lg, was reset to theoretical maximum 1.0\n",
			ms_auto_opt_des_par.m_eta_t));

		ms_auto_opt_des_par.m_eta_t = 1.0;
	}
	if( ms_auto_opt_des_par.m_eta_mc < 0.1 )
	{
		error_msg.append( util::format("The main compressor isentropic efficiency, %lg, was increased to the internal limit of 0.1 to improve solution stability\n", 
			ms_auto_opt_des_par.m_eta_mc));

		ms_auto_opt_des_par.m_eta_mc = 0.1;
	}
	if( ms_auto_opt_des_par.m_eta_rc < 0.1 )
	{
		error_msg.append(util::format("The re-compressor isentropic efficiency, %lg, was increased to the internal limit of 0.1 to improve solution stability\n",
			ms_auto_opt_des_par.m_eta_rc));

		ms_auto_opt_des_par.m_eta_rc = 0.1;
	}
	if( ms_auto_opt_des_par.m_eta_t < 0.1 )
	{
		error_msg.append(util::format("The turbine isentropic efficiency, %lg, was increased to the internal limit of 0.1 to improve solution stability\n", 
			ms_auto_opt_des_par.m_eta_t));

		ms_auto_opt_des_par.m_eta_t = 0.1;
	}

	if( ms_auto_opt_des_par.m_LT_eff_max > 1.0 )
	{
		error_msg.append(util::format("The LT recuperator max effectiveness, %lg, was decreased to the limit of 1.0\n", ms_auto_opt_des_par.m_LT_eff_max));

		ms_auto_opt_des_par.m_LT_eff_max = 1.0;
	}

	if( ms_auto_opt_des_par.m_LT_eff_max < 0.70 )
	{
		error_msg.append(util::format("The LT recuperator max effectiveness, %lg, was increased to the internal limit of 0.70 improve convergence\n", ms_auto_opt_des_par.m_LT_eff_max));

		ms_auto_opt_des_par.m_LT_eff_max = 0.7;
	}

	if( ms_auto_opt_des_par.m_HT_eff_max > 1.0 )
	{
		error_msg.append(util::format("The HT recuperator max effectiveness, %lg, was decreased to the limit of 1.0\n", ms_auto_opt_des_par.m_HT_eff_max));

		ms_auto_opt_des_par.m_HT_eff_max = 1.0;
	}

	if( ms_auto_opt_des_par.m_HT_eff_max < 0.70 )
	{
		error_msg.append(util::format("The LT recuperator max effectiveness, %lg, was increased to the internal limit of 0.70 improve convergence\n", ms_auto_opt_des_par.m_HT_eff_max));

		ms_auto_opt_des_par.m_HT_eff_max = 0.7;
	}

		// Limits on high pressure limit
	if( ms_auto_opt_des_par.m_P_high_limit >= N_co2_props::P_upper_limit )
	{
		error_msg.append( util::format("The upper pressure limit, %lg [MPa], was set to the internal limit in the CO2 properties code %lg [MPa]\n",
			ms_auto_opt_des_par.m_P_high_limit, N_co2_props::P_upper_limit ));
	
		ms_auto_opt_des_par.m_P_high_limit = N_co2_props::P_upper_limit;
	}
	double P_high_limit_min = 10.0*1.E3;	//[kPa]
	if( ms_auto_opt_des_par.m_P_high_limit <= P_high_limit_min )
	{
		error_msg.append(util::format("The upper pressure limit, %lg [MPa], must be greater than %lg [MPa] to ensure solution stability",
			ms_auto_opt_des_par.m_P_high_limit, P_high_limit_min));

		error_code = -1;
		return;
	}

		// Finally, check thermal efficiency
	if( auto_opt_des_hit_eta_in.m_eta_thermal <= 0.0 )
	{
		error_msg.append(util::format("The design cycle thermal efficiency, %lg, must be at least greater than 0 ", 
			auto_opt_des_hit_eta_in.m_eta_thermal));

		error_code = -1;
		return;
	}
	double eta_carnot = 1.0 - ms_auto_opt_des_par.m_T_mc_in / ms_auto_opt_des_par.m_T_t_in;
	if( auto_opt_des_hit_eta_in.m_eta_thermal >= eta_carnot )
	{
		error_msg.append(util::format("To solve the cycle within the allowable recuperator conductance, the design cycle thermal efficiency, %lg, must be at least less than the Carnot efficiency: %lg ",
			auto_opt_des_hit_eta_in.m_eta_thermal, eta_carnot));

		error_code = -1;
		return;
	}

	// Send log update upstream
	if (ms_auto_opt_des_par.mf_callback_log && ms_auto_opt_des_par.mp_mf_active)
	{
		std::string msg_log = "Iterate on total recuperator conductance to hit target cycle efficiency";
		std::string msg_progress = "Designing cycle...";
		if (!ms_auto_opt_des_par.mf_callback_log(msg_log, msg_progress, ms_auto_opt_des_par.mp_mf_active))
		{
			std::string error_msg = "User terminated simulation...";
			std::string loc_msg = "C_MEQ_sco2_design_hit_eta__UA_total";
			throw(C_csp_exception(error_msg, loc_msg, 1));
		}
	}

	// Set up monotonic equation solver to find the total recuperator UA that results in the target efficiency
	C_MEQ_sco2_design_hit_eta__UA_total c_eq(this);
	C_monotonic_eq_solver c_solver(c_eq);

	// Generate min and max values
	double UA_recup_total_max = ms_des_limits.m_UA_net_power_ratio_max*ms_auto_opt_des_par.m_W_dot_net;		//[kW/K]
	double UA_recup_total_min = ms_des_limits.m_UA_net_power_ratio_min*ms_auto_opt_des_par.m_W_dot_net;		//[kW/K]
		// Set solver settings
	c_solver.settings(ms_auto_opt_des_par.m_tol, 50, UA_recup_total_min, UA_recup_total_max, true);

	// Generate guess values
	double UA_recups_guess = 0.1*ms_auto_opt_des_par.m_W_dot_net;

	double UA_recup_total_solved, tol_solved;
	UA_recup_total_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_solved = -1;

	int solver_code = 0;
	try
	{
		solver_code = c_solver.solve(UA_recups_guess, 1.1*UA_recups_guess, auto_opt_des_hit_eta_in.m_eta_thermal,
			UA_recup_total_solved, tol_solved, iter_solved);
	}
	catch (C_csp_exception &csp_except)
	{
		if (csp_except.m_error_code == 1)
		{
			throw(C_csp_exception(csp_except));
		}
		else
		{
			throw(C_csp_exception("C_MEQ_sco2_design_hit_eta__UA_total received an exception from the solver"));
		}
	}

	if (solver_code != C_monotonic_eq_solver::CONVERGED)
	{
		if (solver_code < C_monotonic_eq_solver::CONVERGED)
		{
			error_msg.append("Can't find a value of total recuperator conductance that achieves"
				" the target cycle efficiency. Check design parameters.");
		}
		else if (!c_solver.did_solver_find_negative_error(solver_code))
		{
			error_msg.append("Can't find a value of total recuperator conductance that results"
				" in an efficiency smaller than the target efficiency.");
		}
		else if (!c_solver.did_solver_find_positive_error(solver_code))
		{
			error_msg.append("Can't find a value of total recuperator conductance that results"
				" in an efficiency larger than the target efficiency.");
		}
		else
		{
			error_msg.append("Can't find a value of total recuperator conductance that achieves"
				" the target cycle efficiency. Check design parameters.");
		}

		error_code = -1;

		return;
	}

}

int C_RecompCycle::C_MEQ_sco2_design_hit_eta__UA_total::operator()(double UA_recup_total /*kW/K*/, double *eta /*-*/)
{
	mpc_rc_cycle->ms_auto_opt_des_par.m_UA_rec_total = UA_recup_total;	//[kW/K]

	int error_code = 0;
	mpc_rc_cycle->auto_opt_design_core(error_code);
	if (error_code != 0)
	{
		*eta = std::numeric_limits<double>::quiet_NaN();
		return -1;
	}

	*eta = mpc_rc_cycle->get_design_solved()->m_eta_thermal;	//[-]

	if (mpc_rc_cycle->ms_auto_opt_des_par.mf_callback_log && mpc_rc_cycle->ms_auto_opt_des_par.mp_mf_active)
	{
		msg_log = util::format(" Total recuperator conductance = %lg [kW/K]. Optimized cycle efficiency = %lg [-].  ",
			UA_recup_total, *eta);
		if (!mpc_rc_cycle->ms_auto_opt_des_par.mf_callback_log(msg_log, msg_progress, mpc_rc_cycle->ms_auto_opt_des_par.mp_mf_active))
		{
			std::string error_msg = "User terminated simulation...";
			std::string loc_msg = "C_MEQ_sco2_design_hit_eta__UA_total";
			throw(C_csp_exception(error_msg, loc_msg, 1));
		}
	}

	return 0;
}


double C_RecompCycle::opt_eta(double P_high_opt)
{
	double PR_mc_guess = 1.1;
	if(P_high_opt > P_pseudocritical_1(ms_opt_des_par.m_T_mc_in))
		PR_mc_guess = P_high_opt / P_pseudocritical_1(ms_opt_des_par.m_T_mc_in);
		
	double local_eta_rc = 0.0;
	if( ms_auto_opt_des_par.m_is_recomp_ok )
	{		 
		// Complete 'ms_opt_des_par' for recompression cycle
		ms_opt_des_par.m_P_mc_out_guess = P_high_opt;
		ms_opt_des_par.m_fixed_P_mc_out = true;
		
		//ms_opt_des_par.m_PR_mc_guess = PR_mc_guess;
		//ms_opt_des_par.m_fixed_PR_mc = false;
		ms_opt_des_par.m_fixed_PR_mc = ms_auto_opt_des_par.m_fixed_PR_mc;	//[-]
		if (ms_opt_des_par.m_fixed_PR_mc)
		{
			ms_opt_des_par.m_PR_mc_guess = ms_auto_opt_des_par.m_PR_mc_guess;	//[-]
		}
		else
		{
			ms_opt_des_par.m_PR_mc_guess = PR_mc_guess;		//[-]
		}

		ms_opt_des_par.m_recomp_frac_guess = 0.3;
		ms_opt_des_par.m_fixed_recomp_frac = false;
		ms_opt_des_par.m_LT_frac_guess = 0.5;
		ms_opt_des_par.m_fixed_LT_frac = false;

		int rc_error_code = 0;
		opt_design_core(rc_error_code);
	
		if( rc_error_code == 0 )
			local_eta_rc = m_eta_thermal_opt;
	
		if(rc_error_code == 0 && m_eta_thermal_opt > m_eta_thermal_auto_opt)
		{
			ms_des_par_auto_opt = ms_des_par_optimal;
			m_eta_thermal_auto_opt = m_eta_thermal_opt;
		}
	}

	// Complete 'ms_opt_des_par' for simple cycle
	ms_opt_des_par.m_P_mc_out_guess = P_high_opt;
	ms_opt_des_par.m_fixed_P_mc_out = true;
	
	//ms_opt_des_par.m_PR_mc_guess = PR_mc_guess;
	//ms_opt_des_par.m_fixed_PR_mc = false;
	ms_opt_des_par.m_fixed_PR_mc = ms_auto_opt_des_par.m_fixed_PR_mc;	//[-]
	if (ms_opt_des_par.m_fixed_PR_mc)
	{
		ms_opt_des_par.m_PR_mc_guess = ms_auto_opt_des_par.m_PR_mc_guess;	//[-]
	}
	else
	{
		ms_opt_des_par.m_PR_mc_guess = PR_mc_guess;		//[-]
	}

	ms_opt_des_par.m_recomp_frac_guess = 0.0;
	ms_opt_des_par.m_fixed_recomp_frac = true;
	ms_opt_des_par.m_LT_frac_guess = 1.0;
	ms_opt_des_par.m_fixed_LT_frac = true;

	int s_error_code = 0;
	opt_design_core(s_error_code);

	double local_eta_s = 0.0;
	if( s_error_code == 0 )
		local_eta_s = m_eta_thermal_opt;

	if(s_error_code == 0 && m_eta_thermal_opt > m_eta_thermal_auto_opt)
	{
		ms_des_par_auto_opt = ms_des_par_optimal;
		m_eta_thermal_auto_opt = m_eta_thermal_opt;
	}

	return -max(local_eta_rc, local_eta_s);

}

void C_RecompCycle::finalize_design(int & error_code)
{
	int cpp_offset = 1;

	// Size main compressor
	C_compressor::S_design_parameters  mc_des_par;
		// Compressor inlet conditions
	mc_des_par.m_P_in = m_pres_last[C_RecompCycle::MC_IN];
	mc_des_par.m_T_in = m_temp_last[C_RecompCycle::MC_IN];
	mc_des_par.m_D_in = m_dens_last[C_RecompCycle::MC_IN];
	mc_des_par.m_h_in = m_enth_last[C_RecompCycle::MC_IN];
	mc_des_par.m_s_in = m_entr_last[C_RecompCycle::MC_IN];
		// Compressor outlet conditions
	mc_des_par.m_T_out = m_temp_last[C_RecompCycle::MC_OUT];
	mc_des_par.m_P_out = m_pres_last[C_RecompCycle::MC_OUT];
	mc_des_par.m_h_out = m_enth_last[C_RecompCycle::MC_OUT];
	mc_des_par.m_D_out = m_dens_last[C_RecompCycle::MC_OUT];
		// Mass flow
	mc_des_par.m_m_dot = m_m_dot_mc;

	int comp_size_error_code = 0;
	m_mc.compressor_sizing(mc_des_par, comp_size_error_code);
	if(comp_size_error_code != 0)
	{
		error_code = comp_size_error_code;
		return;
	}

	if( ms_des_par.m_recomp_frac > 0.01 )
	{
		// Size recompressor
		C_recompressor::S_design_parameters  rc_des_par;
		// Compressor inlet conditions
		rc_des_par.m_P_in = m_pres_last[9 - cpp_offset];
		rc_des_par.m_D_in = m_dens_last[9 - cpp_offset];
		rc_des_par.m_h_in = m_enth_last[9 - cpp_offset];
		rc_des_par.m_s_in = m_entr_last[9 - cpp_offset];
		// Compressor outlet conditions
		rc_des_par.m_T_out = m_temp_last[10 - cpp_offset];
		rc_des_par.m_P_out = m_pres_last[10 - cpp_offset];
		rc_des_par.m_h_out = m_enth_last[10 - cpp_offset];
		rc_des_par.m_D_out = m_dens_last[10 - cpp_offset];
		// Mass flow
		rc_des_par.m_m_dot = m_m_dot_rc;

		int recomp_size_error_code = 0;
		m_rc.recompressor_sizing(rc_des_par, recomp_size_error_code);
		if( recomp_size_error_code != 0 )
		{
			error_code = recomp_size_error_code;
			return;
		}

		ms_des_solved.m_is_rc = true;
	}
	else
		ms_des_solved.m_is_rc = false;
	
	// Size turbine
	C_turbine::S_design_parameters  t_des_par;
		// Set turbine shaft speed
	t_des_par.m_N_design = ms_des_par.m_N_turbine;
	t_des_par.m_N_comp_design_if_linked = m_mc.get_design_solved()->m_N_design;
		// Turbine inlet state
	t_des_par.m_P_in = m_pres_last[6-cpp_offset];
	t_des_par.m_T_in = m_temp_last[6-cpp_offset];
	t_des_par.m_D_in = m_dens_last[6-cpp_offset];
	t_des_par.m_h_in = m_enth_last[6-cpp_offset];
	t_des_par.m_s_in = m_entr_last[6-cpp_offset];
		// Turbine outlet state
	t_des_par.m_P_out = m_pres_last[7-cpp_offset];
	t_des_par.m_h_out = m_enth_last[7-cpp_offset];
		// Mass flow
	t_des_par.m_m_dot = m_m_dot_t;

	int turb_size_error_code = 0;
	m_t.turbine_sizing(t_des_par, turb_size_error_code);
	if(turb_size_error_code != 0)
	{
		error_code = turb_size_error_code;
		return;
	}

	// Get 'design_solved' structures from component classes
	ms_des_solved.ms_mc_des_solved = *m_mc.get_design_solved();
	ms_des_solved.ms_rc_des_solved = *m_rc.get_design_solved();
	ms_des_solved.ms_t_des_solved = *m_t.get_design_solved();
	ms_des_solved.ms_LT_recup_des_solved = mc_LT_recup.ms_des_solved;
	ms_des_solved.ms_HT_recup_des_solved = mc_HT_recup.ms_des_solved;

	// Set solved design point metrics
	ms_des_solved.m_temp = m_temp_last;
	ms_des_solved.m_pres = m_pres_last;
	ms_des_solved.m_enth = m_enth_last;
	ms_des_solved.m_entr = m_entr_last;
	ms_des_solved.m_dens = m_dens_last;

	ms_des_solved.m_eta_thermal = m_eta_thermal_last;
	ms_des_solved.m_W_dot_net = m_W_dot_net_last;
	ms_des_solved.m_m_dot_mc = m_m_dot_mc;
	ms_des_solved.m_m_dot_rc = m_m_dot_rc;
	ms_des_solved.m_m_dot_t = m_m_dot_t;
	ms_des_solved.m_recomp_frac = m_m_dot_rc / m_m_dot_t;

	ms_des_solved.m_UA_LT = ms_des_par.m_UA_LT;
	ms_des_solved.m_UA_HT = ms_des_par.m_UA_HT;

}

void C_RecompCycle::off_design(S_od_parameters & od_par_in, int & error_code)
{
	ms_od_par = od_par_in;

	int od_error_code = 0;

	off_design_core(od_error_code);

	error_code = od_error_code;
}

void C_RecompCycle::off_design_phi(S_od_phi_par & od_phi_par_in, int & error_code)
{
	ms_od_phi_par = od_phi_par_in;

	int od_error_code = 0;

	off_design_phi_core(od_error_code);

	error_code = od_error_code;
}

void C_RecompCycle::off_design_fix_shaft_speeds(S_od_phi_par & od_phi_par_in, int & error_code)
{
	ms_od_phi_par = od_phi_par_in;

	int od_error_code = 0;

	off_design_fix_shaft_speeds_core(od_error_code);

	error_code = od_error_code;
}

double C_RecompCycle::get_od_temp(int n_state_point)
{
	return m_temp_od[n_state_point];
}

void C_RecompCycle::set_od_temp(int n_state_point, double temp_K)
{
	m_temp_od[n_state_point] = temp_K;
}

double C_RecompCycle::get_od_pres(int n_state_point)
{
	return m_pres_od[n_state_point];
}

void C_RecompCycle::set_od_pres(int n_state_point, double pres_kPa)
{
	m_pres_od[n_state_point] = pres_kPa;
}

void C_RecompCycle::off_design_recompressor(double T_in, double P_in, double m_dot, double P_out, int & error_code, double & T_out)
{
	m_rc.off_design_recompressor(T_in, P_in, m_dot, P_out, error_code, T_out);
}

int C_RecompCycle::C_mono_eq_turbo_m_dot::operator()(double m_dot_t_in /*kg/s*/, double *diff_m_dot_t /*-*/)
{
	// Calculate main compressor mass flow rate
	double m_dot_mc = (1.0 - m_f_recomp)*m_dot_t_in;		//[kg/s]

	// Calculate main compressor performance
	int mc_err_code = 0;
	double T_mc_out, P_mc_out;
	T_mc_out = P_mc_out = std::numeric_limits<double>::quiet_NaN();

	mpc_rc_cycle->m_mc.od_comp_phi(m_phi_mc, m_T_mc_in, m_P_mc_in, m_dot_mc, mc_err_code, T_mc_out, P_mc_out);
	
		// Check that main compressor performance solved
	if(mc_err_code != 0)
	{
		*diff_m_dot_t = std::numeric_limits<double>::quiet_NaN();
		return mc_err_code;
	}

	mpc_rc_cycle->m_pres_od[C_RecompCycle::MC_OUT] = P_mc_out;	//[kPa]
	mpc_rc_cycle->m_temp_od[C_RecompCycle::MC_OUT] = T_mc_out;	//[K]

		// Calculate main compressor power
	int prop_err_code = CO2_TP(T_mc_out, P_mc_out, &mc_co2_props);

	// Calculate scaled pressure drops through heat exchanger
	std::vector<double> DP_LT, DP_HT, DP_PHX, DP_PC;
	//std::vector<double> m_dot_LT;
	//m_dot_LT.push_back(m_dot_mc);
	//m_dot_LT.push_back(m_dot_t_in);
	//mpc_rc_cycle->m_LT.hxr_pressure_drops(m_dot_LT, DP_LT);
	DP_LT.resize(2);
	DP_LT[0] = mpc_rc_cycle->mc_LT_recup.od_delta_p_cold(m_dot_mc);
	DP_LT[1] = mpc_rc_cycle->mc_LT_recup.od_delta_p_hot(m_dot_t_in);

	//std::vector<double> m_dot_HT;
	//m_dot_HT.push_back(m_dot_t_in);
	//m_dot_HT.push_back(m_dot_t_in);
	//mpc_rc_cycle->m_HT.hxr_pressure_drops(m_dot_HT, DP_HT);
	DP_HT.resize(2);
	DP_HT[0] = mpc_rc_cycle->mc_HT_recup.od_delta_p_cold(m_dot_t_in);
	DP_HT[1] = mpc_rc_cycle->mc_HT_recup.od_delta_p_hot(m_dot_t_in);

	std::vector<double> m_dot_PHX;
	m_dot_PHX.push_back(m_dot_t_in);
	m_dot_PHX.push_back(0.0);
	mpc_rc_cycle->m_PHX.hxr_pressure_drops(m_dot_PHX, DP_PHX);

	std::vector<double> m_dot_PC;
	m_dot_PC.push_back(0.0);
	m_dot_PC.push_back(m_dot_mc);
	mpc_rc_cycle->m_PC.hxr_pressure_drops(m_dot_PC, DP_PC);

	// Apply pressure drops to heat exchangers, fully defining the pressure at all states
	int cpp_offset = 1;
	mpc_rc_cycle->m_pres_od[C_RecompCycle::LTR_HP_OUT] = mpc_rc_cycle->m_pres_od[C_RecompCycle::MC_OUT] - DP_LT[1 - cpp_offset];		// LT recuperator (cold stream)
	mpc_rc_cycle->m_pres_od[C_RecompCycle::MIXER_OUT] = mpc_rc_cycle->m_pres_od[C_RecompCycle::LTR_HP_OUT];								// Assume no pressure drop in mixing valve
	mpc_rc_cycle->m_pres_od[C_RecompCycle::RC_OUT] = mpc_rc_cycle->m_pres_od[C_RecompCycle::LTR_HP_OUT];								// Assume no pressure drop in mixing valve
	mpc_rc_cycle->m_pres_od[C_RecompCycle::HTR_HP_OUT] = mpc_rc_cycle->m_pres_od[C_RecompCycle::MIXER_OUT] - DP_HT[1 - cpp_offset];		// HT recuperator (cold stream)
	double P_t_in = mpc_rc_cycle->m_pres_od[C_RecompCycle::TURB_IN] = mpc_rc_cycle->m_pres_od[C_RecompCycle::LTR_HP_OUT] - DP_PHX[1 - cpp_offset];		// PHX
	mpc_rc_cycle->m_pres_od[C_RecompCycle::LTR_LP_OUT] = mpc_rc_cycle->m_pres_od[C_RecompCycle::MC_IN] + DP_PC[2 - cpp_offset];		// precooler
	mpc_rc_cycle->m_pres_od[C_RecompCycle::HTR_LP_OUT] = mpc_rc_cycle->m_pres_od[C_RecompCycle::LTR_LP_OUT] + DP_LT[2 - cpp_offset];		// LT recuperator (hot stream)
	double P_t_out = mpc_rc_cycle->m_pres_od[C_RecompCycle::TURB_OUT] = mpc_rc_cycle->m_pres_od[C_RecompCycle::HTR_LP_OUT] + DP_HT[2 - cpp_offset];		// HT recuperator (hot stream)

	// Calculate turbine performance
	int t_err_code = 0;
	double m_dot_t_calc, T_t_out;
	m_dot_t_calc = T_t_out = std::numeric_limits<double>::quiet_NaN();
	mpc_rc_cycle->m_t.od_turbine_at_N_des(m_T_t_in, P_t_in, P_t_out, t_err_code, m_dot_t_calc, T_t_out);
		// Check that turbine performance solved
	if(t_err_code != 0)
	{
		*diff_m_dot_t = std::numeric_limits<double>::quiet_NaN();
		return t_err_code;
	}

	mpc_rc_cycle->m_temp_od[C_RecompCycle::TURB_OUT] = T_t_out;	//[K]

	// Calculate difference between calculated and guessed mass flow rate
	*diff_m_dot_t = (m_dot_t_calc - m_dot_t_in) / m_dot_t_in;

	if( m_is_update_ms_od_solved )
	{
		// Get 'od_solved' structures from component classes
		mpc_rc_cycle->ms_od_solved.ms_mc_od_solved = *mpc_rc_cycle->m_mc.get_od_solved();
		mpc_rc_cycle->ms_od_solved.ms_t_od_solved = *mpc_rc_cycle->m_t.get_od_solved();

		// Set ms_od_solved
		mpc_rc_cycle->ms_od_solved.m_m_dot_mc = m_dot_mc;
		mpc_rc_cycle->ms_od_solved.m_m_dot_rc = m_dot_t_calc - m_dot_mc;
		mpc_rc_cycle->ms_od_solved.m_m_dot_t = m_dot_t_calc;
		mpc_rc_cycle->ms_od_solved.m_recomp_frac = m_f_recomp;

		mpc_rc_cycle->ms_od_solved.m_temp = mpc_rc_cycle->m_temp_od;
		mpc_rc_cycle->ms_od_solved.m_pres = mpc_rc_cycle->m_pres_od;
		mpc_rc_cycle->ms_od_solved.m_enth = mpc_rc_cycle->m_enth_od;
		mpc_rc_cycle->ms_od_solved.m_entr = mpc_rc_cycle->m_entr_od;
		mpc_rc_cycle->ms_od_solved.m_dens = mpc_rc_cycle->m_dens_od;
	}

	return 0;
}

void C_RecompCycle::optimize_od_turbo_balance_csp(S_od_turbo_bal_csp_par in_params, std::vector<double> &opt_params)
{
	ms_od_turbo_bal_csp_par = in_params;
	
	int n_vars = 1;
	if(ms_des_solved.m_is_rc)
		n_vars = 2;

	// Set up 2 variable optimization in NLOPT to screen for initial guess for full optimization
	std::vector<double> x_bal(n_vars);
	std::vector<double> lb_bal(n_vars);
	std::vector<double> ub_bal(n_vars);
	std::vector<double> scale_bal(n_vars);

	// Inlet pressure (all in [kPa])
	x_bal[0] = ms_des_solved.m_pres[C_RecompCycle::MC_IN];
	lb_bal[0] = 1000.0;
	ub_bal[0] = (ms_des_solved.m_pres[C_RecompCycle::MC_OUT]);
	scale_bal[0] = 1000.0;

	// Recompression fraction
	if( n_vars == 2 )
	{
		x_bal[1] = ms_des_solved.m_recomp_frac;
		lb_bal[1] = 0.0;
		ub_bal[1] = 1.0;
		scale_bal[1] = -x_bal[1] / 4.0;
	}

	// Reset optimization tracking structure
	reset_ms_od_turbo_bal_csp_solved();

	// Set up instance of nlopt class and set optimization parameters
	nlopt::opt          opt_turb_bal(nlopt::LN_SBPLX, n_vars);
	opt_turb_bal.set_lower_bounds(lb_bal);
	opt_turb_bal.set_upper_bounds(ub_bal);
	opt_turb_bal.set_initial_step(scale_bal);
	opt_turb_bal.set_xtol_rel(ms_des_par.m_tol);

	// Set max objective function
	opt_turb_bal.set_max_objective(nlopt_callback_tub_bal_opt, this);
	double max_W_net = std::numeric_limits<double>::quiet_NaN();
	nlopt::result      W_net_opt_result = opt_turb_bal.optimize(x_bal, max_W_net);

	if( ms_od_turbo_bal_csp_solved.m_W_dot_net_adj < ms_od_turbo_bal_csp_solved.m_W_dot_net )
	{
		double wat_to_do_here = 1.23;
	}

	opt_params = x_bal;
}

void C_RecompCycle::reset_ms_od_turbo_bal_csp_solved()
{
	S_od_turbo_bal_csp_solved s_temp;
	ms_od_turbo_bal_csp_solved = s_temp;
}

void C_RecompCycle::od_turbo_bal_csp(const S_od_turbo_bal_csp_par & par_in)
{
	// Split between member structure
	ms_od_turbo_bal_csp_par.m_T_mc_in = par_in.m_T_mc_in;	//[K]
	ms_od_turbo_bal_csp_par.m_T_t_in = par_in.m_T_t_in;		//[K]

	// And vector
	std::vector<double> x(2);
	x[0] = par_in.m_P_mc_in;		//[kPa]
	x[1] = par_in.m_f_recomp;		//[-]

	od_turbo_bal_csp_Wnet(x);
}


double C_RecompCycle::od_turbo_bal_csp_Wnet(const std::vector<double> &x)
{
	// Get cycle operation arguments from vector input	
	ms_od_turbo_bal_csp_par.m_P_mc_in = x[0];		//[kPa]
	if( ms_des_solved.m_is_rc )
		ms_od_turbo_bal_csp_par.m_f_recomp = x[1];		//[-]
	else
		ms_od_turbo_bal_csp_par.m_f_recomp = 0.0;		//[-]

	// Apply 1 var solver to find the turbine mass flow rate that balances the turbomachinery,
	//     given the Inlet Pressure and Recompression Fraction arguments
	C_mono_eq_turbo_m_dot c_rc_cycle(this, ms_od_turbo_bal_csp_par.m_T_mc_in,
											ms_od_turbo_bal_csp_par.m_P_mc_in,
											ms_od_turbo_bal_csp_par.m_f_recomp,
											ms_od_turbo_bal_csp_par.m_T_t_in,
											ms_od_turbo_bal_csp_par.m_phi_mc);

	C_monotonic_eq_solver c_rc_cycle_solver(c_rc_cycle);

	// Set lower bound on mass flow rate
	double m_dot_lower = ms_des_solved.m_m_dot_t*1.E-3;	//[kg/s]
	double m_dot_upper = std::numeric_limits<double>::quiet_NaN();

	// Set solver settings
	// Because this application of the solver is trying to get the calculated mass flow rate to match the guess,
	//       then need to error in function
	// So it's already relative, and solver is looking at an absolute value
	c_rc_cycle_solver.settings(1.E-3, 100, m_dot_lower, m_dot_upper, false);

	// Generate two guess values
	double m_dot_guess_upper = ms_des_solved.m_m_dot_t;	//[kg/s]
	double m_dot_guess_lower = 0.7*m_dot_guess_upper;	//[kg/s]

	// Now, solve for the turbine mass flow rate
	double m_dot_t_solved, tol_solved;
	m_dot_t_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_solved = -1;

	int m_dot_t_code = c_rc_cycle_solver.solve(m_dot_guess_lower, m_dot_guess_upper, 0.0, 
							m_dot_t_solved, tol_solved, iter_solved);

	if( m_dot_t_code != C_monotonic_eq_solver::CONVERGED )
	{
		return 0.0;
	}

	// Calculate main compressor power and turbine power
	double W_dot_mc = m_mc.get_od_solved()->m_W_dot_in;	//[kW]
	double W_dot_t = m_t.get_od_solved()->m_W_dot_out;	//[kW]

	// Other useful outputs
	double P_out = m_mc.get_od_solved()->m_P_out;	//[kPa]
	
	// Solve recompressor
	double W_dot_rc = 0.0;

	if( ms_des_solved.m_is_rc )
	{
			// Known inputs
		//double P_in = m_mc.get_od_solved()->m_P_in;		//[kPa]
			// Have to guess the inlet temperature
		double T_in = ms_des_solved.m_temp[C_RecompCycle::LTR_LP_OUT];
			// Call performance model
		double m_dot_rc = ms_od_turbo_bal_csp_par.m_f_recomp*m_dot_t_solved;
		double T_out = std::numeric_limits<double>::quiet_NaN();
		int rc_err_code = 0;
		m_rc.off_design_recompressor(T_in, ms_od_turbo_bal_csp_par.m_P_mc_in, m_dot_rc, P_out, rc_err_code, T_out);
			// Check recompressor model errors
		if( rc_err_code != 0 )
		{
			return 0.0;
		}
		// Calculate recompressor power
		W_dot_rc = m_rc.get_od_solved()->m_W_dot_in;	//[kW]
	}

	// Finally, calculate net power
	double W_dot_net = W_dot_t - W_dot_mc - W_dot_rc;

	// Now, need to filter results that exceed temperature/pressure/other limitations
	// 1) Don't let the turbine inlet temperature exceed the design inlet temperature
	//     But we aren't running the entire cycle model here, so instead limit C_dot_co2 >= C_dot_MS
	double co2_to_htf_m_dot_ratio = m_dot_t_solved / ms_od_turbo_bal_csp_par.m_m_dot_htf;	//[-]
	double over_T_t_in = max(0.0, 10.0*(1.1*ms_od_turbo_bal_csp_par.m_co2_to_htf_m_dot_ratio_des - co2_to_htf_m_dot_ratio));
	
	// 2) Don't let the upper pressure in the cycle exceed the specified max (typically also = design point)
	double over_P_high = max(0.0, (P_out - ms_des_par.m_P_high_limit));

	// 3) Check the compressors' tip ratios
	double mc_w_tip_ratio = m_mc.get_od_solved()->m_w_tip_ratio;
		// How does recompressor tip speed ratio work with multiple stages?
	double rc_w_tip_ratio = 0.0;
	if( ms_des_solved.m_is_rc )
		rc_w_tip_ratio = m_rc.get_od_solved()->m_w_tip_ratio;
	double comp_tip_ratio = max(mc_w_tip_ratio, rc_w_tip_ratio);
	double over_tip_ratio = max(0.0, 10.0*(comp_tip_ratio - 1.0));

	// 4) Check recompessor surge
	double rc_phi_s1, rc_phi_s2, rc_phi_min, over_surge;
	rc_phi_s1 = rc_phi_s2 = rc_phi_min = over_surge = 0.0;
	if( ms_des_solved.m_is_rc )
	{
		rc_phi_s1 = m_rc.get_od_solved()->m_phi;
		rc_phi_s2 = m_rc.get_od_solved()->m_phi_2;
		rc_phi_min = min(rc_phi_s1, rc_phi_s2);
		over_surge = max(0.0, C_recompressor::m_snl_phi_min - rc_phi_min);
	}

	double W_dot_net_adjusted = W_dot_net*exp(-over_T_t_in)*exp(-over_P_high)*exp(-over_tip_ratio)*exp(over_surge);

	if(W_dot_net == W_dot_net_adjusted)
	{
		ms_od_turbo_bal_csp_solved.m_is_feasible = true;

		// *********************************************************************
		ms_od_turbo_bal_csp_solved.ms_par = ms_od_turbo_bal_csp_par;

		// Set outputs through member structure
		ms_od_turbo_bal_csp_solved.m_W_dot_net = W_dot_net;					//[kWe]
		ms_od_turbo_bal_csp_solved.m_W_dot_net_adj = W_dot_net_adjusted;	//[kWe]
		ms_od_turbo_bal_csp_solved.m_P_high = P_out;						//[kPa]
		ms_od_turbo_bal_csp_solved.m_m_dot_total = m_dot_t_solved;			//[kg/s]
	
		ms_od_turbo_bal_csp_solved.m_N_mc = m_mc.get_od_solved()->m_N;		//[rpm]
		ms_od_turbo_bal_csp_solved.m_w_tip_ratio_mc = mc_w_tip_ratio;		//[-]
		ms_od_turbo_bal_csp_solved.m_eta_mc = m_mc.get_od_solved()->m_eta;	//[-]

		if( ms_des_solved.m_is_rc )
		{
			ms_od_turbo_bal_csp_solved.m_N_rc = m_rc.get_od_solved()->m_N;		//[rpm]
			ms_od_turbo_bal_csp_solved.m_phi_rc_1 = rc_phi_s1;					//[-]
			ms_od_turbo_bal_csp_solved.m_phi_rc_2 = rc_phi_s2;					//[-]
			ms_od_turbo_bal_csp_solved.m_w_tip_ratio_rc = rc_w_tip_ratio;		//[-]
			ms_od_turbo_bal_csp_solved.m_eta_rc = m_rc.get_od_solved()->m_eta;	//[-]
		}

		ms_od_turbo_bal_csp_solved.m_eta_t = m_t.get_od_solved()->m_eta;	//[-]
		// *********************************************************************
		// *********************************************************************
	}

	return W_dot_net_adjusted;
}

double nlopt_callback_tub_bal_opt(const std::vector<double> &x, std::vector<double> &grad, void *data)
{
	C_RecompCycle *frame = static_cast<C_RecompCycle*>(data);
	if( frame != NULL ) 
		return frame->od_turbo_bal_csp_Wnet(x);
	else
		return std::numeric_limits<double>::quiet_NaN();
}

int C_RecompCycle::C_mono_eq_LTR_od::operator()(double T_LTR_LP_out_guess /*K*/, double *diff_T_LTR_LP_out /*K*/)
{
	m_Q_dot_LTR = std::numeric_limits<double>::quiet_NaN(); 

	mpc_rc_cycle->m_temp_od[LTR_LP_OUT] = T_LTR_LP_out_guess;		//[K]

	int prop_error_code = CO2_TP(mpc_rc_cycle->m_temp_od[LTR_LP_OUT], mpc_rc_cycle->m_pres_od[LTR_LP_OUT], &mpc_rc_cycle->mc_co2_props);
	if( prop_error_code != 0 )
	{
		*diff_T_LTR_LP_out = std::numeric_limits<double>::quiet_NaN();
		return prop_error_code;
	}
	mpc_rc_cycle->m_enth_od[LTR_LP_OUT] = mpc_rc_cycle->mc_co2_props.enth;	//[kJ/kg]
	mpc_rc_cycle->m_entr_od[LTR_LP_OUT] = mpc_rc_cycle->mc_co2_props.entr;	//[kJ/kg-K]
	mpc_rc_cycle->m_dens_od[LTR_LP_OUT] = mpc_rc_cycle->mc_co2_props.dens;	//[kg/m^3]

	if( mpc_rc_cycle->ms_od_phi_par.m_recomp_frac >= 1.E-12 )		// Determine the required shaft speed for the recompressor
	{
		int rc_error_code = 0;
		mpc_rc_cycle->m_rc.off_design_recompressor(mpc_rc_cycle->m_temp_od[LTR_LP_OUT], mpc_rc_cycle->m_pres_od[LTR_LP_OUT], m_m_dot_rc,
							mpc_rc_cycle->m_pres_od[RC_OUT], rc_error_code, mpc_rc_cycle->m_temp_od[RC_OUT]);

		if( rc_error_code != 0 )
		{
			*diff_T_LTR_LP_out = std::numeric_limits<double>::quiet_NaN();
			return rc_error_code;
		}

		// Fully define state 10
		prop_error_code = CO2_TP(mpc_rc_cycle->m_temp_od[RC_OUT], mpc_rc_cycle->m_pres_od[RC_OUT], &mpc_rc_cycle->mc_co2_props);
		if( prop_error_code != 0 )
		{
			*diff_T_LTR_LP_out = std::numeric_limits<double>::quiet_NaN();
			return prop_error_code;
		}

		mpc_rc_cycle->m_enth_od[RC_OUT] = mpc_rc_cycle->mc_co2_props.enth;	//[kJ/kg]
		mpc_rc_cycle->m_entr_od[RC_OUT] = mpc_rc_cycle->mc_co2_props.entr;	//[kJ/kg-K]
		mpc_rc_cycle->m_dens_od[RC_OUT] = mpc_rc_cycle->mc_co2_props.dens;	//[kg/m^3]
	}
	else
	{	// No meaningful recompression
		mpc_rc_cycle->m_temp_od[RC_OUT] = mpc_rc_cycle->m_temp_od[LTR_LP_OUT];	//[K]
		mpc_rc_cycle->m_enth_od[RC_OUT]	= mpc_rc_cycle->m_enth_od[LTR_LP_OUT];	//[K]
		mpc_rc_cycle->m_entr_od[RC_OUT]	= mpc_rc_cycle->m_entr_od[LTR_LP_OUT];	//[K]
		mpc_rc_cycle->m_dens_od[RC_OUT]	= mpc_rc_cycle->m_dens_od[LTR_LP_OUT];	//[K]
	}

	double T_LTR_LP_out_calc = std::numeric_limits<double>::quiet_NaN();

	try
	{
	mpc_rc_cycle->mc_LT_recup.off_design_solution(mpc_rc_cycle->m_temp_od[MC_OUT], mpc_rc_cycle->m_pres_od[MC_OUT], m_m_dot_mc, mpc_rc_cycle->m_pres_od[LTR_HP_OUT],
					mpc_rc_cycle->m_temp_od[HTR_LP_OUT], mpc_rc_cycle->m_pres_od[HTR_LP_OUT], m_m_dot_t, mpc_rc_cycle->m_pres_od[LTR_LP_OUT],
					m_Q_dot_LTR, mpc_rc_cycle->m_temp_od[LTR_HP_OUT], T_LTR_LP_out_calc);     
	}
	catch( C_csp_exception & csp_except )
	{
		*diff_T_LTR_LP_out = std::numeric_limits<double>::quiet_NaN();

		return -1;
	}

	*diff_T_LTR_LP_out = T_LTR_LP_out_calc - mpc_rc_cycle->m_temp_od[LTR_LP_OUT];		//[K]

	return 0;
}

int C_RecompCycle::C_mono_eq_HTR_od::operator()(double T_HTR_LP_out_guess /*K*/, double *diff_T_HTR_LP_out /*K*/)
{
	m_Q_dot_LTR = m_Q_dot_LTR = std::numeric_limits<double>::quiet_NaN();

	mpc_rc_cycle->m_temp_od[HTR_LP_OUT] = T_HTR_LP_out_guess;	//[K]

	int prop_error_code = CO2_TP(mpc_rc_cycle->m_temp_od[HTR_LP_OUT],mpc_rc_cycle->m_pres_od[HTR_LP_OUT], &mpc_rc_cycle->mc_co2_props);
	if( prop_error_code != 0 )
	{
		*diff_T_HTR_LP_out = std::numeric_limits<double>::quiet_NaN();
		return prop_error_code;
	}
	mpc_rc_cycle->m_enth_od[HTR_LP_OUT] = mpc_rc_cycle->mc_co2_props.enth;	//[kJ/kg]
	mpc_rc_cycle->m_entr_od[HTR_LP_OUT] = mpc_rc_cycle->mc_co2_props.entr;	//[kJ/kg-K]
	mpc_rc_cycle->m_dens_od[HTR_LP_OUT] = mpc_rc_cycle->mc_co2_props.dens;	//[kg/m^3]

	// *******************************************************************
	// LTR Solver
	double T_LTR_LP_out_lower = mpc_rc_cycle->m_temp_od[MC_OUT];		//[K] Coldest possible LP outlet temperature
	double T_LTR_LP_out_upper = max(1.02*T_LTR_LP_out_lower, mpc_rc_cycle->m_temp_od[HTR_LP_OUT]);	//[K] Hottest possible LP outlet temperature

	double T_LTR_LP_out_guess_upper = min(T_LTR_LP_out_upper, T_LTR_LP_out_lower + 15.0);				//[K] There is nothing special about using 15 here
	double T_LTR_LP_out_guess_lower = min(T_LTR_LP_out_guess_upper*0.99, T_LTR_LP_out_lower + 2.0);		//[K] There is nothing special about using 2 here

	C_mono_eq_LTR_od LTR_od_eq(mpc_rc_cycle, m_m_dot_rc, m_m_dot_mc, m_m_dot_t);
	C_monotonic_eq_solver LTR_od_solver(LTR_od_eq);

	LTR_od_solver.settings(mpc_rc_cycle->ms_des_par.m_tol*mpc_rc_cycle->ms_des_solved.m_temp[MC_IN], 1000, T_LTR_LP_out_lower, T_LTR_LP_out_upper, false);

	double T_LTR_LP_out_solved, tol_T_LTR_LP_out_solved;
	T_LTR_LP_out_solved = tol_T_LTR_LP_out_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_T_LTR_LP_out = -1;

	int T_LTR_LP_out_code = LTR_od_solver.solve(T_LTR_LP_out_guess_lower, T_LTR_LP_out_guess_upper, 0,
								T_LTR_LP_out_solved, tol_T_LTR_LP_out_solved, iter_T_LTR_LP_out);

	if(T_LTR_LP_out_code != C_monotonic_eq_solver::CONVERGED)
	{
		*diff_T_HTR_LP_out = std::numeric_limits<double>::quiet_NaN();
		int n_call_history = LTR_od_solver.get_solver_call_history()->size();
		int error_code = 0;
		if( n_call_history > 0 )
			error_code = (*(LTR_od_solver.get_solver_call_history()))[n_call_history - 1].err_code;
		if(error_code == 0)
		{
			error_code = T_LTR_LP_out_code;
		}
		return error_code;
	}

	m_Q_dot_LTR = LTR_od_eq.m_Q_dot_LTR;	//[kWt]

	// Now, calculate State 3
	mpc_rc_cycle->m_enth_od[LTR_HP_OUT] = mpc_rc_cycle->m_enth_od[MC_OUT] + m_Q_dot_LTR / m_m_dot_mc;		//[kJ/kg] Energy balance on HP stream of LTR
	prop_error_code = CO2_PH(mpc_rc_cycle->m_pres_od[LTR_HP_OUT], mpc_rc_cycle->m_enth_od[LTR_HP_OUT], &mpc_rc_cycle->mc_co2_props);
	if( prop_error_code != 0 )
	{
		*diff_T_HTR_LP_out = std::numeric_limits<double>::quiet_NaN();
		return prop_error_code;
	}
	mpc_rc_cycle->m_temp_od[LTR_HP_OUT] = mpc_rc_cycle->mc_co2_props.temp;	//[K]
	mpc_rc_cycle->m_entr_od[LTR_HP_OUT] = mpc_rc_cycle->mc_co2_props.entr;	//[kJ/kg-K]
	mpc_rc_cycle->m_dens_od[LTR_HP_OUT] = mpc_rc_cycle->mc_co2_props.dens;	//[kg/m^3]

	// Go through mixing valve
	if( mpc_rc_cycle->ms_od_phi_par.m_recomp_frac >= 1.E-12 )
	{
		// Conservation of energy
		mpc_rc_cycle->m_enth_od[MIXER_OUT] = (1.0 - mpc_rc_cycle->ms_od_phi_par.m_recomp_frac)*mpc_rc_cycle->m_enth_od[LTR_HP_OUT] +
												mpc_rc_cycle->ms_od_phi_par.m_recomp_frac*mpc_rc_cycle->m_enth_od[RC_OUT];
		prop_error_code = CO2_PH(mpc_rc_cycle->m_pres_od[MIXER_OUT], mpc_rc_cycle->m_enth_od[MIXER_OUT], &mpc_rc_cycle->mc_co2_props);
		if( prop_error_code != 0 )
		{
			*diff_T_HTR_LP_out = std::numeric_limits<double>::quiet_NaN();
			return prop_error_code;
		}
		mpc_rc_cycle->m_temp_od[MIXER_OUT] = mpc_rc_cycle->mc_co2_props.temp;	//[K]
		mpc_rc_cycle->m_entr_od[MIXER_OUT] = mpc_rc_cycle->mc_co2_props.entr;	//[kJ/kg-K]
		mpc_rc_cycle->m_dens_od[MIXER_OUT] = mpc_rc_cycle->mc_co2_props.dens;	//[kg/m^3]
	}
	else
	{
		mpc_rc_cycle->m_enth_od[MIXER_OUT] = mpc_rc_cycle->m_enth_od[LTR_HP_OUT];	//[kJ/kg]
		mpc_rc_cycle->m_temp_od[MIXER_OUT] = mpc_rc_cycle->m_temp_od[LTR_HP_OUT];	//[K]
		mpc_rc_cycle->m_entr_od[MIXER_OUT] = mpc_rc_cycle->m_entr_od[LTR_HP_OUT];	//[kJ/kg-K]
		mpc_rc_cycle->m_dens_od[MIXER_OUT] = mpc_rc_cycle->m_dens_od[LTR_HP_OUT];	//[kg/m^3]
	}

	// Know hot and cold inlets, so can solve HTR
	double T_HTR_LP_out_calc = std::numeric_limits<double>::quiet_NaN();
	try
	{
	mpc_rc_cycle->mc_HT_recup.off_design_solution(mpc_rc_cycle->m_temp_od[MIXER_OUT], mpc_rc_cycle->m_pres_od[MIXER_OUT], m_m_dot_t, mpc_rc_cycle->m_pres_od[HTR_HP_OUT],
							mpc_rc_cycle->m_temp_od[TURB_OUT], mpc_rc_cycle->m_pres_od[TURB_OUT], m_m_dot_t, mpc_rc_cycle->m_pres_od[HTR_LP_OUT],
							m_Q_dot_HTR, mpc_rc_cycle->m_temp_od[HTR_HP_OUT], T_HTR_LP_out_calc);
	}
	catch( C_csp_exception & csp_except)
	{
		*diff_T_HTR_LP_out = std::numeric_limits<double>::quiet_NaN();
		if(csp_except.m_error_code == 0)
			return -1;
		else
			return csp_except.m_error_code;
	}

	*diff_T_HTR_LP_out = T_HTR_LP_out_calc - mpc_rc_cycle->m_temp_od[HTR_LP_OUT];	//[K]

	return 0;
}

void C_RecompCycle::estimate_od_turbo_operation(double T_mc_in /*K*/, double P_mc_in /*kPa*/, double f_recomp /*-*/, double T_t_in /*K*/, double phi_mc /*-*/,
	int & mc_error_code, double & mc_w_tip_ratio /*-*/, double & P_mc_out /*kPa*/,
	int & rc_error_code, double & rc_w_tip_ratio /*-*/, double & rc_phi /*-*/,
	bool is_update_ms_od_solved)
{
	// Initialize a few variables
	m_temp_od[C_RecompCycle::MC_IN] = T_mc_in;	//[K]
	m_pres_od[C_RecompCycle::MC_IN] = P_mc_in;	//[kPa]
	m_temp_od[C_RecompCycle::TURB_IN] = T_t_in;	//[K]

	C_RecompCycle::C_mono_eq_turbo_m_dot c_turbo_bal(this, T_mc_in, P_mc_in, 
													f_recomp, T_t_in, phi_mc, is_update_ms_od_solved);

	C_monotonic_eq_solver c_turbo_m_dot_solver(c_turbo_bal);

	// Set lower bound on mass flow rate
	double m_dot_lower = ms_des_solved.m_m_dot_t*1.E-3;					//[kg/s]
	double m_dot_upper = std::numeric_limits<double>::quiet_NaN();

	// Set solver settings
	c_turbo_m_dot_solver.settings(1.E-2, 100, m_dot_lower, m_dot_upper, false);

	// Generate two guess values
	double m_dot_guess_upper = ms_des_solved.m_m_dot_t;
	double m_dot_guess_lower = 0.7*m_dot_guess_upper;

	// Solve for the turbine mass flow rate that balances the main compressor and turbine
	double m_dot_t_solved, tol_solved;
	m_dot_t_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_solved = -1;

	int m_dot_t_code = c_turbo_m_dot_solver.solve(m_dot_guess_lower, m_dot_guess_upper, 0.0,
										m_dot_t_solved, tol_solved, iter_solved);

	if( m_dot_t_code != C_monotonic_eq_solver::CONVERGED )
	{
		mc_error_code = m_dot_t_code;
		mc_w_tip_ratio = 0.0;
		P_mc_out = 0.0;
		rc_error_code = 0;
		rc_w_tip_ratio = 0.0;
		return;	
	}
	else
	{
		mc_error_code = 0;
		double m_dot_rc = f_recomp*m_dot_t_solved;
		P_mc_out = m_pres_od[C_RecompCycle::MC_OUT];			//[kPa]
		mc_w_tip_ratio = m_mc.get_od_solved()->m_w_tip_ratio;	//[-]

		if( ms_des_solved.m_is_rc && m_dot_rc > 0.0 )
		{
			rc_error_code = 0;
			double T_rc_out = std::numeric_limits<double>::quiet_NaN();
			off_design_recompressor(get_design_solved()->m_temp[C_RecompCycle::HTR_LP_OUT],
									P_mc_in,
									m_dot_rc,
									P_mc_out,
									rc_error_code,
									T_rc_out);		
		}

		if( rc_error_code == 0 )
		{
			rc_w_tip_ratio = m_rc.get_od_solved()->m_w_tip_ratio;
			
			double rc_phi_s1 = m_rc.get_od_solved()->m_phi;
			double rc_phi_s2 = m_rc.get_od_solved()->m_phi_2;
			rc_phi = min(rc_phi_s1, rc_phi_s2);
		}
		else
		{
			rc_w_tip_ratio = rc_phi = 0.0;
		}
	}

}

void C_RecompCycle::clear_ms_od_solved()
{
	S_od_solved s_od_solved_temp;
	ms_od_solved = s_od_solved_temp;
}

int C_RecompCycle::C_mono_eq_turbo_N_fixed_m_dot::operator()(double m_dot_t_in /*kg/s*/, double *diff_m_dot_t /*-*/)
{
	// Calculate the main compressor mass flow rate
	double m_dot_mc = (1.0 - m_f_recomp)*m_dot_t_in;		//[kg/s]

	// Calculate main compressor performance
	int mc_error_code = 0;
	double T_mc_out, P_mc_out;
	T_mc_out = P_mc_out = std::numeric_limits<double>::quiet_NaN();

	// Solve main compressor performance at design shaft speed
	mpc_rc_cycle->m_mc.od_comp_at_N_des(m_T_mc_in, m_P_mc_in, m_dot_mc, mc_error_code,
									T_mc_out, P_mc_out);

	// Check that main compressor performance solved
	if(mc_error_code != 0)
	{
		*diff_m_dot_t = std::numeric_limits<double>::quiet_NaN();
		return mc_error_code;
	}

	mpc_rc_cycle->m_pres_od[C_RecompCycle::MC_OUT] = P_mc_out;	//[kPa]
	mpc_rc_cycle->m_temp_od[C_RecompCycle::MC_OUT] = T_mc_out;	//[K]

	// Calculate main compressor power
	int prop_err_code = CO2_TP(T_mc_out, P_mc_out, &mc_co2_props);

	// Calculate scaled pressure drops through heat exchangers
		// LTR
	std::vector<double> DP_LT;
	DP_LT.resize(2);
	DP_LT[0] = mpc_rc_cycle->mc_LT_recup.od_delta_p_cold(m_dot_mc);
	DP_LT[1] = mpc_rc_cycle->mc_LT_recup.od_delta_p_hot(m_dot_t_in);
		// HTR
	std::vector<double> DP_HT;
	DP_HT.resize(2);
	DP_HT[0] = mpc_rc_cycle->mc_HT_recup.od_delta_p_cold(m_dot_t_in);
	DP_HT[1] = mpc_rc_cycle->mc_HT_recup.od_delta_p_hot(m_dot_t_in);
		// PHX
	std::vector<double> DP_PHX;
	std::vector<double> m_dot_PHX;
	m_dot_PHX.push_back(m_dot_t_in);
	m_dot_PHX.push_back(0.0);
	mpc_rc_cycle->m_PHX.hxr_pressure_drops(m_dot_PHX, DP_PHX);
		// Pre-Cooler
	std::vector<double> DP_PC;
	std::vector<double> m_dot_PC;
	m_dot_PC.push_back(0.0);
	m_dot_PC.push_back(m_dot_mc);
	mpc_rc_cycle->m_PC.hxr_pressure_drops(m_dot_PC, DP_PC);

	// Apply pressure drops to heat exchangers, fully defining the pressure at all states
	int cpp_offset = 1;
	mpc_rc_cycle->m_pres_od[C_RecompCycle::LTR_HP_OUT] = mpc_rc_cycle->m_pres_od[C_RecompCycle::MC_OUT] - DP_LT[1 - cpp_offset];		// LT recuperator (cold stream)
	mpc_rc_cycle->m_pres_od[C_RecompCycle::MIXER_OUT] = mpc_rc_cycle->m_pres_od[C_RecompCycle::LTR_HP_OUT];								// Assume no pressure drop in mixing valve
	mpc_rc_cycle->m_pres_od[C_RecompCycle::RC_OUT] = mpc_rc_cycle->m_pres_od[C_RecompCycle::LTR_HP_OUT];								// Assume no pressure drop in mixing valve
	mpc_rc_cycle->m_pres_od[C_RecompCycle::HTR_HP_OUT] = mpc_rc_cycle->m_pres_od[C_RecompCycle::MIXER_OUT] - DP_HT[1 - cpp_offset];		// HT recuperator (cold stream)
	double P_t_in = mpc_rc_cycle->m_pres_od[C_RecompCycle::TURB_IN] = mpc_rc_cycle->m_pres_od[C_RecompCycle::LTR_HP_OUT] - DP_PHX[1 - cpp_offset];		// PHX
	mpc_rc_cycle->m_pres_od[C_RecompCycle::LTR_LP_OUT] = mpc_rc_cycle->m_pres_od[C_RecompCycle::MC_IN] + DP_PC[2 - cpp_offset];		// precooler
	mpc_rc_cycle->m_pres_od[C_RecompCycle::HTR_LP_OUT] = mpc_rc_cycle->m_pres_od[C_RecompCycle::LTR_LP_OUT] + DP_LT[2 - cpp_offset];		// LT recuperator (hot stream)
	double P_t_out = mpc_rc_cycle->m_pres_od[C_RecompCycle::TURB_OUT] = mpc_rc_cycle->m_pres_od[C_RecompCycle::HTR_LP_OUT] + DP_HT[2 - cpp_offset];		// HT recuperator (hot stream)

	// Calculate turbine performance
	int t_err_code = 0;
	double m_dot_t_calc, T_t_out;
	m_dot_t_calc = T_t_out = std::numeric_limits<double>::quiet_NaN();
	mpc_rc_cycle->m_t.od_turbine_at_N_des(m_T_t_in, P_t_in, P_t_out, t_err_code, m_dot_t_calc, T_t_out);

	// Check that turbine performance solved
	if(t_err_code != 0)
	{
		*diff_m_dot_t = std::numeric_limits<double>::quiet_NaN();
		return t_err_code;
	}

	mpc_rc_cycle->m_temp_od[C_RecompCycle::TURB_OUT] = T_t_out;	//[K]

	// Calculate difference between calculated and guessed mass flow rate
	*diff_m_dot_t = (m_dot_t_calc - m_dot_t_in) / m_dot_t_in;

	if( m_is_update_ms_od_solved )
	{
		// Get 'od_solved' structures from component classes
		mpc_rc_cycle->ms_od_solved.ms_mc_od_solved = *mpc_rc_cycle->m_mc.get_od_solved();
		mpc_rc_cycle->ms_od_solved.ms_t_od_solved = *mpc_rc_cycle->m_t.get_od_solved();

		// Set ms_od_solved
		mpc_rc_cycle->ms_od_solved.m_m_dot_mc = m_dot_mc;
		mpc_rc_cycle->ms_od_solved.m_m_dot_rc = m_dot_t_calc - m_dot_mc;
		mpc_rc_cycle->ms_od_solved.m_m_dot_t = m_dot_t_calc;
		mpc_rc_cycle->ms_od_solved.m_recomp_frac = m_f_recomp;

		mpc_rc_cycle->ms_od_solved.m_temp = mpc_rc_cycle->m_temp_od;
		mpc_rc_cycle->ms_od_solved.m_pres = mpc_rc_cycle->m_pres_od;
		mpc_rc_cycle->ms_od_solved.m_enth = mpc_rc_cycle->m_enth_od;
		mpc_rc_cycle->ms_od_solved.m_entr = mpc_rc_cycle->m_entr_od;
		mpc_rc_cycle->ms_od_solved.m_dens = mpc_rc_cycle->m_dens_od;
	}

	return 0;
}

int C_RecompCycle::C_mono_eq_x_f_recomp_y_N_rc::operator()(double f_recomp /*-*/, double *diff_N_rc /*-*/)
{
	// Set f_recomp in S_od_par here
	mpc_rc_cycle->ms_od_phi_par.m_recomp_frac = f_recomp;

	C_mono_eq_turbo_N_fixed_m_dot c_turbo_bal(mpc_rc_cycle, m_T_mc_in,
															m_P_mc_in,
															f_recomp,
															m_T_t_in);

	C_monotonic_eq_solver c_turbo_bal_solver(c_turbo_bal);

	// Set lower bound on mass flow rate
	double m_dot_lower = mpc_rc_cycle->ms_des_solved.m_m_dot_t*1.E-3;		//[kg/s]
	double m_dot_upper = std::numeric_limits<double>::quiet_NaN();

	// Set solver settings
	// Because this application of the solver is trying to get the calculated mass flow rate to match the guess
	//  ... then need to calculate the error in the operator() method
	// So the error is already relative, and the solver is comparing against absolute value of 0
	c_turbo_bal_solver.settings(1.E-3, 100, m_dot_lower, m_dot_upper, false);

	// Generate two guess values
	double m_dot_guess_upper = mpc_rc_cycle->ms_des_solved.m_m_dot_t;		//[kg/s]
	double m_dot_guess_lower = 0.7*m_dot_guess_upper;		//[kg/s]

	// Now, solve for the turbine mass flow rate
	double m_dot_t_solved, tol_solved;
	m_dot_t_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_solved = -1;

	int m_dot_t_code = c_turbo_bal_solver.solve(m_dot_guess_lower, m_dot_guess_upper, 0.0,
		m_dot_t_solved, tol_solved, iter_solved);

	if( m_dot_t_code != C_monotonic_eq_solver::CONVERGED )
	{
		return m_dot_t_code;
	}

	m_m_dot_t = m_dot_t_solved;	//[kg/s]
	m_m_dot_rc = m_m_dot_t*f_recomp;	//[kg/s]
	m_m_dot_mc = m_m_dot_t - m_m_dot_rc;

	// Fully define known states
	int prop_error_code = CO2_TP(mpc_rc_cycle->m_temp_od[MC_IN], mpc_rc_cycle->m_pres_od[MC_IN], &mc_co2_props);
	if( prop_error_code != 0 )
	{
		return prop_error_code;
	}
	mpc_rc_cycle->m_enth_od[MC_IN] = mc_co2_props.enth;
	mpc_rc_cycle->m_entr_od[MC_IN] = mc_co2_props.entr;
	mpc_rc_cycle->m_dens_od[MC_IN] = mc_co2_props.dens;

	prop_error_code = CO2_TP(mpc_rc_cycle->m_temp_od[MC_OUT], mpc_rc_cycle->m_pres_od[MC_OUT], &mc_co2_props);
	if( prop_error_code != 0 )
	{
		return prop_error_code;
	}
	mpc_rc_cycle->m_enth_od[MC_OUT] = mc_co2_props.enth;
	mpc_rc_cycle->m_entr_od[MC_OUT] = mc_co2_props.entr;
	mpc_rc_cycle->m_dens_od[MC_OUT] = mc_co2_props.dens;

	prop_error_code = CO2_TP(mpc_rc_cycle->m_temp_od[TURB_IN], mpc_rc_cycle->m_pres_od[TURB_IN], &mc_co2_props);
	if( prop_error_code != 0 )
	{
		return prop_error_code;
	}
	mpc_rc_cycle->m_enth_od[TURB_IN] = mc_co2_props.enth;
	mpc_rc_cycle->m_entr_od[TURB_IN] = mc_co2_props.entr;
	mpc_rc_cycle->m_dens_od[TURB_IN] = mc_co2_props.dens;

	prop_error_code = CO2_TP(mpc_rc_cycle->m_temp_od[TURB_OUT], mpc_rc_cycle->m_pres_od[TURB_OUT], &mc_co2_props);
	if( prop_error_code != 0 )
	{
		return prop_error_code;
	}
	mpc_rc_cycle->m_enth_od[TURB_OUT] = mc_co2_props.enth;
	mpc_rc_cycle->m_entr_od[TURB_OUT] = mc_co2_props.entr;
	mpc_rc_cycle->m_dens_od[TURB_OUT] = mc_co2_props.dens;

	// Solve recuperators here...
	double T_HTR_LP_out_lower = mpc_rc_cycle->m_temp_od[MC_OUT];		//[K] Coldest possible temperature
	double T_HTR_LP_out_upper = mpc_rc_cycle->m_temp_od[TURB_OUT];		//[K] Hottest possible temperature

	double T_HTR_LP_out_guess_lower = min(T_HTR_LP_out_upper - 2.0, max(T_HTR_LP_out_lower + 15.0, 220.0 + 273.15));	//[K] There is nothing special about these guesses (except 220 is a typical RC outlet temp)...
	double T_HTR_LP_out_guess_upper = min(T_HTR_LP_out_guess_lower + 20.0, T_HTR_LP_out_upper - 1.0);	//[K] There is nothing special about these guesses, either...

	C_mono_eq_HTR_od HTR_od_eq(mpc_rc_cycle, m_m_dot_rc, m_m_dot_mc, m_m_dot_t);
	C_monotonic_eq_solver HTR_od_solver(HTR_od_eq);

	HTR_od_solver.settings(mpc_rc_cycle->ms_des_par.m_tol*mpc_rc_cycle->m_temp_od[MC_IN], 1000, T_HTR_LP_out_lower, T_HTR_LP_out_upper, false);

	double T_HTR_LP_out_solved, tol_T_HTR_LP_out_solved;
	T_HTR_LP_out_solved = tol_T_HTR_LP_out_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_T_HTR_LP_out = -1;

	int T_HTR_LP_out_code = HTR_od_solver.solve(T_HTR_LP_out_guess_lower, T_HTR_LP_out_guess_upper, 0,
		T_HTR_LP_out_solved, tol_T_HTR_LP_out_solved, iter_T_HTR_LP_out);

	if( T_HTR_LP_out_code != C_monotonic_eq_solver::CONVERGED )
	{
		int n_call_history = HTR_od_solver.get_solver_call_history()->size();
		int error_code = 0;
		if( n_call_history > 0 )
			error_code = (*(HTR_od_solver.get_solver_call_history()))[n_call_history - 1].err_code;
		if( error_code == 0 )
		{
			error_code = T_HTR_LP_out_code;
		}
		return error_code;
	}

	double Q_dot_HTR = HTR_od_eq.m_Q_dot_HTR;		//[kWt]

	// State 5 can now be fully defined
	mpc_rc_cycle->m_enth_od[HTR_HP_OUT] = mpc_rc_cycle->m_enth_od[MIXER_OUT] + Q_dot_HTR / m_m_dot_t;		//[kJ/kg] Energy balance on cold stream of high-temp recuperator
	prop_error_code = CO2_PH(mpc_rc_cycle->m_pres_od[HTR_HP_OUT], mpc_rc_cycle->m_enth_od[HTR_HP_OUT], &mc_co2_props);
	if( prop_error_code != 0 )
	{
		return prop_error_code;
	}
	mpc_rc_cycle->m_temp_od[HTR_HP_OUT] = mc_co2_props.temp;
	mpc_rc_cycle->m_entr_od[HTR_HP_OUT] = mc_co2_props.entr;
	mpc_rc_cycle->m_dens_od[HTR_HP_OUT] = mc_co2_props.dens;

	// Get recompressor shaft speed
	double N_rc = mpc_rc_cycle->m_rc.get_od_solved()->m_N;
	double N_rc_des = mpc_rc_cycle->m_rc.get_design_solved()->m_N_design;

	// Get difference between solved and design shaft speed
	*diff_N_rc = (N_rc - N_rc_des) / N_rc_des;

	return 0;
}

void C_RecompCycle::off_design_fix_shaft_speeds_core(int & error_code)
{
	// Need to reset 'ms_od_solved' here
	clear_ms_od_solved();
	// Check if recompression fraction is > 0 and whether cycle is simple or recompression
	if( !ms_des_solved.m_is_rc )
	{
		ms_od_phi_par.m_recomp_frac = 0.0;
	}

	CO2_state co2_props;

	// Initialize a few variables
	m_temp_od[C_RecompCycle::MC_IN] = ms_od_phi_par.m_T_mc_in;
	m_pres_od[C_RecompCycle::MC_IN] = ms_od_phi_par.m_P_mc_in;
	m_temp_od[C_RecompCycle::TURB_IN] = ms_od_phi_par.m_T_t_in;

	// Outer loop: Solve for the recompression fraction that results in the recompressor
	//                operating at its design shaft speed
	C_mono_eq_x_f_recomp_y_N_rc c_turbo_bal_f_recomp(this, ms_od_phi_par.m_T_mc_in,
															ms_od_phi_par.m_P_mc_in,
															ms_od_phi_par.m_T_t_in);

	C_monotonic_eq_solver c_turbo_bal_f_recomp_solver(c_turbo_bal_f_recomp);

	if(ms_des_solved.m_is_rc)
	{
		// Define bounds on the recompression fraction
		double f_recomp_lower = 0.0;
		double f_recomp_upper = 1.0;
		
		c_turbo_bal_f_recomp_solver.settings(1.E-3, 50, f_recomp_lower, f_recomp_upper, false);
		
		C_monotonic_eq_solver::S_xy_pair f_recomp_pair_1st;
		C_monotonic_eq_solver::S_xy_pair f_recomp_pair_2nd;

		double f_recomp_guess = ms_des_solved.m_recomp_frac;
		double y_f_recomp_guess = std::numeric_limits<double>::quiet_NaN();
		// Send a guess recompression fraction to method; see if it returns a calculated N_rc or fails
		int turb_bal_err_code = c_turbo_bal_f_recomp_solver.call_mono_eq(f_recomp_guess, &y_f_recomp_guess);

		// If guessed recompression fraction fails, then try to find a recompression fraction that works
		if( turb_bal_err_code != 0 )
		{			
			double delta = 0.02;
			bool is_iter = true;
			for(int i = 1; is_iter; i++)
			{
				for(int j = -1; j <= 1; j += 2)
				{
					f_recomp_guess = min(1.0, max(0.0, ms_des_solved.m_recomp_frac + j*i*delta));
					turb_bal_err_code = c_turbo_bal_f_recomp_solver.call_mono_eq(f_recomp_guess, &y_f_recomp_guess);
					if(turb_bal_err_code == 0)
					{
						is_iter = false;
						break;
					}
					if( f_recomp_guess == 0.0 )
					{
						// Have tried a fairly fine grid of recompression fraction values; have not found one that works
						error_code = -40;
						return;
					}	
				}
			}
		}

		f_recomp_pair_1st.x = f_recomp_guess;
		f_recomp_pair_1st.y = y_f_recomp_guess;

		f_recomp_guess = 1.02*f_recomp_pair_1st.x;
		turb_bal_err_code = c_turbo_bal_f_recomp_solver.call_mono_eq(f_recomp_guess, &y_f_recomp_guess);

		if(turb_bal_err_code == 0)
		{
			f_recomp_pair_2nd.x = f_recomp_guess;
			f_recomp_pair_2nd.y = y_f_recomp_guess;
		}
		else
		{
			f_recomp_guess = 0.98*f_recomp_pair_1st.x;
			turb_bal_err_code = c_turbo_bal_f_recomp_solver.call_mono_eq(f_recomp_guess, &y_f_recomp_guess);

			if(turb_bal_err_code == 0)
			{
				f_recomp_pair_2nd.x = f_recomp_guess;
				f_recomp_pair_2nd.y = y_f_recomp_guess;
			}
			else
			{
				// Found one recompression fraction that works, but can't find another nearby value that also works
				error_code = -41;
				return;
			}
		}
		
		// Now, using the two solved guess values, solve for the recompression fraction that results in:
		// ... balanced turbomachinery at their design shaft speed
		double f_recomp_solved, tol_solved;
		f_recomp_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
		int iter_solved = -1;

		int f_recomp_code = 0;
		try
		{
			f_recomp_code = c_turbo_bal_f_recomp_solver.solve(f_recomp_pair_1st, f_recomp_pair_2nd, 0.0, 
																f_recomp_solved, tol_solved, iter_solved);
		}
		catch( C_csp_exception )
		{
			error_code = -42;
			return;
		}

		if( f_recomp_code != C_monotonic_eq_solver::CONVERGED )
		{
			int n_call_history = c_turbo_bal_f_recomp_solver.get_solver_call_history()->size();

			if( n_call_history > 0 )
				error_code = -(*(c_turbo_bal_f_recomp_solver.get_solver_call_history()))[n_call_history - 1].err_code;

			if( error_code == 0 )
			{
				error_code = f_recomp_code;
			}

			return;
		}
	}
	else
	{
		double y_eq = std::numeric_limits<double>::quiet_NaN();
		int turb_bal_err_code = c_turbo_bal_f_recomp_solver.call_mono_eq(0.0, &y_eq);
		if(turb_bal_err_code != 0)
		{
			throw(C_csp_exception("C_RecompCycle::off_design_fix_shaft_speeds_core does not yet have ability to solve for cycles with recompression"));
		}
	}

	// Get mass flow rates from solver
	double m_dot_mc = c_turbo_bal_f_recomp.m_m_dot_mc;		//[kg/s]
	double m_dot_rc = c_turbo_bal_f_recomp.m_m_dot_rc;		//[kg/s]
	double m_dot_t = c_turbo_bal_f_recomp.m_m_dot_t;		//[kg/s]

	// Calculate cycle performance metrics
	double w_mc = m_enth_od[MC_IN] - m_enth_od[MC_OUT];		//[kJ/kg] (negative) specific work of compressor
	double w_t = m_enth_od[TURB_IN] - m_enth_od[TURB_OUT];	//[kJ/kg] (positive) specific work of turbine

	double w_rc = 0.0;
	if( ms_od_phi_par.m_recomp_frac > 0.0 )
		w_rc = m_enth_od[LTR_LP_OUT] - m_enth_od[RC_OUT];	//[kJ/kg] (negative) specific work of recompressor


	m_Q_dot_PHX_od = m_dot_t*(m_enth_od[TURB_IN] - m_enth_od[HTR_HP_OUT]);
	m_W_dot_net_od = w_mc*m_dot_mc + w_rc*m_dot_rc + w_t*m_dot_t;
	m_eta_thermal_od = m_W_dot_net_od / m_Q_dot_PHX_od;

	// Get 'od_solved' structures from component classes
	ms_od_solved.ms_mc_od_solved = *m_mc.get_od_solved();
	ms_od_solved.ms_rc_od_solved = *m_rc.get_od_solved();
	ms_od_solved.ms_t_od_solved = *m_t.get_od_solved();
	ms_od_solved.ms_LT_recup_od_solved = mc_LT_recup.ms_od_solved;
	ms_od_solved.ms_HT_recup_od_solved = mc_HT_recup.ms_od_solved;

	// Set ms_od_solved
	ms_od_solved.m_eta_thermal = m_eta_thermal_od;
	ms_od_solved.m_W_dot_net = m_W_dot_net_od;
	ms_od_solved.m_Q_dot = m_Q_dot_PHX_od;
	ms_od_solved.m_m_dot_mc = m_dot_mc;
	ms_od_solved.m_m_dot_rc = m_dot_rc;
	ms_od_solved.m_m_dot_t = m_dot_t;
	ms_od_solved.m_recomp_frac = ms_od_phi_par.m_recomp_frac;

	ms_od_solved.m_temp = m_temp_od;
	ms_od_solved.m_pres = m_pres_od;
	ms_od_solved.m_enth = m_enth_od;
	ms_od_solved.m_entr = m_entr_od;
	ms_od_solved.m_dens = m_dens_od;

	return;
}

void C_RecompCycle::off_design_phi_core(int & error_code)
{
	// Need to reset 'ms_od_solved' here
	clear_ms_od_solved();
	// Check if recompression fraction is > 0 and whether cycle is simple or recompression
	if( !ms_des_solved.m_is_rc )
	{
		ms_od_phi_par.m_recomp_frac = 0.0;
	}

	CO2_state co2_props;

	// Initialize a few variables
	m_temp_od[C_RecompCycle::MC_IN] = ms_od_phi_par.m_T_mc_in;
	m_pres_od[C_RecompCycle::MC_IN] = ms_od_phi_par.m_P_mc_in;
	m_temp_od[C_RecompCycle::TURB_IN] = ms_od_phi_par.m_T_t_in;

	C_mono_eq_turbo_m_dot c_turbo_bal(this, ms_od_phi_par.m_T_mc_in,
											ms_od_phi_par.m_P_mc_in,
											ms_od_phi_par.m_recomp_frac,
											ms_od_phi_par.m_T_t_in,
											ms_od_phi_par.m_phi_mc);

	C_monotonic_eq_solver c_turbo_bal_solver(c_turbo_bal);

	// Set lower bound on mass flow rate
	double m_dot_lower = ms_des_solved.m_m_dot_t*1.E-3;					//[kg/s]
	double m_dot_upper = std::numeric_limits<double>::quiet_NaN();

	// Set solver settings
	// Because this application of the solver is trying to get the calculated mass flow rate to match the guess,
	//    then need to calculate error in function
	// So it's already relatiave, and solver is looking at an absolute value
	c_turbo_bal_solver.settings(1.E-3, 100, m_dot_lower, m_dot_upper, false);

	// Generate two guess values
	double m_dot_guess_upper = ms_des_solved.m_m_dot_t;		//[kg/s]
	double m_dot_guess_lower = 0.7*m_dot_guess_upper;		//[kg/s]

	// Now, solve for the turbine mass flow rate
	double m_dot_t_solved, tol_solved;
	m_dot_t_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_solved = -1;

	int m_dot_t_code = c_turbo_bal_solver.solve(m_dot_guess_lower, m_dot_guess_upper, 0.0,
								m_dot_t_solved, tol_solved, iter_solved);

	if( m_dot_t_code != C_monotonic_eq_solver::CONVERGED )
	{
		error_code = m_dot_t_code;
		return;
	}

	double m_dot_t = m_dot_t_solved;	//[kg/s]
	double m_dot_rc = m_dot_t*ms_od_phi_par.m_recomp_frac;	//[kg/s]
	double m_dot_mc = m_dot_t - m_dot_rc;

	// Fully define known states
	int prop_error_code = CO2_TP(m_temp_od[MC_IN], m_pres_od[MC_IN], &co2_props);
	if( prop_error_code != 0 )
	{
		error_code = prop_error_code;
		return;
	}
	m_enth_od[MC_IN] = co2_props.enth;
	m_entr_od[MC_IN] = co2_props.entr;
	m_dens_od[MC_IN] = co2_props.dens;

	prop_error_code = CO2_TP(m_temp_od[MC_OUT], m_pres_od[MC_OUT], &co2_props);
	if( prop_error_code != 0 )
	{
		error_code = prop_error_code;
		return;
	}
	m_enth_od[MC_OUT] = co2_props.enth;
	m_entr_od[MC_OUT] = co2_props.entr;
	m_dens_od[MC_OUT] = co2_props.dens;

	prop_error_code = CO2_TP(m_temp_od[TURB_IN], m_pres_od[TURB_IN], &co2_props);
	if( prop_error_code != 0 )
	{
		error_code = prop_error_code;
		return;
	}
	m_enth_od[TURB_IN] = co2_props.enth;
	m_entr_od[TURB_IN] = co2_props.entr;
	m_dens_od[TURB_IN] = co2_props.dens;

	prop_error_code = CO2_TP(m_temp_od[TURB_OUT], m_pres_od[TURB_OUT], &co2_props);
	if( prop_error_code != 0 )
	{
		error_code = prop_error_code;
		return;
	}
	m_enth_od[TURB_OUT] = co2_props.enth;
	m_entr_od[TURB_OUT] = co2_props.entr;
	m_dens_od[TURB_OUT] = co2_props.dens;

	// Solve recuperators here...
	double T_HTR_LP_out_lower = m_temp_od[MC_OUT];			//[K] Coldest possible temperature
	double T_HTR_LP_out_upper = m_temp_od[TURB_OUT];		//[K] Hottest possible temperature

	double T_HTR_LP_out_guess_lower = min(T_HTR_LP_out_upper - 2.0, max(T_HTR_LP_out_lower + 15.0, 220.0 + 273.15));	//[K] There is nothing special about these guesses (except 220 is a typical RC outlet temp)...
	double T_HTR_LP_out_guess_upper = min(T_HTR_LP_out_guess_lower + 20.0, T_HTR_LP_out_upper - 1.0);	//[K] There is nothing special about these guesses, either...

	C_mono_eq_HTR_od HTR_od_eq(this, m_dot_rc, m_dot_mc, m_dot_t);
	C_monotonic_eq_solver HTR_od_solver(HTR_od_eq);

	HTR_od_solver.settings(ms_des_par.m_tol*m_temp_od[MC_IN], 1000, T_HTR_LP_out_lower, T_HTR_LP_out_upper, false);

	double T_HTR_LP_out_solved, tol_T_HTR_LP_out_solved;
	T_HTR_LP_out_solved = tol_T_HTR_LP_out_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_T_HTR_LP_out = -1;

	int T_HTR_LP_out_code = HTR_od_solver.solve(T_HTR_LP_out_guess_lower, T_HTR_LP_out_guess_upper, 0,
								T_HTR_LP_out_solved, tol_T_HTR_LP_out_solved, iter_T_HTR_LP_out);

	if( T_HTR_LP_out_code != C_monotonic_eq_solver::CONVERGED )
	{
		int n_call_history = HTR_od_solver.get_solver_call_history()->size();
		if( n_call_history > 0 )
			error_code = (*(HTR_od_solver.get_solver_call_history()))[n_call_history - 1].err_code;
		if(error_code == 0)
		{
			error_code = T_HTR_LP_out_code;		
		}
		return;
	}

	double Q_dot_HTR = HTR_od_eq.m_Q_dot_HTR;		//[kWt]

	// State 5 can now be fully defined
	m_enth_od[HTR_HP_OUT] = m_enth_od[MIXER_OUT] + Q_dot_HTR / m_dot_t;		//[kJ/kg] Energy balance on cold stream of high-temp recuperator
	prop_error_code = CO2_PH(m_pres_od[HTR_HP_OUT], m_enth_od[HTR_HP_OUT], &co2_props);
	if( prop_error_code != 0 )
	{
		error_code = prop_error_code;
		return;
	}
	m_temp_od[HTR_HP_OUT] = co2_props.temp;
	m_entr_od[HTR_HP_OUT] = co2_props.entr;
	m_dens_od[HTR_HP_OUT] = co2_props.dens;

	// Calculate cycle performance metrics
	double w_mc = m_enth_od[MC_IN] - m_enth_od[MC_OUT];		//[kJ/kg] (negative) specific work of compressor
	double w_t = m_enth_od[TURB_IN] - m_enth_od[TURB_OUT];	//[kJ/kg] (positive) specific work of turbine

	double w_rc = 0.0;
	if( ms_od_phi_par.m_recomp_frac > 0.0 )
		w_rc = m_enth_od[LTR_LP_OUT] - m_enth_od[RC_OUT];	//[kJ/kg] (negative) specific work of recompressor


	m_Q_dot_PHX_od = m_dot_t*(m_enth_od[TURB_IN] - m_enth_od[HTR_HP_OUT]);
	m_W_dot_net_od = w_mc*m_dot_mc + w_rc*m_dot_rc + w_t*m_dot_t;
	m_eta_thermal_od = m_W_dot_net_od / m_Q_dot_PHX_od;

	// Get 'od_solved' structures from component classes
	ms_od_solved.ms_mc_od_solved = *m_mc.get_od_solved();
	ms_od_solved.ms_rc_od_solved = *m_rc.get_od_solved();
	ms_od_solved.ms_t_od_solved = *m_t.get_od_solved();
	ms_od_solved.ms_LT_recup_od_solved = mc_LT_recup.ms_od_solved;
	ms_od_solved.ms_HT_recup_od_solved = mc_HT_recup.ms_od_solved;

	// Set ms_od_solved
	ms_od_solved.m_eta_thermal = m_eta_thermal_od;
	ms_od_solved.m_W_dot_net = m_W_dot_net_od;
	ms_od_solved.m_Q_dot = m_Q_dot_PHX_od;
	ms_od_solved.m_m_dot_mc = m_dot_mc;
	ms_od_solved.m_m_dot_rc = m_dot_rc;
	ms_od_solved.m_m_dot_t = m_dot_t;
	ms_od_solved.m_recomp_frac = ms_od_phi_par.m_recomp_frac;
	// ms_od_solved.m_N_mc = ms_od_par.m_N_mc;
	// ms_od_solved.m_N_t = ms_od_par.m_N_t;

	ms_od_solved.m_temp = m_temp_od;
	ms_od_solved.m_pres = m_pres_od;
	ms_od_solved.m_enth = m_enth_od;
	ms_od_solved.m_entr = m_entr_od;
	ms_od_solved.m_dens = m_dens_od;

	return;
}

void C_RecompCycle::off_design_core(int & error_code)
{	
	// Need to reset 'ms_od_solved' here
	clear_ms_od_solved();
	// Check if recompression fraction is > 0 and whether cycle is simple or recompression
	if( !ms_des_solved.m_is_rc )
	{
		ms_od_phi_par.m_recomp_frac = 0.0;
	}

	CO2_state co2_props;

	int cpp_offset = 1;

	// Initialize a few variables
	m_temp_od[1-cpp_offset] = ms_od_par.m_T_mc_in;
	m_pres_od[1-cpp_offset] = ms_od_par.m_P_mc_in;
	m_temp_od[6-cpp_offset] = ms_od_par.m_T_t_in;

	// mc.N          t.N           tol

	// Prepare the mass flow rate iteration loop
	int prop_error_code = CO2_TP(m_temp_od[1-cpp_offset], m_pres_od[1-cpp_offset], &co2_props);
	if(prop_error_code != 0)
	{
		error_code = prop_error_code;
		return;
	}
	double rho_in = co2_props.dens;

	double tip_speed = m_mc.get_design_solved()->m_D_rotor * 0.5 * ms_od_par.m_N_mc * 0.10471975512;	//[m/s] Main compressor tip speed
	double partial_phi = rho_in*pow(m_mc.get_design_solved()->m_D_rotor,2)*tip_speed;					// ... reduces computation on next two lines
	double m_dot_mc_guess = C_compressor::m_snl_phi_design * partial_phi;					//[kg/s] mass flow rate corresponding to design-point phi in main compressor
	double m_dot_mc_max = C_compressor::m_snl_phi_max * partial_phi * 1.2;					//[kg/s] largest possible mass flow rate in main compressor (with safety factor)
	double m_dot_t = m_dot_mc_guess/(1.0 - ms_od_par.m_recomp_frac);						//[kg/s] first guess for mass flow rate through turbine
	double m_dot_upper_bound = m_dot_mc_max/(1.0 - ms_od_par.m_recomp_frac);				//[kg/s] largest possible mass flow rate through turbine
	double m_dot_lower_bound = 0.0;															//[-] this lower bound allows for surge (checked after iteration)
	bool first_pass = true;

	// Enter the mass flow rate iteration loop
	int m_dot_iter = -1;
	int max_iter = 100;
	double temperature_tolerance = 1.E-6;

	double m_dot_rc = std::numeric_limits<double>::quiet_NaN();
	double m_dot_mc = std::numeric_limits<double>::quiet_NaN();

	double last_m_dot_guess = -999.9;
	double last_m_dot_residual = std::numeric_limits<double>::quiet_NaN();

	for( m_dot_iter = 0; m_dot_iter < max_iter; m_dot_iter++ )
	{
		m_dot_rc = m_dot_t * ms_od_par.m_recomp_frac;
		m_dot_mc = m_dot_t - m_dot_rc;

		// Calculate the pressure rise through the main compressor
		int comp_error_code = 0;
		m_mc.off_design_compressor(m_temp_od[1-cpp_offset], m_pres_od[1-cpp_offset], m_dot_mc, ms_od_par.m_N_mc, 
			comp_error_code, m_temp_od[2-cpp_offset], m_pres_od[2-cpp_offset]);

		if(comp_error_code == 1)			// m_dot is too high because the given shaft speed is not possible
		{
			m_dot_upper_bound = m_dot_t;
			m_dot_t = 0.5*(m_dot_lower_bound + m_dot_upper_bound);		// use bisection for new mass flow rate guess
			continue;
		}
		else if(comp_error_code == 2)		// m_dot is too low because P_out is (likely) above properties limits
		{
			m_dot_lower_bound = m_dot_t;
			m_dot_t = 0.5*(m_dot_lower_bound + m_dot_upper_bound);		// use bisection for new mass flow rate guess
			continue;
		}
		else if(comp_error_code != 0)		// unexpected error
		{
			error_code = comp_error_code;
			return;
		}

		// Calculate scaled pressure drops through heat exchangers.
		std::vector<double> DP_LT, DP_HT, DP_PHX, DP_PC;
		//std::vector<double> m_dot_LT;
		//m_dot_LT.push_back( m_dot_mc );
		//m_dot_LT.push_back( m_dot_t );
		//m_LT.hxr_pressure_drops(m_dot_LT, DP_LT);
		DP_LT.resize(2);
		DP_LT[0] = mc_LT_recup.od_delta_p_cold(m_dot_mc);
		DP_LT[1] = mc_LT_recup.od_delta_p_hot(m_dot_t);

		//std::vector<double> m_dot_HT;
		//m_dot_HT.push_back( m_dot_t );
		//m_dot_HT.push_back( m_dot_t );
		//m_HT.hxr_pressure_drops(m_dot_HT, DP_HT);
		DP_HT.resize(2);
		DP_HT[0] = mc_HT_recup.od_delta_p_cold(m_dot_t);
		DP_HT[1] = mc_HT_recup.od_delta_p_hot(m_dot_t);

		std::vector<double> m_dot_PHX;
		m_dot_PHX.push_back( m_dot_t );
		m_dot_PHX.push_back( 0.0 );
		m_PHX.hxr_pressure_drops(m_dot_PHX, DP_PHX);
		
		std::vector<double> m_dot_PC;
		m_dot_PC.push_back( 0.0 );
		m_dot_PC.push_back( m_dot_mc);
		m_PC.hxr_pressure_drops(m_dot_PC, DP_PC);

		// Apply pressure drops to heat exchangers, fully defining the pressure at all states
		m_pres_od[3-cpp_offset] = m_pres_od[2-cpp_offset] - DP_LT[1-cpp_offset];		// LT recuperator (cold stream)
		m_pres_od[4-cpp_offset] = m_pres_od[3-cpp_offset];								// Assume no pressure drop in mixing valve
		m_pres_od[10-cpp_offset] = m_pres_od[3-cpp_offset];								// Assume no pressure drop in mixing valve
		m_pres_od[5-cpp_offset] = m_pres_od[4-cpp_offset] - DP_HT[1-cpp_offset];		// HT recuperator (cold stream)
		m_pres_od[6-cpp_offset] = m_pres_od[5-cpp_offset] - DP_PHX[1-cpp_offset];		// PHX
		m_pres_od[9-cpp_offset] = m_pres_od[1-cpp_offset] + DP_PC[2-cpp_offset];		// precooler
		m_pres_od[8-cpp_offset] = m_pres_od[9-cpp_offset] + DP_LT[2-cpp_offset];		// LT recuperator (hot stream)
		m_pres_od[7-cpp_offset] = m_pres_od[8-cpp_offset] + DP_HT[2-cpp_offset];		// HT recuperator (hot stream)

		// Calculate the mass flow rate through the turbine
		int turbine_error_code = 0;
		double m_dot_t_allowed = std::numeric_limits<double>::quiet_NaN();
		m_t.off_design_turbine(m_temp_od[6-cpp_offset], m_pres_od[6-cpp_offset], m_pres_od[7-cpp_offset], ms_od_par.m_N_t,
			turbine_error_code, m_dot_t_allowed, m_temp_od[7-cpp_offset]);
		
		if(turbine_error_code != 0)		// unexpected error
		{
			error_code = turbine_error_code;
			return;
		}

		// Determine the mass flow rate residual and prepare the next iteration
		double m_dot_residual = m_dot_t - m_dot_t_allowed;
		
		// twn: during the first iteration 'last_m_dot_guess' won't be initialized
		double secant_guess = m_dot_t - m_dot_residual*(last_m_dot_guess-m_dot_t)/(last_m_dot_residual-m_dot_residual);		// next guess predicted using secant method
		
		if(m_dot_residual > 0.0)	// pressure rise is too small, so m_dot_t is too big
		{
			if( m_dot_residual / m_dot_t < ms_od_par.m_tol )		// residual is positive; check for convergence
				break;

			m_dot_upper_bound = m_dot_t;		// reset upper bound
		}
		else	// pressure rise is too high, so m_dot_t is too small
		{
			if( -m_dot_residual / m_dot_t < ms_od_par.m_tol )	// residual is negative; check for converge
				break;

			m_dot_lower_bound = m_dot_t;		// reset lower bound
		}

		last_m_dot_residual = m_dot_residual;		// reset last stored residual value
		last_m_dot_guess = m_dot_t;					// reset last stored guess value

		// Check if the secant method overshoots and fall back to bisection if it does
		if(first_pass)
		{
			m_dot_t = 0.5*(m_dot_upper_bound + m_dot_lower_bound);
			first_pass = false;
		}
		else if(secant_guess < m_dot_lower_bound || secant_guess > m_dot_upper_bound)		// secant method overshot, use bisection
			m_dot_t = 0.5*(m_dot_upper_bound + m_dot_lower_bound);
		else
			m_dot_t = secant_guess;
	
	}	// End m_dot loop

	// Check for convergence
	if(m_dot_iter >= max_iter)
	{
		error_code = 42;
		return;
	}

	// Fully define known states
	prop_error_code = CO2_TP(m_temp_od[1-cpp_offset], m_pres_od[1-cpp_offset], &co2_props);
	if(prop_error_code != 0)
	{
		error_code = prop_error_code;
		return;
	}
	m_enth_od[1-cpp_offset] = co2_props.enth;
	m_entr_od[1-cpp_offset] = co2_props.entr;
	m_dens_od[1-cpp_offset] = co2_props.dens;

	prop_error_code = CO2_TP(m_temp_od[2-cpp_offset], m_pres_od[2-cpp_offset], &co2_props);
	if(prop_error_code != 0)
	{
		error_code = prop_error_code;
		return;
	}
	m_enth_od[2-cpp_offset] = co2_props.enth;
	m_entr_od[2-cpp_offset] = co2_props.entr;
	m_dens_od[2-cpp_offset] = co2_props.dens;

	prop_error_code = CO2_TP(m_temp_od[6-cpp_offset], m_pres_od[6-cpp_offset], &co2_props);
	if(prop_error_code != 0)
	{
		error_code = prop_error_code;
		return;
	}
	m_enth_od[6-cpp_offset] = co2_props.enth;
	m_entr_od[6-cpp_offset] = co2_props.entr;
	m_dens_od[6-cpp_offset] = co2_props.dens;

	prop_error_code = CO2_TP(m_temp_od[7-cpp_offset], m_pres_od[7-cpp_offset], &co2_props);
	if(prop_error_code != 0)
	{
		error_code = prop_error_code;
		return;
	}
	m_enth_od[7-cpp_offset] = co2_props.enth;
	m_entr_od[7-cpp_offset] = co2_props.entr;
	m_dens_od[7-cpp_offset] = co2_props.dens;

	// Get the recuperator conductances corresponding to the converged mass flow rates
	double UA_LT, UA_HT;
	UA_LT = UA_HT = std::numeric_limits<double>::quiet_NaN();
	std::vector<double> m_dot_LT;
	m_dot_LT.push_back( m_dot_mc );
	m_dot_LT.push_back( m_dot_t );
	std::vector<double> m_dot_HT;
	m_dot_HT.push_back( m_dot_t );
	m_dot_HT.push_back( m_dot_t );
	//m_LT.hxr_conductance(m_dot_LT, UA_LT);
	UA_LT = mc_LT_recup.od_UA(m_dot_mc, m_dot_t);
	//m_HT.hxr_conductance(m_dot_HT, UA_HT);
	UA_HT = mc_HT_recup.od_UA(m_dot_t, m_dot_t);

	// Outer iteration loop: temp(8), checking against UA_HT
	double T8_lower_bound = std::numeric_limits<double>::quiet_NaN();
	double T8_upper_bound = std::numeric_limits<double>::quiet_NaN();
	double UA_HT_calc = std::numeric_limits<double>::quiet_NaN();
	double last_HT_residual = std::numeric_limits<double>::quiet_NaN();
	double last_T8_guess = std::numeric_limits<double>::quiet_NaN();
	if(UA_HT < 1.E-12)		// no high-temperature recuperator
	{
		T8_lower_bound = m_temp_od[7-cpp_offset];	// no iteration necessary
		T8_upper_bound = m_temp_od[7-cpp_offset];	// no iteration necessary
		m_temp_od[8-cpp_offset] = m_temp_od[7-cpp_offset];
		UA_HT_calc = 0.0;
		last_HT_residual = 0.0;
		last_T8_guess = m_temp_od[7-cpp_offset];
	}
	else
	{
		T8_lower_bound = m_temp_od[2-cpp_offset];	// the absolute lowest temp(8) could be
		T8_upper_bound = m_temp_od[7-cpp_offset];	// the absolute highest temp(8) could be
		m_temp_od[8-cpp_offset] = 0.5*(T8_lower_bound + T8_upper_bound);	// bisect bounds for first guess
		UA_HT_calc = -1.0;
		last_HT_residual = UA_HT;					// know a priori that with T8 = T7, UA_calc = 0 therefore residual is UA_HT - 0.0
		last_T8_guess = m_temp_od[7-cpp_offset];
	}

	int T8_iter = -1;

	double Q_dot_LT = std::numeric_limits<double>::quiet_NaN();
	double Q_dot_HT = std::numeric_limits<double>::quiet_NaN();

	for( T8_iter = 0; T8_iter < max_iter; T8_iter++ )
	{
		// Fully define state 8
		prop_error_code = CO2_TP(m_temp_od[8-cpp_offset], m_pres_od[8-cpp_offset], &co2_props);
		if(prop_error_code != 0)
		{
			error_code = prop_error_code;
			return;
		}
		m_enth_od[8-cpp_offset] = co2_props.enth;
		m_entr_od[8-cpp_offset] = co2_props.entr;
		m_dens_od[8-cpp_offset] = co2_props.dens;
	
		// Inner iteration loop: temp(9), checking against UA_LT
		double T9_lower_bound = std::numeric_limits<double>::quiet_NaN();
		double T9_upper_bound = std::numeric_limits<double>::quiet_NaN();
		double UA_LT_calc = std::numeric_limits<double>::quiet_NaN();
		double last_LT_residual = std::numeric_limits<double>::quiet_NaN();
		double last_T9_guess = std::numeric_limits<double>::quiet_NaN();
		if(UA_LT < 1.E-12)		// no low-temperature recuperator
		{
			T9_lower_bound = m_temp_od[8-cpp_offset];	// no iteration necessary
			T9_upper_bound = m_temp_od[8-cpp_offset];	// no iteration necessary
			m_temp_od[9-cpp_offset] = m_temp_od[8-cpp_offset];	
			UA_LT_calc = 0.0;
			last_LT_residual = 0.0;
			last_T9_guess = m_temp_od[8-cpp_offset];
		}
		else
		{
			T9_lower_bound = m_temp_od[2-cpp_offset];		// the absolute lowest temp(9) could be
			T9_upper_bound = m_temp_od[8-cpp_offset];		// the absolute highest temp(9) could be
			m_temp_od[9-cpp_offset] = 0.5*(T9_lower_bound + T9_upper_bound);	// bisect bounds for first guess
			UA_LT_calc = -1.0;
			last_LT_residual = UA_LT;			// know a priori that with T9=T8, UA_calc = 0 therefore residual is UA_LT - 0
			last_T9_guess = m_temp_od[8-cpp_offset];
		}

		int T9_iter = -1;

		for( T9_iter = 0; T9_iter < max_iter; T9_iter++ )
		{
			prop_error_code = CO2_TP(m_temp_od[9-cpp_offset], m_pres_od[9-cpp_offset], &co2_props);		// fully define state 9
			if(prop_error_code != 0)
			{
				error_code = prop_error_code;
				return;
			}
			m_enth_od[9-cpp_offset] = co2_props.enth;
			m_entr_od[9-cpp_offset] = co2_props.entr;
			m_dens_od[9-cpp_offset] = co2_props.dens;
		
			if(ms_od_par.m_recomp_frac >= 1.E-12)		// determine the required shaft speed for the recompressing compressor
			{
				int rc_error_code = 0;
				m_rc.off_design_recompressor(m_temp_od[9-cpp_offset], m_pres_od[9-cpp_offset], m_dot_rc, m_pres_od[10-cpp_offset], rc_error_code, m_temp_od[10-cpp_offset]);
			
				if(rc_error_code != 0)
				{
					error_code = rc_error_code;
					return;
				}

				// Fully define state 10
				prop_error_code = CO2_TP(m_temp_od[10-cpp_offset], m_pres_od[10-cpp_offset], &co2_props);
				if(prop_error_code != 0)
				{
					error_code = prop_error_code;
					return;
				}
				m_enth_od[10-cpp_offset] = co2_props.enth;
				m_entr_od[10-cpp_offset] = co2_props.entr;
				m_dens_od[10-cpp_offset] = co2_props.dens;			
			}
			else
			{
				m_temp_od[10-cpp_offset] = m_temp_od[9-cpp_offset];
				m_enth_od[10-cpp_offset] = m_enth_od[9-cpp_offset];
				m_entr_od[10-cpp_offset] = m_entr_od[9-cpp_offset];
				m_dens_od[10-cpp_offset] = m_dens_od[9-cpp_offset];
			}

			// Calculate the UA value of the low-temperature recuperator
			Q_dot_LT = std::numeric_limits<double>::quiet_NaN();
			if( UA_LT < 1.E-12 )			// no low-temp recuperator (this check is necessary to prevent pressure drops with ua=0 from causing problems)
				Q_dot_LT = 0.0;
			else
				Q_dot_LT = m_dot_t * (m_enth_od[8-cpp_offset] - m_enth_od[9-cpp_offset]);

			double min_DT_LT, eff_LT_hx, NTU_LT_hx, T_h_out_LT_hx, T_c_out_LT_hx, q_dot_LT_hx;
			min_DT_LT = eff_LT_hx = NTU_LT_hx = T_h_out_LT_hx = T_c_out_LT_hx = q_dot_LT_hx = std::numeric_limits<double>::quiet_NaN();

			try
			{
				mc_LT_recup.calc_req_UA(Q_dot_LT, m_dot_mc, m_dot_t, m_temp_od[MC_OUT], m_temp_od[HTR_LP_OUT],
					m_pres_od[MC_OUT], m_pres_od[LTR_HP_OUT], m_pres_od[HTR_LP_OUT], m_pres_od[LTR_LP_OUT],
					UA_LT_calc, min_DT_LT, eff_LT_hx, NTU_LT_hx, T_h_out_LT_hx, T_c_out_LT_hx, q_dot_LT_hx);
			}
			catch( C_csp_exception & csp_except )
			{
				if( csp_except.m_error_code == 11 )		// second-law violation in hxr, therefore temp(9) is too low
				{
					T9_lower_bound = m_temp_od[9 - cpp_offset];
					m_temp_od[9 - cpp_offset] = 0.5*(T9_lower_bound + T9_upper_bound);	// bisect bounds for next guess
					continue;
				}
				else
				{
					error_code = csp_except.m_error_code;
					return;
				}
			}

			//calculate_hxr_UA_1(ms_od_par.m_N_sub_hxrs, Q_dot_LT, m_dot_mc, m_dot_t, m_temp_od[2-cpp_offset], m_temp_od[8-cpp_offset], 
			//	m_pres_od[2-cpp_offset], m_pres_od[3-cpp_offset], m_pres_od[8-cpp_offset], m_pres_od[9-cpp_offset], hx_error_code, UA_LT_calc, min_DT_LT);
			//
			//if(hx_error_code > 0)
			//{
			//	if(hx_error_code == 11)			// second-law violation in hxr, therefore temp(9) is too low
			//	{
			//		T9_lower_bound = m_temp_od[9-cpp_offset];
			//		m_temp_od[9-cpp_offset] = 0.5*(T9_lower_bound + T9_upper_bound);	// bisect bounds for next guess
			//		hx_error_code = 0;
			//		continue;				
			//	}
			//	else
			//	{
			//		error_code = hx_error_code;
			//		return;
			//	}
			//}

			// Check for convergence and adjust T9 appropriately
			double UA_LT_residual = UA_LT - UA_LT_calc;

			if( fabs(UA_LT_residual) < 1.E-12 )		// catches no LT case
				break;

			double secant_guess = m_temp_od[9-cpp_offset] - UA_LT_residual*(last_T9_guess-m_temp_od[9-cpp_offset])/(last_LT_residual-UA_LT_residual);	// next guess predicted using secant method

			if(UA_LT_residual < 0.0)		// UA_LT_calc is too big, temp(9) needs to be higher
			{
				if( fabs(UA_LT_residual) / UA_LT < ms_od_par.m_tol )		// UA_LT converged (residual is negative
					break;
				T9_lower_bound = m_temp_od[9-cpp_offset];
			}
			else			// UA_LT_calc is too small, temp(9) needs to be lower
			{
				if( UA_LT_residual / UA_LT < ms_od_par.m_tol )				// UA_LT converged
					break;

				if( min_DT_LT < temperature_tolerance )			// UA_calc is still too low but there isn't anywhere to go so it's ok (catches huge UA values)
					break;

				T9_upper_bound = m_temp_od[9-cpp_offset];
			}

			last_LT_residual = UA_LT_residual;			// reset last stored residual value
			last_T9_guess = m_temp_od[9-cpp_offset];	// reset last stored guess value

			// Check if the secant method overshoots and fall abck to bisection if it does
			if( secant_guess <= T9_lower_bound || secant_guess >= T9_upper_bound || secant_guess != secant_guess )		// secant method overshot (or is NaN), use bisection
				m_temp_od[9-cpp_offset] = 0.5*(T9_lower_bound + T9_upper_bound);
			else
				m_temp_od[9-cpp_offset] = secant_guess;

		}	// End of T9 iteration

		// Check that T9_loop converged
		if( T9_iter >= max_iter )
		{
			error_code = 31;
			return;
		}

		// State 3 can now be fully defined
		m_enth_od[3-cpp_offset] = m_enth_od[2-cpp_offset] + Q_dot_LT/m_dot_mc;		// Energy balance on cold stream of low-temp recuperator
		prop_error_code = CO2_PH(m_pres_od[3-cpp_offset], m_enth_od[3-cpp_offset], &co2_props);
		if(prop_error_code != 0)
		{
			error_code = prop_error_code;
			return;
		}
		m_temp_od[3-cpp_offset] = co2_props.temp;
		m_entr_od[3-cpp_offset] = co2_props.entr;
		m_dens_od[3-cpp_offset] = co2_props.dens;

		// Go through mixing valve
		if( ms_od_par.m_recomp_frac >= 1.E-12 )
		{
			// Conservation of energy (both sides divided by m_dot_t)
			m_enth_od[4-cpp_offset] = (1.0 - ms_od_par.m_recomp_frac)*m_enth_od[3-cpp_offset] + ms_od_par.m_recomp_frac*m_enth_od[10-cpp_offset];	
			prop_error_code = CO2_PH(m_pres_od[4-cpp_offset], m_enth_od[4-cpp_offset], &co2_props);
			if(prop_error_code != 0)
			{
				error_code = prop_error_code;
				return;
			}
			m_temp_od[4-cpp_offset] = co2_props.temp;
			m_entr_od[4-cpp_offset] = co2_props.entr;
			m_dens_od[4-cpp_offset] = co2_props.dens;		
		}
		else	// no mixing valve, therefore state 4 is equal to state 3
		{
			m_temp_od[4-cpp_offset] = m_temp_od[3-cpp_offset];
			m_enth_od[4-cpp_offset] = m_enth_od[3-cpp_offset];
			m_entr_od[4-cpp_offset] = m_entr_od[3-cpp_offset];
			m_dens_od[4-cpp_offset] = m_dens_od[3-cpp_offset];
		}

		// Check for a second law violation at the outlet of the high-temp recuperator
		if(m_temp_od[4-cpp_offset] >= m_temp_od[8-cpp_offset])		// temp(8) is not valid and it must be increased
		{
			T8_lower_bound = m_temp_od[8-cpp_offset];
			m_temp_od[8-cpp_offset] = 0.5*(T8_lower_bound + T8_upper_bound);
			continue;
		}

		// Calculate the UA value of the high-temperature recuperator
		Q_dot_HT = std::numeric_limits<double>::quiet_NaN();
		if( UA_HT < 1.E-12 )		// no high-temp recuperator (this check is necessary to prevent pressure drops with UA=0 from causing problems)
			Q_dot_HT = 0.0;
		else
			Q_dot_HT = m_dot_t*(m_enth_od[7-cpp_offset] - m_enth_od[8-cpp_offset]);


		double min_DT_HT, eff_HT_hx, NTU_HT_hx, T_h_out_HT_hx, T_c_out_HT_hx, q_dot_HT_hx;
		min_DT_HT = eff_HT_hx = NTU_HT_hx = T_h_out_HT_hx = T_c_out_HT_hx = q_dot_HT_hx = std::numeric_limits<double>::quiet_NaN();

		try
		{
			mc_HT_recup.calc_req_UA(Q_dot_HT, m_dot_t, m_dot_t, m_temp_od[MIXER_OUT], m_temp_od[TURB_OUT],
				m_pres_od[MIXER_OUT], m_pres_od[HTR_HP_OUT], m_pres_od[TURB_OUT], m_pres_od[HTR_LP_OUT],
				UA_HT_calc, min_DT_HT, eff_HT_hx, NTU_HT_hx, T_h_out_HT_hx, T_c_out_HT_hx, q_dot_HT_hx);
		}
		catch( C_csp_exception & csp_except )
		{
			if( csp_except.m_error_code == 11 )		// second-law violation in hxr, therefore temp(8) is too low
			{
				T8_lower_bound = m_temp_od[8 - cpp_offset];
				m_temp_od[8 - cpp_offset] = 0.5*(T8_lower_bound + T8_upper_bound);	// bisect bounds for next guess
				continue;
			}
			else
			{
				error_code = csp_except.m_error_code;
				return;
			}
		}

		//int HT_hx_error_code = 0;
		//double min_DT_HT = std::numeric_limits<double>::quiet_NaN();
		//calculate_hxr_UA_1(ms_od_par.m_N_sub_hxrs, Q_dot_HT, m_dot_t, m_dot_t, m_temp_od[4-cpp_offset], m_temp_od[7-cpp_offset], m_pres_od[4-cpp_offset], m_pres_od[5-cpp_offset],
		//	m_pres_od[7-cpp_offset], m_pres_od[8-cpp_offset], HT_hx_error_code, UA_HT_calc, min_DT_HT);
		//
		//if(HT_hx_error_code != 0)
		//{
		//	if(HT_hx_error_code == 11)		// second-law violation in hxr, therefore temp(8) is too low
		//	{
		//		T8_lower_bound = m_temp_od[8-cpp_offset];
		//		m_temp_od[8-cpp_offset] = 0.5*(T8_lower_bound + T8_upper_bound);	// bisect bounds for next guess
		//		HT_hx_error_code = 0;
		//		continue;
		//	}
		//	else
		//	{
		//		error_code = HT_hx_error_code;
		//		return;
		//	}		
		//}

		// Check for convergence and adjust T8 appropriately
		double UA_HT_residual = UA_HT - UA_HT_calc;

		if( fabs(UA_HT_residual) < 1.E-12 )			// catches no HT case
			break;

		double secant_guess = m_temp_od[8-cpp_offset] - UA_HT_residual*(last_T8_guess-m_temp_od[8-cpp_offset])/(last_HT_residual - UA_HT_residual);	// next guess predicted using secant method

		if(UA_HT_residual < 0.0)		// UA_HT_calc is too big, temp(8) needs to be higher
		{
			if( fabs(UA_HT_residual) / UA_HT < ms_od_par.m_tol )		// UA_HT converged (residual is negative)
				break;

			T8_lower_bound = m_temp_od[8-cpp_offset];
		}
		else		// UA_HT_calc is too small, temp(8) needs to be lower
		{
			if( UA_HT_residual / UA_HT < ms_od_par.m_tol )	// UA _HT converged
				break;

			if( min_DT_HT < temperature_tolerance )			// UA_calc is still too low, but there isn't anywhere to go so it's ok (catches huge UA values)
				break;

			T8_upper_bound = m_temp_od[8-cpp_offset];
		}

		last_HT_residual = UA_HT_residual;			// reset last stored residual value
		last_T8_guess = m_temp_od[8-cpp_offset];	// reset last stored guess value

		// Check if the secant method overshoots and fall back to bisection if it does
		if(secant_guess <= T8_lower_bound || secant_guess >= T8_upper_bound)
			m_temp_od[8-cpp_offset] = 0.5*(T8_lower_bound + T8_upper_bound);
		else
			m_temp_od[8-cpp_offset] = secant_guess;

	}	// End of T8 iteration

	// Check that T8_loop converged
	if(T8_iter >= max_iter)
	{
		error_code = 35;
		return;
	}

	// State 5 can now be fully defined
	m_enth_od[5-cpp_offset] = m_enth_od[4-cpp_offset] + Q_dot_HT/m_dot_t;		//[kJ/kg] Energy balance on cold stream of high-temp recuperator
	prop_error_code = CO2_PH(m_pres_od[5-cpp_offset], m_enth_od[5-cpp_offset], &co2_props);
	if(prop_error_code != 0)
	{
		error_code = prop_error_code;
		return;
	}
	m_temp_od[5-cpp_offset] = co2_props.temp;
	m_entr_od[5-cpp_offset] = co2_props.entr;
	m_dens_od[5-cpp_offset] = co2_props.dens;

	// Calculate cycle performance metrics
	double w_mc = m_enth_od[1-cpp_offset] - m_enth_od[2-cpp_offset];		//[kJ/kg] (negative) specific work of compressor
	double w_t = m_enth_od[6-cpp_offset] - m_enth_od[7-cpp_offset];			//[kJ/kg] (positive) specific work of turbine

	double w_rc = 0.0;
	if(ms_od_par.m_recomp_frac > 0.0)
		w_rc = m_enth_od[9-cpp_offset] - m_enth_od[10-cpp_offset];			//[kJ/kg] (negative) specific work of recompressor


	m_Q_dot_PHX_od = m_dot_t*(m_enth_od[6-cpp_offset] - m_enth_od[5-cpp_offset]);
	m_W_dot_net_od = w_mc*m_dot_mc + w_rc*m_dot_rc + w_t*m_dot_t;			
	m_eta_thermal_od = m_W_dot_net_od / m_Q_dot_PHX_od;

	// Get 'od_solved' structures from component classes
	ms_od_solved.ms_mc_od_solved = *m_mc.get_od_solved();
	ms_od_solved.ms_rc_od_solved = *m_rc.get_od_solved();
	ms_od_solved.ms_t_od_solved = *m_t.get_od_solved();

	// Set ms_od_solved
	ms_od_solved.m_eta_thermal = m_eta_thermal_od;
	ms_od_solved.m_W_dot_net = m_W_dot_net_od;
	ms_od_solved.m_Q_dot = m_Q_dot_PHX_od;
	ms_od_solved.m_m_dot_mc = m_dot_mc;
	ms_od_solved.m_m_dot_rc = m_dot_rc;
	ms_od_solved.m_m_dot_t = m_dot_t;
	ms_od_solved.m_recomp_frac = ms_od_par.m_recomp_frac;
	// ms_od_solved.m_N_mc = ms_od_par.m_N_mc;
	// ms_od_solved.m_N_t = ms_od_par.m_N_t;

	ms_od_solved.m_temp = m_temp_od;
	ms_od_solved.m_pres = m_pres_od;
	ms_od_solved.m_enth = m_enth_od;
	ms_od_solved.m_entr = m_entr_od;
	ms_od_solved.m_dens = m_dens_od;

	return;
}

void C_RecompCycle::target_off_design(S_target_od_parameters & tar_od_par_in, int & error_code)
{
	ms_tar_od_par = tar_od_par_in;

	int tar_od_error_code = 0;

	target_off_design_core(tar_od_error_code);

	error_code = tar_od_error_code;
}

void C_RecompCycle::target_off_design_core(int & error_code)
{
	int max_iter = 100;

	// 10.6.14 twn: Increase from 20 to 40 to hopefully improve convergence when q_target is close (but less than) q_max
	int search_intervals; 
	if( ms_tar_od_par.m_use_default_res )
		search_intervals = 20;
	else
		search_intervals = 50;		// number of intervals to check for valid bounds before starting secant loop

	// Determine the interval containing the solution
	bool lower_bound_found = false;
	bool upper_bound_found = false;

	double left_residual = -1.E12;		// Initialized to large negative value
	double right_residual = 1.E12;		// Initialized to large positive value

	double P_low = ms_tar_od_par.m_lowest_pressure;
	//double P_high = ms_tar_od_par.m_highest_pressure;
	double P_high = min(ms_tar_od_par.m_highest_pressure, 12000.0);

	std::vector<double> P_guesses(search_intervals+1);
	for( int i = 0; i <= search_intervals; i++ )
		P_guesses[i] = P_low + i*(P_high-P_low)/double(search_intervals);

	double biggest_value = 0.0;
	double biggest_cycle = 0.0;			// Track pressure instead of 'cycle'

	// Set 'ms_od_par' that are known
	ms_od_par.m_T_mc_in = ms_tar_od_par.m_T_mc_in;
	ms_od_par.m_T_t_in = ms_tar_od_par.m_T_t_in;
	ms_od_par.m_recomp_frac = ms_tar_od_par.m_recomp_frac;
	ms_od_par.m_N_mc = ms_tar_od_par.m_N_mc;
	ms_od_par.m_N_t = ms_tar_od_par.m_N_t;
	ms_od_par.m_N_sub_hxrs = ms_tar_od_par.m_N_sub_hxrs;
	ms_od_par.m_tol = ms_tar_od_par.m_tol;

	for( int i = 0; i <= search_intervals; i++ )
	{
		double P_guess = P_guesses[i];

		ms_od_par.m_P_mc_in = P_guess;

		int od_error_code = 0;
		off_design_core(od_error_code);

		if(od_error_code == 0)
		{
			if( m_pres_last[2 - 1] > ms_des_par.m_P_high_limit*1.2 )
				break;		// Compressor inlet pressure is getting too big

			double target_value = -999.9;
			if( ms_tar_od_par.m_is_target_Q )
				target_value = m_Q_dot_PHX_od;
			else
				target_value = m_W_dot_net_od;

			double residual = target_value - ms_tar_od_par.m_target;

			if(target_value > biggest_value)		// keep track of the largest value seen
			{
				biggest_value = target_value;
				biggest_cycle = P_guess;
			}

			if( residual >= 0.0 )					// value is above target
			{
				if( residual < right_residual )	// first rightbound or a better bound, use it
				{
					P_high = P_guess;
					right_residual = residual;
					upper_bound_found = true;
				}
			}
			else									// value is below target
			{
				if( residual > left_residual )	// note: residual and left_residual are negative
				{
					P_low = P_guess;
					left_residual = residual;
					lower_bound_found = true;
				}
			}		
		}		// End od_error_code = 0 loop
		if( lower_bound_found && upper_bound_found )
			break;
	}

	if( !lower_bound_found || !upper_bound_found )
	{
		error_code = 26;
		// return biggest cycle??
		return;
	}

	// Enter secant / bisection loop
	double P_guess = (P_low + P_high)*0.5;		// start with bisection (note: could use left and right bounds and residuals to get a better first guess)

	double last_P_guess = 1.E12;				// twn: set to some large value here???
	double last_residual = 1.23;				// twn: set to some small value here???

	int iter = 0;
	for( iter = 1; iter <= max_iter; iter++ )
	{
		ms_od_par.m_P_mc_in = P_guess;

		int od_error_code = 0;
		off_design_core(od_error_code);
		
		if( od_error_code != 0 )			// results not valid; choose a random value between P_low and P_high for next guess
		{
			double P_frac = rand() / (double)(RAND_MAX);
			P_guess = P_low + (P_high - P_low)*P_guess;
			continue;
		}

		// Check residual
		double residual = std::numeric_limits<double>::quiet_NaN();
		if( ms_tar_od_par.m_is_target_Q )
			residual = m_Q_dot_PHX_od - ms_tar_od_par.m_target;
		else
			residual = m_W_dot_net_od - ms_tar_od_par.m_target;
	
		if( residual >= 0.0 )		// value is above target
		{
			if( residual / ms_tar_od_par.m_target <= ms_tar_od_par.m_tol )		// converged
				break;
			P_high = P_guess;
		}
		else						// value is below target
		{
			if( -residual / ms_tar_od_par.m_target <= ms_tar_od_par.m_tol )
				break;
			P_low = P_guess;
		}

		if( fabs(P_high - P_low) < 0.1 )		// Interval is tiny; consider it converged
			break;

		// Determine next guess
		double P_secant = P_guess - residual*(last_P_guess-P_guess)/(last_residual-residual);		// next guess predicted using secant method
		last_P_guess = P_guess;
		last_residual = residual;
		P_guess = P_secant;

		if( P_guess <= P_low || P_guess >= P_high )
			P_guess = (P_low + P_high)*0.5;				// Secant overshot, use bisection

	}

	// Check for convergence
	if( iter >= max_iter )
	{
		error_code = 82;
		return;
	}

}

void C_RecompCycle::optimal_off_design(S_opt_od_parameters & opt_od_par_in, int & error_code)
{
	ms_opt_od_par = opt_od_par_in;

	int opt_od_error_code = 0;

	optimal_off_design_core(opt_od_error_code);

	error_code = opt_od_error_code;
}

void C_RecompCycle::optimal_off_design_core(int & error_code)
{
	// Set known values for ms_od_par
	ms_od_par.m_T_mc_in = ms_opt_od_par.m_T_mc_in;
	ms_od_par.m_T_t_in = ms_opt_od_par.m_T_t_in;
	ms_od_par.m_N_sub_hxrs = ms_opt_od_par.m_N_sub_hxrs;
	ms_od_par.m_tol = ms_opt_od_par.m_tol;

	// Initialize guess array
	int index = 0;

	std::vector<double> x(0);
	std::vector<double> lb(0);
	std::vector<double> ub(0);
	std::vector<double> scale(0);

	if( !ms_opt_od_par.m_fixed_P_mc_in )
	{
		x.push_back(ms_opt_od_par.m_P_mc_in_guess);
		lb.push_back(100.0);
		ub.push_back(ms_des_par.m_P_high_limit);
		//scale.push_back(0.25*ms_opt_od_par.m_P_mc_in_guess);
		scale.push_back(50.0);

		index++;
	}

	if( !ms_opt_od_par.m_fixed_recomp_frac )
	{
		x.push_back(ms_opt_od_par.m_recomp_frac_guess);
		lb.push_back(0.0);
		ub.push_back(1.0);
		scale.push_back(0.05);

		index++;
	}

	if( !ms_opt_od_par.m_fixed_N_mc )
	{
		x.push_back(ms_opt_od_par.m_N_mc_guess);
		lb.push_back(1.0);
		ub.push_back(HUGE_VAL);
		scale.push_back(0.25*ms_opt_od_par.m_N_mc_guess);
		//scale.push_back(100.0);

		index++;
	}

	if( !ms_opt_od_par.m_fixed_N_t )
	{
		x.push_back(ms_opt_od_par.m_N_t_guess);
		lb.push_back(1.0);
		ub.push_back(HUGE_VAL);
		scale.push_back(100.0);

		index++;
	}

	m_W_dot_net_max = 0.0;
	bool solution_found = false;
	if(index > 0)		// need to call subplex
	{
		// Set up instance of nlopt class and set optimization parameters
		nlopt::opt		opt_od_cycle(nlopt::LN_SBPLX, index);
		opt_od_cycle.set_lower_bounds(lb);
		opt_od_cycle.set_upper_bounds(ub);
		opt_od_cycle.set_initial_step(scale);
		opt_od_cycle.set_xtol_rel(ms_opt_od_par.m_opt_tol);

		// Set max objective function
		opt_od_cycle.set_max_objective(nlopt_cb_opt_od, this);
		double max_f = std::numeric_limits<double>::quiet_NaN();
		nlopt::result   result_od_cycle = opt_od_cycle.optimize(x, max_f);

		int opt_od_error_code = 0;
		if(m_W_dot_net_max > 0.0)
		{
			ms_od_par = ms_od_par_optimal;
			off_design_core(opt_od_error_code);
			error_code = 0;
		}
		else
		{
			error_code = 111;
			return;
		}

		if( opt_od_error_code != 0 )
		{
			error_code = opt_od_error_code;
			return;
		}

	}
	else		// Just call off design subroutine (with fixed inputs)
	{
		double blah = 1.23;
	}

}

double C_RecompCycle::off_design_point_value(const std::vector<double> &x)
{
	// 'x' is array of inputs either being adjusted by optimizer or set constant
	// Finish defining 'ms_od_par' based on current 'x' values

	int index = 0;

	if( !ms_opt_od_par.m_fixed_P_mc_in )
	{
		ms_od_par.m_P_mc_in = x[index];
		index++;
	}
	else
		ms_od_par.m_P_mc_in = ms_opt_od_par.m_P_mc_in_guess;

	if( !ms_opt_od_par.m_fixed_recomp_frac )
	{
		ms_od_par.m_recomp_frac = x[index];
		index++;
	}
	else
		ms_od_par.m_recomp_frac = ms_opt_od_par.m_recomp_frac_guess;

	if( !ms_opt_od_par.m_fixed_N_mc )
	{
		ms_od_par.m_N_mc = x[index];
		index++;
	}
	else
		ms_od_par.m_N_mc = ms_opt_od_par.m_N_mc_guess;

	if( !ms_opt_od_par.m_fixed_N_t )
	{
		ms_od_par.m_N_t = x[index];
		index++;
	}
	else
		ms_od_par.m_N_t = ms_opt_od_par.m_N_t_guess;

	if( ms_od_par.m_N_t <= 0.0 )
		ms_od_par.m_N_t = ms_od_par.m_N_mc;		// link turbine and main compressor shafts

	// Check inputs
	if(ms_od_par.m_recomp_frac < 0.0)
	{
		return 0.0;
	}

	// Call off_design subroutine
		// 'ms_od_par' has been defined here and in 'optimal_off_design_core'
	int od_error_code = 0;
	off_design_core(od_error_code);

	if( od_error_code != 0 )
		return 0.0;

	double off_design_point_value = 0.0;
	if( ms_opt_od_par.m_is_max_W_dot )
		off_design_point_value = m_W_dot_net_od;
	else
		off_design_point_value = m_eta_thermal_od;

	// Hardcode some compressor checks to 'true', per John's code. Could revisit later
	bool surge_allowed = true;
	bool supersonic_tip_speed_allowed = true;
	
	// Check validity
	if( m_pres_od[2 - 1] > ms_des_par.m_P_high_limit )		// above high-pressure limit; provide optimizer with more information
	{
		//off_design_point_value = max(1.0, off_design_point_value / (10.0 + m_pres_od[2 - 1] - ms_des_par.m_P_high_limit, 4.0));
		double penalty = 5.0;
		//off_design_point_value = off_design_point_value * (1.0 - max(0.0, 1.0 - penalty*(ms_des_par.m_P_high_limit / m_pres_od[2 - 1])));
		off_design_point_value = off_design_point_value*(1.0 - penalty*max(0.0,(m_pres_od[2-1] - ms_des_par.m_P_high_limit)/ms_des_par.m_P_high_limit));
	}

	if(!surge_allowed)		// twn: Note that 'surge_allowed' is currently hardcoded to true so this won't be executed
	{
		if( m_mc.get_od_solved()->m_surge )
			off_design_point_value = 0.0;
		
		if( ms_od_par.m_recomp_frac > 0.0 && m_rc.get_od_solved()->m_surge )
			off_design_point_value = 0.0;
	}

	if(!supersonic_tip_speed_allowed)
	{
		double penalty = 5.0;

		if( m_mc.get_od_solved()->m_w_tip_ratio > 1.0 )
			off_design_point_value = fabs(off_design_point_value)*(1.0 - penalty*max(0.0, m_mc.get_od_solved()->m_w_tip_ratio - 1.0));

		if( ms_od_par.m_recomp_frac > 0.0 && m_rc.get_od_solved()->m_w_tip_ratio > 1.0 )
			off_design_point_value = fabs(off_design_point_value)*(1.0 - penalty*max(0.0, m_rc.get_od_solved()->m_w_tip_ratio - 1.0));

		if( m_t.get_od_solved()->m_w_tip_ratio > 1.0 )
			off_design_point_value = fabs(off_design_point_value)*(1.0 - penalty*max(0.0, m_t.get_od_solved()->m_w_tip_ratio - 1.0));
	}

	// Check if this is the optimal cycle?
	if(off_design_point_value > m_W_dot_net_max)
	{
		ms_od_par_optimal = ms_od_par;
		m_W_dot_net_max = off_design_point_value;
	}

	return off_design_point_value;

}

void C_RecompCycle::get_max_output_od(S_opt_target_od_parameters & opt_tar_od_par_in, int & error_code)
{
	ms_opt_tar_od_par = opt_tar_od_par_in;

	// Determine the largest possible power output of the cycle
	bool point_found = false;
	double P_low = ms_opt_tar_od_par.m_lowest_pressure;

	// Define known members of 'ms_opt_od_par' from 'ms_opt_tar_od_par'
	ms_opt_od_par.m_T_mc_in = ms_opt_tar_od_par.m_T_mc_in;
	ms_opt_od_par.m_T_t_in = ms_opt_tar_od_par.m_T_t_in;
	// .m_is_max_W_dot   --- need to define in loop
	ms_opt_od_par.m_N_sub_hxrs = ms_opt_tar_od_par.m_N_sub_hxrs;
	// m_P_mc_in_guess   --- need to define in loop
	// m_fixed_P_mc_in   --- should be 'false', but define in loop
	ms_opt_od_par.m_recomp_frac_guess = ms_opt_tar_od_par.m_recomp_frac_guess;
	ms_opt_od_par.m_fixed_recomp_frac = ms_opt_tar_od_par.m_fixed_recomp_frac;

	ms_opt_od_par.m_N_mc_guess = ms_opt_tar_od_par.m_N_mc_guess*1.25;		// twn: Start with assuming at max power the compressor speed will be greater than design
	ms_opt_od_par.m_fixed_N_mc = ms_opt_tar_od_par.m_fixed_N_mc;

	ms_opt_od_par.m_N_t_guess = ms_opt_tar_od_par.m_N_t_guess;
	ms_opt_od_par.m_fixed_N_t = ms_opt_tar_od_par.m_fixed_N_t;

	ms_opt_od_par.m_tol = ms_opt_tar_od_par.m_tol;
	ms_opt_od_par.m_opt_tol = ms_opt_tar_od_par.m_opt_tol;

	do
	{
		ms_opt_od_par.m_is_max_W_dot = true;
		ms_opt_od_par.m_P_mc_in_guess = P_low;
		ms_opt_od_par.m_fixed_P_mc_in = false;

		// Try fixing inlet pressure and see if f_recomp and N_mc are modified
		// ms_opt_od_par.m_P_mc_in_guess = 8700.0;
		// ms_opt_od_par.m_fixed_P_mc_in = true;

		int od_error_code = 0;

		optimal_off_design_core(od_error_code);

		if( od_error_code == 0 )
		{

			// Update guess parameters
			ms_opt_od_par.m_recomp_frac_guess = ms_od_par.m_recomp_frac;
			ms_opt_od_par.m_N_mc_guess = ms_od_par.m_N_mc;
			ms_opt_od_par.m_N_t_guess = ms_od_par.m_N_t;
			ms_opt_od_par.m_P_mc_in_guess = ms_od_par.m_P_mc_in;
			P_low = ms_od_par.m_P_mc_in;

			if( point_found )		// exit only after testing two starting points (prevents optimization near-misses)
				break;

			point_found = true;
		}
		else
		{
			P_low = 1.1*P_low;
		}
		

		if( P_low > ms_opt_tar_od_par.m_highest_pressure )
			break;

	} while( true );

	m_biggest_target = -999.9;

	if( !point_found )
	{
		error_code = 99;
		return;
	}

	if( ms_opt_tar_od_par.m_is_target_Q )
		m_biggest_target = m_Q_dot_PHX_od;
	else
		m_biggest_target = m_W_dot_net_od;

}

void C_RecompCycle::optimal_target_off_design_no_check(S_opt_target_od_parameters & opt_tar_od_par_in, int & error_code)
{
	ms_opt_tar_od_par = opt_tar_od_par_in;

	// Populate 'ms_tar_od_par' from info in 'ms_opt_tar_od_par'
	ms_tar_od_par.m_T_mc_in = ms_opt_tar_od_par.m_T_mc_in;
	ms_tar_od_par.m_T_t_in = ms_opt_tar_od_par.m_T_t_in;
	// ms_tar_od_par.m_recomp_frac ... Defined by optimizer
	// ms_tar_od_par.m_N_mc ... Defined by optimizer
	// ms_tar_od_par.m_N_t  ... Defined by optimizer
	ms_tar_od_par.m_N_sub_hxrs = ms_opt_tar_od_par.m_N_sub_hxrs;
	ms_tar_od_par.m_tol = ms_opt_tar_od_par.m_tol;
	ms_tar_od_par.m_target = ms_opt_tar_od_par.m_target;
	ms_tar_od_par.m_is_target_Q = ms_opt_tar_od_par.m_is_target_Q;
	ms_tar_od_par.m_lowest_pressure = ms_opt_tar_od_par.m_lowest_pressure;
	ms_tar_od_par.m_highest_pressure = ms_opt_tar_od_par.m_highest_pressure;

	ms_tar_od_par.m_use_default_res = ms_opt_tar_od_par.m_use_default_res;

	// Initialize guess array
	int index = 0;

	std::vector<double> x(0);
	std::vector<double> lb(0);
	std::vector<double> ub(0);
	std::vector<double> scale(0);

	if( !ms_opt_tar_od_par.m_fixed_recomp_frac )
	{
		x.push_back(ms_opt_tar_od_par.m_recomp_frac_guess);
		lb.push_back(0.0);
		ub.push_back(1.0);
		scale.push_back(0.01);

		index++;
	}

	if( !ms_opt_tar_od_par.m_fixed_N_mc )
	{
		x.push_back(ms_opt_tar_od_par.m_N_mc_guess);
		lb.push_back(1.0);
		ub.push_back(HUGE_VAL);
		scale.push_back(0.25*ms_opt_tar_od_par.m_N_mc_guess);

		index++;
	}

	if( !ms_opt_tar_od_par.m_fixed_N_t )
	{
		x.push_back(ms_opt_tar_od_par.m_N_t_guess);
		lb.push_back(1.0);
		ub.push_back(HUGE_VAL);
		scale.push_back(100.0);
	}

	bool solution_found = false;
	m_eta_best = 0.0;

	if( index > 0 )
	{
		// Set up instance of nlopt class and set optimization parameters
		nlopt::opt		opt_tar_od_cycle(nlopt::LN_SBPLX, index);
		opt_tar_od_cycle.set_lower_bounds(lb);
		opt_tar_od_cycle.set_upper_bounds(ub);
		opt_tar_od_cycle.set_initial_step(scale);
		opt_tar_od_cycle.set_xtol_rel(ms_opt_tar_od_par.m_opt_tol);

		// Set max objective function
		opt_tar_od_cycle.set_max_objective(nlopt_cb_eta_at_target, this);
		double max_f = std::numeric_limits<double>::quiet_NaN();
		nlopt::result     result_tar_od_cycle = opt_tar_od_cycle.optimize(x, max_f);
	}
	else
	{
		eta_at_target(x);
	}

	// Final call to off-design model using 'ms_od_par_tar_optimal'
	int od_error_code = 0;
	if( m_eta_best > 0.0 )
	{
		ms_od_par = ms_od_par_tar_optimal;
		off_design_core(od_error_code);
		error_code = 0;
	}
	else
	{
		error_code = 98;
		return;
	}

	if( od_error_code != 0 )
	{
		error_code = od_error_code;
		return;
	}
}

void C_RecompCycle::optimal_target_off_design(S_opt_target_od_parameters & opt_tar_od_par_in, int & error_code)
{
	int error_code_local = 0;

	if( !opt_tar_od_par_in.m_is_target_Q )		// If W_dot_net is target, can check max value. This exercise isn't useful for q_dot_in target
	{

		get_max_output_od(opt_tar_od_par_in, error_code_local);

		if( error_code_local != 0 )
		{
			error_code = error_code_local;
			return;
		}

		// If the target is not possible, return the cycle with the largest (based on power output)
		if( m_biggest_target < ms_opt_tar_od_par.m_target )
		{
			error_code = 123;
			return;
		}
	}

	optimal_target_off_design_no_check(opt_tar_od_par_in, error_code_local);

	if(error_code_local != 0)
	{
		error_code = error_code_local;
		return;
	}

	return;

	/*
	ms_opt_tar_od_par = opt_tar_od_par_in;

	// Determine the largest possible power output of the cycle
	bool point_found = false;
	double P_low = ms_opt_tar_od_par.m_lowest_pressure;

	// Define known members of 'ms_opt_od_par' from 'ms_opt_tar_od_par'
	ms_opt_od_par.m_T_mc_in = ms_opt_tar_od_par.m_T_mc_in;
	ms_opt_od_par.m_T_t_in = ms_opt_tar_od_par.m_T_t_in;
		// .m_is_max_W_dot   --- need to define in loop
	ms_opt_od_par.m_N_sub_hxrs = ms_opt_tar_od_par.m_N_sub_hxrs;
		// m_P_mc_in_guess   --- need to define in loop
		// m_fixed_P_mc_in   --- should be 'false', but define in loop
	ms_opt_od_par.m_recomp_frac_guess = ms_opt_tar_od_par.m_recomp_frac_guess;
	ms_opt_od_par.m_fixed_recomp_frac = ms_opt_tar_od_par.m_fixed_recomp_frac;

	ms_opt_od_par.m_N_mc_guess = ms_opt_tar_od_par.m_N_mc_guess*1.25;		// twn: Start with assuming at max power the compressor speed will be greater than design
	ms_opt_od_par.m_fixed_N_mc = ms_opt_tar_od_par.m_fixed_N_mc;

	ms_opt_od_par.m_N_t_guess = ms_opt_tar_od_par.m_N_t_guess;
	ms_opt_od_par.m_fixed_N_t = ms_opt_tar_od_par.m_fixed_N_t;

	ms_opt_od_par.m_tol = ms_opt_tar_od_par.m_tol;
	ms_opt_od_par.m_opt_tol = ms_opt_tar_od_par.m_opt_tol;

	do
	{
		ms_opt_od_par.m_is_max_W_dot = true;
		ms_opt_od_par.m_P_mc_in_guess = P_low;
		ms_opt_od_par.m_fixed_P_mc_in = false;

		// Try fixing inlet pressure and see if f_recomp and N_mc are modified
		// ms_opt_od_par.m_P_mc_in_guess = 8700.0;
		// ms_opt_od_par.m_fixed_P_mc_in = true;

		int od_error_code = 0;

		optimal_off_design_core(od_error_code);

		if(od_error_code == 0)
		{									

			// Update guess parameters
			ms_opt_od_par.m_recomp_frac_guess = ms_od_par.m_recomp_frac;
			ms_opt_od_par.m_N_mc_guess = ms_od_par.m_N_mc;
			ms_opt_od_par.m_N_t_guess = ms_od_par.m_N_t;
			ms_opt_od_par.m_P_mc_in_guess = ms_od_par.m_P_mc_in;

			if( point_found )		// exit only after testing two starting points (prevents optimization near-misses)
				break;

			point_found = true; 
		}

		P_low = 1.1*ms_opt_od_par.m_P_mc_in_guess;

		if( P_low > ms_opt_tar_od_par.m_highest_pressure )
			break;

	} while( true );

	m_biggest_target = -999.9;

	if( !point_found )
	{
		error_code = 99;
		return;
	}

	if( ms_opt_tar_od_par.m_is_target_Q )
		m_biggest_target = m_Q_dot_PHX_od;
	else
		m_biggest_target = m_W_dot_net_od;

	// If the target is not possible, return the cycle with the largest (based on power output)
	if( m_biggest_target < ms_opt_tar_od_par.m_target )
	{
		error_code = 123;
		return;
	}

	// Populate 'ms_tar_od_par' from info in 'ms_opt_tar_od_par'
	ms_tar_od_par.m_T_mc_in = ms_opt_tar_od_par.m_T_mc_in;
	ms_tar_od_par.m_T_t_in = ms_opt_tar_od_par.m_T_t_in;
		// ms_tar_od_par.m_recomp_frac ... Defined by optimizer
		// ms_tar_od_par.m_N_mc ... Defined by optimizer
		// ms_tar_od_par.m_N_t  ... Defined by optimizer
	ms_tar_od_par.m_N_sub_hxrs = ms_opt_tar_od_par.m_N_sub_hxrs;
	ms_tar_od_par.m_tol = ms_opt_tar_od_par.m_tol;
	ms_tar_od_par.m_target = ms_opt_tar_od_par.m_target;
	ms_tar_od_par.m_is_target_Q = ms_opt_tar_od_par.m_is_target_Q;
	ms_tar_od_par.m_lowest_pressure = ms_opt_tar_od_par.m_lowest_pressure;
	ms_tar_od_par.m_highest_pressure = ms_opt_tar_od_par.m_highest_pressure;

	// Initialize guess array
	int index = 0;

	std::vector<double> x(0);
	std::vector<double> lb(0);
	std::vector<double> ub(0);
	std::vector<double> scale(0);

	if(!ms_opt_tar_od_par.m_fixed_recomp_frac)
	{
		x.push_back(ms_opt_tar_od_par.m_recomp_frac_guess);
		lb.push_back(0.0);
		ub.push_back(1.0);
		scale.push_back(0.01);

		index++;
	}

	if(!ms_opt_tar_od_par.m_fixed_N_mc)
	{
		x.push_back(ms_opt_tar_od_par.m_N_mc_guess);
		lb.push_back(1.0);
		ub.push_back(HUGE_VAL);
		scale.push_back(0.25*ms_opt_tar_od_par.m_N_mc_guess);

		index++;
	}

	if(!ms_opt_tar_od_par.m_fixed_N_t)
	{
		x.push_back(ms_opt_tar_od_par.m_N_t_guess);
		lb.push_back(1.0);
		ub.push_back(HUGE_VAL);
		scale.push_back(100.0);
	}

	bool solution_found = false;
	m_eta_best = 0.0;

	if(index > 0)
	{
		// Set up instance of nlopt class and set optimization parameters
		nlopt::opt		opt_tar_od_cycle(nlopt::LN_SBPLX, index);
		opt_tar_od_cycle.set_lower_bounds(lb);
		opt_tar_od_cycle.set_upper_bounds(ub);
		opt_tar_od_cycle.set_initial_step(scale);
		opt_tar_od_cycle.set_xtol_rel(ms_opt_tar_od_par.m_opt_tol);

		// Set max objective function
		opt_tar_od_cycle.set_max_objective(nlopt_cb_eta_at_target, this);
		double max_f = std::numeric_limits<double>::quiet_NaN();
		nlopt::result     result_tar_od_cycle = opt_tar_od_cycle.optimize(x, max_f);
	}
	else
	{
		eta_at_target(x);
	}

	// Final call to off-design model using 'ms_od_par_tar_optimal'
	int od_error_code = 0;
	if(m_eta_best > 0.0)
	{
		ms_od_par = ms_od_par_tar_optimal;
		off_design_core(od_error_code);
		error_code = 0;
	}
	else
	{
		error_code = 98;
		return;
	}

	if(od_error_code != 0)
	{
		error_code = od_error_code;
		return;
	}
	*/
}

double C_RecompCycle::eta_at_target(const std::vector<double> &x)
{
	// Get input variables from 'x'
	int index = 0;

	if( !ms_opt_tar_od_par.m_fixed_recomp_frac )
	{
		ms_tar_od_par.m_recomp_frac = x[index];
		index++;
	}
	else
		ms_tar_od_par.m_recomp_frac = ms_opt_tar_od_par.m_recomp_frac_guess;

	if( !ms_opt_tar_od_par.m_fixed_N_mc )
	{
		ms_tar_od_par.m_N_mc = x[index];
		index++;
	}
	else
		ms_tar_od_par.m_N_mc = ms_opt_tar_od_par.m_N_mc_guess;

	if( !ms_opt_tar_od_par.m_fixed_N_t )
	{
		ms_tar_od_par.m_N_t = x[index];
		index++;
	}
	else
		ms_tar_od_par.m_N_t = ms_opt_tar_od_par.m_N_t_guess;

	if( ms_tar_od_par.m_N_t <= 0.0 )
		ms_tar_od_par.m_N_t = ms_tar_od_par.m_N_mc;

	// Check inputs
	if( ms_tar_od_par.m_recomp_frac < 0.0 )
	{
		return 0.0;
	}

	int target_od_error_code = 0;

	// Call target_off_design subroutine
	target_off_design_core(target_od_error_code);

	double eta_at_target = std::numeric_limits<double>::quiet_NaN();
	if(target_od_error_code==26)
	{
		//return 1.0 / (100.0 + m_W_dot_net_od);
		return 0.0;
	}
	else if(target_od_error_code != 0)
	{
		return 0.0;
	}
	else
		eta_at_target = m_eta_thermal_od;

	// Check validity
	if( m_pres_od[2 - 1] > ms_des_par.m_P_high_limit )		// above high-pressure limit; provide optimizer with more information
	{
		//off_design_point_value = max(1.0, off_design_point_value / (10.0 + m_pres_od[2 - 1] - ms_des_par.m_P_high_limit, 4.0));
		double penalty = 5.0;
		//off_design_point_value = off_design_point_value * (1.0 - max(0.0, 1.0 - penalty*(ms_des_par.m_P_high_limit / m_pres_od[2 - 1])));
		eta_at_target = eta_at_target*(1.0 - penalty*max(0.0, (m_pres_od[2 - 1] - ms_des_par.m_P_high_limit) / ms_des_par.m_P_high_limit));
	}

	// Hardcode some compressor checks to 'true', per John's code. Could revisit later
	bool surge_allowed = true;
	bool supersonic_tip_speed_allowed = true;

	if( !surge_allowed )		// twn: Note that 'surge_allowed' is currently hardcoded to true so this won't be executed
	{
		if( m_mc.get_od_solved()->m_surge )
			eta_at_target = 0.0;

		if( ms_od_par.m_recomp_frac > 0.0 && m_rc.get_od_solved()->m_surge )
			eta_at_target = 0.0;
	}

	if( !supersonic_tip_speed_allowed )
	{
		double penalty = 5.0;

		if( m_mc.get_od_solved()->m_w_tip_ratio > 1.0 )
			eta_at_target = fabs(eta_at_target)*(1.0 - penalty*max(0.0, m_mc.get_od_solved()->m_w_tip_ratio - 1.0));

		if( ms_od_par.m_recomp_frac > 0.0 && m_rc.get_od_solved()->m_w_tip_ratio > 1.0 )
			eta_at_target = fabs(eta_at_target)*(1.0 - penalty*max(0.0, m_rc.get_od_solved()->m_w_tip_ratio - 1.0));

		if( m_t.get_od_solved()->m_w_tip_ratio > 1.0 )
			eta_at_target = fabs(eta_at_target)*(1.0 - penalty*max(0.0, m_t.get_od_solved()->m_w_tip_ratio - 1.0));
	}

	// Check if this is the optimal cycle?
	if( eta_at_target > m_eta_best)
	{
		ms_od_par_tar_optimal = ms_od_par;
		m_eta_best = eta_at_target;
	}

	return eta_at_target;
}

void C_RecompCycle::opt_od_eta_for_hx(S_od_parameters & od_par_in, S_PHX_od_parameters phx_od_par_in, int & error_code)
{
	ms_od_par = od_par_in;
	ms_phx_od_par = phx_od_par_in;

	// Set up 3-D optimization in NLOPT
	std::vector<double> x(0);
	std::vector<double> lb(0);
	std::vector<double> ub(0);
	std::vector<double> scale(0);
	int index = 0;

	// Inlet pressure
	//x.push_back(ms_des_solved.m_pres[1 - 1]);
	x.push_back(1000.0);
	lb.push_back(1000.0);		// This must be set to a variable somewhere?
	ub.push_back(17000.0);		// This also must be set somewhere?
	scale.push_back(4000.0);	// Solution is probably less than design pressure
	index++;

	// Recompression Fraction
	if( ms_des_solved.m_is_rc )
	{
		x.push_back(ms_des_solved.m_recomp_frac);
		lb.push_back(0.0);
		ub.push_back(1.0);
		scale.push_back(-.02);
		index++;
	}

	m_found_opt = false;
	m_eta_phx_max = 0.0;

	// Compressor Speed
	x.push_back(ms_des_solved.ms_mc_des_solved.m_N_design);
	lb.push_back(ms_des_solved.ms_mc_des_solved.m_N_design*0.1);
	ub.push_back(ms_des_solved.ms_mc_des_solved.m_N_design*1.5);
	scale.push_back(ms_des_solved.ms_mc_des_solved.m_N_design*0.1);
	index++;

	// Save initial vectors
	std::vector<double> x_base = x;
	std::vector<double> lb_base = lb;
	std::vector<double> ub_base = ub;
	std::vector<double> sc_base = scale;

	// Set up instance of nlopt class and set optimization parameters
	nlopt::opt          opt_od_cycle(nlopt::LN_SBPLX, index);
	opt_od_cycle.set_lower_bounds(lb);
	opt_od_cycle.set_upper_bounds(ub);
	opt_od_cycle.set_initial_step(scale);
	opt_od_cycle.set_xtol_rel(ms_des_par.m_tol);

	// Set max objective function
	opt_od_cycle.set_max_objective(nlopt_cb_opt_od_eta, this);
	double max_f = std::numeric_limits<double>::quiet_NaN();
	nlopt::result       result_od_cycle = opt_od_cycle.optimize(x, max_f);

	int od_error_code = 0;

	if( !m_found_opt )
	{
		x = x_base;
		lb = lb_base;
		ub = ub_base;
		scale = sc_base;

		x[index-1] = ms_des_solved.ms_mc_des_solved.m_N_design*1.5;
		lb[index-1] = ms_des_solved.ms_mc_des_solved.m_N_design*0.5;
		ub[index-1] = ms_des_solved.ms_mc_des_solved.m_N_design*1.75;
		scale[index-1] = -ms_des_solved.ms_mc_des_solved.m_N_design*0.1;
		
		opt_od_cycle.set_lower_bounds(lb);
		opt_od_cycle.set_upper_bounds(ub);
		opt_od_cycle.set_initial_step(scale);

		max_f = 0.0;

		result_od_cycle = opt_od_cycle.optimize(x, max_f);

		if( !m_found_opt )
		{
			od_error_code = 1;
		}
	}

	index = 0;
	ms_od_par.m_P_mc_in = x[index];
	index++;

	ms_od_par.m_recomp_frac = 0.0;
	if( ms_des_solved.m_is_rc )
	{
		ms_od_par.m_recomp_frac = x[index];
		index++;
	}

	ms_od_par.m_N_mc = x[index];
	index++;

	off_design_core(od_error_code);

	if( od_error_code != 0 )
	{
		error_code = od_error_code;
		return;
	}
		
}

double C_RecompCycle::opt_od_eta(const std::vector<double> &x)
{
	CO2_state co2_props;
	
	int index = 0;

	ms_od_par.m_P_mc_in = x[index];
	index++;

	ms_od_par.m_recomp_frac = 0.0;
	if( ms_des_solved.m_is_rc )
	{
		ms_od_par.m_recomp_frac = x[index];
		index++;
	}

	ms_od_par.m_N_mc = x[index];
	index++;

	// ms_od_par.m_P_mc_in = 3438.0;
	// ms_od_par.m_recomp_frac = 0.0;
	// ms_od_par.m_N_mc = 10244.0;

	// return ms_od_par.m_P_mc_in + ms_od_par.m_recomp_frac + ms_od_par.m_N_mc;

	double T_t_in_upper = ms_phx_od_par.m_T_htf_hot - 0.01;
	bool know_T_in_upper = false;
	
	double T_t_in_lower = ms_phx_od_par.m_T_htf_hot - 50.0;
	bool know_T_in_lower = false;

	double T_t_in_guess = ms_od_par.m_T_t_in;

	double diff_T_t_in = ms_od_par.m_tol*2.0;

	int od_error_code = 0;

	double Q_dot_PHX = 0.0;
	double C_dot_htf = 0.0;
	double T_t_in_calc = 0.0;

	for( int iter_T_t_in = 0; fabs(diff_T_t_in) > ms_od_par.m_tol && iter_T_t_in < 50; iter_T_t_in++ )
	{

		if(iter_T_t_in > 0)	// Guess new T_t_in.... diff_T_t_in = (T_t_in_calc - T_t_in_guess) / T_t_in_guess;
		{
			if(od_error_code != 0)
			{
				T_t_in_lower = T_t_in_guess;
				know_T_in_lower = true;
				T_t_in_guess = 0.5*(T_t_in_lower + T_t_in_guess);
			}
			else
			{
				if( diff_T_t_in > 0.0 )	// T_t_in_guess too small
				{
					T_t_in_lower = T_t_in_guess;
					know_T_in_lower = true;
					if( know_T_in_upper )
						T_t_in_guess = 0.5*(T_t_in_lower + T_t_in_upper);
					else
						T_t_in_guess = T_t_in_upper;
				}
				else
				{
					T_t_in_upper = T_t_in_guess;
					know_T_in_upper = true;
					if( know_T_in_lower )
						T_t_in_guess = 0.5*(T_t_in_lower + T_t_in_upper);
					else
						T_t_in_guess = T_t_in_guess - 10.0;
				}
			}
		}

		if(fabs(T_t_in_upper-T_t_in_lower) < 0.1)
		{
			break;
		}

		ms_od_par.m_T_t_in = T_t_in_guess;

		od_error_code = 0;

		off_design_core(od_error_code);

		if( od_error_code != 0 && iter_T_t_in == 0 )
			return 0.0;
		else if( od_error_code != 0 )
			continue;

		// Get off design values for PHX calcs
		double m_dot_PHX = ms_od_solved.m_m_dot_t;
		double T_PHX_in = ms_od_solved.m_temp[5 - 1];

		//double Q_dot_PHX = ms_od_solved.m_Q_dot;

		// Calculate off-design UA
		double m_dot_ratio = 0.5*(ms_phx_od_par.m_m_dot_htf / ms_phx_od_par.m_m_dot_htf_des + m_dot_PHX / ms_des_solved.m_m_dot_t);
		double UA_PHX_od = ms_phx_od_par.m_UA_PHX_des*pow(m_dot_ratio, 0.8);

		double C_dot_co2 = m_dot_PHX*(ms_od_solved.m_enth[6 - 1] - ms_od_solved.m_enth[5 - 1]) /
			(ms_od_solved.m_temp[6 - 1] - ms_od_solved.m_temp[5 - 1]);	//[kW/K]

		C_dot_htf = ms_phx_od_par.m_cp_htf*ms_phx_od_par.m_m_dot_htf;

		double C_dot_min = min(C_dot_co2, C_dot_htf);
		double C_dot_max = max(C_dot_co2, C_dot_htf);

		double C_R = C_dot_min / C_dot_max;

		double NTU = UA_PHX_od / C_dot_min;

		double eff = 0.0;
		if(C_R < 1.0)
			eff = (1.0 - exp(-NTU*(1.0-C_R)))/(1.0 - C_R*exp(-NTU*(1.0-C_R)));
		else
			eff = NTU / (1.0 + NTU);

		Q_dot_PHX = eff * (C_dot_min*(ms_phx_od_par.m_T_htf_hot - T_PHX_in));

		//m_dot(h_t_in - h_phx_in) = Q_dot_PHX
		double h_t_in = ms_od_solved.m_enth[5 - 1] + Q_dot_PHX / m_dot_PHX;

		CO2_PH(ms_od_solved.m_pres[6-1], h_t_in, &co2_props);

		T_t_in_calc = co2_props.temp;

		double T_htf_cold = ms_phx_od_par.m_T_htf_hot - Q_dot_PHX / C_dot_htf;

		diff_T_t_in = (T_t_in_calc - T_t_in_guess) / T_t_in_guess;
	}

	double T_htf_cold = ms_phx_od_par.m_T_htf_hot - Q_dot_PHX / C_dot_htf;

	double eta_thermal = ms_od_solved.m_eta_thermal;

	double diff_T_cold = max(0.0, fabs(ms_phx_od_par.m_T_htf_cold - T_htf_cold) / T_htf_cold - ms_od_par.m_tol);
	//diff_T_cold = 0.0;	// overwrite for now...

	double over_deltaT = max(0.0, fabs(diff_T_t_in) - ms_od_par.m_tol);

	double over_deltaP = max(0.0, ms_od_solved.m_pres[2-1] - ms_des_par.m_P_high_limit);

	double eta_return = eta_thermal*exp(-diff_T_cold)*exp(-over_deltaP)*exp(-over_deltaT);

	if( diff_T_cold == 0.0 && over_deltaT == 0.0 && over_deltaP == 0.0 )
		m_found_opt = true;

	if( eta_return > m_eta_phx_max )
	{
		m_eta_phx_max = eta_return;
		m_over_deltaP_eta_max = over_deltaP;
		m_UA_diff_eta_max = diff_T_t_in;
	}

	return eta_return;
}

double fmin_callback_opt_eta_1(double x, void *data)
{
	C_RecompCycle *frame = static_cast<C_RecompCycle*>(data);

	return frame->opt_eta(x);
}

double nlopt_callback_opt_des_1(const std::vector<double> &x, std::vector<double> &grad, void *data)
{
	C_RecompCycle *frame = static_cast<C_RecompCycle*>(data);
	if( frame != NULL ) return frame->design_point_eta(x);
}

double nlopt_cb_opt_od(const std::vector<double> &x, std::vector<double> &grad, void *data)
{
	C_RecompCycle *frame = static_cast<C_RecompCycle*>(data);
	if( frame != NULL ) return frame->off_design_point_value(x);
}

double nlopt_cb_eta_at_target(const std::vector<double> &x, std::vector<double> &grad, void *data)
{
	C_RecompCycle *frame = static_cast<C_RecompCycle*>(data);
	if( frame != NULL ) return frame->eta_at_target(x);
}

double nlopt_cb_opt_od_eta(const std::vector<double> &x, std::vector<double> &grad, void *data)
{
	C_RecompCycle *frame = static_cast<C_RecompCycle*>(data);
	if( frame != NULL ) return frame->opt_od_eta(x);
}

double P_pseudocritical_1(double T_K)
{
	return (0.191448*T_K + 45.6661)*T_K - 24213.3;
}





bool C_poly_curve_r_squared::init(const std::vector<double> x_data, const std::vector<double> y_data)
{
	m_x = x_data;
	m_y = y_data;

	m_n_points = x_data.size();
	if(m_n_points != y_data.size() || m_n_points < 5)
	{
		return false;
	}

	m_y_bar = 0.0;

	for( int i = 0; i < m_n_points; i++ )
	{
		m_y_bar += m_y[i];
	}

	m_y_bar /= (double)m_n_points;

	m_SS_tot = 0.0;

	for( int i = 0; i < m_n_points; i++ )
	{
		m_SS_tot += pow(m_y[i] - m_y_bar, 2);
	}

	return true;
}

double C_poly_curve_r_squared::calc_r_squared(const std::vector<double> coefs)
{
	double SS_res = 0.0;
	int n_coefs = coefs.size();
	double y_pred = 0.0;
	for( int i = 0; i < m_n_points; i++ )
	{
		y_pred = 0.0;
		for( int j = 0; j < n_coefs; j++ )
		{
			y_pred += coefs[j] * pow(m_x[i], j);
		}
		SS_res += pow(m_y[i] - y_pred, 2);
	}

	return 1.0 - SS_res / m_SS_tot;
}

double nlopt_callback_poly_coefs(const std::vector<double> &x, std::vector<double> &grad, void *data)
{
	C_poly_curve_r_squared *frame = static_cast<C_poly_curve_r_squared*>(data);
	if( frame != NULL ) return frame->calc_r_squared(x);
}

bool find_polynomial_coefs(const std::vector<double> x_data, const std::vector<double> y_data, int n_coefs, std::vector<double> & coefs_out, double & r_squared)
{
	C_poly_curve_r_squared mc_data;

	if( n_coefs < 1 || n_coefs > 5)
	{
		return false;
	}
	else
	{
		coefs_out.resize(n_coefs);
		for( int i = 0; i < n_coefs; i++ )
		{
			coefs_out[i] = std::numeric_limits<double>::quiet_NaN();
		}
	}

	if( !mc_data.init(x_data, y_data) )
	{
		return false;
	}

	std::vector<double> x(n_coefs);

	bool solution_found = false;
	
	// Set up instance of nlopt class and set optimization parameters
		// nlopt::opt surf(nlopt::LN_NELDERMEAD, nbeta); from Autopilot_api.cpp
	nlopt::opt		opt_tar_od_cycle(nlopt::LN_NELDERMEAD, n_coefs);
	opt_tar_od_cycle.set_xtol_rel(0.00001);

	// Set max objective function
	opt_tar_od_cycle.set_max_objective(nlopt_callback_poly_coefs, &mc_data);
	double max_f = std::numeric_limits<double>::quiet_NaN();
	nlopt::result     result_tar_od_cycle = opt_tar_od_cycle.optimize(x, max_f);


	if( max_f > 0.01 && max_f <= 1.00 )
	{
		for( int i = 0; i < n_coefs; i++ )
		{
			coefs_out[i] = x[i];
		}

		r_squared = max_f;

		return true;
	}
	else
	{
		r_squared = -999.9;

		return false;
	}

}
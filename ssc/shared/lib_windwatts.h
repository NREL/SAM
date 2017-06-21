#ifndef __lib_windwatts_h
#define __lib_windwatts_h

#include <vector>
#include "lib_util.h"

enum {PAT_QUINLAN_WAKE_MODEL, PARK_WAKE_MODEL, SIMPLE_EDDY_VISCOSITY_WAKE_MODEL, OLD_PQ};

#ifdef _DEBUG
	#define IMITATE_OPENWIND true // true or false, depending on whether or not you want to compare wind wake models with openWind
#else
	#define IMITATE_OPENWIND false // don't set this to true in release mode, it might show users unintelligible error messages
#endif

class wind_power_calculator
{
public:
	wind_power_calculator() {
		m_dShearExponent = 1.0/7.0;
		m_iNumberOfTurbinesInFarm=m_iLengthOfTurbinePowerCurveArray=m_iControlMode=0;
		m_dMeasurementHeight=m_dHubHeight=m_dRotorDiameter=m_dCutInSpeed=m_dRatedSpeed=m_dRatedPower=m_dLossesAbsolute=m_dLossesPercent=0;
		m_sErrDetails="";

		// private member vars, acting like constants right now (no access to change them)
		m_dAxialResolution = 0.5; // in rotor diameters, default in openWind=0.5
		m_dRadialResultion = 0.2; // in rotor diameters, default in openWind=0.2
		m_dMaxRotorDiameters = 50; // in rotor diameters, default in openWind=50
		m_dMinThrustCoeff = 0.02;
		m_dMinDeficit=0.0002;
		m_bFilter = true;
		m_iNumberOfBlades = 3;
	}
	
	static const int MAX_WIND_TURBINES = 300; // Max turbines in the farm
	static const int MIN_DIAM_EV = 2; // Minimum number of rotor diameters between turbines for EV wake modeling to work
	static const int EV_SCALE = 1; // Uo or 1.0 depending on how you read Ainslie 1988


	double m_dShearExponent;		// also referred to as Alpha
	double m_dTurbulenceIntensity;	// also referred to as Sigma
	size_t m_iNumberOfTurbinesInFarm;
	size_t m_iLengthOfTurbinePowerCurveArray;
	double m_dMeasurementHeight;	// height at which the reource data was measured
	double m_dHubHeight;			// hub height of turbines (m)
	double m_dRotorDiameter;		// rotor diameter (m)
	int m_iControlMode;				// control mode 0=pitch, 1=variable, 2=simple/stall
	int m_iWakeModelChoice;			// 0=Original Model (based on Pat Quinlan's thesis), 1=Park Model, 2=Eddy viscosity model
	double m_dCutInSpeed;			// wind speed Cut in (m/s)
	double m_dRatedSpeed;			// rated wind speed
	double m_dRatedPower;			// rated power (kw)
	double m_dLossesAbsolute;		// constant loss
	double m_dLossesPercent;		// loss as percent
	double m_dWakeDecayCoefficient; // wake decay coefficient (k)

	std::vector<double> m_adPowerCurveWS, m_adPowerCurveKW, m_adPowerCurveRPM, m_adXCoords, m_adYCoords, m_adDensityCorrectedWS;

	size_t GetMaxTurbines() {return MAX_WIND_TURBINES;}
	bool InitializeModel(); // if necessary, allocate memory in util::matrix arrays
	std::string GetWakeModelShortName();
	std::string GetWakeModelName();
	std::string GetErrorDetails() { return m_sErrDetails; }

	int wind_power(
		// INPUTS
			double Vel_T,    // wind velocity m/s
			double Theta_T,  // wind direction 0-360, 0=N
			double BarPAtm,  // barometric pressure (Atm)
			double TdryC,    // dry bulb temp ('C)
			
		// OUTPUTS
			double *FarmP,    // total farm power output
			double aPower[],  // calculated power of each WT
			double aThrust[], // thrust calculation at each WT
			double aEff[],    // downwind efficiency of each WT
			double aWind[],   // wind speed at each WT
			double aTurbul[], // turbulence coeff at each WT
			double aDistDown[], // distance down wind
			double aDistCross[] // distance cross wind
		);

	double turbine_output_using_weibull(
		double weibull_k, 
		double max_cp, 
		double avg_speed, 
		double ref_height,
		double energy_turbine[]
	);

private:
	std::string m_sErrDetails;
	util::matrix_t<double> matEVWakeDeficits; // wind velocity deficit behind each turbine, indexed by axial distance downwind
	util::matrix_t<double> matEVWakeWidths; // width of wake (in diameters) for each turbine, indexed by axial distance downwind

	// private variables acting like constants
	double m_dAxialResolution;	// resolution for EV wake calculations along axial direction
	double m_dRadialResultion;	// resolution for EV wake calculations along radial direction
	double m_dMaxRotorDiameters; // how far down wind will EV calculations go
	double m_dMinThrustCoeff;	// limit lower bound of thrust coeff (Ct) for calculations
	bool m_bFilter;
	double m_dMinDeficit; // below this value, the simple EV model stops calculating deficits, so they are let at zero
	int m_iNumberOfBlades;

	struct VMLN
	{
		VMLN(){}
		virtual ~VMLN(){}
	
		double m;
		double Ro;
		double Xh;
		double Xn;
		double Rh;
		double Rn;
		double Xf;
		double Rf;
		double dUc_Uinf_Xn;
		double diam;
	};

	void wake_calculations_pat_quinlan_mod(
		/*INPUTS*/
		double fAir_density,
		double aDistanceDownwind[],			// downwind coordinate of each WT
		double aDistanceCrosswind[],		// crosswind coordinate of each WT

		/*OUTPUTS*/
		double Power[],						// calculated power of each WT
		double Thrust[],					// thrust calculation at each WT
		double Eff[],						// downwind efficiency of each WT
		double aWind_speed[],				// wind speed at each WT
		double aTurbulence_intensity[]		// turbulence intensity at each WT
	);

	void wake_calculations_Park(
		/*INPUTS*/
		double fAir_density,
		double aDistanceDownwind[],			// downwind coordinate of each WT
		double aDistanceCrosswind[],		// crosswind coordinate of each WT

		/*OUTPUTS*/
		double Power[],						// calculated power of each WT
		double Thrust[],					// thrust calculation at each WT
		double Eff[],						// downwind efficiency of each WT
		double aWind_speed[]				// wind speed at each WT
	);

	 // Implements a simplified Eddy-Viscosity model as per "Simplified Soultion To The Eddy Viscosity Wake Model" - 2009 by Dr Mike Anderson of RES
	bool wake_calculations_EddyViscosity_Simple(
		/*INPUTS*/
		double fAir_density,
		double aDistanceDownwind[],			// downwind coordinate of each WT
		double aDistanceCrosswind[],		// crosswind coordinate of each WT

		/*OUTPUTS*/
		double Power[],						// calculated power of each WT
		double Thrust[],					// thrust calculation at each WT
		double Eff[],						// downwind efficiency of each WT
		double aWind_speed[],				// wind speed at each WT
		double aTurbulence_intensity[]		// turbulence intensity at each WT
	);

	// original Pat Quinlan model, before other wake models added
	void wake_calculations_pat_quinlan_old(
		/*INPUTS*/
		double fAir_density,
		double aDistanceDownwind[],			// downwind coordinate of each WT
		double aDistanceCrosswind[],		// crosswind coordinate of each WT

		/*OUTPUTS*/
		double Power[],						// calculated power of each WT
		double Thrust[],					// thrust calculation at each WT
		double Eff[],						// downwind efficiency of each WT
		double aWind_speed[],				// wind speed at each WT
		double aTurbulence_intensity[]		// turbulence intensity at each WT
	);


	double get_EV_wake_width(int iUpwindTurbine, double dAxialDistanceInDiameters);
	double get_EV_velocity_deficit(int iUpwindTurbine, double dAxialDistanceInDiameters);
	double wake_deficit_EV(int iUpwindTurbine, double dDistCrossWind, double dDistDownWind);
	double calc_EV_added_turbulence_intensity(double IatUpstreamTurbine, double Ct,double deltaX, VMLN& vmln);
	double calc_EV_total_turbulence_intensity(double ambientTI, double additionalTI, double Uo, double Uw, double partial);
	bool fill_turbine_wake_arrays_for_EV(int iTurbineNumber, double dAmbientVelocity, double dVelocityAtTurbine, double dPower, double dThrustCoeff, double dTurbulenceIntensity, double maxX);
	void calc_EV_vm_for_turbine(double U, double Ii, double Ct, double airDensity, VMLN& vmln);
	double tip_speed_ratio(double dWindSpeed);

	void turbine_power( double fWindVelocityAtDataHeight, double fAirDensity, double *fTurbineOutput, double *fThrustCoefficient);
	double vel_delta_PQ( double fRadiiCrosswind, double fRadiiDownwind, double fThrustCoeff, double *fNewTurbulenceIntensity);
	double delta_V_Park( double dVelFreeStream, double dVelUpwind, double dDistCrossWind, double dDistDownWind, double dRadiusUpstream, double dRadiusDownstream, double dThrustCoeff);
	void coordtrans( double fMetersNorth, double fMetersEast, double fWind_dir_degrees, double *fMetersDownWind, double *fMetersCrosswind);
	double circle_overlap(double dist_center_to_center, double rad1, double rad2);
	double simple_intersect(double dist_center_to_center, double rad1, double rad2);
	double gammaln(double x);

};

#endif

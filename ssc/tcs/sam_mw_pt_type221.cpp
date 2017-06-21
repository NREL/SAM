#define _TCSTYPEINTERFACE_
#include "tcstype.h"
//#include <shared/lib_util.h>
#include "lib_util.h"
#include <algorithm>

#include "interpolation_routines.h"

using namespace std;

enum{	//Parameters
		P_eta_map,
		P_n_zen,
		P_n_azi,
		P_n_hel,
		P_q_start,
		P_p_run,
		P_v_wind_max,
		P_hel_stow_deploy,

		//Inputs
		I_v_wind,
		I_field_control,
		I_theta,
		I_phi,

		//Outputs
		O_pparasi,
		O_eta_field,

		//N_MAX
		N_MAX};

tcsvarinfo sam_mw_pt_type221_variables[] = {
	//PARAMETERS
	{TCS_PARAM, TCS_MATRIX, P_eta_map,			"eta_map",			"Field efficiency matrix",             			        "-",        "3 columns (azimuth, zenith, field efficiency)", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_n_zen,			"n_zen",			"Number of zenith angle data points in file",			"-",		"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_n_azi,			"n_azi",			"Number of azimuth angle data points in file",			"-",		"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_n_hel,            "n_hel",            "Number of heliostats in the field",					"-",		"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_q_start,			"q_start",			"Electric work for starting up one heliostat",			"kWe-hr",	"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_p_run,			"p_run",			"Electric power for tracking one heliostat",			"kWe",		"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_v_wind_max,		"v_wind_max",		"Maximum tolerable wind speed",							"m/s",		"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_hel_stow_deploy,	"hel_stow_deploy",	"Heliostat field stow/deploy solar elevation angle",	"deg",		"", "", ""},

	//INPUTS
	{TCS_INPUT, TCS_NUMBER, I_v_wind,			"vwind",			"Wind velocity",										"m/s",		"", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_field_control,	"field_control",	"Field defocus control",								"",			"", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_theta,			"theta",			"Solar zenith angle",									"deg",	    "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_phi,				"phi",				"Solar azimuth angle: 0 due north, clockwise to +360",	"deg",		"", "", ""},

	//OUTPUTS
	{TCS_OUTPUT, TCS_NUMBER, O_pparasi,			"pparasi",			"Parasitic tracking/startup power",						"MWe",		"", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_eta_field,		"eta_field",		"Total field efficiency",								"",			"", "", ""},

	//N_MAX
	{TCS_INVALID, TCS_INVALID, N_MAX,			0,					0, 0, 0, 0, 0	}
};
	
	
class sam_mw_pt_type221 : public tcstypeinterface
{
private:
	// Class Instances
	Bilinear_Interp field_efficiency_table;

	//Parameters
	double n_zen;
	double n_azi;
	double n_hel;
	double q_start;
	double p_run;
	double v_wind_max;	
	double hel_stow_deploy;
		
	//Stored Variables
	double eta_prev;
	double v_wind_prev;

public:
	sam_mw_pt_type221( tcscontext *cst, tcstypeinfo *ti)
		: tcstypeinterface( cst, ti)
	{
		n_zen = std::numeric_limits<double>::quiet_NaN();
		n_azi = std::numeric_limits<double>::quiet_NaN();
		n_hel = std::numeric_limits<double>::quiet_NaN();
		q_start = std::numeric_limits<double>::quiet_NaN();
		p_run = std::numeric_limits<double>::quiet_NaN();
		v_wind_max = std::numeric_limits<double>::quiet_NaN();
		hel_stow_deploy = std::numeric_limits<double>::quiet_NaN();

		eta_prev = std::numeric_limits<double>::quiet_NaN();
		v_wind_prev = std::numeric_limits<double>::quiet_NaN();
	}

	virtual ~sam_mw_pt_type221()
	{
	}

	virtual int init()
	{
		// Read in solar field efficiency file: 3 rows (0: azimuth, 1: zenith, 2: solar field efficiency)
		// TFF, Sept 17 2013: Note that the script DSG_PT_Defaults.lk loads this array with [ 0: zenith, 1: azimuth, 2: solar field efficiency]
		int angle_rows, angle_cols = 0.0;
		double *p_map = value( P_eta_map, &angle_rows, &angle_cols );
		util::matrix_t<double> eta_map( angle_rows, angle_cols, 0.0 );
		if( p_map != 0 && angle_cols == 3 && angle_rows > 25 )
		{
			for( int r = 0; r < angle_rows; r++ )
				for( int c = 0; c < angle_cols; c++ )
					eta_map.at(r,c) = TCS_MATRIX_INDEX( var( P_eta_map ), r, c );
		}
		else
		{
			message( TCS_ERROR, "Solar efficiency map must have at least 25 solar positions. P_eta_map: %d", angle_rows*angle_cols );
			return -1;
		}
		
		// Set up Bilinear Interpolation class for field efficiency data
		if( !field_efficiency_table.Set_2D_Lookup_Table( eta_map ) )
		{
			message( TCS_ERROR, "Initialization of 2D interpolation class failed" );
			return -1;
		}

		// Check interpolation routine
		// double afad = field_efficiency_table.bilinear_2D_interp( 45.0, 30.0 );

		// **************************************
		// Read in parameters
		n_zen = value( P_n_zen );					// [-] Number of zenith angles in field efficiency file
		n_azi = value( P_n_azi );					// [-] Number of azimuth angles in field efficiency file
		n_hel = value( P_n_hel );					// [-] Number of heliostats
		q_start = value( P_q_start ) * 3600.0;		// [kJ] convert from kWe-hr
		p_run = value( P_p_run ) * 3600.0;			// [kJ/hr] convert from kWe
		v_wind_max = value( P_v_wind_max );			// [m/s] Wind speed at which heliostats are stowed
		hel_stow_deploy = value( P_hel_stow_deploy ); // [deg] Heliostat field stow/deploy solar elevation angle

		// Initialize stored variables
		eta_prev = 0.0;
		v_wind_prev = 0.0;

		return 0;
	}

	virtual int call( double time, double step, int ncall )
	{						
		// GET AND CHECK INPUT VALUES
		double v_wind = value( I_v_wind );	// [m/s] wind speed
		double field_control = value( I_field_control ); // Control Parameter ( range from 0 to 1; 0=off, 1=all on)
		if( field_control > 1.0 )
			field_control = 1.0;
		if( field_control < 0.0 )
			field_control = 0.0;
		double theta = value( I_theta );	// solar zenith angle 
		if( theta >= 90.0 )
			field_control = 0.0;		// No tracking before sunrise of after sunset
		double phi = value( I_phi );	
		// Weather reader convention: 0 due north - 360 clockwise
		// solar azimuth angle - Convention used HERE: 0 at south - 360 clockwise
		// Convert
		if(phi <= 180.0)
			phi += 180.0;
		else
			phi -= 180.0;
      
		// Parasitics for startup or shutdown
		double pparasi = 0.0; 
		
		// If starting up or shutting down, calculate parasitics
		if( (field_control > 1.e-4 && eta_prev < 1.e-4) ||		// Startup by setting of control paramter (Field_control 0-> 1)
		(field_control < 1.e-4 && eta_prev >= 1.e-4) ||			// OR Shutdown by setting of control paramter (Field_control 1->0 )
		(field_control > 1.e-4 && v_wind >= v_wind_max ) ||		// OR Shutdown by high wind speed
		(eta_prev > 1.e-4 && v_wind_prev >= v_wind_max && v_wind < v_wind_max)  )	// OR Startup after high wind speed
			pparasi = n_hel * q_start / (step/3600.0);			// kJ/hr 
     
		// Parasitics for tracking      
		if( v_wind < v_wind_max && v_wind_prev < v_wind_max )
				pparasi += n_hel * p_run * field_control;	// kJ/hr

		// Use current solar position to interpolate field efficiency table and fied solar field efficiency
		double eta_field = field_efficiency_table.bilinear_2D_interp( theta, phi );
		eta_field = min( max ( eta_field, 0.0 ), 1.0 );		// Ensure physical behavior 
		
		if( theta >= 90.0 || (90.0-theta) < max( hel_stow_deploy, 0.1 ) )
			eta_field = 1.e-6;

		if( v_wind < v_wind_max )
			eta_field = max( eta_field*field_control, 1.e-6 );
		else
			eta_field = 1.e-6;

		// Set output parameters
		value( O_pparasi, pparasi/3.6e6 );	// [MW], convert from kJ/hr: Parasitic power for tracking
		value( O_eta_field, eta_field );	// [-], field efficiency

		return 0;
	}

	virtual int converged( double time )
	{
		eta_prev = value( O_eta_field );
		v_wind_prev = value( I_v_wind );
		
		return 0;
	}

};

TCS_IMPLEMENT_TYPE( sam_mw_pt_type221, "Basic heliostat field", "Ty Neises", 1, sam_mw_pt_type221_variables, NULL, 1 )


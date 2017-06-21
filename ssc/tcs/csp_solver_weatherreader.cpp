#include "csp_solver_core.h"
#include "csp_solver_util.h"
#include "sam_csp_util.h"

#include <shared/lib_weatherfile.h>
#include <shared/lib_irradproc.h>

C_csp_weatherreader::C_csp_weatherreader()
{
	m_filename = "";
	m_trackmode = -1;
	m_tilt = std::numeric_limits<double>::quiet_NaN();
	m_azimuth = std::numeric_limits<double>::quiet_NaN();

	m_ncall = -1;

	day_prev = -1;

	m_is_wf_init = false;
}


void C_csp_weatherreader::init()
{
	if(m_is_wf_init)
		return;
	
	if( !m_wfile.open(m_filename) )
	{
		m_error_msg = util::format("Could not open %s for reading", m_filename.c_str());
		throw(C_csp_exception(m_error_msg, ""));
	}
	
	m_wfile.header( &m_hdr );
	
	// Set solved parameters
	ms_solved_params.m_lat = m_hdr.lat;		//[deg]
	ms_solved_params.m_lon = m_hdr.lon;		//[deg]
	ms_solved_params.m_tz = m_hdr.tz;		//[deg]
	ms_solved_params.m_shift = (m_hdr.lon - m_hdr.tz*15.0);	//[deg]
	ms_solved_params.m_elev = m_hdr.elev;	//[m]
    /* 
    Leap year:
        The year is evenly divisible by 4;
        If the year can be evenly divided by 100, it is NOT a leap year, unless;
        The year is also evenly divisible by 400. Then it is a leap year.
    */
    weather_record r;
    m_wfile.read( &r );
    m_wfile.rewind();

    ms_solved_params.m_leapyear = (r.year % 4 == 0) && ( (r.year % 100 != 0) || (r.year % 400 == 0) );
    //do a special check to see if it's a leap year but the weather file supplies 8760 values nonetheless
    if( ms_solved_params.m_leapyear && (m_wfile.nrecords() % 8760 == 0) )
        ms_solved_params.m_leapyear = false;
    
	// ***********************************************************

	m_first = true;		// True the first time call() is accessed

	if(m_trackmode < 0 || m_trackmode > 2)
	{
		m_error_msg = util::format("invalid tracking mode specified %d [0..2]", m_trackmode);
		throw(C_csp_exception(m_error_msg, ""));
	}

	m_is_wf_init = true;
}

double C_csp_weatherreader::get_n_records()
{
	return m_wfile.nrecords();		//[-] Number of weather records in weather file
}

void C_csp_weatherreader::timestep_call(const C_csp_solver_sim_info &p_sim_info)
{
	// Increase call-per-timestep counter
	// Converge() sets it to -1, so on first call this line will adjust it = 0
	m_ncall++;
	
	double time = p_sim_info.ms_ts.m_time;		//[s]
	double step = p_sim_info.ms_ts.m_step;		//[s]
	//int ncall = p_sim_info->m_ncall;

	if( m_ncall == 0 ) // only read data values once per timestep
	{
		//If the start time does not correspond to the first record in the weather file, loop to the correct record
		int nread = 1;
		if( m_first )
		{
			nread = (int)time / step;
			m_first = false;
		}

		for( int i = 0; i<nread; i++ )		//for all calls except the first, nread=1
		{
			if( !m_wfile.read( &m_rec ) )
			{
				m_error_msg = util::format("failed to read from weather file %s at time %lg", m_wfile.filename().c_str(), time);
				throw(C_csp_exception(m_error_msg, ""));
			}
		}
	}

	double sunn[9], angle[5], poa[3], diffc[3];

	poa[0] = poa[1] = poa[2] = 0;
	angle[0] = angle[1] = angle[2] = angle[3] = angle[4] = 0;
	diffc[0] = diffc[1] = diffc[2] = 0;

	solarpos(m_rec.year, m_rec.month, m_rec.day, m_rec.hour, m_rec.minute,
		m_hdr.lat, m_hdr.lon, m_hdr.tz, sunn);

	if( sunn[2] > 0.0087 )
	{
		/* sun elevation > 0.5 degrees */
		incidence(m_trackmode, m_tilt, m_azimuth, 45.0, sunn[1], sunn[0], 0, 0, angle);
		perez(sunn[8], m_rec.dn, m_rec.df, 0.2, angle[0], angle[1], sunn[1], poa, diffc);		 // diffuse shading factor not enabled (set to 1.0 by default)
	}
	
	ms_outputs.m_year = m_rec.year;
	ms_outputs.m_month = m_rec.month;
	ms_outputs.m_day = m_rec.day;
	ms_outputs.m_hour = m_rec.hour;
	ms_outputs.m_minute = m_rec.minute;

	ms_outputs.m_global = m_rec.gh;
	ms_outputs.m_beam = m_rec.dn;	
	ms_outputs.m_diffuse = m_rec.df;
	ms_outputs.m_tdry = m_rec.tdry;
	ms_outputs.m_twet = m_rec.twet;
	ms_outputs.m_tdew = m_rec.tdew;
	ms_outputs.m_wspd = m_rec.wspd;
	ms_outputs.m_wdir = m_rec.wdir;
	ms_outputs.m_rhum = m_rec.rhum;
	ms_outputs.m_pres = m_rec.pres;
	ms_outputs.m_snow = m_rec.snow;
	ms_outputs.m_albedo = m_rec.alb;
    ms_outputs.m_aod = m_rec.aod;

	ms_outputs.m_poa = poa[0] + poa[1] + poa[2];
	ms_outputs.m_solazi = sunn[0] * 180 / CSP::pi;
	ms_outputs.m_solzen = sunn[1] * 180 / CSP::pi;
	ms_outputs.m_lat = m_hdr.lat;
	ms_outputs.m_lon = m_hdr.lon;
	ms_outputs.m_tz = m_hdr.tz;
	ms_outputs.m_shift = (m_hdr.lon - m_hdr.tz*15.0);
	ms_outputs.m_elev = m_hdr.elev;

	ms_outputs.m_hor_beam = m_rec.dn*cos(sunn[1]);
	
	// Recalculate sunrise and sunset if necessary
	if( m_rec.day != day_prev )
	{

		// Sunset and sunrise calculations from Type250

		int day_of_year = (int) ceil(time/3600.0);	// Day of year
		// Duffie & Beckman 1.5.3b
		double B = (day_of_year-1)*360.0/365.0*CSP::pi/180.0;	//[rad]
		// Eqn of time in minutes
		double EOT = 229.2 * (0.000075 + 0.001868 * cos(B) - 0.032077 * sin(B) - 0.014615 * cos(B*2.0) - 0.04089 * sin(B*2.0));
		// Declination in radians (Duffie & Beckman 1.6.1)
		double Dec = 23.45 * sin(360.0*(284.0 + day_of_year) / 365.0*CSP::pi / 180.0) * CSP::pi / 180.0;
		// Solar Noon and time in hours
		double SolarNoon = 12.0 - (ms_outputs.m_shift)/15.0 - EOT/60.0;

		// Sunrise and Sunset times in hours
			// Eq 1.6.11
		double N_daylight_hours = (2.0/15.0)*acos( -tan(m_hdr.lat*CSP::pi/180.0)*tan(Dec) )*180.0/CSP::pi;

		ms_outputs.m_time_rise = SolarNoon - N_daylight_hours/2.0;	//[hr]
		ms_outputs.m_time_set = SolarNoon + N_daylight_hours/2.0;	//[hr]
	}
	
}

bool C_csp_weatherreader::read_time_step(int time_step, C_csp_solver_sim_info &p_sim_info)
{
    /* 
    Read in the weather file for the specified time step
    */

    if(time_step < 0)
    {
        m_wfile.rewind();
        converged();
    }
    else
    {
        converged();
    
		p_sim_info.ms_ts.m_time = (time_step + 1.) * p_sim_info.ms_ts.m_step;
    
        m_wfile.set_counter_to( time_step );
        m_first = false;

        timestep_call(p_sim_info);

        converged();
    }
    return true;
}

int C_csp_weatherreader::get_current_step()
{
    return m_wfile.get_counter_value();
}

void C_csp_weatherreader::converged()
{
	m_ncall = -1;

	// Reset day_prev
	day_prev = m_rec.day;
}

#include <stdio.h>
#include <stdlib.h>

#include "sscapi.h"

ssc_bool_t my_handler( ssc_module_t p_mod, ssc_handler_t p_handler, int action,
	float f0, float f1, const char *s0, const char *s1, void *user_data )
{
	if (action == SSC_LOG)
	{
		// print log message to console
		switch( (int)f0 )
		{
		case SSC_NOTICE: printf("Notice: %s\n", s0); break;
		case SSC_WARNING: printf("Warning: %s\n", s0); break;
		case SSC_ERROR: printf("Error: %s\n", s0); break;
		}
		return 1;
	}
	else if (action == SSC_UPDATE)
	{
		// print status update to console
		printf("(%.2f %%) %s\n",f0,s0);
		return 1; // return 0 to abort simulation as needed.
	}
	else
		return 0;
}

int main(int argc, char *argv[])
{

	/*******************************
		create an empty SSC data container and assign input variables
		************************************/
	ssc_data_t p_data = ssc_data_create();

	// time and location data
	ssc_data_set_number( p_data, "year", 1970); // general year (tiny effect in sun position)
	ssc_data_set_number( p_data, "month", 1); // 1-12
	ssc_data_set_number( p_data, "day", 1); //1-number of days in month
	ssc_data_set_number( p_data, "hour", 9); // 0-23
	ssc_data_set_number( p_data, "minute", 30); // minute of the hour (typically 30 min for midpoint calculation)
	ssc_data_set_number( p_data, "lat", 33.4); // latitude, degrees
	ssc_data_set_number( p_data, "lon", -112); // longitude, degrees
	ssc_data_set_number( p_data, "tz", -7); // timezone from gmt, hours
	ssc_data_set_number( p_data, "time_step", 1 ); // time step, hours

	// solar and weather data
	ssc_data_set_number( p_data, "beam", 824); // beam (DNI) irradiance, W/m2
	ssc_data_set_number( p_data, "diffuse", 29); // diffuse (DHI) horizontal irradiance, W/m2
	ssc_data_set_number( p_data, "tamb", 9.4); // ambient temp, degree C
	ssc_data_set_number( p_data, "wspd", 2.1); // wind speed, m/s
	ssc_data_set_number( p_data, "snow", 0); // snow depth, cm (0 is default - when there is snow, ground reflectance is increased.  assumes panels have been cleaned off)

	// system specifications
	ssc_data_set_number( p_data, "system_size", 4); // system DC nameplate rating (kW)
	ssc_data_set_number( p_data, "derate", 0.77); // derate factor
	ssc_data_set_number( p_data, "track_mode", 0); // tracking mode 0=fixed, 1=1axis, 2=2axis
	ssc_data_set_number( p_data, "azimuth", 180); // azimuth angle 0=north, 90=east, 180=south, 270=west
	ssc_data_set_number( p_data, "tilt", 20); // tilt angle from horizontal 0=flat, 90=vertical


	// previous timestep values of cell temperature and POA
	ssc_data_set_number( p_data, "tcell", 6.94); // calculated cell temperature from previous timestep, degree C, (can default to ambient for morning or if you don't know)
	ssc_data_set_number( p_data, "poa", 84.5); // plane of array irradiance (W/m2) from previous time step


	/*******************************
		create instance and run calculation function
		************************************/
	ssc_module_t p_mod = ssc_module_create( "pvwattsv1_1ts" );
	if ( p_mod == 0 )
	{
		printf("could not create 'pvwattsfunc' model\n");
		return -2;

	}

	if ( !ssc_module_exec_with_handler( p_mod, p_data, my_handler, 0 ) )
	{
		printf("error in pvwattsfunc() module call...\n");
		return -1;
	}

	// at this point, the data container 'p_data' will be populated with
	// all the outputs
	ssc_module_free( p_mod );


	/*******************************
		print some outputs
		************************************/

	ssc_number_t ac, dc, poa, tcell;

	ssc_data_get_number( p_data, "poa", &poa ); // calculated POA at this time step W/m2
	ssc_data_get_number( p_data, "tcell", &tcell ); // cell temperature degree C
	ssc_data_get_number( p_data, "dc", &dc ); // DC output, Watts-dc
	ssc_data_get_number( p_data, "ac", &ac ); // AC output, Watts-ac

	printf("poa: %lg W/m2 tcell: %lg C dc: %lg W ac: %lg W\n", poa, tcell, dc, ac );

	// free the data container
	ssc_data_free( p_data );

	return 0;
}

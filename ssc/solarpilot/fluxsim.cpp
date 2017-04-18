#include "fluxsim.h"
#include "solpos00.h"

void FluxSimData::Create(var_map &V)
{
    updateCalculatedParameters(V);
}

void FluxSimData::updateCalculatedParameters(var_map &V)
{
    //may need to calculate the solar position for a particular day/time, otherwise set the solar position 
	//to the input value
	double az,zen;
	if (V.flux.flux_time_type.mapval() == var_fluxsim::FLUX_TIME_TYPE::SUN_POSITION){
		//Sun position are input, just set the corresponding values
		V.flux.flux_solar_az.Setval( V.flux.flux_solar_az_in.val*D2R );
		V.flux.flux_solar_el.Setval( V.flux.flux_solar_el_in.val*D2R );
	}
	else{
		//hour/day are provided, calculate the solar position
		int flux_day = V.flux.flux_day.val; //Day of the month
		int flux_month = V.flux.flux_month.val; //month of the year
		double flux_hour = V.flux.flux_hour.val; //hour of the day
		double lat = V.amb.latitude.val;
		double lon = V.amb.longitude.val; 
		double tmz = V.amb.time_zone.val; 

		DateTime DT;
		int doy = DT.GetDayOfYear(2011, int(flux_month), int(flux_day));
		
		//Instantiate the solpos object
		struct posdata SP, *pdat;
		pdat = &SP;	//point to structure for convenience
		S_init(pdat);		//Initialize the values

		//Calculate minutes/seconds
		double
			mins = 60.*(flux_hour - floor(flux_hour)),
			secs = 60.*(mins - floor(mins));
	
		pdat->latitude = float(lat);		//[deg] {float} North is positive
		pdat->longitude = float(lon);		//[deg] {float} Degrees east. West is negative
		pdat->timezone = float(tmz);			//[hr] {float} Time zone, east pos. west negative. Mountain -7, Central -6, etc..
		pdat->year = 2011;		//[year] {int} 4-digit year
		pdat->month = int(flux_month);	//[mo] {int} (1-12)
		pdat->day = int(flux_day);		//[day] {int} Day of the month
		pdat->daynum = doy;	//[day] {int} Day of the year
		pdat->hour = int(flux_hour+.0001);		//[hr] {int} 0-23
		pdat->minute = int(mins);	//[min] {int} 0-59
		pdat->second = int(secs);	//[sec]	{int} 0-59
		pdat->interval = 0;		//[sec] {int} Measurement interval. See solpos documentation.


		long retcode = 0;		//Initialize with no errors
		retcode = S_solpos(pdat);	//Call the solar posotion algorithm
		S_decode(retcode, pdat);	//Check the return code

		az = SP.azim;
		zen = SP.zenetr;

		V.flux.flux_solar_az.Setval( az ); 
		V.flux.flux_solar_el.Setval( 90. - zen ); 

	}

}
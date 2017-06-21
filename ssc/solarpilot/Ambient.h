#ifndef _AMBIENT_H_
#define _AMBIENT_H_ 1
#include <string>
#include <vector>

//#include "Toolbox.h"
#include "mod_base.h"
#include "definitions.h"

/*
Compiler note: If solpos or field_core code is not found, 
Add "solpos.lib;field_core.lib;" to PiLOT_GUI project settings, Linker->Input->Additional Dependencies.  
Add "$(SolutionDir)\solpos;" to field_core project settings, C/C++->Additional Include Directories.

The weather file reader is provided in SAM SIM CORE (SSC).
*/


//------------------
class Ambient : public mod_base
 {
    var_ambient *_amb_map;
 
 public:
	//enum CLRSKY_MODEL { MEINEL, HOTTEL, CONSTANT, MOON, ALLEN, WEATHER=-1 };

	void Create(var_map &V);
    void updateCalculatedParameters(var_map &V);
	void Clean();

	static std::string getDefaultSimStep();	//d.o.m., hour, month, dni, pressure, wind
	
	DateTime *getDateTimeObj();
	
	//Calculation methods
    static void setDateTime(DateTime &DT, double day_hour, double year_day, double year = 2011.);
	static void calcSunPosition(var_map &V, DTobj &DT, double *az, double *zen); //use some local info, some other info
	static void calcSunPosition(double lat, double lon, double timezone, double tstep, const DTobj &dt, double *az, double *zen);	//Calculate with these arguments
    static Vect calcSunVectorFromAzZen(double azimuth, double zenith);	//Calculate sun position given specified az/zen values
	
    static void calcDaytimeHours(double hrs[2], double lat, double lon, double timezone, const DTobj &dt);
	static bool readWeatherFile(var_map &V); 
	static double calcAttenuation(var_map &V, double &len);
	static void calcSpacedDaysHours(double lat, double lon, double tmz, int nday, double delta_hr, std::vector<std::vector<double> > &utime, std::vector<int> &uday); //calculate days and times that produce evenly spaced sun positions over the year
	static double calcInsolation(var_map &V, double azimuth, double zenith, int day_of_year); //calculate clear-sky radiation using one of the DELSOL models

 } ;

#endif

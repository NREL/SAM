#ifndef __lib_physics_h
#define __lib_physics_h

#include <math.h>
#include <assert.h>

namespace physics
{
	const double PI = 2.0 * acos(0.0);
	const double FT_PER_METER =			3.280839895;			// feet per meter
	const double PSI_PER_BAR =		   14.50377373066;			// psi per bar
	const double PSI_PER_INHG =			0.4911541474703;		// psi per inch of mercury
	const double Pa_PER_Atm =	   101325.00;					// pascals per atm;  101300.0 is value from FORTRAN code
	const double Pa_PER_inHg =		 3386.00;
	const double Atm_PER_Bar =			0.986923267;			// atmospheres per bar
	const double KGM3_PER_LBF3 =	   16.01846337396;			// lbs/ft^3 per kg/m^3 
	const double LB_PER_KG =			2.204622621849;			// pounds per kilogram
	const double KW_PER_HP =			0.7456998715801;		// kilowatts per unit of horsepower
	const double GRAVITY_MS2 =			9.8;					// meters per second^2; this varies between 9.78 and 9.82 depending on latitude
	const double GRAVITY_FTS2 =		   32.174;					// ft per second^2
	const double SPECIFIC_HEAT_LIQUID_WATER = 4.183 /*4.1813*/;	// J/g*K = joules per gram-degrees K; 4.183 is value currently in Fortran
	const double WATER_DENSITY =	   62.4;					// lb/ft^3
	const double R_Gas =              287.058;

	const double GAS_CONSTANT_SUPER_HEATED_STEAM =		0.461522;		// kJ/kg-K
	const double MIN_TEMP_FOR_SUPER_HEATED =		  647.073;			// deg K
	const double MIN_TEMP_FOR_STEAM1 =				  623.15;			// K

	inline double areaCircle(const double &radius) { return PI * pow(radius,2.0); }

	// temperature conversions
	inline double FarenheitToCelcius(const double &dTempInFarenheit) { return ((5.0/9.0) * (dTempInFarenheit - 32.0)); };
	inline double CelciusToFarenheit(const double &dTempInCelcius) { return (1.8 * dTempInCelcius) + 32.0; };

	inline double KelvinToCelcius(const double &dTempInKelvin) { return (dTempInKelvin-273.15); }
	inline double CelciusToKelvin(const double &dTempInCelcius) { return (dTempInCelcius+273.15); }
	
	inline double FarenheitToKelvin(const double &dTempInFarenheit) {return (CelciusToKelvin(FarenheitToCelcius(dTempInFarenheit))); };
	inline double KelvinToFarenheit(const double &dTempInKelvin) {return (CelciusToFarenheit(KelvinToCelcius(dTempInKelvin))); };

	// pressure conversions
	inline double AtmToPa(const double &dPressureInAtm) { return dPressureInAtm * Pa_PER_Atm; }
	inline double PaToAtm(const double &dPressureInPa) { return dPressureInPa / Pa_PER_Atm; }

	inline double InHgToPa(const double &dPressureInInchesHg) { return dPressureInInchesHg * Pa_PER_inHg; }
	inline double PaToInHg(const double &dPressureInPa) { return dPressureInPa / Pa_PER_inHg; }

	inline double mBarToAtm(const double &PressureInmBar) { return PressureInmBar * Atm_PER_Bar/1000; }
	inline double mBarToPSI(const double &PressureInmBar) { return PressureInmBar * PSI_PER_BAR/1000; }
	inline double PsiToBar(const double &psi){ return psi / PSI_PER_BAR; }

	inline double toWattHr(const double &btu) { return (btu/3.413); }
	inline double PSItoFT(const double &psi) { return psi * 144 / WATER_DENSITY; }  // convert PSI to pump 'head' in feet.  assumes water density ~ 62.4 lb/ft^3

	bool EnthalpyFromTempAndPressure(double tempK, double pressureBar, double& enthalpy );

	const double AIR_DENSITY_SEA_LEVEL = Pa_PER_Atm/(R_Gas * CelciusToKelvin(15)); // kg/m^3 at sea level (1 atm) and 15 C
};


#endif //__lib_physics_h

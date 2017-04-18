#ifndef _HELIODATA_
#define _HELIODATA_ 1

#include <vector>

struct helio_perf_data
{
private:
	std::vector<double*> _dvars;
public:
	//Power to receiver=0;Total efficiency=1;Cosine efficiency=2;Attenuation efficiency=3;Intercept efficiency=4;Blocking efficiency=5;Shadowing efficiency=6;TOU-weighted power=7
	
	//This enumeration is order specific based on the indices specified in the gui variable "solarfield.0.hsort_method"
	struct PERF_VALUES { enum A {
		POWER_TO_REC=0, ETA_TOT, ETA_COS, ETA_ATT, ETA_INT, ETA_BLOCK, ETA_SHADOW, POWER_VALUE, /* after this, order not significant */
        REFLECTIVITY, SOILING, REC_ABSORPTANCE, RANK_METRIC, ETA_CLOUD };
	};
	helio_perf_data();
//	helio_perf_data( const helio_perf_data &hp);
	void resetMetrics();
	double calcTotalEfficiency();
	
	double getDataByIndex( const int id );
	void setDataByIndex( const int id, double value);
	int n_metric;
	double
		eta_cos,	//[-] Heliostat cosine efficiency
		eta_att,	//[-] Atmospheric attenuation efficiency
		eta_int,	//[-] Intercept efficiency
		eta_block,  //[-] Blocking efficiency
		eta_shadow, //[-] Shadowing efficiency
		eta_tot,	//[-] Total heliostat intercept
		reflectivity,
		soiling,
		rec_absorptance,	//Absorptance of the receiver this heliostat is aiming at
		power_to_rec,	//[W] delivered power
		power_value,
		rank_metric,	//Power weighted by the payment allocation factor, if applicable
		eta_cloud;	//[-] Loss due to cloudiness (performance simulation only)
		
};

#endif
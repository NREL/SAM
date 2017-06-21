#ifndef __irradproc_h
#define __irradproc_h

/* aug2011 - apd
	solar position and radiation processing split out from pvwatts.
	added isotropic sky model and hdkr model for diffuse on a tilted surface
	*/

void solarpos(int year,int month,int day,int hour,double minute,double lat,double lng,double tz,double sunn[9]);
void incidence(int mode,double tilt,double sazm,double rlim,double zen,double azm, bool en_backtrack, double gcr, double angle[5]);
void perez( double hextra, double dn,double df,double alb,double inc,double tilt,double zen, double poa[3], double diffc[3] /* can be NULL */ );
void isotropic( double hextra, double dn, double df, double alb, double inc, double tilt, double zen, double poa[3], double diffc[3] /* can be NULL */ );
void hdkr( double hextra, double dn, double df, double alb, double inc, double tilt, double zen, double poa[3], double diffc[3] /* can be NULL */ );

// Sev: 2015-11-24 Added to keep track of what each radmode interger means
enum RADMODE {DN_DF, DN_GH, GH_DF, POA_R, POA_P};

// 2015-10-25: Added by Sev to allow for the poa decomp model to take all daily POA measurements into consideration
struct poaDecompReq {
	poaDecompReq() : i(0), dayStart(0), stepSize(1), stepScale('h'), doy(-1) {}
	size_t i; // Current time index
	size_t dayStart; // time index corresponding to the start of the current day
	double stepSize;
	char stepScale; // indicates whether time steps are hours (h) or minutes (m)
	double* POA; // Pointer to entire POA array (will have size 8760 if time step is 1 hour)
	double* inc; // Pointer to angle of incident array (same size as POA)
	double* tilt; // Pointer to angle of incident array (same size as POA)
	double* zen; // Pointer to angle of incident array (same size as POA)
	double* exTer; // Pointer to angle of incident array (same size as POA)
	double tDew;
	int doy;
	double elev;
};

// 2015/09/11 - Sev
// Create functions for POA decomposition

void poaDecomp( double wfPOA, double angle[], double sun[], double alb, poaDecompReq* pA, double &dn, double &df, double &gh, double poa[3], double diffc[3]);
double ModifiedDISC(const double g[3], const double z[3], double td, double alt, int doy, double &dn);
void ModifiedDISC(const double kt[3], const double kt1[3], const double g[3], const double z[3], double td, double alt, int doy, double &dn);


class irrad
{
private:
	int year, month, day, hour;
	double minute, delt;

	double lat, lon, tz;
	int radmode, skymodel, track;
	double gh, dn, df, wfpoa, alb;
	double tilt, sazm, rlim, gcr;
	bool en_backtrack;
	double sun[9], angle[5], poa[3], diffc[3];
	int tms[3];
	double ghi;

	poaDecompReq* poaAll;

public:

	irrad();
	int check();
	
	// if delt_hr is less than zero, do not interpolate sunrise and sunset hours
#define IRRADPROC_NO_INTERPOLATE_SUNRISE_SUNSET (-1.0)
	void set_time( int year, int month, int day, int hour, double minute, double delt_hr );
	void set_location( double lat, double lon, double tz);
	//skymodel: 0 is isotropic, 1 is hdkr, 2 is perez
	void set_sky_model( int skymodel, double albedo );
	void set_surface( int tracking, double tilt_deg, double azimuth_deg, double rotlim_deg, bool en_backtrack, double gcr );
	void set_beam_diffuse( double beam, double diffuse );
	void set_global_beam( double global, double beam );
	void set_global_diffuse(double global, double diffuse);
	void set_poa_reference( double poa, poaDecompReq* );
	void set_poa_pyranometer( double poa, poaDecompReq* );

	int calc();
	
	void get_sun( double *solazi,
		double *solzen,
		double *solelv,
		double *soldec,
		double *sunrise,
		double *sunset,
		int *sunup,
		double *eccfac,
		double *tst,
		double *hextra );
	void get_angles( double *aoi,
		double *surftilt,
		double *surfazi,
		double *axisrot,
		double *btdiff );
	void get_poa( double *beam, double *skydiff, double *gnddiff,
		double *isotrop, double *circum, double *horizon );
	void get_irrad (double *ghi, double *dni, double *dhi);
	double get_ghi();
	double get_sunpos_calc_hour();
};




double shade_fraction_1x( double solazi, double solzen, 
						 double axis_tilt, double axis_azimuth, 
						 double gcr, double rotation );

double backtrack( double solazi, double solzen, 
				 double axis_tilt, double axis_azimuth, 
				 double rotlim, double gcr, double rotation_ideal);

#endif

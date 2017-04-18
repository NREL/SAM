#include "core.h"
#include "lib_util.h"
#include "lib_irradproc.h"

#ifndef M_PI
#define M_PI 3.141592653589793238462643
#endif

static var_info _cm_vtab_irradproc[] = {
/*   VARTYPE           DATATYPE         NAME                           LABEL                              UNITS     META                      GROUP                      REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	
	{ SSC_INPUT,        SSC_NUMBER,      "irrad_mode",                 "Irradiance input mode",           "0/1/2",   "Beam+Diff,Global+Beam, Global+Diff",  "Irradiance Processor",      "?=0",                     "INTEGER,MIN=0,MAX=2", ""},

	{ SSC_INPUT,        SSC_ARRAY,       "beam",                       "Beam normal irradiance",          "W/m2",   "",                      "Irradiance Processor",      "irrad_mode~2",                        "",                      "" },
	{ SSC_INPUT,        SSC_ARRAY,       "diffuse",                    "Diffuse horizontal irradiance",   "W/m2",   "",                      "Irradiance Processor",      "irrad_mode~1",             "LENGTH_EQUAL=beam",     "" },
	{ SSC_INPUT,        SSC_ARRAY,       "global",                     "Global horizontal irradiance",    "W/m2",   "",                      "Irradiance Processor",      "irrad_mode~0",              "LENGTH_EQUAL=beam",     "" },

	{ SSC_INPUT,        SSC_ARRAY,       "albedo",                     "Ground reflectance (time depend.)","frac",  "0..1",                   "Irradiance Processor",      "?",                        "LENGTH_EQUAL=beam",     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "albedo_const",               "Ground reflectance (single value)","frac",  "0..1",                   "Irradiance Processor",      "?=0.2",                    "",                      "" },
	
	{ SSC_INPUT,        SSC_ARRAY,       "year",                       "Year",                             "yr",     "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",               "" },
	{ SSC_INPUT,        SSC_ARRAY,       "month",                      "Month",                            "mn",     "1-12",                  "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
	{ SSC_INPUT,        SSC_ARRAY,       "day",                        "Day",                              "dy",     "1-days in month",                 "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
	{ SSC_INPUT,        SSC_ARRAY,       "hour",                       "Hour",                             "hr",     "0-23",                  "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
	{ SSC_INPUT,        SSC_ARRAY,       "minute",                     "Minute",                           "min",    "0-59",                  "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },

	
	{ SSC_INPUT,        SSC_NUMBER,      "lat",                        "Latitude",                         "deg",    "",                      "Irradiance Processor",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "lon",                        "Longitude",                        "deg",    "",                      "Irradiance Processor",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tz",                         "Time zone",                        "hr",     "",                      "Irradiance Processor",      "*",                        "",                      "" },

	{ SSC_INPUT,        SSC_NUMBER,      "sky_model",                  "Tilted surface irradiance model", "0/1/2", "Isotropic,HDKR,Perez",  "Irradiance Processor",      "?=2",                     "INTEGER,MIN=0,MAX=2", ""},

	{ SSC_INPUT,        SSC_NUMBER,      "track_mode",                 "Tracking mode",                  "0/1/2",  "Fixed,1Axis,2Axis",     "Irradiance Processor",      "*",                       "MIN=0,MAX=2,INTEGER",                      "" }, 
	{ SSC_INPUT,        SSC_NUMBER,      "azimuth",                    "Azimuth angle",                  "deg",    "E=90,S=180,W=270",      "Irradiance Processor",      "*",                       "MIN=0,MAX=360",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tilt",                       "Tilt angle",                     "deg",    "H=0,V=90",              "Irradiance Processor",      "?",                       "MIN=0,MAX=90",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rotlim",                     "Rotational limit on tracker",    "deg",    "",                      "Irradiance Processor",      "?=45",                    "MIN=0,MAX=90",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,      "backtrack",                  "Enable backtracking",            "0/1",    "",                      "Irradiance Processor",      "?=0",                    "BOOLEAN",                                  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "gcr",                        "Ground coverage ratio",          "0..1",   "",                      "Irradiance Processor",      "backtrack=1",              "MIN=0,MAX=1",                               "" },
	
	
	{ SSC_OUTPUT,       SSC_ARRAY,       "poa_beam",                   "Incident Beam Irradiance",       "W/m2",   "",                      "Irradiance Processor",      "*",                       "",                  "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "poa_skydiff",                "Incident Sky Diffuse",           "W/m2",   "",                      "Irradiance Processor",      "*",                       "",                  "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "poa_gnddiff",                "Incident Ground Reflected Diffuse", "W/m2", "",                     "Irradiance Processor",      "*",                       "",                  "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "poa_skydiff_iso",            "Incident Diffuse Isotropic Component", "W/m2", "",                  "Irradiance Processor",      "*",                       "",                  "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "poa_skydiff_cir",            "Incident Diffuse Circumsolar Component", "W/m2", "",                "Irradiance Processor",      "*",                       "",                  "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "poa_skydiff_hor",            "Incident Diffuse Horizon Brightening Component", "W/m2", "",        "Irradiance Processor",      "*",                       "",                  "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "incidence",                  "Incidence angle to surface",     "deg",    "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "surf_tilt",                  "Surface tilt angle",             "deg",    "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "surf_azm",                   "Surface azimuth angle",          "deg",    "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "axis_rotation",              "Tracking axis rotation angle",   "deg",    "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "bt_diff",                    "Backtracking difference from ideal rotation",   "deg",    "",       "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
	
	{ SSC_OUTPUT,       SSC_ARRAY,       "sun_azm",                    "Solar azimuth",                  "deg",    "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "sun_zen",                    "Solar zenith",                   "deg",    "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "sun_elv",                    "Sun elevation",                  "deg",    "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "sun_dec",                    "Sun declination",                "deg",    "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },


var_info_invalid };

class cm_irradproc : public compute_module
{
public:
	cm_irradproc()
	{
		add_var_info( _cm_vtab_irradproc );
	}

	void exec( ) throw( general_error )
	{
		size_t count;
		ssc_number_t *beam = 0, *glob = 0, *diff = 0;
		int irrad_mode = as_integer("irrad_mode");
		if (irrad_mode == 0) //beam and diffuse
		{
			beam = as_array("beam", &count);
			if (count < 2) throw general_error("need at least 2 data points in irradproc");
			diff = as_array("diffuse", &count);
		}
		if (irrad_mode == 1) //global and beam
		{
			beam = as_array("beam", &count);
			if (count < 2) throw general_error("need at least 2 data points in irradproc");
			glob = as_array("global", &count);
		}
		else //global and diffuse
		{
			diff = as_array("diffuse", &count);
			if (count < 2) throw general_error("need at least 2 data points in irradproc");
			glob = as_array("global", &count);
		}			

		ssc_number_t *year = as_array("year", &count);		
		ssc_number_t *month = as_array("month", &count);
		ssc_number_t *day = as_array("day", &count);
		ssc_number_t *hour = as_array("hour", &count);
		ssc_number_t *minute = as_array("minute", &count);

		int sky_model = as_integer("sky_model");

		double lat = as_double("lat");
		double lon = as_double("lon");
		double tz = as_double("tz");

		double tilt = lat;
		if (is_assigned("tilt"))
			tilt = as_double("tilt");

		double azimuth = as_double("azimuth");
		int track_mode = as_integer("track_mode");
		double rotlim = as_double("rotlim");
		bool en_backtrack = as_boolean("backtrack");
		double gcr = as_double("gcr");

		double alb_const = as_double("albedo_const");
		ssc_number_t *albvec = 0;
		if (is_assigned("albedo")) albvec = as_array("albedo", &count);
	
		
		// allocate outputs
		ssc_number_t *p_inc = allocate("incidence", count);
		ssc_number_t *p_surftilt = allocate("surf_tilt", count);
		ssc_number_t *p_surfazm = allocate("surf_azm", count);
		ssc_number_t *p_rot = allocate("axis_rotation", count);
		ssc_number_t *p_btdiff = allocate("bt_diff", count);
		ssc_number_t *p_azm = allocate("sun_azm", count);
		ssc_number_t *p_zen = allocate("sun_zen", count);
		ssc_number_t *p_elv = allocate("sun_elv", count);
		ssc_number_t *p_dec = allocate("sun_dec", count);

		ssc_number_t *p_poa_beam = allocate("poa_beam", count);
		ssc_number_t *p_poa_skydiff = allocate("poa_skydiff", count);
		ssc_number_t *p_poa_gnddiff = allocate("poa_gnddiff", count);
		
		ssc_number_t *p_poa_skydiff_iso = allocate("poa_skydiff_iso", count);
		ssc_number_t *p_poa_skydiff_cir = allocate("poa_skydiff_cir", count);
		ssc_number_t *p_poa_skydiff_hor = allocate("poa_skydiff_hor", count);


		// "temporary" debugging output
		ssc_number_t *p_sunup = allocate("sunup", count);
		ssc_number_t *p_sunrise = allocate("sunrise", count);
		ssc_number_t *p_sunset = allocate("sunset", count);
		
		for (size_t i = 0; i < count ;i ++ )
		{
			double t_cur = hour[i] + minute[i]/60.0;
			double delt = 1.0;
			if ( i == 0 )
			{
				double t_next = hour[i+1] + minute[i+1]/60.0;
				if (t_cur > t_next) t_next += 24;
				delt = t_next - t_cur;
			}
			else
			{
				double t_prev = hour[i-1] + minute[i-1]/60.0;
				if (t_cur < t_prev) t_cur += 24;
				delt = t_cur - t_prev;
			}

			// double precsion issue (15 digits IEEE 754) encountered by Anthony Lopez 4/29/13 for 
			// minutes other than 15,30,45 and 60
			if (fabs(delt-1.0)<1e-14) delt=1.0;

			double alb = alb_const;
			// if we have array of albedo values, use it
			if ( albvec != 0  && albvec[i] >= 0 && albvec[i] <= (ssc_number_t)1.0)
				alb = albvec[i];

			irrad x;

			x.set_time( (int)year[i], (int)month[i], (int)day[i], (int)hour[i], minute[i], IRRADPROC_NO_INTERPOLATE_SUNRISE_SUNSET );
			x.set_location( lat, lon, tz );
			x.set_sky_model( sky_model, alb );
			if ( irrad_mode == 1 ) x.set_global_beam( glob[i], beam[i] );
			else if (irrad_mode == 2) x.set_global_diffuse(glob[i], diff[i]);
			else x.set_beam_diffuse( beam[i], diff[i] );
			x.set_surface( track_mode, tilt, azimuth, rotlim, en_backtrack, gcr );
			
			int code = x.calc();
			if (code < 0)
				throw general_error( util::format("irradiance processor issued error code %d", code ));

			double solazi, solzen, solelv, soldec, sunrise, sunset;
			int sunup;

			x.get_sun( &solazi,
				&solzen,
				&solelv,
				&soldec,
				&sunrise,
				&sunset,
				&sunup,
				0,
				0,
				0 );
			
			p_azm[i] = (ssc_number_t) solazi;
			p_zen[i] = (ssc_number_t) solzen;
			p_elv[i] = (ssc_number_t) solelv;
			p_dec[i] = (ssc_number_t) soldec;	
			p_sunrise[i] = (ssc_number_t) sunrise;
			p_sunset[i] = (ssc_number_t) sunset;
			p_sunup[i] = (ssc_number_t) sunup;
			
			
			double aoi, stilt, sazi, rot, btd;
			x.get_angles( &aoi, &stilt, &sazi, &rot, &btd );

			// assign outputs
			p_inc[i] = (ssc_number_t) aoi;
			p_surftilt[i] = (ssc_number_t) stilt;
			p_surfazm[i] = (ssc_number_t) sazi;
			p_rot[i] = (ssc_number_t) rot;
			p_btdiff[i] = (ssc_number_t) btd;
			
			double beam, skydiff, gnddiff, iso, cir, hor;
			x.get_poa( &beam, &skydiff, &gnddiff, &iso, &cir, &hor );

			p_poa_beam[i] = (ssc_number_t) beam;
			p_poa_skydiff[i] = (ssc_number_t) skydiff;
			p_poa_gnddiff[i] = (ssc_number_t) gnddiff;
			p_poa_skydiff_iso[i] = (ssc_number_t) iso;
			p_poa_skydiff_cir[i] = (ssc_number_t) cir;
			p_poa_skydiff_hor[i] = (ssc_number_t) hor;
			
		}
	}
};

DEFINE_MODULE_ENTRY( irradproc, "Irradiance Processor", 1 )

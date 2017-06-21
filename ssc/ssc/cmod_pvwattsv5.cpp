#include <memory>

#include "core.h"

#include "common.h"

#include "lib_weatherfile.h"
#include "lib_irradproc.h"
#include "lib_pvwatts.h"
#include "lib_pvshade.h"
#include "lib_pvmodel.h"

#ifndef DTOR
#define DTOR 0.0174532925
#endif
#ifndef M_PI
#define M_PI 3.14159265358979323846264338327
#endif
#define sind(x) sin( (M_PI/180.0)*(x) )
#define cosd(x) cos( (M_PI/180.0)*(x) )
#define tand(x) tan( (M_PI/180.0)*(x) )
#define asind(x) (180/M_PI*asin(x))

static var_info _cm_vtab_pvwattsv5_part1[] = {
/*   VARTYPE           DATATYPE          NAME                         LABEL                                               UNITS        META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_STRING,      "solar_resource_file",            "Weather file path",                           "",          "",                       "Weather",     "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_TABLE,       "solar_resource_data",            "Weather data",                                "",          "dn,df,tdry,wspd,lat,lon,tz", "Weather", "?",                        "",                              "" },
	
	var_info_invalid };

static var_info _cm_vtab_pvwattsv5_common[] = {
	{ SSC_INPUT,        SSC_NUMBER,      "system_capacity",                "System size (DC nameplate)",                  "kW",        "",                           "PVWatts",      "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "module_type",                    "Module type",                                 "0/1/2",     "Standard,Premium,Thin film", "PVWatts",      "?=0",                       "MIN=0,MAX=2,INTEGER",                      "" }, 
	{ SSC_INPUT,        SSC_NUMBER,      "dc_ac_ratio",                    "DC to AC ratio",                              "ratio",     "",                           "PVWatts",      "?=1.1",                   "POSITIVE",                                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_eff",                        "Inverter efficiency at rated power",          "%",         "",                           "PVWatts",      "?=96",                        "MIN=90,MAX=99.5",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "losses",                         "System losses",                               "%",         "Total system losses",                               "PVWatts",      "*",                       "MIN=-5,MAX=99",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "array_type",                     "Array type",                                  "0/1/2/3/4", "Fixed OR,Fixed Roof,1Axis,Backtracked,2Axis",  "PVWatts",      "*",                       "MIN=0,MAX=4,INTEGER",                      "" }, 
	{ SSC_INPUT,        SSC_NUMBER,      "tilt",                           "Tilt angle",                                  "deg",       "H=0,V=90",                                     "PVWatts",      "*",                       "MIN=0,MAX=90",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,      "azimuth",                        "Azimuth angle",                               "deg",       "E=90,S=180,W=270",                             "PVWatts",      "*",                       "MIN=0,MAX=360",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "gcr",                            "Ground coverage ratio",                       "0..1",      "",                                             "PVWatts",      "?=0.4",                   "MIN=0,MAX=3",               "" },
	
	var_info_invalid };

static var_info _cm_vtab_pvwattsv5_part2[] = {
	{ SSC_INPUT,        SSC_MATRIX,      "shading:timestep",               "Time step beam shading loss",                 "%",         "",                        "PVWatts",      "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_MATRIX,      "shading:mxh",                    "Month x Hour beam shading loss",              "%",         "",                        "PVWatts",      "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_MATRIX,      "shading:azal",                   "Azimuth x altitude beam shading loss",        "%",         "",                        "PVWatts",      "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "shading:diff",                   "Diffuse shading loss",                        "%",         "",                        "PVWatts",      "?",                        "",                              "" },
	
	/* battery */
	{ SSC_INPUT,        SSC_NUMBER,      "batt_simple_enable",             "Enable Battery",                              "0/1",        "",                      "battwatts",     "?=0",                     "BOOLEAN",                        "" },

	/* outputs */
	{ SSC_OUTPUT,       SSC_ARRAY,       "gh",                             "Global horizontal irradiance",                "W/m2",   "",                        "Time Series",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "dn",                             "Beam irradiance",                             "W/m2",   "",                        "Time Series",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "df",                             "Diffuse irradiance",                          "W/m2",   "",                        "Time Series",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "tamb",                           "Ambient temperature",                         "C",      "",                        "Time Series",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "wspd",                           "Wind speed",                                  "m/s",    "",                        "Time Series",      "*",                       "",                          "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "sunup",                          "Sun up over horizon",                         "0/1",    "",                        "Time Series",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "shad_beam_factor",               "Shading factor for beam radiation",           "",       "",                        "Time Series",      "*",                       "",                                     "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "aoi",                            "Angle of incidence",                          "deg",    "",                        "Time Series",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "poa",                            "Plane of array irradiance",                   "W/m2",   "",                        "Time Series",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "tpoa",                           "Transmitted plane of array irradiance",       "W/m2",   "",                        "Time Series",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "tcell",                          "Module temperature",                          "C",      "",                        "Time Series",      "*",                       "",                          "" },	
	
	{ SSC_OUTPUT,       SSC_ARRAY,       "dc",                             "DC array power",                              "W",     "",                         "Time Series",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "ac",                             "AC inverter power",                           "W",     "",                         "Time Series",      "*",                       "",                          "" },
	
	{ SSC_OUTPUT,       SSC_ARRAY,       "poa_monthly",                    "Plane of array irradiance",                   "kWh/m2",    "",                     "Monthly",          "*",                       "LENGTH=12",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "solrad_monthly",                 "Daily average solar irradiance",              "kWh/m2/day","",                     "Monthly",          "*",                       "LENGTH=12",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "dc_monthly",                     "DC array output",                             "kWh",       "",                     "Monthly",          "*",                       "LENGTH=12",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "ac_monthly",                     "AC system output",                            "kWh",       "",                     "Monthly",          "*",                       "LENGTH=12",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_energy",                 "Monthly energy",                              "kWh",       "",                     "Monthly",          "*",                       "LENGTH=12",                          "" },

	{ SSC_OUTPUT,       SSC_NUMBER,      "solrad_annual",                  "Daily average solar irradiance",              "kWh/m2/day",    "",              "Annual",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "ac_annual",                      "Annual AC system output",                     "kWh",         "",              "Annual",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_energy",                  "Annual energy",                               "kWh",           "",              "Annual",      "*",                       "",                          "" },

	{ SSC_OUTPUT,       SSC_NUMBER,      "capacity_factor",                "Capacity factor",                             "%",             "",              "Annual",        "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "kwh_per_kw",                     "First year kWh/kW",                           "",              "",              "Annual",        "*",                       "",                          "" },

	{ SSC_OUTPUT,       SSC_STRING,      "location",                       "Location ID",                                 "",    "",                        "Location",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_STRING,      "city",                           "City",                                        "",    "",                        "Location",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_STRING,      "state",                          "State",                                       "",    "",                        "Location",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "lat",                            "Latitude",                                    "deg", "",                        "Location",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "lon",                            "Longitude",                                   "deg", "",                        "Location",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "tz",                             "Time zone",                                   "hr",  "",                        "Location",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "elev",                           "Site elevation",                              "m",   "",                        "Location",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "inverter_model",                 "Inverter model specifier",                     "",                               "0=cec,1=datasheet,2=partload,3=coefficientgenerator,4=generic", "", "", "INTEGER,MIN=0,MAX=4", "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "inverter_efficiency",            "Inverter efficiency at rated power",          "%",         "",                   "PVWatts",      "?=96",                        "MIN=90,MAX=99.5",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "ts_shift_hours",                 "Time offset for interpreting time series outputs",  "hours", "",                 "Miscellaneous", "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system_use_lifetime_output",     "Use lifetime output",                         "0/1", "",                         "Miscellaneous", "*",                       "",                          "" },


	var_info_invalid };

class cm_pvwattsv5_base : public compute_module
{
protected:
	double dc_nameplate, dc_ac_ratio, ac_nameplate, inv_eff_percent;
	double loss_percent, tilt, azimuth, gamma;
	bool use_ar_glass;
	int module_type;
	int track_mode;
	double inoct;
	int shade_mode_1x;
	int array_type;
	double gcr;

	
	double ibeam, iskydiff, ignddiff;
	double solazi, solzen, solalt, aoi, stilt, sazi, rot, btd;
	int sunup;		

	pvwatts_celltemp *tccalc;

	double poa, tpoa, pvt, dc, ac;

public:
	cm_pvwattsv5_base() {
		tccalc = 0;
		dc_nameplate = dc_ac_ratio = ac_nameplate = inv_eff_percent = std::numeric_limits<double>::quiet_NaN();
		loss_percent = tilt = azimuth = gamma = std::numeric_limits<double>::quiet_NaN();
		use_ar_glass = false;
		module_type = track_mode = array_type = shade_mode_1x = -999;
		inoct = gcr = std::numeric_limits<double>::quiet_NaN();

		ibeam = iskydiff = ignddiff = std::numeric_limits<double>::quiet_NaN();
		solazi = solzen = solalt = aoi = stilt = sazi = rot = btd = std::numeric_limits<double>::quiet_NaN();
		sunup = 0;

		poa = tpoa = pvt = dc = ac = std::numeric_limits<double>::quiet_NaN();
	}

	virtual ~cm_pvwattsv5_base()
	{
		if ( tccalc ) delete tccalc;
	}

	void setup_system_inputs()
	{
		dc_nameplate = as_double("system_capacity")*1000;
		dc_ac_ratio = as_double("dc_ac_ratio");
		ac_nameplate = dc_nameplate / dc_ac_ratio;
		inv_eff_percent = as_double("inv_eff");
		
		loss_percent = as_double("losses");        
		tilt = as_double("tilt");
		azimuth = as_double("azimuth");

		gamma = 0;
		use_ar_glass = false;

		module_type = as_integer("module_type");
		switch( module_type )
		{
		case 0: // standard module
			gamma = -0.0047; use_ar_glass = false; break;
		case 1: // premium module
			gamma = -0.0035; use_ar_glass = true; break;
		case 2: // thin film module
			gamma = -0.0020; use_ar_glass = false; break;
		}

		track_mode =  0;
		inoct = 45;
		shade_mode_1x = 0; // self shaded
		
		array_type = as_integer("array_type"); // 0, 1, 2, 3, 4		
		switch( array_type )
		{
		case 0: // fixed open rack
			track_mode = 0; inoct = 45; shade_mode_1x = 0; break;
		case 1: // fixed roof mount
			track_mode = 0; inoct = 49; shade_mode_1x = 0; break;
		case 2: // 1 axis self-shaded
			track_mode = 1; inoct = 45; shade_mode_1x = 0; break;
		case 3: // 1 axis backtracked
			track_mode = 1; inoct = 45; shade_mode_1x = 1; break;
		case 4: // 2 axis
			track_mode = 2; inoct = 45; shade_mode_1x = 0; break;
		case 5: // azimuth axis
			track_mode = 3; inoct = 45; shade_mode_1x = 0; break;
		}

		
		gcr = 0.4;
		if ( track_mode == 1 && is_assigned("gcr") ) gcr = as_double("gcr");
	}

	void initialize_cell_temp( double ts_hour, double last_tcell = -9999, double last_poa = -9999 )
	{
		tccalc = new pvwatts_celltemp ( inoct+273.15, PVWATTS_HEIGHT, ts_hour );
		if ( last_tcell > -99 && last_poa >= 0 )
			tccalc->set_last_values( last_tcell, last_poa );
	}

	
	int process_irradiance(int year, int month, int day, int hour, double minute, double ts_hour,
		double lat, double lon, double tz, double dn, double df, double alb )
	{
		irrad irr;
		irr.set_time( year, month, day, hour, minute, ts_hour );
		irr.set_location( lat, lon, tz );
		irr.set_sky_model(2, alb );
		irr.set_beam_diffuse(dn, df);
		irr.set_surface( track_mode, tilt, azimuth, 45.0, 
			shade_mode_1x == 1, // backtracking mode
			gcr );

		int code = irr.calc();
			

		irr.get_sun( &solazi, &solzen, &solalt, 0, 0, 0, &sunup, 0, 0, 0 );		
		irr.get_angles( &aoi, &stilt, &sazi, &rot, &btd );
		irr.get_poa( &ibeam, &iskydiff, &ignddiff, 0, 0, 0);	

		return code;
	}

	void powerout(double time, double &shad_beam, double shad_diff, double dni, double alb, double wspd, double tdry)
	{
		
		if (sunup > 0)
		{				
			if ( sunup > 0 && track_mode == 1
				&& shade_mode_1x == 0 ) // selfshaded mode
			{	
				double shad1xf = shade_fraction_1x( solazi, solzen, tilt, azimuth, gcr, rot );					
				shad_beam *= (ssc_number_t)(1-shad1xf);

				if ( shade_mode_1x == 0 && iskydiff > 0 )
				{
					double reduced_skydiff = iskydiff;
					double Fskydiff = 1.0;
					double reduced_gnddiff = ignddiff;
					double Fgnddiff = 1.0;
						
					// worst-case mask angle using calculated surface tilt
					double phi0 = 180/3.1415926*atan2( sind( stilt ), 1/gcr - cosd( stilt ) );

					// calculate sky and gnd diffuse derate factors
					// based on view factor reductions from self-shading
					diffuse_reduce( solzen, stilt,
						dni, iskydiff+ignddiff,
						gcr, phi0, alb, 1000,

						// outputs (pass by reference)
						reduced_skydiff, Fskydiff,
						reduced_gnddiff, Fgnddiff );

					if ( Fskydiff >= 0 && Fskydiff <= 1 ) iskydiff *= Fskydiff;
					else log( util::format("sky diffuse reduction factor invalid at time %lg: fskydiff=%lg, stilt=%lg", time, Fskydiff, stilt), SSC_NOTICE, (float)time );

					if ( Fgnddiff >= 0 && Fgnddiff <= 1 ) ignddiff *= Fgnddiff;
					else log( util::format("gnd diffuse reduction factor invalid at time %lg: fgnddiff=%lg, stilt=%lg", time, Fgnddiff, stilt), SSC_NOTICE, (float)time );
				}

			}

			// apply hourly shading factors to beam (if none enabled, factors are 1.0)
			ibeam *= shad_beam;
				
			// apply sky diffuse shading factor (specified as constant, nominally 1.0 if disabled in UI)
			iskydiff *= shad_diff;
				
			poa = ibeam + iskydiff +ignddiff;
				
			double wspd_corr = wspd < 0 ? 0 : wspd;					

			// module cover			
			tpoa = poa;
			if ( aoi > AOI_MIN && aoi < AOI_MAX )
			{
				double mod = iam( aoi, use_ar_glass );
				tpoa = poa - ( 1.0 - mod )*dni*cosd(aoi);
				if( tpoa < 0.0 ) tpoa = 0.0;
				if( tpoa > poa ) tpoa = poa;
			}
						
			// cell temperature
			pvt = (*tccalc)( poa, wspd_corr, tdry );

			// dc power output (Watts)
			dc = dc_nameplate*(1.0+gamma*(pvt-25.0))*tpoa/1000.0;

			// dc losses
			dc = dc*(1-loss_percent/100);

			// inverter efficiency
			double etanom = inv_eff_percent/100.0;
			double etaref = 0.9637;
			double A =  -0.0162;
			double B = -0.0059;
			double C =  0.9858;
			double pdc0 = ac_nameplate/etanom;
			double plr = dc / pdc0;
			ac = 0;
				
			if ( plr > 0 )
			{ // normal operation
				double eta = (A*plr + B/plr + C)*etanom/etaref;
				ac = dc*eta;
			}

			if ( ac > ac_nameplate ) // clipping
				ac = ac_nameplate;

			// make sure no negative AC values (no parasitic nighttime losses calculated)
			if ( ac < 0 ) ac = 0;
		}
		else
		{
			poa = 0;
			tpoa = 0;
			pvt = tdry;
			dc = 0;
			ac = 0;
		}
	}
};

class cm_pvwattsv5 : public cm_pvwattsv5_base
{
public:
	
	cm_pvwattsv5()
	{
		add_var_info( _cm_vtab_pvwattsv5_part1 );
		add_var_info( _cm_vtab_pvwattsv5_common );
		add_var_info( _cm_vtab_pvwattsv5_part2 );
		add_var_info(vtab_adjustment_factors);
	}


	void exec( ) throw( general_error )
	{
		// no lifetime simulation
		assign("system_use_lifetime_output", 0);

		// don't add "gen" output if battery enabled, gets added later
		if (!as_boolean("batt_simple_enable"))
			add_var_info(vtab_technology_outputs);

		std::auto_ptr<weather_data_provider> wdprov;

		if ( is_assigned( "solar_resource_file" ) )
		{
			const char *file = as_string("solar_resource_file");
			wdprov = std::auto_ptr<weather_data_provider>( new weatherfile( file ) );

			weatherfile *wfile = dynamic_cast<weatherfile*>(wdprov.get());
			if (!wfile->ok()) throw exec_error("pvwattsv5", wfile->message());
			if( wfile->has_message() ) log( wfile->message(), SSC_WARNING);
		}
		else if ( is_assigned( "solar_resource_data" ) )
		{
			wdprov = std::auto_ptr<weather_data_provider>( new weatherdata( lookup("solar_resource_data") ) );
		}
		else
			throw exec_error("pvwattsv5", "no weather data supplied");

		setup_system_inputs(); // setup all basic system specifications
				
		adjustment_factors haf( this, "adjust" );
		if ( !haf.setup() )
			throw exec_error("pvwattsv5", "failed to setup adjustment factors: " + haf.error() );
		
		// read all the shading input data and calculate the hourly factors for use subsequently
		shading_factor_calculator shad;
		if ( !shad.setup( this, "" ) )
			throw exec_error( "pvwattsv5", shad.get_error() );

		weather_header hdr;
		wdprov->header( &hdr );
					
		// assumes instantaneous values, unless hourly file with no minute column specified
		double ts_shift_hours = 0.0;
		bool instantaneous = true;
		if ( wdprov->has_data_column( weather_data_provider::MINUTE ) )
		{
			// if we have an file with a minute column, then
			// the starting time offset equals the time 
			// of the first record (for correct plotting)
			// this holds true even for hourly data with a minute column
			weather_record rec;
			if ( wdprov->read( &rec ) )
				ts_shift_hours = rec.minute/60.0;

			wdprov->rewind();
		}
		else if ( wdprov->nrecords() == 8760 )
		{
			// hourly file with no minute data column.  assume
			// integrated/averaged values and use mid point convention for interpreting results
			instantaneous = false;
			ts_shift_hours = 0.5;
		}
		else
			throw exec_error("pvwattsv5", "subhourly weather files must specify the minute for each record" );

		assign( "ts_shift_hours", var_data( (ssc_number_t)ts_shift_hours ) );

		weather_record wf;
		
		size_t nrec = wdprov->nrecords();
		size_t step_per_hour = nrec/8760;
		if ( step_per_hour < 1 || step_per_hour > 60 || step_per_hour*8760 != nrec )
			throw exec_error( "pvwattsv5", util::format("invalid number of data records (%d): must be an integer multiple of 8760", (int)nrec ) );
		
		/* allocate output arrays */		
		ssc_number_t *p_gh = allocate("gh", nrec);
		ssc_number_t *p_dn = allocate("dn", nrec);
		ssc_number_t *p_df = allocate("df", nrec);
		ssc_number_t *p_tamb = allocate("tamb", nrec);
		ssc_number_t *p_wspd = allocate("wspd", nrec);
		
		ssc_number_t *p_sunup = allocate("sunup", nrec);
		ssc_number_t *p_aoi = allocate("aoi", nrec);
		ssc_number_t *p_shad_beam = allocate("shad_beam_factor", nrec); // just for reporting output

		ssc_number_t *p_tcell = allocate("tcell", nrec);
		ssc_number_t *p_poa = allocate("poa", nrec);
		ssc_number_t *p_tpoa = allocate("tpoa", nrec);
		ssc_number_t *p_dc = allocate("dc", nrec);
		ssc_number_t *p_ac = allocate("ac", nrec);
		ssc_number_t *p_gen = allocate("gen", nrec);

		double ts_hour = 1.0/step_per_hour;

		initialize_cell_temp( ts_hour );

		double annual_kwh = 0; 
					
		size_t hour=0, idx=0;
		while( hour < 8760 )
		{
			
#define NSTATUS_UPDATES 50  // set this to the number of times a progress update should be issued for the simulation
			if ( hour % (8760/NSTATUS_UPDATES) == 0 )
			{
				float percent = 100.0f * ((float)hour+1) / ((float)8760);
				if ( !update( "", percent , (float)hour ) )
					throw exec_error("pvwattsv5", "simulation canceled at hour " + util::to_string(hour+1.0) );
			}


			for( size_t jj=0;jj<step_per_hour;jj++)
			{
				if (!wdprov->read( &wf ))
					throw exec_error("pvwattsv5", util::format("could not read data line %d of %d in weather file", (int)(idx+1), (int)nrec ));
				

				p_gh[idx] = (ssc_number_t)wf.gh;
				p_dn[idx] = (ssc_number_t)wf.dn;
				p_df[idx] = (ssc_number_t)wf.df;
				p_tamb[idx] = (ssc_number_t)wf.tdry;
				p_wspd[idx] = (ssc_number_t)wf.wspd;			
				p_tcell[idx] = (ssc_number_t)wf.tdry;
				
				double alb = 0.2; // do not increase albedo if snow exists in TMY2			
				if ( std::isfinite( wf.alb ) && wf.alb > 0 && wf.alb < 1 )
					alb = wf.alb;					
				
				int code = process_irradiance(wf.year, wf.month, wf.day, wf.hour, wf.minute, 
					instantaneous ? IRRADPROC_NO_INTERPOLATE_SUNRISE_SUNSET : ts_hour,
					hdr.lat, hdr.lon, hdr.tz, wf.dn, wf.df, alb );

				if ( -1 == code )
				{
					log(  util::format("beam irradiance exceeded extraterrestrial value at record [y:%d m:%d d:%d h:%d]", 
							 wf.year, wf.month, wf.day, wf.hour) );
				}
				else if ( 0 != code )
					throw exec_error( "pvwattsv5", 
						util::format("failed to process irradiation on surface (code: %d) [y:%d m:%d d:%d h:%d]", 
							code, wf.year, wf.month, wf.day, wf.hour));
			
				p_sunup[idx] = (ssc_number_t)sunup;
				p_aoi[idx] = (ssc_number_t)aoi;
				
				double shad_beam = 1.0;
				if ( shad.fbeam(hour, solalt, solazi, jj, step_per_hour) )
					shad_beam = shad.beam_shade_factor();
				
				p_shad_beam[idx] = (ssc_number_t)shad_beam ;
				
				if ( sunup > 0 )
				{
					powerout((double)idx, shad_beam, shad.fdiff(), wf.dn, alb, wf.wspd, wf.tdry);
					p_shad_beam[idx] = (ssc_number_t)shad_beam; // might be updated by 1 axis self shading so report updated value

					p_poa[idx] = (ssc_number_t)poa; // W/m2
					p_tpoa[idx] = (ssc_number_t)tpoa;  // W/m2
					p_tcell[idx] = (ssc_number_t)pvt;
					p_dc[idx] = (ssc_number_t)dc; // power, Watts
					p_ac[idx] = (ssc_number_t)ac; // power, Watts

					// accumulate hourly energy (kWh) (was initialized to zero when allocated)
					p_gen[idx] = (ssc_number_t)(ac * haf(hour) * 0.001f); // W to kW
					
					annual_kwh += p_gen[idx];
				}
						
				idx++;
			}

			hour++;
		}

		accumulate_monthly( "dc", "dc_monthly", 0.001*ts_hour );
		accumulate_monthly( "ac", "ac_monthly", 0.001*ts_hour );
		accumulate_monthly("gen", "monthly_energy", ts_hour);

		ssc_number_t *poam = accumulate_monthly( "poa", "poa_monthly", 0.001*ts_hour ); // convert to energy
		ssc_number_t *solrad = allocate( "solrad_monthly", 12 );
		ssc_number_t solrad_ann = 0;
		for ( int m=0;m<12;m++ )
		{
			solrad[m] = poam[m]/util::nday[m];
			solrad_ann += solrad[m];
		}
		assign( "solrad_annual", var_data( solrad_ann/12 ) );

		accumulate_annual( "ac", "ac_annual", 0.001*ts_hour );
		accumulate_annual("gen", "annual_energy", ts_hour);

		assign( "location", var_data( hdr.location ) );
		assign( "city", var_data( hdr.city ) );
		assign( "state", var_data( hdr.state ) );
		assign( "lat", var_data( (ssc_number_t)hdr.lat ) );
		assign( "lon", var_data( (ssc_number_t)hdr.lon ) );
		assign( "tz", var_data( (ssc_number_t)hdr.tz ) );
		assign( "elev", var_data( (ssc_number_t)hdr.elev ) );

		// for battery model, force inverter model
		assign("inverter_model", var_data((ssc_number_t)4) );
		assign("inverter_efficiency", var_data((ssc_number_t)(as_double("inv_eff"))));

		// metric outputs moved to technology
		double kWhperkW = 1000.0*annual_kwh / dc_nameplate;
		// adjustment for timestep values
		kWhperkW *= ts_hour;
		assign("capacity_factor", var_data((ssc_number_t)(kWhperkW / 87.6)));
		assign("kwh_per_kw", var_data((ssc_number_t)kWhperkW));
	}
};

DEFINE_MODULE_ENTRY( pvwattsv5, "PVWatts V5 - integrated hourly weather reader and PV system simulator.", 3 )



/* *****************************************************************************
			SINGLE TIME STEP VERSION   29oct2014
 ***************************************************************************** */


static var_info _cm_vtab_pvwattsv5_1ts_weather[] = {
	{ SSC_INPUT,        SSC_NUMBER,      "year",                     "Year",                                        "yr",     "",                        "PVWatts",      "*",                       "",               "" },
	{ SSC_INPUT,        SSC_NUMBER,      "month",                    "Month",                                       "mn",     "1-12",                    "PVWatts",      "*",                       "",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "day",                      "Day",                                         "dy",     "1-days in month",         "PVWatts",      "*",                       "",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hour",                     "Hour",                                        "hr",     "0-23",                    "PVWatts",      "*",                       "",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "minute",                   "Minute",                                      "min",    "0-59",                    "PVWatts",      "*",                       "",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "lat",                      "Latitude",                                    "deg",    "",                        "PVWatts",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "lon",                      "Longitude",                                   "deg",    "",                        "PVWatts",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tz",                       "Time zone",                                   "hr",     "",                        "PVWatts",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "beam",                     "Beam normal irradiance",                      "W/m2",   "",                        "PVWatts",      "*",                       "",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "diffuse",                  "Diffuse irradiance",                          "W/m2",   "",                        "PVWatts",      "*",                       "",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tamb",                     "Ambient temperature",                         "C",      "",                        "PVWatts",      "*",                       "",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "wspd",                     "Wind speed",                                  "m/s",    "",                        "PVWatts",      "*",                       "",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "alb",                      "Albedo",                                      "frac",     "",                      "PVWatts",      "?=0.2",                     "",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "time_step",                "Time step of input data",                     "hr",    "",                         "PVWatts",      "?=1",                     "POSITIVE",                  "" },
	
	var_info_invalid };
	
static var_info _cm_vtab_pvwattsv5_1ts_outputs[] = {
	/* input/output variable: tcell & poa from previous time must be given */
	{ SSC_INOUT,        SSC_NUMBER,      "tcell",                    "Module temperature",                          "C",      "",                        "PVWatts",      "*",                       "",                          "" },	
	{ SSC_INOUT,        SSC_NUMBER,      "poa",                      "Plane of array irradiance",                   "W/m2",   "",                        "PVWatts",      "*",                       "",                          "" },
			
	/* outputs */
	{ SSC_OUTPUT,       SSC_NUMBER,      "dc",                      "DC array output",                             "Wdc",    "",                        "PVWatts",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "ac",                      "AC system output",                            "Wac",    "",                        "PVWatts",      "*",                       "",                          "" },

	var_info_invalid };

class cm_pvwattsv5_1ts : public cm_pvwattsv5_base
{
public:
	
	cm_pvwattsv5_1ts()
	{
		add_var_info( _cm_vtab_pvwattsv5_1ts_weather );
		add_var_info( _cm_vtab_pvwattsv5_common );
		add_var_info( _cm_vtab_pvwattsv5_1ts_outputs );
	}

	void exec( ) throw( general_error )
	{	
		int year = as_integer("year");
		int month = as_integer("month");
		int day = as_integer("day");
		int hour = as_integer("hour");
		double minute = as_double("minute");
		double lat = as_double("lat");
		double lon = as_double("lon");
		double tz = as_double("tz");
		double beam = as_double("beam");
		double diff = as_double("diffuse");
		double tamb = as_double("tamb");
		double wspd = as_double("wspd");
		double alb = as_double("alb");
		double time_step = as_double("time_step");
			
		double last_tcell = as_double("tcell");
		double last_poa = as_double("poa");

		setup_system_inputs();
		initialize_cell_temp( time_step, last_tcell, last_poa );
		
		int code = process_irradiance(year, month, day, hour, minute, 
			IRRADPROC_NO_INTERPOLATE_SUNRISE_SUNSET,
			lat, lon, tz, beam, diff, alb );

		if (code != 0)
			throw exec_error( "pvwattsv5_1ts", "failed to calculate plane of array irradiance with given input parameters" );

		double shad_beam = 1.0;
		powerout(0, shad_beam, 1.0, beam, alb, wspd, tamb);

		assign( "poa", var_data( (ssc_number_t)poa ) );
		assign( "tcell", var_data( (ssc_number_t)pvt ) );
		assign( "dc", var_data( (ssc_number_t)dc ) );
		assign( "ac", var_data( (ssc_number_t)ac ) );		
	}
};

DEFINE_MODULE_ENTRY( pvwattsv5_1ts, "pvwattsv5_1ts- single timestep calculation of PV system performance.", 1 )

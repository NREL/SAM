#include "core.h"
#include "lib_windfile.h"
#include "lib_windwatts.h"
// for adjustment factors
#include "common.h"
#include "lib_util.h"

static var_info _cm_vtab_windpower[] = {
//	  VARTYPE           DATATYPE         NAME                                       LABEL                                  UNITS     META     GROUP             REQUIRED_IF                                 CONSTRAINTS                                        UI_HINTS
	{ SSC_INPUT,        SSC_STRING,      "wind_resource_filename",                  "local wind data file path",           "",       "",      "WindPower",      "?",                                        "LOCAL_FILE",                                       "" },
	{ SSC_INPUT,        SSC_TABLE,       "wind_resource_data",                      "wind resouce data in memory",         "",       "",      "WindPower",      "?",                                        "",                                                 "" },

	{ SSC_INPUT,        SSC_NUMBER,      "wind_resource_shear",                     "Shear exponent",                      "",       "",      "WindPower",      "*",                                        "",                                                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "wind_resource_turbulence_coeff",          "Turbulence coefficient",              "%",      "",      "WindPower",      "*",                                        "",                                                 "" },
	{ SSC_INPUT,		SSC_NUMBER,		 "system_capacity",							"Nameplate capacity",				   "kW",	 "",	  "WindPower",		"*",										"",													"" },


//	{ SSC_INPUT,        SSC_NUMBER,      "meas_ht",                                 "Height of resource measurement",      "m",      "",      "WindPower",      "*",                                        "INTEGER",                                          "" },
//	{ SSC_INPUT,        SSC_NUMBER,      "elevation",                               "Elevation",                           "m",      "",      "WindPower",      "*",                                        "",		                                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "wind_resource_model_choice",              "Hourly or Weibull model",		       "0/1",    "",      "WindPower",      "*",                                        "INTEGER",                                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "weibull_reference_height",                "Reference height for Weibull wind speed","m",   "",      "WindPower",      "?=50",                                     "MIN=0",		                                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "weibull_k_factor",                        "Weibull K factor for wind resource",  "",       "",      "WindPower",      "wind_resource_model_choice=1",             "",		                                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "weibull_wind_speed",                      "Average wind speed for Weibull model","",       "",      "WindPower",      "wind_resource_model_choice=1",             "",		                                            "" },
																																																		                             
	{ SSC_INPUT,        SSC_NUMBER,      "wind_turbine_rotor_diameter",             "Rotor diameter",                      "m",      "",      "WindPower",      "*",                                        "",                                                 "" },
	{ SSC_INOUT,        SSC_ARRAY,       "wind_turbine_powercurve_windspeeds",      "Power curve wind speed array",        "m/s",    "",      "WindPower",      "*",                                        "",                                                 "" }, 
	{ SSC_INOUT,        SSC_ARRAY,       "wind_turbine_powercurve_powerout",        "Power curve turbine output array",    "kW",     "",      "WindPower",      "*",                                        "LENGTH_EQUAL=wind_turbine_powercurve_windspeeds",  "" },
//	{ SSC_INPUT,        SSC_ARRAY,       "wind_turbine_powercurve_pc_rpm",	        "Turbine RPM curve",                   "rpm",    "",      "WindPower",      "*",                                        "LENGTH_EQUAL=wind_turbine_powercurve_windspeeds",  "" },
//	{ SSC_INPUT,        SSC_ARRAY,       "wind_turbine_powercurve_hub_efficiency",  "Array of hub efficiencies",		   "%",      "",      "WindPower",      "*",                                        "LENGTH_EQUAL=wind_turbine_powercurve_windspeeds",  "" },
//	{ SSC_INPUT,        SSC_NUMBER,      "wind_turbine_cutin",                      "Cut-in wind speed",                   "m/s",    "",      "WindPower",      "*",                                        "",                                                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "wind_turbine_hub_ht",                     "Hub height",                          "m",      "",      "WindPower",      "*",                                        "",                                                 "" },
//	{ SSC_INPUT,        SSC_NUMBER,      "wind_turbine_ctl_mode",                   "Control mode",                        "0/1/2",  "",      "WindPower",      "*",                                        "INTEGER",                                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "wind_turbine_max_cp",                     "Max cp",						       "",       "",      "WindPower",      "wind_resource_model_choice=1",             "",		                                            "" },
																																																		                             
	{ SSC_INPUT,        SSC_ARRAY,       "wind_farm_xCoordinates",                  "Turbine X coordinates",               "m",      "",      "WindPower",      "*",                                        "",                                                 "" },
	{ SSC_INPUT,        SSC_ARRAY,       "wind_farm_yCoordinates",                  "Turbine Y coordinates",               "m",      "",      "WindPower",      "*",                                        "LENGTH_EQUAL=wind_farm_xCoordinates",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "wind_farm_losses_percent",                "Percentage losses",                   "%",      "",      "WindPower",      "*",                                        "",                                                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "wind_farm_wake_model",                    "Wake Model",                          "0/1/2",  "",      "WindPower",      "*",                                        "INTEGER",                                          "" },
																																												                            
																																												                            
	// OUTPUTS ----------------------------------------------------------------------------													annual_energy									                            
	{ SSC_OUTPUT,       SSC_ARRAY,       "turbine_output_by_windspeed_bin",         "Turbine output by wind speed bin",    "kW",     "",      "Power Curve",      "",                                        "LENGTH_EQUAL=wind_turbine_powercurve_windspeeds",  "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "wind_direction",                          "Wind direction",                      "deg",    "",      "Time Series",      "*",                                        "",                                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "wind_speed",                              "Wind speed",                          "m/s",    "",      "Time Series",      "*",                                        "",                                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "temp",                                    "Air temperature",                     "'C",     "",      "Time Series",      "*",                                        "",                                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "pressure",                                "Pressure",                            "atm",    "",      "Time Series",      "*",                                        "",                                      "" },
	
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_energy",                          "Monthly Energy",                      "kWh",     "",      "Monthly",     "*",                                        "LENGTH=12",                                        "" },

	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_energy",                           "Annual Energy",                       "kWh",     "",      "Annual",      "*",                                        "",                                                 "" },
	{ SSC_OUTPUT,		SSC_NUMBER,		 "capacity_factor",							"Capacity factor",					   "%",		 "",	  "Annual",		 "*",										"",													"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		 "kwh_per_kw",								"First year kWh/kW",				   "kWh/kW",		 "",	  "Annual",		 "*",										"",													"" },



var_info_invalid };

class winddata : public winddata_provider
{
	size_t irecord;
	util::matrix_t<float> data;
public:
	winddata( var_data *data_table )
	{
		irecord = 0;

		if ( data_table->type != SSC_TABLE ) 
		{
			m_errorMsg = "wind data must be an SSC table variable with fields: "
				"(number): lat, lon, elev, year, "
				"(array): heights, fields (temp=1,pres=2,speed=3,dir=4), "
				"(matrix): data (nstep x Nheights)";
			return;
		}
		
		lat = get_number( data_table, "lat" );
		lon = get_number( data_table, "lon" );
		elev = get_number( data_table, "elev" );
		year = (int) get_number( data_table, "year" );

		size_t len = 0;
		ssc_number_t *p = get_vector( data_table, "heights", &len );
		for( size_t i=0;i<len;i++ )
			m_heights.push_back( (double)p[i] );

		p = get_vector( data_table, "fields", &len );
		for( size_t i=0;i<len;i++ )
			m_dataid.push_back( (int)p[i] );

		if ( m_dataid.size() != m_heights.size() || m_heights.size() == 0 ) return;

		if ( var_data *D = data_table->table.lookup("data") )
			if ( D->type == SSC_MATRIX )
				data = D->num;
	}
	
	virtual size_t nrecords()
	{
		return data.nrows();
	}

	ssc_number_t get_number( var_data *v, const char *name )
	{
		if ( var_data *value = v->table.lookup( name ) )
		{
			if ( value->type == SSC_NUMBER )
				return value->num;
		}

		return std::numeric_limits<ssc_number_t>::quiet_NaN();
	}

	ssc_number_t *get_vector( var_data *v, const char *name, size_t *len )
	{
		ssc_number_t *p = 0;
		*len = 0;
		if ( var_data *value = v->table.lookup( name ) )
		{
			if ( value->type == SSC_ARRAY )
			{
				*len = value->num.length();
				p = value->num.data();
			}
		}
		return p;
	}

	virtual ~winddata()
	{
		// nothing to do
	}
	
	virtual bool read_line( std::vector<double> &values )
	{
		if (irecord >= data.nrows() 
			|| data.ncols() == 0 
			|| data.nrows() == 0 ) return false;

		values.resize( data.ncols(), 0.0 );
		for( size_t j=0;j<data.ncols();j++ )
			values[j] = (double) data(irecord,j);

		irecord++;
		return true;
	}
};

class cm_windpower : public compute_module
{
private:
public:
	
	cm_windpower()
	{
		add_var_info( _cm_vtab_windpower );
		// performance adjustment factors
		add_var_info(vtab_adjustment_factors);
		add_var_info(vtab_technology_outputs);
	}

	void exec( ) throw( general_error )
	{
		wind_power_calculator wpc;

		wpc.m_dShearExponent =  as_double("wind_resource_shear");
		wpc.m_dHubHeight = as_double("wind_turbine_hub_ht");
		wpc.m_dRotorDiameter = as_double("wind_turbine_rotor_diameter");
		//double meas_ht = as_double("meas_ht");
		//wpc.m_dCutInSpeed = as_double("wind_turbine_cutin");
		wpc.m_dLossesAbsolute = 0 ; // as_double("lossc");
		wpc.m_dLossesPercent = as_double("wind_farm_losses_percent")/100.0;
		wpc.m_dWakeDecayCoefficient = 0.07;	// necessary for Park model
		wpc.m_iWakeModelChoice = as_integer("wind_farm_wake_model");
		wpc.m_dTurbulenceIntensity = as_double("wind_resource_turbulence_coeff");

		ssc_number_t *pc_w = as_array( "wind_turbine_powercurve_windspeeds", &wpc.m_iLengthOfTurbinePowerCurveArray );
		ssc_number_t *pc_p = as_array( "wind_turbine_powercurve_powerout", NULL );
		//ssc_number_t *pc_rpm = as_array( "pc_rpm", NULL );

		ssc_number_t *wind_farm_xCoordinates = as_array( "wind_farm_xCoordinates", &wpc.m_iNumberOfTurbinesInFarm );
		ssc_number_t *wind_farm_yCoordinates = as_array( "wind_farm_yCoordinates", NULL );
		if (wpc.m_iNumberOfTurbinesInFarm < 1)
			throw exec_error( "windpower", util::format("the number of wind turbines was zero.") );
		if (wpc.m_iNumberOfTurbinesInFarm > wpc.GetMaxTurbines())
			throw exec_error( "windpower", util::format("the wind model is only configured to handle up to %d turbines.", wpc.GetMaxTurbines()) );

		adjustment_factors haf(this, "adjust");
		if (!haf.setup())
			throw exec_error("windpower", "failed to setup adjustment factors: " + haf.error());

		// setup the power curve data
		wpc.m_adPowerCurveWS.resize(wpc.m_iLengthOfTurbinePowerCurveArray);
		wpc.m_adDensityCorrectedWS.resize(wpc.m_iLengthOfTurbinePowerCurveArray);
		wpc.m_adPowerCurveKW.resize(wpc.m_iLengthOfTurbinePowerCurveArray);
		wpc.m_adPowerCurveRPM.resize(wpc.m_iLengthOfTurbinePowerCurveArray);
		size_t i;
		for (i=0;i<wpc.m_iLengthOfTurbinePowerCurveArray;i++)
		{
			wpc.m_adPowerCurveWS[i] = (double)pc_w[i];
			wpc.m_adDensityCorrectedWS[i] = (double)pc_w[i]; //for starters, corrected by air density at each time step
			wpc.m_adPowerCurveKW[i] = (double)pc_p[i];
			wpc.m_adPowerCurveRPM[i] = 0.0;//(double)pc_rpm[i];
		}

		
		// now choose which model to run
		// 0=time step farm model (hourly or subhourly array outputs), 1=weibull statistical model (single outputs)
		int iModelType = as_integer("wind_resource_model_choice"); 


		size_t nstep = 8760;		
		std::auto_ptr<winddata_provider> wdprov;
		if ( iModelType == 0 )
		{
			// initialize the weather file reader here to find out the number of steps
			// before the output arrays are allocated
			
			if ( is_assigned( "wind_resource_filename" ) )
			{
				const char *file = as_string("wind_resource_filename");

				// read the wind data file
				windfile *wp = new windfile(file);

				nstep = wp->nrecords();

				// assign the pointer
				wdprov = std::auto_ptr<winddata_provider>( wp );

				// make sure it's OK
				if (!wp->ok()) 
					throw exec_error("windpower", "failed to read local weather file: " + std::string(file) + " " + wp->error());
			}
			else if ( is_assigned( "wind_resource_data" ) )
			{
				wdprov = std::auto_ptr<winddata_provider>( new winddata( lookup("wind_resource_data") ) );
			}
			else
				throw exec_error("windpower", "no wind resource data supplied");

		}

		// check for leap day
		bool contains_leap_day = false;
		if (std::fmod((double)nstep, 8784) == 0)
		{
			contains_leap_day = true;
			int leap_steps_per_hr = nstep / 8784; //this will be an even multiple of 8760 because of the if statement above
			log("This weather file appears to contain leap day. SAM will skip all of the lines of the weather file that occur on leap day. If your weather file does not contain a leap day, please check your file.", SSC_WARNING);
			//throw exec_error("windpower", "Error: this weather file appears to contain leap day. SAM requires weather files to be a multiple of 8760 timesteps long.");
			nstep = leap_steps_per_hr * 8760; //need to resize nrec so that it is correct for holding output variables
		}
		
		// check for even multiple of 8760 timesteps (subhourly)
		size_t steps_per_hour = nstep / 8760;

		if ( steps_per_hour * 8760 != nstep  && !contains_leap_day)
			throw exec_error("windpower", util::format("invalid number of data records (%d): must be an integer multiple of 8760", (int)nstep));

		// allocate the time step output vectors to the right length
		ssc_number_t *farmpwr = allocate("gen", nstep);
		ssc_number_t *wspd = allocate("wind_speed", nstep);
		ssc_number_t *wdir = allocate("wind_direction", nstep);
		ssc_number_t *air_temp = allocate("temp", nstep);
		ssc_number_t *air_pres = allocate("pressure", nstep);


		if (iModelType == 1) // doing a Weibull estimate, not an hourly or subhourly simulation
		{
			// allocate this output for the Weibull estimate only
			ssc_number_t *turbine_output = allocate("turbine_output_by_windspeed_bin", wpc.m_iLengthOfTurbinePowerCurveArray);
			std::vector<double> turbine_outkW(wpc.m_iLengthOfTurbinePowerCurveArray); 

			double weibull_k = as_double("weibull_k_factor");
			double max_cp = as_double("wind_turbine_max_cp");
			double avg_speed = as_double("weibull_wind_speed");
			double ref_height = as_double("weibull_reference_height");
			//double elevation = as_double("elevation");
			
			//ssc_number_t *hub_efficiency = as_array( "hub_efficiency", NULL );
			//std::vector<double> dp_hub_eff(wpc.m_iLengthOfTurbinePowerCurveArray);
			//for (i=0;i<wpc.m_iLengthOfTurbinePowerCurveArray;i++)
			//	dp_hub_eff[i] = (double)hub_efficiency[i];

			double turbine_kw = wpc.turbine_output_using_weibull(weibull_k, max_cp, avg_speed, ref_height, &turbine_outkW[0]);
			turbine_kw = turbine_kw * (1 - wpc.m_dLossesPercent) - wpc.m_dLossesAbsolute;

			ssc_number_t farm_kw = (ssc_number_t) turbine_kw * wpc.m_iNumberOfTurbinesInFarm;

			for (i = 0; i < nstep; i++) //nstep is always 8760 for Weibull
			{
				farmpwr[i] = farm_kw / (ssc_number_t)nstep; // fill "gen"
				farmpwr[i] *= haf(i); //apply adjustment factor/availability and curtailment losses
			}

			for (i=0; i<wpc.m_iLengthOfTurbinePowerCurveArray; i++)
				turbine_output[i] = (ssc_number_t) turbine_outkW[i];

			accumulate_monthly("gen", "monthly_energy");
			accumulate_annual("gen", "annual_energy");

			// metric outputs moved to technology
			double kWhperkW = 0.0;
			double nameplate = as_double("system_capacity");
			double annual_energy = 0.0;
			for (i = 0; i < nstep; i++)
				annual_energy += farmpwr[i];
			if (nameplate > 0) kWhperkW = annual_energy / nameplate;
			assign("capacity_factor", var_data((ssc_number_t)(kWhperkW / 87.6)));
			assign("kwh_per_kw", var_data((ssc_number_t)kWhperkW));

			return;
		}
		
		
		/* wind_turbine_ctl_mode hardwired to '2'.  apparently not implemented
		  correctly for modes 0 and 1, so no point exposing it.
		  apd 03jan11 */

		wpc.m_iControlMode = 2; // if control mode is changed from 2, rated power has to be set!
		//wpc.m_dRatedPower = 0;

		// X-Y coordinates are necessary for calculation of output from farm
		wpc.m_adXCoords.resize(wpc.m_iNumberOfTurbinesInFarm);
		wpc.m_adYCoords.resize(wpc.m_iNumberOfTurbinesInFarm);
		for (i=0;i<wpc.m_iNumberOfTurbinesInFarm;i++)
		{
			wpc.m_adXCoords[i] = (double)wind_farm_xCoordinates[i];
			wpc.m_adYCoords[i] = (double)wind_farm_yCoordinates[i];
		}

		std::vector<double> Power(wpc.m_iNumberOfTurbinesInFarm), Thrust(wpc.m_iNumberOfTurbinesInFarm), 
							Eff(wpc.m_iNumberOfTurbinesInFarm), Wind(wpc.m_iNumberOfTurbinesInFarm), Turb(wpc.m_iNumberOfTurbinesInFarm),
							DistDown(wpc.m_iNumberOfTurbinesInFarm), DistCross(wpc.m_iNumberOfTurbinesInFarm);

		// these are only useful for debugging until matrix variables can be passed back as outputs
		util::matrix_t<ssc_number_t> &mat_wtpwr = allocate_matrix( "wtpwr", nstep, wpc.m_iNumberOfTurbinesInFarm );
		util::matrix_t<ssc_number_t> &mat_wteff = allocate_matrix( "wteff", nstep, wpc.m_iNumberOfTurbinesInFarm );
		util::matrix_t<ssc_number_t> &mat_wtvel = allocate_matrix( "wtvel", nstep, wpc.m_iNumberOfTurbinesInFarm );
		util::matrix_t<ssc_number_t> &mat_thrust = allocate_matrix("dn", nstep, wpc.m_iNumberOfTurbinesInFarm );
		util::matrix_t<ssc_number_t> &mat_turb = allocate_matrix("cs", nstep, wpc.m_iNumberOfTurbinesInFarm );
		util::matrix_t<ssc_number_t> &mat_distdown = allocate_matrix("dist_d", nstep, wpc.m_iNumberOfTurbinesInFarm );
		util::matrix_t<ssc_number_t> &mat_distcross = allocate_matrix("dist_c", nstep, wpc.m_iNumberOfTurbinesInFarm );

		std::string cwd = "";
		if (IMITATE_OPENWIND) cwd = util::get_cwd(); // if we're trying to imitate openWind, we might want to create detailed debug files
		bool bCreateFarmOutput = (cwd == "C:\\svn_NREL\\main\\samwx\\deploy"); // limit debug file creation to tom's computers

		// if the model needs arrays allocated, this command does it once - has to be done after all properties are set above
		if (!wpc.InitializeModel() )
			throw exec_error( "windpower", util::format("error allocating memory: %s",  wpc.GetErrorDetails().c_str() )  );

		double annual = 0.0;
		ssc_number_t *monthly = allocate( "monthly_energy", 12 );
		for( i=0;i<12;i++ ) monthly[i] = 0.0f;

		i = 0;
		for( size_t hr=0;hr<8760;hr++ )
		{
			int imonth = util::month_of(hr)-1;

			for( size_t istep=0;istep < steps_per_hour;istep++ )
			{
				if ( i % (nstep/20) == 0)
					update( "", 100.0f * ((float)i) / ((float)nstep), (float)i );

				double wind, dir, temp, pres, closest_dir_meas_ht;

				//skip leap day if applicable
				if (contains_leap_day)
				{
					if (hr == 1416) //(31 days in Jan  + 28 days in Feb) * 24 hours a day, +1 to be the start of Feb 29, -1 because of 0 indexing
						for (int j = 0; j < 24 * steps_per_hour; j++) //trash 24 hours' worth of lines in the weather file to skip the entire day of Feb 29
						{
							if (!wdprov->read(wpc.m_dHubHeight, &wind, &dir, &temp, &pres, &wpc.m_dMeasurementHeight, &closest_dir_meas_ht, true))
								throw exec_error("windpower", util::format("error reading wind resource file at %d: ", i) + wdprov->error());
						}
				} //now continue with the normal process, none of the counters have been incremented so everything else should be ok

				// if wf.read is set to interpolate (last input), and it's able to do so, then it will set wpc.m_dMeasurementHeight equal to hub_ht
				// direction will not be interpolated, pressure and temperature will be if possible
				if (!wdprov->read( wpc.m_dHubHeight, &wind, &dir, &temp, &pres, &wpc.m_dMeasurementHeight, &closest_dir_meas_ht, true))
					throw exec_error( "windpower", util::format("error reading wind resource file at %d: ", i) + wdprov->error() );

				if ( fabs(wpc.m_dMeasurementHeight - wpc.m_dHubHeight) > 35.0 )
					throw exec_error( "windpower", util::format("the closest wind speed measurement height (%lg m) found is more than 35 m from the hub height specified (%lg m)", wpc.m_dMeasurementHeight, wpc.m_dHubHeight ));

				if ( fabs(closest_dir_meas_ht - wpc.m_dMeasurementHeight) > 10.0 )
				{
					if (i>0) // if this isn't the first hour, then it's probably because of interpolation
					{
						// probably interpolated wind speed, but could not interpolate wind direction because the directions were too far apart.
						// first, verify:
						if ( (wpc.m_dMeasurementHeight == wpc.m_dHubHeight) && (closest_dir_meas_ht != wpc.m_dHubHeight) )
							// now, alert the user of this discrepancy
							throw exec_error( "windpower", util::format("on hour %d, SAM interpolated the wind speed to an %lgm measurement height, but could not interpolate the wind direction from the two closest measurements because the directions encountered were too disparate", i+1, wpc.m_dMeasurementHeight ));
						else
							throw exec_error( "windpower", util::format("SAM encountered an error at hour %d: hub height = %lg, closest wind speed meas height = %lg, closest wind direction meas height = %lg ", i+1, wpc.m_dHubHeight, wpc.m_dMeasurementHeight, closest_dir_meas_ht ));
					}
					else
						throw exec_error( "windpower", util::format("the closest wind speed measurement height (%lg m) and direction measurement height (%lg m) were more than 10m apart", wpc.m_dMeasurementHeight, closest_dir_meas_ht ));
				}

				double farmp = 0;

				if ( (int)wpc.m_iNumberOfTurbinesInFarm != wpc.wind_power( 
							/* inputs */
							wind,	/* m/s */
							dir,	/* degrees */
							pres,	/* Atm */
							temp,	/* deg C */

							/* outputs */
							&farmp,
							&Power[0],
							&Thrust[0],
							&Eff[0],
							&Wind[0],
							&Turb[0],
							&DistDown[0],
							&DistCross[0]) ) 
					throw exec_error( "windpower", util::format("error in wind calculation at time %d, details: %s", i, wpc.GetErrorDetails().c_str()) );


				farmpwr[i] = (ssc_number_t) farmp*haf( hr ); //adjustment factors are constrained to be hourly, not sub-hourly, so it's correct for this to be indexed on the hour
				wspd[i] = (ssc_number_t) wind;
				wdir[i] = (ssc_number_t) dir;
				air_temp[i] = (ssc_number_t) temp;
				air_pres[i] = (ssc_number_t) pres;

				if (bCreateFarmOutput)
				{
					for (size_t j=0; j<wpc.m_iNumberOfTurbinesInFarm; j++)
					{
						mat_wtpwr.at(i,j) = (ssc_number_t) Power[j];
						mat_wtvel.at(i,j) = (ssc_number_t) Wind[j];
						mat_thrust.at(i,j) = (ssc_number_t)Thrust[j];
						mat_turb.at(i,j) = (ssc_number_t)Turb[j];
						mat_wteff.at(i,j) = (ssc_number_t) Eff[j];

						mat_distdown.at(i,j) = (ssc_number_t) DistDown[j];
						mat_distcross.at(i,j) = (ssc_number_t) DistCross[j];
					}
				}

				// accumulate monthly and annual energy
				monthly[imonth] += farmpwr[i]/steps_per_hour;
				annual += farmpwr[i]/steps_per_hour;
				
				i++;
			} // end steps_per_hour loop
		} // end 1->8760 loop

		assign( "annual_energy", var_data((ssc_number_t)annual) );

		// metric outputs moved to technology
		double kWhperkW = 0.0;
		double nameplate = as_double("system_capacity");
		if (nameplate > 0) kWhperkW = annual / nameplate;
		assign("capacity_factor", var_data((ssc_number_t)(kWhperkW / 87.6)));
		assign("kwh_per_kw", var_data((ssc_number_t)kWhperkW));


		if (bCreateFarmOutput)
		{
			// make separate files so it's easy for Excel 2010 to automatically update sheets to display info
			// files should get put into directory with .exe file
			util::stdfile f1;
			std::string s = "windfarm_diagnostic_turbine_locations.txt";
			if (f1.open(s.c_str(),"w") )
			{
				// first, create a table of all the inputs to the farm
				fprintf(f1, "Turbine#\tX\tY\n", s.c_str() );
				for ( i=0; i<wpc.m_iNumberOfTurbinesInFarm; i++)
					fprintf(f1, "%d\t%lg\t%lg\n", i, wpc.m_adXCoords[i], wpc.m_adYCoords[i] );
				f1.close();
			}

			//s = "windfarm_diagnostic_weather_inputs.txt";
			//if (f1.open(s.c_str(),"w") )
			//{
			//	fprintf(f1, "Hour\tFree Flow WS\tWind Direction\tAir Temperature\tAir Pressure\n" );
			//	for ( i=0; i<nstep; i++)
			//	{
			//		if ( i % (nstep/10) == 0) update( "writing farm inputs", 100.0f * ((float)i) / ((float)nstep), (float)i );
			//		fprintf(f1, "%d\t%lg\t%lg\t%lg\t%lg\n", i, wspd[i], wdir[i], air_temp[i], air_pres[i] );
			//	}
			//	f1.close();
			//}

			s = "windfarm_diagnostic_turbine_info_" + wpc.GetWakeModelShortName() + ".txt";
			if (f1.open(s.c_str(),"w") )
			{
				fprintf(f1, "Wake model:\t%s\n", wpc.GetWakeModelName().c_str() );
				fprintf(f1, "Hour\tFree Flow WS\tWind Direction\tTurbine #\tWS at Turbine\tTurbine Output\tThrust\tTurbulence Intensity\tDistance Downwind\tDistance Crosswind\n" );
				for (i=0;i<nstep;i++)
				{
					if ( i % (nstep/20) == 0) update( "writing turbine specific outputs", 100.0f * ((float)i) / ((float)nstep), (float)i );
					for (size_t j=0; j<wpc.m_iNumberOfTurbinesInFarm; j++)
						fprintf(f1, "%d\t%lg\t%lg\t%d\t%lg\t%lg\t%lg\t%lg\t%lg\t%lg\n", i, wspd[i], wdir[i], j, mat_wtvel.at(i,j), mat_wtpwr.at(i,j), mat_thrust.at(i,j), mat_turb.at(i,j), mat_distdown.at(i,j), mat_distcross.at(i,j) );
				}
				f1.close();
			}
		} // create wind farm debug output files



	} // exec
};

DEFINE_MODULE_ENTRY( windpower, "Utility scale wind farm model (adapted from TRNSYS code by P.Quinlan and openWind software by AWS Truepower)", 2 );



#include <string>
#include <cmath>
#include <limits>
#include <vector>
#include <memory>

#include "core.h"
// for adjustment factors
#include "common.h"

// for battery model, leverage common code with standalone compute module.
#include "cmod_battery.h"
#include "lib_power_electronics.h"

#include "lib_weatherfile.h"
#include "lib_irradproc.h"
#include "lib_cec6par.h"
#include "lib_sandia.h"
#include "lib_pvinv.h"
#include "6par_jacobian.h"
#include "6par_lu.h"
#include "6par_search.h"
#include "6par_newton.h"
#include "6par_gamma.h"
#include "6par_solve.h"
#include "lib_pvshade.h"
#include "lib_snowmodel.h"
#include "lib_iec61853.h"

#include "lib_util.h"

// non linear shading database
#include "lib_pv_shade_loss_mpp.h"
// comment following define if do not want shading database validation outputs
//#define SHADE_DB_OUTPUTS


#ifndef M_PI
#define M_PI 3.14159265358979323846264338327
#endif
#define sind(x) sin( (M_PI/180.0)*(x) )
#define cosd(x) cos( (M_PI/180.0)*(x) )

static var_info _cm_vtab_pvsamv1[] = {
/*   VARTYPE           DATATYPE         NAME                                            LABEL                                                   UNITS      META                             GROUP                  REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_STRING,      "solar_resource_file",                         "Weather file in TMY2, TMY3, EPW, or SAM CSV.",         "",         "",                              "pvsamv1",              "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_TABLE,       "solar_resource_data",                         "Weather data",                                         "",         "lat,lon,tz,elev,year,month,hour,minute,gh,dn,df,poa,tdry,twet,tdew,rhum,pres,snow,alb,aod,wspd,wdir",    "pvsamv1",              "?",                        "",                              "" },
	
	// transformer model percent of rated ac output
	{ SSC_INPUT, SSC_NUMBER, "transformer_no_load_loss", "Power transformer no load loss", "%", "", "pvsamv1", "?=0", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "transformer_load_loss", "Power transformer load loss", "%", "", "pvsamv1", "?=0", "", "" },
	
	// optional for lifetime analysis
	{ SSC_INPUT,        SSC_NUMBER,      "system_use_lifetime_output",                  "PV lifetime simulation",                               "0/1",      "",                              "pvsamv1",             "?=0",                        "INTEGER,MIN=0,MAX=1",          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "analysis_period",                             "Lifetime analysis period",                             "years",    "",                              "pvsamv1",             "system_use_lifetime_output=1",   "",                             "" },
	{ SSC_INPUT,        SSC_ARRAY,       "dc_degradation",                              "Annual module degradation",                            "%/year",   "",                              "pvsamv1",             "system_use_lifetime_output=1",   "",                             "" },
//	{ SSC_INPUT,        SSC_ARRAY,       "ac_degradation",                              "Annual AC degradation",                                "%/year",   "",                              "pvsamv1",             "system_use_lifetime_output=1",   "",                             "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "dc_degrade_factor",                           "Annual module degrade factor",                         "",         "",                              "pvsamv1",             "system_use_lifetime_output=1",   "",                             "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,       "ac_degrade_factor",                           "Annual AC degrade factor",                             "",         "",                              "pvsamv1",             "system_use_lifetime_output=1",   "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,      "en_dc_lifetime_losses",                       "Enable lifetime daily DC losses",                      "0/1",      "",                              "pvsamv1",             "?=0",                        "INTEGER,MIN=0,MAX=1",          "" },
	{ SSC_INPUT,        SSC_ARRAY,       "dc_lifetime_losses",                          "Lifetime daily DC losses",                             "%",        "",                              "pvsamv1",             "en_dc_lifetime_losses=1",    "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,      "en_ac_lifetime_losses",                       "Enable lifetime daily AC losses",                      "0/1",      "",                              "pvsamv1",             "?=0",                        "INTEGER,MIN=0,MAX=1",          "" },
	{ SSC_INPUT,        SSC_ARRAY,       "ac_lifetime_losses",                          "Lifetime daily AC losses",                             "%",        "",                              "pvsamv1",             "en_ac_lifetime_losses=1",    "",                             "" },
														                        																		                             
	//SEV: Activating the snow model							                        																		                             
	{ SSC_INPUT,        SSC_NUMBER,      "en_snow_model",                               "Toggle snow loss estimation",                          "0/1",      "",                              "snowmodel",            "?=0",                       "BOOLEAN",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "system_capacity",                             "Nameplate capacity",                                   "kW",       "",                              "pvsamv1",              "*",                         "",                             "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "use_wf_albedo",                               "Use albedo in weather file if provided",               "0/1",      "",                              "pvsamv1",              "?=1",                      "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_ARRAY,       "albedo",                                      "User specified ground albedo",                         "0..1",     "",                              "pvsamv1",              "*",						  "LENGTH=12",					  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "irrad_mode",                                  "Irradiance input translation mode",                    "",         "0=beam&diffuse,1=total&beam,2=total&diffuse,3=poa_reference,4=poa_pyranometer",   "pvsamv1",              "?=0",      "INTEGER,MIN=0,MAX=4",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sky_model",                                   "Diffuse sky model",                                    "",         "0=isotropic,1=hkdr,2=perez",    "pvsamv1",              "?=2",                      "INTEGER,MIN=0,MAX=2",           "" },
	 
	{ SSC_INPUT,        SSC_NUMBER,      "modules_per_string",                          "Modules per string",                                    "",        "",                              "pvsamv1",              "*",                        "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "strings_in_parallel",                         "String in parallel",                                    "",        "",                              "pvsamv1",              "*",                        "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inverter_count",                              "Number of inverters",                                   "",        "",                              "pvsamv1",              "*",                        "INTEGER,POSITIVE",              "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "enable_mismatch_vmax_calc",                   "Enable mismatched subarray Vmax calculation",           "",        "",                              "pvsamv1",              "?=0",                      "BOOLEAN",                       "" },

	{ SSC_INPUT,        SSC_NUMBER,      "subarray1_tilt",                              "Sub-array 1 Tilt",                                      "deg",     "0=horizontal,90=vertical",      "pvsamv1",              "naof:subarray1_tilt_eq_lat", "MIN=0,MAX=90",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray1_tilt_eq_lat",                       "Sub-array 1 Tilt=latitude override",                    "0/1",     "",                              "pvsamv1",              "na:subarray1_tilt",          "BOOLEAN",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray1_azimuth",                           "Sub-array 1 Azimuth",                                   "deg",     "0=N,90=E,180=S,270=W",          "pvsamv1",              "*",                        "MIN=0,MAX=359.9",               "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray1_track_mode",                        "Sub-array 1 Tracking mode",                             "",        "0=fixed,1=1axis,2=2axis,3=azi,4=monthly", "pvsamv1",    "*",                        "INTEGER,MIN=0,MAX=4",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray1_rotlim",                            "Sub-array 1 Tracker rotation limit",                    "deg",     "",                              "pvsamv1",              "?=45",                     "MIN=5,MAX=85",                  "" },
	{ SSC_INPUT,		SSC_NUMBER,		 "subarray1_shade_mode",				     	"Sub-array 1 shading mode (fixed tilt or 1x tracking)",	 "0/1/2",   "0=none,1=standard(non-linear),2=thin film(linear)",  "pvsamv1",			     "*",                        "INTEGER,MIN=0,MAX=2",		      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray1_gcr",                               "Sub-array 1 Ground coverage ratio",                     "0..1",    "",                              "pvsamv1",              "?=0.3",                    "MIN=0,MAX=3",               "" },
	{ SSC_INPUT,        SSC_ARRAY,       "subarray1_monthly_tilt",                      "Sub-array 1 monthly tilt input",                        "deg",     "",                              "pvsamv1",              "subarray1_track_mode=4",   "LENGTH=12",                     "" },
//	{ SSC_INPUT, SSC_ARRAY, "subarray1_shading:hourly", "Sub-array 1 Hourly beam shading losses", "%", "", "pvsamv1", "?", "", "" },
//	{ SSC_INPUT, SSC_NUMBER, "subarray1_shading:shading_db_lookup", "Sub-array 1 enable shading database lookup", "", "", "pvsamv1", "?=0", "BOOLEAN", "" },
//	{ SSC_INPUT, SSC_NUMBER, "subarray1_shading:string_option", "Sub-array 1 shading string option", "", "0=shadingdb,1=average,2=maximum,3=minimum", "pvsamv1", "?=-1", "INTEGER,MIN=-1,MAX=3", "" },
	{ SSC_INPUT, SSC_NUMBER, "subarray1_shading:string_option", "Sub-array 1 shading string option", "", "0=shadingdb,1=shadingdb_notc,2=average,3=maximum,4=minimum", "pvsamv1", "?=-1", "INTEGER,MIN=-1,MAX=4", "" },
	{ SSC_INPUT, SSC_MATRIX, "subarray1_shading:timestep", "Sub-array 1 timestep beam shading losses", "%", "", "pvsamv1", "?", "", "" },
	{ SSC_INPUT, SSC_MATRIX, "subarray1_shading:mxh", "Sub-array 1 Month x Hour beam shading losses", "%", "", "pvsamv1", "?", "", "" },
	{ SSC_INPUT,        SSC_MATRIX,      "subarray1_shading:azal",                      "Sub-array 1 Azimuth x altitude beam shading losses",    "%",       "",                              "pvsamv1",              "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray1_shading:diff",                      "Sub-array 1 Diffuse shading loss",                      "%",       "",                              "pvsamv1",              "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,       "subarray1_soiling",                           "Sub-array 1 Monthly soiling loss",                      "%",       "",                              "pvsamv1",              "*",                        "LENGTH=12",                      "" },         

	// loss diagram outputs, also used to calculate total dc derate
	{ SSC_INPUT, SSC_NUMBER, "subarray1_mismatch_loss", "Sub-array 1 DC mismatch loss", "%", "", "pvsamv1", "*", "MIN=0,MAX=100", "" },
	{ SSC_INPUT, SSC_NUMBER, "subarray1_diodeconn_loss", "Sub-array 1 DC diodes and connections loss", "%", "", "pvsamv1", "*", "MIN=0,MAX=100", "" },
	{ SSC_INPUT, SSC_NUMBER, "subarray1_dcwiring_loss", "Sub-array 1 DC wiring loss", "%", "", "pvsamv1", "*", "MIN=0,MAX=100", "" },
	{ SSC_INPUT, SSC_NUMBER, "subarray1_tracking_loss", "Sub-array 1 DC tracking error loss", "%", "", "pvsamv1", "*", "MIN=0,MAX=100", "" },
	{ SSC_INPUT, SSC_NUMBER, "subarray1_nameplate_loss", "Sub-array 1 DC nameplate loss", "%", "", "pvsamv1", "*", "MIN=-5,MAX=100", "" },

	{ SSC_INPUT, SSC_NUMBER, "subarray2_mismatch_loss", "Sub-array 2 DC mismatch loss", "%", "", "pvsamv1", "?", "MIN=0,MAX=100", "" },
	{ SSC_INPUT, SSC_NUMBER, "subarray2_diodeconn_loss", "Sub-array 2 DC diodes and connections loss", "%", "", "pvsamv1", "?", "MIN=0,MAX=100", "" },
	{ SSC_INPUT, SSC_NUMBER, "subarray2_dcwiring_loss", "Sub-array 2 DC wiring loss", "%", "", "pvsamv1", "?", "MIN=0,MAX=100", "" },
	{ SSC_INPUT, SSC_NUMBER, "subarray2_tracking_loss", "Sub-array 2 DC tracking error loss", "%", "", "pvsamv1", "?", "MIN=0,MAX=100", "" },
	{ SSC_INPUT, SSC_NUMBER, "subarray2_nameplate_loss", "Sub-array 2 DC nameplate loss", "%", "", "pvsamv1", "?", "MIN=-5,MAX=100", "" },

	{ SSC_INPUT, SSC_NUMBER, "subarray3_mismatch_loss", "Sub-array 3 DC mismatch loss", "%", "", "pvsamv1", "?", "MIN=0,MAX=100", "" },
	{ SSC_INPUT, SSC_NUMBER, "subarray3_diodeconn_loss", "Sub-array 3 DC diodes and connections loss", "%", "", "pvsamv1", "?", "MIN=0,MAX=100", "" },
	{ SSC_INPUT, SSC_NUMBER, "subarray3_dcwiring_loss", "Sub-array 3 DC wiring loss", "%", "", "pvsamv1", "?", "MIN=0,MAX=100", "" },
	{ SSC_INPUT, SSC_NUMBER, "subarray3_tracking_loss", "Sub-array 3 DC tracking error loss", "%", "", "pvsamv1", "?", "MIN=0,MAX=100", "" },
	{ SSC_INPUT, SSC_NUMBER, "subarray3_nameplate_loss", "Sub-array 3 DC nameplate loss", "%", "", "pvsamv1", "?", "MIN=-5,MAX=100", "" },

	{ SSC_INPUT, SSC_NUMBER, "subarray4_mismatch_loss", "Sub-array 4 DC mismatch loss", "%", "", "pvsamv1", "?", "MIN=0,MAX=100", "" },
	{ SSC_INPUT, SSC_NUMBER, "subarray4_diodeconn_loss", "Sub-array 4 DC diodes and connections loss", "%", "?", "pvsamv1", "?", "MIN=0,MAX=100", "" },
	{ SSC_INPUT, SSC_NUMBER, "subarray4_dcwiring_loss", "Sub-array 4 DC wiring loss", "%", "", "pvsamv1", "?", "MIN=0,MAX=100", "" },
	{ SSC_INPUT, SSC_NUMBER, "subarray4_tracking_loss", "Sub-array 4 DC tracking error loss", "%", "", "pvsamv1", "?", "MIN=0,MAX=100", "" },
	{ SSC_INPUT, SSC_NUMBER, "subarray4_nameplate_loss", "Sub-array 4 DC nameplate loss", "%", "", "pvsamv1", "?", "MIN=-5,MAX=100", "" },

	//this is a DC loss that is applied uniformly to all subarrays
	{ SSC_INPUT, SSC_NUMBER, "dcoptimizer_loss", "DC power optimizer loss", "%", "", "pvsamv1", "*", "MIN=0,MAX=100", "" },
	//AC losses are also applied uniformly to all subarrays
	{ SSC_INPUT, SSC_NUMBER, "acwiring_loss", "AC wiring loss", "%", "", "pvsamv1", "*", "MIN=0,MAX=100", "" },
//	{ SSC_INPUT, SSC_NUMBER, "transformer_loss", "AC step-up transformer loss", "%", "", "pvsamv1", "*", "MIN=0,MAX=100", "" },


	//

	{ SSC_INPUT,        SSC_NUMBER,      "subarray1_mod_orient",                        "Sub-array 1 Module orientation for self-shading",         "0/1",    "0=portrait,1=landscape",        "pvsamv1",              "subarray1_shade_mode>0", "INTEGER,MIN=0,MAX=1",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray1_nmodx",                             "Sub-array 1 no. of modules along bottom for self-shading","",       "",                              "pvsamv1",              "subarray1_shade_mode>0", "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray1_nmody",                             "Sub-array 1 no. of modules along side for self-shading",  "",       "",                              "pvsamv1",              "subarray1_shade_mode>0", "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray1_backtrack",                         "Sub-array 1 Backtracking enabled",                        "",       "0=no backtracking,1=backtrack", "pvsamv1",              "subarray1_track_mode=1",   "BOOLEAN",                       "" },

	{ SSC_INPUT,        SSC_NUMBER,      "subarray2_enable",                            "Sub-array 2 Enable",                                      "0/1",    "0=disabled,1=enabled",          "pvsamv1",              "?=0",                      "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray2_nstrings",                          "Sub-array 2 Number of parallel strings",                  "",       "",                              "pvsamv1",              "subarray2_enable=1",       "INTEGER",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray2_tilt",                              "Sub-array 2 Tilt",                                        "deg",    "0=horizontal,90=vertical",      "pvsamv1",              "naof:subarray2_tilt_eq_lat", "MIN=0,MAX=90",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray2_tilt_eq_lat",                       "Sub-array 2 Tilt=latitude override",                      "0/1",    "",                              "pvsamv1",              "na:subarray2_tilt",          "BOOLEAN",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray2_azimuth",                           "Sub-array 2 Azimuth",                                     "deg",    "0=N,90=E,180=S,270=W",          "pvsamv1",              "subarray2_enable=1",       "MIN=0,MAX=359.9",               "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray2_track_mode",                        "Sub-array 2 Tracking mode",                               "",       "0=fixed,1=1axis,2=2axis,3=azi,4=monthly", "pvsamv1",    "subarray2_enable=1",       "INTEGER,MIN=0,MAX=4",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray2_rotlim",                            "Sub-array 2 Tracker rotation limit",                      "deg",    "",                              "pvsamv1",              "?=45",                     "MIN=5,MAX=85",                  "" },
	{ SSC_INPUT,		SSC_NUMBER,		 "subarray2_shade_mode",				     	"Sub-array 2 shading mode (fixed tilt or 1x tracking)",	   "0/1/2",   "0=none,1=standard(non-linear),2=thin film(linear)",  "pvsamv1",		      "*",                        "INTEGER,MIN=0,MAX=2",		   "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray2_gcr",                               "Sub-array 2 Ground coverage ratio",                       "0..1",   "",                              "pvsamv1",              "?=0.3",                    "MIN=0,MAX=3",               "" },
	{ SSC_INPUT,        SSC_ARRAY,       "subarray2_monthly_tilt",                      "Sub-array 2 monthly tilt input",                          "deg",    "",                              "pvsamv1",              "subarray2_track_mode=4",   "LENGTH=12",                     "" },
//	{ SSC_INPUT,        SSC_ARRAY,       "subarray2_shading:hourly",                    "Sub-array 2 Hourly beam shading losses",                 "%",       "",                              "pvsamv1",              "?",                        "",                              "" },
//	{ SSC_INPUT, SSC_NUMBER, "subarray2_shading:shading_db_lookup", "Sub-array 2 enable shading database lookup", "", "", "pvsamv1", "?=0", "BOOLEAN", "" },
//	{ SSC_INPUT, SSC_NUMBER, "subarray2_shading:string_option", "Sub-array 2 shading string option", "", "0=shadingdb,1=average,2=maximum,3=minimum", "pvsamv1", "?=-1", "INTEGER,MIN=-1,MAX=3", "" },
	{ SSC_INPUT, SSC_NUMBER, "subarray2_shading:string_option", "Sub-array 2 shading string option", "", "0=shadingdb,1=shadingdb_notc,2=average,3=maximum,4=minimum", "pvsamv1", "?=-1", "INTEGER,MIN=-1,MAX=4", "" },
	{ SSC_INPUT, SSC_MATRIX, "subarray2_shading:timestep", "Sub-array 2 timestep beam shading losses", "%", "", "pvsamv1", "?", "", "" },
	{ SSC_INPUT, SSC_MATRIX, "subarray2_shading:mxh", "Sub-array 2 Month x Hour beam shading losses", "%", "", "pvsamv1", "?", "", "" },
	{ SSC_INPUT,        SSC_MATRIX,      "subarray2_shading:azal",                      "Sub-array 2 Azimuth x altitude beam shading losses",     "%",       "",                              "pvsamv1",              "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray2_shading:diff",                      "Sub-array 2 Diffuse shading loss",                       "%",       "",                              "pvsamv1",              "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,       "subarray2_soiling",                           "Sub-array 2 Monthly soiling loss",                       "%",   "",                              "pvsamv1",              "subarray2_enable=1",       "LENGTH=12",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray2_mod_orient",                        "Sub-array 2 Module orientation for self-shading",         "0/1",    "0=portrait,1=landscape",        "pvsamv1",              "subarray2_shade_mode>0",  "INTEGER,MIN=0,MAX=1",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray2_nmodx",                             "Sub-array 2 no. of modules along bottom for self-shading","",       "",                              "pvsamv1",              "subarray2_shade_mode>0",  "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray2_nmody",                             "Sub-array 2 no. of modules along side for self-shading",  "",       "",                              "pvsamv1",              "subarray2_shade_mode>0",  "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray2_backtrack",                         "Sub-array 2 Backtracking enabled",                        "",       "0=no backtracking,1=backtrack", "pvsamv1",              "subarray2_track_mode=1",   "BOOLEAN",                       "" },

	{ SSC_INPUT,        SSC_NUMBER,      "subarray3_enable",                            "Sub-array 3 Enable",                                      "0/1",    "0=disabled,1=enabled",          "pvsamv1",              "?=0",                      "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray3_nstrings",                          "Sub-array 3 Number of parallel strings",                  "",       "",                              "pvsamv1",              "subarray3_enable=1",       "INTEGER",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray3_tilt",                              "Sub-array 3 Tilt",                                        "deg",    "0=horizontal,90=vertical",      "pvsamv1",              "naof:subarray3_tilt_eq_lat", "MIN=0,MAX=90",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray3_tilt_eq_lat",                       "Sub-array 3 Tilt=latitude override",                      "0/1",    "",                              "pvsamv1",              "na:subarray3_tilt",          "BOOLEAN",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray3_azimuth",                           "Sub-array 3 Azimuth",                                     "deg",    "0=N,90=E,180=S,270=W",          "pvsamv1",              "subarray3_enable=1",       "MIN=0,MAX=359.9",               "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray3_track_mode",                        "Sub-array 3 Tracking mode",                               "",       "0=fixed,1=1axis,2=2axis,3=azi,4=monthly", "pvsamv1",    "subarray3_enable=1",       "INTEGER,MIN=0,MAX=4",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray3_rotlim",                            "Sub-array 3 Tracker rotation limit",                      "deg",    "",                              "pvsamv1",              "?=45",                     "MIN=5,MAX=85",                  "" },
	{ SSC_INPUT,		SSC_NUMBER,		 "subarray3_shade_mode",				     	"Sub-array 3 shading mode (fixed tilt or 1x tracking)",	   "0/1/2",   "0=none,1=standard(non-linear),2=thin film(linear)", "pvsamv1",			  "*",                        "INTEGER,MIN=0,MAX=2",		   "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray3_gcr",                               "Sub-array 3 Ground coverage ratio",                       "0..1",   "",                              "pvsamv1",              "?=0.3",                    "MIN=0,MAX=3",               "" },
	{ SSC_INPUT,        SSC_ARRAY,       "subarray3_monthly_tilt",                      "Sub-array 3 monthly tilt input",                          "deg",    "",                              "pvsamv1",              "subarray3_track_mode=4",   "LENGTH=12",                     "" },
//	{ SSC_INPUT,        SSC_ARRAY,       "subarray3_shading:hourly",                    "Sub-array 3 Hourly beam shading losses",                 "%",       "",                              "pvsamv1",              "?",                        "",                              "" },
//	{ SSC_INPUT, SSC_NUMBER, "subarray3_shading:shading_db_lookup", "Sub-array 3 enable shading database lookup", "", "", "pvsamv1", "?=0", "BOOLEAN", "" },
//	{ SSC_INPUT, SSC_NUMBER, "subarray3_shading:string_option", "Sub-array 3 shading string option", "", "0=shadingdb,1=average,2=maximum,3=minimum", "pvsamv1", "?=-1", "INTEGER,MIN=-1,MAX=3", "" },
	{ SSC_INPUT, SSC_NUMBER, "subarray3_shading:string_option", "Sub-array 3 shading string option", "", "0=shadingdb,1=shadingdb_notc,2=average,3=maximum,4=minimum", "pvsamv1", "?=-1", "INTEGER,MIN=-1,MAX=4", "" },
	{ SSC_INPUT, SSC_MATRIX, "subarray3_shading:timestep", "Sub-array 3 timestep beam shading losses", "%", "", "pvsamv1", "?", "", "" },
	{ SSC_INPUT, SSC_MATRIX, "subarray3_shading:mxh", "Sub-array 3 Month x Hour beam shading losses", "%", "", "pvsamv1", "?", "", "" },
	{ SSC_INPUT,        SSC_MATRIX,      "subarray3_shading:azal",                      "Sub-array 3 Azimuth x altitude beam shading losses",     "%",       "",                              "pvsamv1",              "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray3_shading:diff",                      "Sub-array 3 Diffuse shading loss",                       "%",       "",                              "pvsamv1",              "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,       "subarray3_soiling",                           "Sub-array 3 Monthly soiling loss",                       "%",   "",                              "pvsamv1",              "subarray3_enable=1",       "LENGTH=12",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray3_mod_orient",                        "Sub-array 3 Module orientation for self-shading",         "0/1",    "0=portrait,1=landscape",        "pvsamv1",              "subarray1_shade_mode>0", "INTEGER,MIN=0,MAX=1",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray3_nmodx",                             "Sub-array 3 no. of modules along bottom for self-shading","",       "",                              "pvsamv1",              "subarray3_shade_mode>0", "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray3_nmody",                             "Sub-array 3 no. of modules along side for self-shading",  "",       "",                              "pvsamv1",              "subarray3_shade_mode>0", "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray3_backtrack",                         "Sub-array 3 Backtracking enabled",                        "",       "0=no backtracking,1=backtrack", "pvsamv1",              "subarray3_track_mode=1",   "BOOLEAN",                       "" },

	{ SSC_INPUT,        SSC_NUMBER,      "subarray4_enable",                            "Sub-array 4 Enable",                                      "0/1",    "0=disabled,1=enabled",          "pvsamv1",              "?=0",                      "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray4_nstrings",                          "Sub-array 4 Number of parallel strings",                  "",       "",                              "pvsamv1",              "subarray4_enable=1",       "INTEGER",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray4_tilt",                              "Sub-array 4 Tilt",                                        "deg",    "0=horizontal,90=vertical",      "pvsamv1",              "naof:subarray4_tilt_eq_lat", "MIN=0,MAX=90",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray4_tilt_eq_lat",                       "Sub-array 4 Tilt=latitude override",                      "0/1",    "",                              "pvsamv1",              "na:subarray4_tilt",          "BOOLEAN",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray4_azimuth",                           "Sub-array 4 Azimuth",                                     "deg",    "0=N,90=E,180=S,270=W",          "pvsamv1",              "subarray4_enable=1",       "MIN=0,MAX=359.9",               "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray4_track_mode",                        "Sub-array 4 Tracking mode",                               "",       "0=fixed,1=1axis,2=2axis,3=azi,4=monthly", "pvsamv1",    "subarray4_enable=1",       "INTEGER,MIN=0,MAX=4",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray4_rotlim",                            "Sub-array 4 Tracker rotation limit",                      "deg",    "",                              "pvsamv1",              "?=45",                     "MIN=5,MAX=85",                  "" },
	{ SSC_INPUT,		SSC_NUMBER,		 "subarray4_shade_mode",				     	"Sub-array 4 shading mode (fixed tilt or 1x tracking)",	   "0/1/2",  "0=none,1=standard(non-linear),2=thin film(linear)",  "pvsamv1",			  "*",                        "INTEGER,MIN=0,MAX=2",		   "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray4_gcr",                               "Sub-array 4 Ground coverage ratio",                       "0..1",   "",                              "pvsamv1",              "?=0.3",                    "MIN=0,MAX=3",               "" },
	{ SSC_INPUT,        SSC_ARRAY,       "subarray4_monthly_tilt",                      "Sub-array 4 monthly tilt input",                          "deg",    "",                              "pvsamv1",              "subarray4_track_mode=4",   "LENGTH=12",                     "" },
//	{ SSC_INPUT,        SSC_ARRAY,       "subarray4_shading:hourly",                    "Sub-array 4 Hourly beam shading losses",                 "%",       "",                              "pvsamv1",              "?",                        "",                              "" },
//	{ SSC_INPUT, SSC_NUMBER, "subarray4_shading:shading_db_lookup", "Sub-array 4 enable shading database lookup", "", "", "pvsamv1", "?=0", "BOOLEAN", "" },
//	{ SSC_INPUT, SSC_NUMBER, "subarray4_shading:string_option", "Sub-array 4 shading string option", "", "0=shadingdb,1=average,2=maximum,3=minimum", "pvsamv1", "?=-1", "INTEGER,MIN=-1,MAX=3", "" },
	{ SSC_INPUT, SSC_NUMBER, "subarray4_shading:string_option", "Sub-array 4 shading string option", "", "0=shadingdb,1=shadingdb_notc,2=average,3=maximum,4=minimum", "pvsamv1", "?=-1", "INTEGER,MIN=-1,MAX=4", "" },
	{ SSC_INPUT, SSC_MATRIX, "subarray4_shading:timestep", "Sub-array 4 timestep beam shading losses", "%", "", "pvsamv1", "?", "", "" },
	{ SSC_INPUT, SSC_MATRIX, "subarray4_shading:mxh", "Sub-array 4 Month x Hour beam shading losses", "%", "", "pvsamv1", "?", "", "" },
	{ SSC_INPUT,        SSC_MATRIX,      "subarray4_shading:azal",                      "Sub-array 4 Azimuth x altitude beam shading losses",     "%",       "",                              "pvsamv1",              "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray4_shading:diff",                      "Sub-array 4 Diffuse shading loss",                       "%",       "",                              "pvsamv1",              "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,       "subarray4_soiling",                           "Sub-array 4 Monthly soiling loss",                       "%",   "",                              "pvsamv1",              "subarray4_enable=1",       "LENGTH=12",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray4_mod_orient",                        "Sub-array 4 Module orientation for self-shading",         "0/1",    "0=portrait,1=landscape",        "pvsamv1",              "subarray4_shade_mode>0", "INTEGER,MIN=0,MAX=1",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray4_nmodx",                             "Sub-array 4 no. of modules along bottom for self-shading","",       "",                              "pvsamv1",              "subarray4_shade_mode>0", "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray4_nmody",                             "Sub-array 4 no. of modules along side for self-shading",  "",       "",                              "pvsamv1",              "subarray4_shade_mode>0", "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray4_backtrack",                         "Sub-array 4 Backtracking enabled",                        "",       "0=no backtracking,1=backtrack", "pvsamv1",              "subarray4_track_mode=1",   "BOOLEAN",                       "" },

	{ SSC_INPUT,        SSC_NUMBER,      "module_model",                                "Photovoltaic module model specifier",                     "",       "0=spe,1=cec,2=6par_user,3=snl,4=sd11-iec61853", "pvsamv1",              "*",                        "INTEGER,MIN=0,MAX=4",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "module_aspect_ratio",                         "Module aspect ratio",                                     "",       "",                              "pvsamv1",              "?=1.7",                    "",                              "POSITIVE" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_area",                                    "Module area",                                             "m2",     "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_rad0",                                    "Irradiance level 0",                                      "W/m2",   "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_rad1",                                    "Irradiance level 1",                                      "W/m2",   "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_rad2",                                    "Irradiance level 2",                                      "W/m2",   "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_rad3",                                    "Irradiance level 3",                                      "W/m2",   "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_rad4",                                    "Irradiance level 4",                                      "W/m2",   "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_eff0",                                    "Efficiency at irradiance level 0",                        "%",      "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_eff1",                                    "Efficiency at irradiance level 1",                        "%",      "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_eff2",                                    "Efficiency at irradiance level 2",                        "%",      "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_eff3",                                    "Efficiency at irradiance level 3",                        "%",      "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_eff4",                                    "Efficiency at irradiance level 4",                        "%",      "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_reference",                               "Reference irradiance level",                              "",       "",                              "pvsamv1",              "module_model=0",           "INTEGER,MIN=0,MAX=4",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_module_structure",                        "Mounting and module structure",                           "",       "0=glass/cell/polymer sheet - open rack,1=glass/cell/glass - open rack,2=polymer/thin film/steel - open rack,3=Insulated back, building-integrated PV,4=close roof mount,5=user-defined",                      "pvsamv1",       "module_model=0",                    "INTEGER,MIN=0,MAX=5",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_a",                                       "Cell temp parameter a",                                   "",       "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_b",                                       "Cell temp parameter b",                                   "",       "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_dT",                                      "Cell temp parameter dT",                                  "",       "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_temp_coeff",                              "Temperature coefficient",                                 "%/C",    "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_fd",                                      "Diffuse fraction",                                        "0..1",   "",                              "pvsamv1",              "module_model=0",           "MIN=0,MAX=1",                   "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_vmp",                                     "Nominal max power voltage",                               "V",      "",                              "pvsamv1",              "module_model=0",           "POSITIVE",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_voc",                                     "Nominal open circuit voltage",                            "V",      "",                              "pvsamv1",              "module_model=0",           "POSITIVE",                      "" },

	{ SSC_INPUT,        SSC_NUMBER,      "cec_area",                                    "Module area",                                             "m2",     "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_a_ref",                                   "Nonideality factor a",                                    "",       "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_adjust",                                  "Temperature coefficient adjustment",                      "%",      "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_alpha_sc",                                "Short circuit current temperature coefficient",           "A/C",    "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_beta_oc",                                 "Open circuit voltage temperature coefficient",            "V/C",    "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_gamma_r",                                 "Maximum power point temperature coefficient",             "%/C",    "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_i_l_ref",                                 "Light current",                                           "A",      "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_i_mp_ref",                                "Maximum power point current",                             "A",      "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_i_o_ref",                                 "Saturation current",                                      "A",      "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_i_sc_ref",                                "Short circuit current",                                   "A",      "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_n_s",                                     "Number of cells in series",                               "",       "",                              "pvsamv1",              "module_model=1",           "POSITIVE",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_r_s",                                     "Series resistance",                                       "ohm",    "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_r_sh_ref",                                "Shunt resistance",                                        "ohm",    "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_t_noct",                                  "Nominal operating cell temperature",                      "C",      "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_v_mp_ref",                                "Maximum power point voltage",                             "V",      "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_v_oc_ref",                                "Open circuit voltage",                                    "V",      "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_temp_corr_mode",                          "Cell temperature model selection",                        "",       "0=noct,1=mc",                   "pvsamv1",              "module_model=1",           "INTEGER,MIN=0,MAX=1",           "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "cec_standoff",                                "Standoff mode",                                           "",       "0=bipv,1=>3.5in,2=2.5-3.5in,3=1.5-2.5in,4=0.5-1.5in,5=<0.5in,6=ground/rack",  "pvsamv1",       "module_model=1",                           "INTEGER,MIN=0,MAX=6",       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_height",                                  "Array mounting height",                                   "",       "0=one story,1=two story",                                           "pvsamv1",       "module_model=1",                           "INTEGER,MIN=0,MAX=1",       "" },

	{ SSC_INPUT,        SSC_NUMBER,      "cec_mounting_config",                         "Mounting configuration",                                  "",       "0=rack,1=flush,2=integrated,3=gap",                                 "pvsamv1",       "module_model=1&cec_temp_corr_mode=1",      "INTEGER,MIN=0,MAX=3",       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_heat_transfer",                           "Heat transfer dimensions",                                "",       "0=module,1=array",                                                  "pvsamv1",       "module_model=1&cec_temp_corr_mode=1",      "INTEGER,MIN=0,MAX=1",       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_mounting_orientation",                    "Mounting structure orientation",                          "",       "0=do not impede flow,1=vertical supports,2=horizontal supports",    "pvsamv1",       "module_model=1&cec_temp_corr_mode=1",      "INTEGER,MIN=0,MAX=2",       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_gap_spacing",                             "Gap spacing",                                             "m",      "",                                                                  "pvsamv1",       "module_model=1&cec_temp_corr_mode=1",      "",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_module_width",                            "Module width",                                            "m",      "",                                                                  "pvsamv1",       "module_model=1&cec_temp_corr_mode=1",      "",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_module_length",                           "Module height",                                           "m",      "",                                                                  "pvsamv1",       "module_model=1&cec_temp_corr_mode=1",      "",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_array_rows",                              "Rows of modules in array",                                "",       "",                                                                  "pvsamv1",       "module_model=1&cec_temp_corr_mode=1",      "",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_array_cols",                              "Columns of modules in array",                             "",       "",                                                                  "pvsamv1",       "module_model=1&cec_temp_corr_mode=1",      "",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_backside_temp",                           "Module backside temperature",                             "C",      "",                                                                  "pvsamv1",       "module_model=1&cec_temp_corr_mode=1",      "POSITIVE",                  "" },
		
	{ SSC_INPUT,        SSC_NUMBER,      "6par_celltech",                               "Solar cell technology type",                              "",       "monoSi=0,multiSi=1,CdTe=2,CIS=3,CIGS=4,Amorphous=5",                "pvsamv1",       "module_model=2",                           "INTEGER,MIN=0,MAX=5",       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_vmp",                                    "Maximum power point voltage",                             "V",      "",                                                                  "pvsamv1",       "module_model=2",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_imp",                                    "Imp",                                                     "A",      "",                                                                  "pvsamv1",       "module_model=2",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_voc",                                    "Voc",                                                     "V",      "",                                                                  "pvsamv1",       "module_model=2",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_isc",                                    "Isc",                                                     "A",      "",                                                                  "pvsamv1",       "module_model=2",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_bvoc",                                   "Short circuit current temperature coefficient",           "V/C",    "",                                                                  "pvsamv1",       "module_model=2",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_aisc",                                   "Open circuit voltage temperature coefficient",            "A/C",    "",                                                                  "pvsamv1",       "module_model=2",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_gpmp",                                   "Maximum power point temperature coefficient",             "%/C",    "",                                                                  "pvsamv1",       "module_model=2",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_nser",                                   "Nseries",                                                 "",       "",                                                                  "pvsamv1",       "module_model=2",                           "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_area",                                   "Module area",                                             "m2",     "",                                                                  "pvsamv1",       "module_model=2",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_tnoct",                                  "Nominal operating cell temperature",                      "C",      "",                                                                  "pvsamv1",       "module_model=2",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_standoff",                               "Standoff mode",                                           "",       "0=bipv,1=>3.5in,2=2.5-3.5in,3=1.5-2.5in,4=0.5-1.5in,5=<0.5in,6=ground/rack",  "pvsamv1",       "module_model=2",                           "INTEGER,MIN=0,MAX=6",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_mounting",                               "Array mounting height",                                   "",       "0=one story,1=two story",                                           "pvsamv1",       "module_model=2",                           "INTEGER,MIN=0,MAX=1",           "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "snl_module_structure",                        "Module and mounting structure configuration",             "",       "0=Use Database Values,1=glass/cell/polymer sheet - open rack,2=glass/cell/glass - open rack,3=polymer/thin film/steel - open rack,4=Insulated back building-integrated PV,5=close roof mount,6=user-defined",                      "pvsamv1",       "module_model=3",                    "INTEGER,MIN=0,MAX=6",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_a",                                       "Temperature coefficient a",                               "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_b",                                       "Temperature coefficient b",                               "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_dtc",                                     "Temperature coefficient dT",                              "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_ref_a",                                   "User-specified a",                                        "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_ref_b",                                   "User-specified b",                                        "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_ref_dT",                                  "User-specified dT",                                       "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_fd",                                      "Diffuse fraction",                                        "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_a0",                                      "Air mass polynomial coeff 0",                             "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_a1",                                      "Air mass polynomial coeff 1",                             "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_a2",                                      "Air mass polynomial coeff 2",                             "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_a3",                                      "Air mass polynomial coeff 3",                             "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_a4",                                      "Air mass polynomial coeff 4",                             "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_aimp",                                    "Max power point current temperature coefficient",         "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_aisc",                                    "Short circuit current temperature coefficient",           "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_area",                                    "Module area",                                             "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_b0",                                      "Incidence angle modifier polynomial coeff 0",             "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_b1",                                      "Incidence angle modifier polynomial coeff 1",             "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_b2",                                      "Incidence angle modifier polynomial coeff 2",             "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_b3",                                      "Incidence angle modifier polynomial coeff 3",             "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_b4",                                      "Incidence angle modifier polynomial coeff 4",             "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_b5",                                      "Incidence angle modifier polynomial coeff 5",             "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_bvmpo",                                   "Max power point voltage temperature coefficient",         "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_bvoco",                                   "Open circuit voltage temperature coefficient",            "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_c0",                                      "C0",                                                      "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_c1",                                      "C1",                                                      "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_c2",                                      "C2",                                                      "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_c3",                                      "C3",                                                      "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_c4",                                      "C4",                                                      "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_c5",                                      "C5",                                                      "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_c6",                                      "C6",                                                      "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_c7",                                      "C7",                                                      "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_impo",                                    "Max power point current",                                 "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_isco",                                    "Short circuit current",                                   "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_ixo",                                     "Ix midpoint current",                                     "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_ixxo",                                    "Ixx midpoint current",                                    "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_mbvmp",                                   "Irradiance dependence of Vmp temperature coefficient",    "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_mbvoc",                                   "Irradiance dependence of Voc temperature coefficient",    "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_n",                                       "Diode factor",                                            "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_series_cells",                            "Number of cells in series",                               "",       "",                      "pvsamv1",       "module_model=3",                    "INTEGER",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_vmpo",                                    "Max power point voltage",                                 "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_voco",                                    "Open circuit voltage",                                    "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },

	//{ SSC_INPUT,        SSC_NUMBER,      "sd11par_type",                                "Cell technology type",                                    "",       "monoSi=0,multiSi=1,CdTe=2,CIS=3,CIGS=4,Amorphous=5",                "pvsamv1",       "module_model=4",                           "INTEGER,MIN=0,MAX=5",       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sd11par_nser",                                "Nseries",                                                 "",       "",                                                                  "pvsamv1",       "module_model=4",                           "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sd11par_area",                                "Module area",                                             "m2",     "",                                                                  "pvsamv1",       "module_model=4",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sd11par_AMa0",                                "Air mass modifier coeff 0",                               "",       "",                                                                  "pvsamv1",       "module_model=4",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sd11par_AMa1",                                "Air mass modifier coeff 1",                               "",       "",                                                                  "pvsamv1",       "module_model=4",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sd11par_AMa2",                                "Air mass modifier coeff 2",                               "",       "",                                                                  "pvsamv1",       "module_model=4",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sd11par_AMa3",                                "Air mass modifier coeff 3",                               "",       "",                                                                  "pvsamv1",       "module_model=4",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sd11par_AMa4",                                "Air mass modifier coeff 4",                               "",       "",                                                                  "pvsamv1",       "module_model=4",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sd11par_glass",                               "Cover glass type",                                        "",       "0=normal,1=AR glass",                                               "pvsamv1",       "module_model=4",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sd11par_tnoct",                               "Nominal operating cell temperature",                      "C",      "",                                                                  "pvsamv1",       "module_model=4",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sd11par_standoff",                            "Standoff mode",                                           "",       "0=bipv,1=>3.5in,2=2.5-3.5in,3=1.5-2.5in,4=0.5-1.5in,6=<0.5in,5=ground/rack",  "pvsamv1",       "module_model=4",                 "INTEGER,MIN=0,MAX=6",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sd11par_mounting",                            "Array mounting height",                                   "",       "0=one story,1=two story",                                           "pvsamv1",       "module_model=4",                           "INTEGER,MIN=0,MAX=1",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sd11par_Vmp0",                                "Vmp (STC)",                                               "V",      "",                                                                  "pvsamv1",       "module_model=4",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sd11par_Imp0",                                "Imp (STC)",                                               "A",      "",                                                                  "pvsamv1",       "module_model=4",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sd11par_Voc0",                                "Voc (STC)",                                               "V",      "",                                                                  "pvsamv1",       "module_model=4",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sd11par_Isc0",                                "Isc (STC)",                                               "A",      "",                                                                  "pvsamv1",       "module_model=4",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sd11par_alphaIsc",                            "Open circuit voltage temperature coefficient",            "A/C",    "",                                                                  "pvsamv1",       "module_model=4",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sd11par_n",                                   "Diode nonideality factor",                                "",       "",                                                                  "pvsamv1",       "module_model=4",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sd11par_Il",                                  "Light current",                                           "A",      "",                                                                  "pvsamv1",       "module_model=4",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sd11par_Io",                                  "Saturation current",                                      "A",      "",                                                                  "pvsamv1",       "module_model=4",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sd11par_Egref",                               "Bandgap voltage",                                         "eV",     "",                                                                  "pvsamv1",       "module_model=4",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sd11par_d1",                                  "Rs fit parameter 1",                                      "",       "",                                                                  "pvsamv1",       "module_model=4",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sd11par_d2",                                  "Rs fit parameter 2",                                      "",       "",                                                                  "pvsamv1",       "module_model=4",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sd11par_d3",                                  "Rs fit parameter 3",                                      "",       "",                                                                  "pvsamv1",       "module_model=4",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sd11par_c1",                                  "Rsh fit parameter 1",                                     "",       "",                                                                  "pvsamv1",       "module_model=4",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sd11par_c2",                                  "Rsh fit parameter 2",                                     "",       "",                                                                  "pvsamv1",       "module_model=4",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sd11par_c3",                                  "Rsh fit parameter 3",                                     "",       "",                                                                  "pvsamv1",       "module_model=4",                           "",                              "" },
	
// inverter model
	{ SSC_INPUT,        SSC_NUMBER,      "inverter_model",                              "Inverter model specifier",                                "",        "0=cec,1=datasheet,2=partload,3=coefficientgenerator",        "pvsamv1",               "*",                         "INTEGER,MIN=0,MAX=3",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "mppt_low_inverter",                           "Minimum inverter MPPT voltage window",                    "Vdc",     "",                     "pvsamv1",       "",                    "?=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "mppt_hi_inverter",                            "Maximum inverter MPPT voltage window",                    "Vdc",     "",                     "pvsamv1",       "",                    "?=0",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_c0",                                  "Curvature between AC power and DC power at ref",          "1/W",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_c1",                                  "Coefficient of Pdco variation with DC input voltage",     "1/V",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_c2",                                  "Coefficient of Pso variation with DC input voltage",      "1/V",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_c3",                                  "Coefficient of Co variation with DC input voltage",       "1/V",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_paco",                                "AC maximum power rating",                                 "Wac",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_pdco",                                "DC input power at which AC power rating is achieved",     "Wdc",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_pnt",                                 "AC power consumed by inverter at night",                  "Wac",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_pso",                                 "DC power required to enable the inversion process",       "Wdc",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_vdco",                                "DC input voltage for the rated AC power rating",          "Vdc",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_vdcmax",                              "Maximum DC input operating voltage",                      "Vdc",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },

	{ SSC_INPUT, SSC_NUMBER, "inv_cec_cg_c0", "Curvature between AC power and DC power at ref", "1/W", "", "pvsamv1", "inverter_model=3", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "inv_cec_cg_c1", "Coefficient of Pdco variation with DC input voltage", "1/V", "", "pvsamv1", "inverter_model=3", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "inv_cec_cg_c2", "Coefficient of Pso variation with DC input voltage", "1/V", "", "pvsamv1", "inverter_model=3", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "inv_cec_cg_c3", "Coefficient of Co variation with DC input voltage", "1/V", "", "pvsamv1", "inverter_model=3", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "inv_cec_cg_paco", "AC maximum power rating", "Wac", "", "pvsamv1", "inverter_model=3", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "inv_cec_cg_pdco", "DC input power at which AC power rating is achieved", "Wdc", "", "pvsamv1", "inverter_model=3", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "inv_cec_cg_pnt", "AC power consumed by inverter at night", "Wac", "", "pvsamv1", "inverter_model=3", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "inv_cec_cg_psco", "DC power required to enable the inversion process", "Wdc", "", "pvsamv1", "inverter_model=3", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "inv_cec_cg_vdco", "DC input voltage for the rated AC power rating", "Vdc", "", "pvsamv1", "inverter_model=3", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "inv_cec_cg_vdcmax", "Maximum DC input operating voltage", "Vdc", "", "pvsamv1", "inverter_model=3", "", "" },

	{ SSC_INPUT,        SSC_NUMBER,      "inv_ds_paco",                                "AC maximum power rating",                                 "Wac",     "",                     "pvsamv1",       "inverter_model=1",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_ds_eff",                                 "Weighted or Peak or Nominal Efficiency",     "Wdc",     "",                     "pvsamv1",       "inverter_model=1",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_ds_pnt",                                 "AC power consumed by inverter at night",                  "Wac",     "",                     "pvsamv1",       "inverter_model=1",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_ds_pso",                                 "DC power required to enable the inversion process",       "Wdc",     "",                     "pvsamv1",       "inverter_model=1",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_ds_vdco",                                "DC input voltage for the rated AC power rating",          "Vdc",     "",                     "pvsamv1",       "inverter_model=1",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_ds_vdcmax",                              "Maximum DC input operating voltage",                      "Vdc",     "",                     "pvsamv1",       "inverter_model=1",                    "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,      "inv_pd_paco",                                "AC maximum power rating",                                 "Wac",     "",                     "pvsamv1",       "inverter_model=2",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_pd_pdco",                                "DC input power at which AC power rating is achieved",     "Wdc",     "",                     "pvsamv1",       "inverter_model=2",                    "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,       "inv_pd_partload",                            "Partload curve partload values",                          "%",       "",                     "pvsamv1",       "inverter_model=2",                    "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,       "inv_pd_efficiency",                          "Partload curve efficiency values",                        "%",       "",                     "pvsamv1",       "inverter_model=2",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_pd_pnt",                                 "AC power consumed by inverter at night",                  "Wac",     "",                     "pvsamv1",       "inverter_model=2",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_pd_vdco",                                "DC input voltage for the rated AC power rating",          "Vdc",     "",                     "pvsamv1",       "inverter_model=2",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_pd_vdcmax",                              "Maximum DC input operating voltage",                      "Vdc",     "",                     "pvsamv1",       "inverter_model=2",                    "",                              "" },
	
	// battery storage and dispatch
	{ SSC_INPUT,        SSC_NUMBER,      "en_batt",                                    "Enable battery storage model",                            "0/1",     "",                     "Battery",       "?=0",                                 "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_replacement_option",                    "Enable battery replacement?",                             "0=none,1=capacity based,2=user schedule", "", "Battery", "?=0", "INTEGER,MIN=0,MAX=2", "" },
	{ SSC_INPUT,        SSC_ARRAY,       "batt_replacement_schedule",                  "Battery bank replacements per year (user specified)",     "number/year", "", "Battery", "batt_replacement_option=2", "", "" },

	{ SSC_INPUT,        SSC_ARRAY,       "load",                                       "Electricity load (year 1)",                         "kW", "", "Battery", "?", "", "" },
	
	// NOTE:  other battery storage model inputs and outputs are defined in batt_common.h/batt_common.cpp
	
	// outputs

/* environmental conditions */
	// irradiance data from weather file
	{ SSC_OUTPUT,        SSC_ARRAY,      "gh",                                         "Irradiance GHI from weather file",                                     "W/m2",   "",                      "Time Series",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "dn",                                         "Irradiance DNI from weather file",                                     "W/m2",   "",                      "Time Series",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "df",                                         "Irradiance DHI from weather file",                                     "W/m2",   "",                      "Time Series",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "wfpoa",                                      "Irradiance POA from weather file",                                     "W/m2",   "",                      "Time Series",       "",                     "",                              "" },
	
	//not all of these three calculated values will be reported, based on irrad_mode selection
	{ SSC_OUTPUT,        SSC_ARRAY,      "gh_calc",                                    "Irradiance GHI calculated",                                       "W/m2",   "",                      "Time Series",       "",                     "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "dn_calc",                                    "Irradiance DNI calculated",                                       "W/m2",   "",                      "Time Series",       "",                     "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "df_calc",                                    "Irradiance DHI calculated",                                       "W/m2",   "",                      "Time Series",       "",                     "",                              "" },

	// non-irradiance data from weather file
	{ SSC_OUTPUT,        SSC_ARRAY,      "wspd",                                       "Weather file wind speed",                                                        "m/s",    "",                      "Time Series",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "tdry",                                       "Weather file ambient temperature",                                               "C",      "",                      "Time Series",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "alb",                                        "Weather file albedo",							                                 "",       "",                     "Time Series",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "snowdepth",                                  "Weather file snow depth",							                            "cm",       "",                    "Time Series",       "",                    "",                              "" },

	// calculated sun position data
	{ SSC_OUTPUT,        SSC_ARRAY,      "sol_zen",                                    "Sun zenith angle",                                                  "deg",    "",                      "Time Series",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "sol_alt",                                    "Sun altitude angle",                                                "deg",    "",                      "Time Series",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "sol_azi",                                    "Sun azimuth angle",                                                 "deg",    "",                      "Time Series",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "sunup",                                      "Sun up over horizon",                                               "0/1/2/3", "",                     "Time Series",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "sunpos_hour",                                "Sun position time",                                     "hour",   "",                      "Time Series",       "",                     "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "airmass",                                    "Absolute air mass",                                                 "",       "",                      "Time Series",       "*",                    "",                              "" },

	/* sub-array level outputs */
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray1_surf_tilt",                  "Subarray 1 Surface tilt",                                              "deg",    "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray1_surf_azi",                   "Subarray 1 Surface azimuth",                                           "deg",    "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray1_aoi",                        "Subarray 1 Angle of incidence",                                        "deg",    "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray1_axisrot",                    "Subarray 1 Axis rotation for 1 axis trackers",                         "deg",    "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray1_idealrot",                   "Subarray 1 Axis rotation ideal for 1 axis trackers",                   "deg",    "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray1_poa_eff_beam",               "Subarray 1 POA beam irradiance after shading and soiling",             "W/m2",   "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray1_poa_eff_diff",               "Subarray 1 POA diffuse irradiance after shading and soiling",          "W/m2",   "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray1_poa_nom",                    "Subarray 1 POA total irradiance nominal",                              "W/m2",   "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray1_poa_shaded",                 "Subarray 1 POA total irradiance after shading only",                   "W/m2",   "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray1_poa_eff",                    "Subarray 1 POA total irradiance after shading and soiling",            "W/m2",   "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray1_soiling_derate",             "Subarray 1 Soiling beam irradiance factor",                            "frac",   "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray1_beam_shading_factor",        "Subarray 1 External shading and soiling beam irradiance factor",       "frac",   "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray1_linear_derate",              "Subarray 1 Self-shading linear beam irradiance factor",                "frac",   "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray1_ss_diffuse_derate",          "Subarray 1 Self-shading non-linear sky diffuse irradiance factor",     "frac",   "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray1_ss_reflected_derate",        "Subarray 1 Self-shading non-linear ground diffuse irradiance factor",  "frac",   "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray1_ss_derate",                  "Subarray 1 Self-shading non-linear DC factor",                         "frac",   "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "shadedb_subarray1_shade_frac",         "Subarray 1 Partial external shading DC factor",                        "frac",   "", "Time Series (Subarray 1)",       "",                     "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray1_snow_coverage",              "Subarray 1 Snow cover",                                                "0..1",   "", "Time Series (Subarray 1)",       "",                     "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray1_snow_loss",                  "Subarray 1 Snow cover DC power loss",                                  "kW",     "", "Time Series (Subarray 1)",       "",                     "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray1_modeff",                     "Subarray 1 Module efficiency",                                         "%",      "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray1_celltemp",                   "Subarray 1 Cell temperature",                                          "C",      "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray1_dc_voltage",                 "Subarray 1 Operating voltage",                                         "V",      "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray1_voc",                        "Subarray 1 Open circuit voltage",                                      "V",      "", "Time Series (Subarray 1)",       "",                     "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray1_isc",                        "Subarray 1 Short circuit current",                                     "A",      "", "Time Series (Subarray 1)",       "",                     "",                              "" },


	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_surf_tilt",                  "Subarray 2 Surface tilt",                                              "deg",    "", "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_surf_azi",                   "Subarray 2 Surface azimuth",                                           "deg",    "", "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_aoi",                        "Subarray 2 Angle of incidence",                                        "deg",    "", "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_axisrot",                    "Subarray 2 Axis rotation for 1 axis trackers",                         "deg",    "", "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_idealrot",                   "Subarray 2 Axis rotation ideal for 1 axis trackers",                   "deg",    "", "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_poa_eff_beam",               "Subarray 2 POA beam irradiance after shading and soiling",             "W/m2",   "", "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_poa_eff_diff",               "Subarray 2 POA diffuse irradiance after shading and soiling",          "W/m2",   "", "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_poa_nom",                    "Subarray 2 POA total irradiance nominal",                              "W/m2",   "", "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_poa_shaded",                 "Subarray 2 POA total irradiance after shading only",                   "W/m2",   "", "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_poa_eff",                    "Subarray 2 POA total irradiance after shading and soiling",            "W/m2",   "", "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_soiling_derate",             "Subarray 2 Soiling beam irradiance factor",                            "frac",   "", "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_beam_shading_factor",        "Subarray 2 External shading and soiling beam irradiance factor",       "frac",   "", "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_linear_derate",              "Subarray 2 Self-shading linear beam irradiance factor",                "frac",   "", "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_ss_diffuse_derate",          "Subarray 2 Self-shading non-linear sky diffuse irradiance factor",     "frac",   "", "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_ss_reflected_derate",        "Subarray 2 Self-shading non-linear ground diffuse irradiance factor",  "frac",   "", "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_ss_derate",                  "Subarray 2 Self-shading non-linear DC factor",                         "frac",   "", "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "shadedb_subarray2_shade_frac",         "Subarray 2 Partial shading DC factor",                                 "frac",   "", "Time Series (Subarray 2)",       "",                     "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_snow_coverage",				 "Subarray 2 Snow cover",                                                "0..1",   "", "Time Series (Subarray 2)",       "",                     "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_snow_loss",					 "Subarray 2 Snow cover DC power loss",                                  "kW",     "", "Time Series (Subarray 2)",       "",                     "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_modeff",                     "Subarray 2 Module efficiency",                                         "%",      "", "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_celltemp",                   "Subarray 2 Cell temperature",                                          "C",      "", "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_dc_voltage",                 "Subarray 2 Operating voltage",                                         "V",      "", "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_voc",                        "Subarray 2 Open circuit voltage",                                      "V",      "", "Time Series (Subarray 2)",       "",                     "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_isc",                        "Subarray 2 Short circuit current",                                     "A",      "", "Time Series (Subarray 2)",       "",                     "",                              "" },

	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_surf_tilt",                  "Subarray 3 Surface tilt",                                              "deg",    "", "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_surf_azi",                   "Subarray 3 Surface azimuth",                                           "deg",    "", "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_aoi",                        "Subarray 3 Angle of incidence",                                        "deg",    "", "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_axisrot",                    "Subarray 3 Axis rotation for 1 axis trackers",                         "deg",    "", "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_idealrot",                   "Subarray 3 Axis rotation ideal for 1 axis trackers",                   "deg",    "", "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_poa_eff_beam",               "Subarray 3 POA beam irradiance after shading and soiling",             "W/m2",   "", "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_poa_eff_diff",               "Subarray 3 POA diffuse irradiance after shading and soiling",          "W/m2",   "", "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_poa_nom",                    "Subarray 3 POA total irradiance nominal",                              "W/m2",   "", "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_poa_shaded",                 "Subarray 3 POA total irradiance after shading only",                   "W/m2",   "", "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_poa_eff",                    "Subarray 3 POA total irradiance after shading and soiling",            "W/m2",   "", "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_soiling_derate",             "Subarray 3 Soiling beam irradiance factor",                            "frac",   "", "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_beam_shading_factor",        "Subarray 3 External shading and soiling beam irradiance factor",       "frac",   "", "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_linear_derate",              "Subarray 3 Self-shading linear beam irradiance factor",                "frac",   "", "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_ss_diffuse_derate",          "Subarray 3 Self-shading non-linear sky diffuse irradiance factor",     "frac",   "", "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_ss_reflected_derate",        "Subarray 3 Self-shading non-linear ground diffuse irradiance factor",  "frac",   "", "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_ss_derate",                  "Subarray 3 Self-shading non-linear DC factor",                         "frac",   "", "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "shadedb_subarray3_shade_frac",         "Subarray 3 Partial external shading DC factor",                        "frac",   "", "Time Series (Subarray 3)",       "",                     "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_snow_coverage",				 "Subarray 3 Snow cover",                                                "0..1",   "", "Time Series (Subarray 3)",       "",                     "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_snow_loss",					 "Subarray 3 Snow cover DC power loss",			                         "kW",     "", "Time Series (Subarray 3)",       "",                     "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_modeff",                     "Subarray 3 Module efficiency",                                         "%",      "", "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_celltemp",                   "Subarray 3 Cell temperature",                                          "C",      "", "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_dc_voltage",                 "Subarray 3 Operating voltage",                                         "V",      "", "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_voc",                        "Subarray 3 Open circuit voltage",                                      "V",      "", "Time Series (Subarray 3)",       "",                     "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_isc",                        "Subarray 3 Short circuit current",                                     "A",      "", "Time Series (Subarray 3)",       "",                     "",                              "" },

	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_surf_tilt",                  "Subarray 4 Surface tilt",                                              "deg",    "", "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_surf_azi",                   "Subarray 4 Surface azimuth",                                           "deg",    "", "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_aoi",                        "Subarray 4 Angle of incidence",                                        "deg",    "", "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_axisrot",                    "Subarray 4 Axis rotation for 1 axis trackers",                         "deg",    "", "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_idealrot",                   "Subarray 4 Axis rotation ideal for 1 axis trackers",                   "deg",    "", "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_poa_eff_beam",               "Subarray 4 POA beam irradiance after shading and soiling",             "W/m2",   "", "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_poa_eff_diff",               "Subarray 4 POA diffuse irradiance after shading and soiling",          "W/m2",   "", "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_poa_nom",                    "Subarray 4 POA total irradiance nominal",                              "W/m2",   "", "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_poa_shaded",                 "Subarray 4 POA total irradiance after shading only",                   "W/m2",   "", "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_poa_eff",                    "Subarray 4 POA total irradiance after shading and soiling",            "W/m2",   "", "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_soiling_derate",             "Subarray 4 Soiling beam irradiance factor",                            "frac",   "", "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_beam_shading_factor",        "Subarray 4 External shading and soiling beam irradiance factor",       "frac",   "", "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_linear_derate",              "Subarray 4 Self-shading linear beam irradiance factor",                "frac",   "", "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_ss_diffuse_derate",          "Subarray 4 Self-shading non-linear sky diffuse irradiance factor",     "frac",   "", "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_ss_reflected_derate",        "Subarray 4 Self-shading non-linear ground diffuse irradiance factor",  "frac",   "", "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_ss_derate",                  "Subarray 4 Self-shading non-linear DC factor",                         "frac",   "", "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "shadedb_subarray4_shade_frac",         "Subarray 4 Partial external shading DC factor",                        "frac",   "", "Time Series (Subarray 4)",       "",                     "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_snow_coverage",				 "Subarray 4 Snow cover",                                                "0..1",   "", "Time Series (Subarray 4)",       "",                     "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_snow_loss",					 "Subarray 4 Snow cover DC power loss",                                  "kW",     "", "Time Series (Subarray 4)",       "",                     "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_modeff",                     "Subarray 4 Module efficiency",                                         "%",      "", "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_celltemp",                   "Subarray 4 Cell temperature",                                          "C",      "", "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_dc_voltage",                 "Subarray 4 Operating voltage",                                         "V",      "", "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_voc",                        "Subarray 4 Open circuit voltage",                                      "V",      "", "Time Series (Subarray 4)",       "",                     "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_isc",                        "Subarray 4 Short circuit current",                                     "A",      "", "Time Series (Subarray 4)",       "",                     "",                              "" },

/*	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_aoi",                        "Subarray 2 Angle of incidence",                                     "deg",    "",                      "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_surf_tilt",                  "Subarray 2 Surface tilt",                                           "deg",    "",                      "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_surf_azi",                   "Subarray 2 Surface azimuth",                                        "deg",    "",                      "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_axisrot",                    "Subarray 2 Axis rotation for 1 axis trackers",                      "deg",    "",                      "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_idealrot",                   "Subarray 2 Ideal axis rotation for 1 axis trackers",                "deg",    "",                      "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_poa_nom",                    "Subarray 2 POA total irradiance (nominal)",                           "W/m2",   "",                      "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_poa_shaded",                 "Subarray 2 POA total irradiance after shading only",                "W/m2",   "",                      "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_poa_eff",                    "Subarray 2 POA total irradiance after shading and soiling",         "W/m2",   "",                      "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_poa_eff_beam",               "Subarray 2 POA beam irradiance after shading and soiling",          "W/m2",   "",                      "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_poa_eff_diff",               "Subarray 2 POA diffuse irradiance after shading and soiling",       "W/m2",   "",                      "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_beam_shading_factor",        "Subarray 2 Beam irradiance shading factor",                         "frac",   "",                      "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_linear_derate",              "Subarray 2 Self-shading linear derate",                             "",       "",                      "Time Series (Subarray 1)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_ss_derate",                  "Subarray 2 Self-shading DC derate",                                    "",       "",                      "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_ss_diffuse_derate",          "Subarray 2 Self-shading diffuse derate",                            "",       "",                      "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_ss_reflected_derate",        "Subarray 2 Self-shading reflected derate",                          "",       "",                      "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_modeff",                     "Subarray 2 Module efficiency",                                      "%",      "",                      "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_soiling_derate",             "Subarray 2 Soiling derate",                                         "frac",   "",                      "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_celltemp",                   "Subarray 2 Cell temperature",                                       "C",      "",                      "Time Series (Subarray 2)",       "",                    "",                              "" },

	//SEV: Snow loss and coverage % time series arrays for SA2
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_snow_loss",					 "Subarray 2 Power loss due to snow (DC)",			                  "kW",    "",                       "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_snow_coverage",				 "Subarray 2 Snow coverage",										  "0..1",    "",                     "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_voc",                        "Subarray 2 Open circuit voltage",                                   "V",      "",                      "Time Series (Subarray 2)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_isc",                        "Subarray 2 Short circuit current",                                  "A",      "",                      "Time Series (Subarray 2)",       "",                    "",                              "" },
	
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray2_dc_voltage",                 "Subarray 2 DC string voltage",                                      "V",      "",                      "Time Series (Subarray 2)",       "",                    "",                              "" },
*/

/*	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_aoi",                        "Subarray 3 Angle of incidence",                                     "deg",    "",                      "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_surf_tilt",                  "Subarray 3 Surface tilt",                                           "deg",    "",                      "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_surf_azi",                   "Subarray 3 Surface azimuth",                                        "deg",    "",                      "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_axisrot",                    "Subarray 3 Axis rotation for 1 axis trackers",                      "deg",    "",                      "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_idealrot",                   "Subarray 3 Ideal axis rotation for 1 axis trackers",                "deg",    "",                      "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_poa_nom",                    "Subarray 3 POA total irradiance (nominal)",                           "W/m2",   "",                      "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_poa_shaded",                 "Subarray 3 POA total irradiance after shading only",                "W/m2",   "",                      "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_poa_eff",                    "Subarray 3 POA total irradiance after shading and soiling",         "W/m2",   "",                      "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_poa_eff_beam",               "Subarray 3 POA beam irradiance after shading and soiling",          "W/m2",   "",                      "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_poa_eff_diff",               "Subarray 3 POA diffuse irradiance after shading and soiling",       "W/m2",   "",                      "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_beam_shading_factor",        "Subarray 3 Beam irradiance shading factor",                         "frac",   "",                      "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_linear_derate",              "Subarray 3 Self-shading linear derate",                             "",       "",                      "Time Series (Subarray 1)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_ss_derate",                  "Subarray 3 Self-shading DC derate",                                    "",       "",                      "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_ss_diffuse_derate",          "Subarray 3 Self-shading diffuse derate",                            "",       "",                      "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_ss_reflected_derate",        "Subarray 3 Self-shading reflected derate",                          "",       "",                      "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_modeff",                     "Subarray 3 Module efficiency",                                      "%",      "",                      "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_soiling_derate",             "Subarray 3 Soiling derate",                                         "frac",   "",                      "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_celltemp",                   "Subarray 3 Cell temperature",                                       "C",      "",                      "Time Series (Subarray 3)",       "",                    "",                              "" },
	//SEV: Snow loss and coverage % time series arrays for SA3
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_snow_loss",					 "Subarray 3 Power loss due to snow (DC)",							  "kW",    "",                       "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_snow_coverage",				 "Subarray 3 Snow coverage",										  "0..1",    "",                     "Time Series (Subarray 3)",       "",                    "",                              "" },
// 	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_dc_gross",                   "Subarray 3 Gross DC power",                                         "kW",    "",                       "Time Series (Subarray 3)",       "",                    "",                              "" },
	
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_voc",                        "Subarray 3 Open circuit voltage",                                   "V",      "",                      "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_isc",                        "Subarray 3 Short circuit current",                                  "A",      "",                      "Time Series (Subarray 3)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray3_dc_voltage",                 "Subarray 3 DC string voltage",                                      "V",      "",                      "Time Series (Subarray 3)",       "",                    "",                              "" },
*/

/*	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_aoi",                        "Subarray 4 Angle of incidence",                                     "deg",    "",                      "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_surf_tilt",                  "Subarray 4 Surface tilt",                                           "deg",    "",                      "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_surf_azi",                   "Subarray 4 Surface azimuth",                                        "deg",    "",                      "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_axisrot",                    "Subarray 4 Axis rotation for 1 axis trackers",                      "deg",    "",                      "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_idealrot",                   "Subarray 4 Ideal axis rotation for 1 axis trackers",                "deg",    "",                      "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_poa_nom",                    "Subarray 4 POA total irradiance (nominal)",                         "W/m2",   "",                      "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_poa_shaded",                 "Subarray 4 POA total irradiance after shading only",                "W/m2",   "",                      "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_poa_eff",                    "Subarray 4 POA total irradiance after shading and soiling",         "W/m2",   "",                      "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_poa_eff_beam",               "Subarray 4 POA beam irradiance after shading and soiling",          "W/m2",   "",                      "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_poa_eff_diff",               "Subarray 4 POA diffuse irradiance after shading and soiling",       "W/m2",   "",                      "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_beam_shading_factor",        "Subarray 4 Beam irradiance shading factor",                         "frac",   "",                      "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_linear_derate",              "Subarray 4 Self-shading linear derate",                             "",       "",                      "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_ss_derate",                  "Subarray 4 Self-shading DC derate",                                    "",       "",                      "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_ss_diffuse_derate",          "Subarray 4 Self-shading diffuse derate",                            "",       "",                      "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_ss_reflected_derate",        "Subarray 4 Self-shading reflected derate",                          "",       "",                      "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_modeff",                     "Subarray 4 Module efficiency",                                      "%",      "",                      "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_soiling_derate",             "Subarray 4 Soiling derate",                                         "frac",   "",                      "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_celltemp",                   "Subarray 4 Cell temperature",                                       "C",      "",                      "Time Series (Subarray 4)",       "",                    "",                              "" },
	//SEV: Snow loss and coverage % time series arrays for SA3
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_snow_loss",					 "Subarray 4 Power loss due to snow (DC)",						      "kW",    "",                       "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_snow_coverage",				 "Subarray 4 snow coverage",										  "0..1",    "",                       "Time Series (Subarray 4)",       "",                    "",                              "" },
//	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_dc_gross",                   "Subarray 4 Gross DC power",                                         "kW",    "",                       "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_voc",                        "Subarray 4 Open circuit voltage",                                   "V",      "",                      "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_isc",                        "Subarray 4 Short circuit current",                                  "A",      "",                      "Time Series (Subarray 4)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "subarray4_dc_voltage",                 "Subarray 4 DC string voltage",                                      "V",      "",                      "Time Series (Subarray 4)",       "",                    "",                              "" },
*/

/* aggregate array level outputs */
	{ SSC_OUTPUT,        SSC_ARRAY,      "poa_nom",                              "Array POA total radiation nominal",                    "kW",   "",  "Time Series (Array)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "poa_beam_nom",                         "Array POA beam radiation nominal",                     "kW",   "",  "Time Series (Array)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "poa_shaded",                           "Array POA total radiation after shading only",         "kW",   "",  "Time Series (Array)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "poa_eff",                              "Array POA total radiation after shading and soiling",  "kW",   "",  "Time Series (Array)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "poa_beam_eff",                         "Array POA beam radiation after shading and soiling",   "kW",   "",  "Time Series (Array)",       "*",                    "",                              "" },

	//SEV: total dc snow loss time series (not a required output) 
	{ SSC_OUTPUT,        SSC_ARRAY,      "dc_snow_loss",                         "Array DC power loss due to snow",						 "kW",   "",   "Time Series (Array)",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "dc_net",                               "Array DC power",                                       "kW",   "",   "Time Series (Array)",       "*",                    "",                              "" },
	
	//inverter outputs
	{ SSC_OUTPUT,        SSC_ARRAY,      "inverter_dc_voltage",                  "Inverter DC input voltage",                            "V",    "",  "Time Series (Inverter)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "inv_eff",                              "Inverter efficiency",                                  "%",    "",  "Time Series (Inverter)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "dc_invmppt_loss",                      "Inverter clipping loss DC MPPT voltage limits",         "kW",  "",  "Time Series (Inverter)",       "*",                    "",                              "" },
    { SSC_OUTPUT,        SSC_ARRAY,      "inv_cliploss",                         "Inverter clipping loss AC power limit",                "kW",   "",  "Time Series (Inverter)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "inv_psoloss",                          "Inverter power consumption loss",                      "kW",   "",  "Time Series (Inverter)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "inv_pntloss",                          "Inverter night time loss",                             "kW",   "",  "Time Series (Inverter)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "ac_wiring_loss",                        "AC wiring loss",                                       "kW",   "",   "Time Series (Inverter)",              "*",                        "",                   "" },

	// transformer model outputs
	{ SSC_OUTPUT,        SSC_ARRAY,      "xfmr_nll_ts",                          "Transformer no load loss",                              "kW", "",    "Time Series (Transformer)", "", "", "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "xfmr_ll_ts",                           "Transformer load loss",                                 "kW", "",    "Time Series (Transformer)", "", "", "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "xfmr_loss_ts",                         "Transformer total loss",                                "kW", "",    "Time Series (Transformer)", "", "", "" },

	//total losses- not part of loss diagram but now outputs instead of inputs JMF 11/25/15
	{ SSC_OUTPUT,        SSC_NUMBER,      "ac_loss",                             "AC wiring loss",                                       "%",   "",    "Annual (Year 1)",              "*",                        "",                   "" },

	// monthly and annual outputs

	{ SSC_OUTPUT, SSC_NUMBER, "annual_energy", "Annual energy", "kWh", "", "Annual (Year 1)", "*", "", "" },

	{ SSC_OUTPUT,        SSC_NUMBER,     "annual_dc_invmppt_loss",                      "Inverter clipping loss DC MPPT voltage limits",          "kWh/yr",    "",                      "Annual (Year 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "annual_inv_cliploss",                         "Inverter clipping loss AC power limit",                  "kWh/yr",    "",                      "Annual (Year 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "annual_inv_psoloss",                          "Inverter power consumption loss",                        "kWh/yr",    "",                      "Annual (Year 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "annual_inv_pntloss",                          "Inverter night time loss",                               "kWh/yr",    "",                      "Annual (Year 1)",       "*",                    "",                              "" },

	{ SSC_OUTPUT,        SSC_NUMBER,      "subarray1_dcloss",                    "Subarray 1 Total DC power loss",                                       "%",      "", "Annual (Year 1)",              "*",                        "",                   "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "subarray2_dcloss",                    "Subarray 2 Total DC power loss",                                       "%",      "", "Annual (Year 1)",              "",                        "",                   "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "subarray3_dcloss",                    "Subarray 3 Total DC power loss",                                       "%",      "", "Annual (Year 1)",              "",                        "",                   "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "subarray4_dcloss",                    "Subarray 4 Total DC power loss",                                       "%",      "", "Annual (Year 1)",              "",                        "",                   "" },

	{ SSC_OUTPUT,        SSC_NUMBER,     "xfmr_nll_year1",                              "Transformer no load loss",                               "kWh/yr", "", "Annual (Year 1)", "", "", "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "xfmr_ll_year1",                               "Transformer load loss",                                  "kWh/yr", "", "Annual (Year 1)", "", "", "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "xfmr_loss_year1",                             "Transformer total loss",                                 "kWh/yr", "", "Annual (Year 1)", "", "", "" },

	{ SSC_OUTPUT,        SSC_ARRAY,      "monthly_poa_nom",                             "POA irradiance total nominal",                          "kWh/mo",    "",                      "Monthly",       "*",                    "LENGTH=12",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "monthly_poa_beam_nom",                        "POA irradiance beam nominal",                           "kWh/mo",    "",                      "Monthly",       "*",                    "LENGTH=12",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "monthly_poa_eff",                             "POA irradiance total after shading and soiling",          "kWh/mo",    "",                      "Monthly",       "*",                    "LENGTH=12",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "monthly_poa_beam_eff",                        "POA irradiance beam after shading and soiling",           "kWh/mo",    "",                      "Monthly",       "*",                    "LENGTH=12",                              "" },

	{ SSC_OUTPUT,        SSC_ARRAY,      "monthly_dc",                                  "PV array DC energy",                                   "kWh/mo",    "",                      "Monthly",       "*",                    "LENGTH=12",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "monthly_energy",                              "System AC energy",                                     "kWh/mo",    "",                      "Monthly",       "*",                    "LENGTH=12",                              "" },

	{ SSC_OUTPUT,        SSC_NUMBER,     "annual_gh",                                   "Annual GHI",                                              "kWh/m2/yr", "",                      "Annual (Year 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "annual_poa_nom",                              "POA irradiance total nominal",                          "kWh/yr",    "",                      "Annual (Year 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "annual_poa_beam_nom",                         "POA irradiance beam nominal",                           "kWh/yr",    "",                      "Annual (Year 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "annual_poa_shaded",                           "POA irradiancetotal after shading only",                 "kWh/yr",    "",                      "Annual (Year 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "annual_poa_eff",                              "POA irradiancetotal after shading and soiling",          "kWh/yr",    "",                      "Annual (Year 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "annual_poa_beam_eff",                         "POA irradiancebeam after shading and soiling",           "kWh/yr",    "",                      "Annual (Year 1)",       "*",                    "",                              "" },

	{ SSC_OUTPUT,        SSC_NUMBER,     "annual_dc_nominal",                           "Annual DC energy nominal",                           "kWh/yr",    "",                      "Annual (Year 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "annual_dc_gross",                             "Annual DC energy gross",                             "kWh/yr",    "",                      "Annual (Year 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "annual_dc_net",                               "Annual DC energy",                                   "kWh/yr",    "",                      "Annual (Year 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "annual_ac_gross",                             "Annual AC energy gross",                               "kWh/yr",    "",                      "Annual (Year 1)",       "*",                    "",                              "" },


	//SEV: total dc snow loss monthy array and annual value (not a required output) 
	{ SSC_OUTPUT,        SSC_ARRAY,      "monthly_snow_loss",                    "Snow DC energy loss",					       "kWh/mo",    "",                       "Monthly",       "",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "annual_snow_loss",                     "Snow DC energy loss",						   "kWh/yr",    "",                       "Annual (Year 1)",       "",                    "",                              "" },

	// loss diagram - order applied
	{ SSC_OUTPUT, SSC_NUMBER, "annual_subarray1_dc_gross", "Subarray 1 gross DC energy", "kWh", "", "Annual (Year 1)", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_subarray1_dc_mismatch_loss", "Subarray 1 DC mismatch loss", "kWh", "", "Annual (Year 1)", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_subarray1_dc_diodes_loss", "Subarray 1 DC diodes and connections loss", "kWh", "", "Annual (Year 1)", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_subarray1_dc_wiring_loss", "Subarray 1 DC wiring loss", "kWh", "", "Annual (Year 1)", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_subarray1_dc_tracking_loss", "Subarray 1 DC tracking loss", "kWh", "", "Annual (Year 1)", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_subarray1_dc_nameplate_loss", "Subarray 1 DC nameplate loss", "kWh", "", "Annual (Year 1)", "*", "", "" },

	{ SSC_OUTPUT, SSC_NUMBER, "annual_subarray2_dc_gross", "Subarray 2 gross DC energy", "kWh", "", "Annual (Year 1)", "", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_subarray2_dc_mismatch_loss", "Subarray 2 DC mismatch loss", "kWh", "", "Annual (Year 1)", "", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_subarray2_dc_diodes_loss", "Subarray 2 DC diodes and connections loss", "kWh", "", "Annual (Year 1)", "", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_subarray2_dc_wiring_loss", "Subarray 2 DC wiring loss", "kWh", "", "Annual (Year 1)", "", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_subarray2_dc_tracking_loss", "Subarray 2 DC tracking loss", "kWh", "", "Annual (Year 1)", "", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_subarray2_dc_nameplate_loss", "Subarray 2 DC nameplate loss", "kWh", "", "Annual (Year 1)", "", "", "" },

	{ SSC_OUTPUT, SSC_NUMBER, "annual_subarray3_dc_gross", "Subarray 3 gross DC energy", "kWh", "", "Annual (Year 1)", "", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_subarray3_dc_mismatch_loss", "Subarray 3 DC mismatch loss", "kWh", "", "Annual (Year 1)", "", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_subarray3_dc_diodes_loss", "Subarray 3 DC diodes and connections loss", "kWh", "", "Annual (Year 1)", "", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_subarray3_dc_wiring_loss", "Subarray 3 DC wiring loss", "kWh", "", "Annual (Year 1)", "", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_subarray3_dc_tracking_loss", "Subarray 3 DC tracking loss", "kWh", "", "Annual (Year 1)", "", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_subarray3_dc_nameplate_loss", "Subarray 3 DC nameplate loss", "kWh", "", "Annual (Year 1)", "", "", "" },

	{ SSC_OUTPUT, SSC_NUMBER, "annual_subarray4_dc_gross", "Subarray 4 gross DC energy", "kWh", "", "Annual (Year 1)", "", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_subarray4_dc_mismatch_loss", "Subarray 4 DC mismatch loss", "kWh", "", "Annual (Year 1)", "", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_subarray4_dc_diodes_loss", "Subarray 4 DC diodes and connections loss", "kWh", "", "Annual (Year 1)", "", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_subarray4_dc_wiring_loss", "Subarray 4 DC wiring loss", "kWh", "", "Annual (Year 1)", "", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_subarray4_dc_tracking_loss", "Subarray 4 DC tracking loss", "kWh", "", "Annual (Year 1)", "", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_subarray4_dc_nameplate_loss", "Subarray 4 DC nameplate loss", "kWh", "", "Annual (Year 1)", "", "", "" },

	{ SSC_OUTPUT, SSC_NUMBER, "annual_dc_mismatch_loss", "DC mismatch loss", "kWh", "", "Annual (Year 1)", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_dc_diodes_loss", "DC diodes and connections loss", "kWh", "", "Annual (Year 1)", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_dc_wiring_loss", "DC wiring loss", "kWh", "", "Annual (Year 1)", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_dc_tracking_loss", "DC tracking loss", "kWh", "", "Annual (Year 1)", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_dc_nameplate_loss", "DC nameplate loss", "kWh", "", "Annual (Year 1)", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_dc_optimizer_loss", "DC power optimizer loss", "kWh", "", "Annual (Year 1)", "*", "", "" },

	// loss diagram energy outputs nominal poa, nominal array at STC, net dc, net ac, system output
	// annual_poa_nom, annual_dc_nominal, annual_dc_net, annual_ac_net, annual_energy
	// loss diagram % losses 
	// annual_poa_nom
	{ SSC_OUTPUT, SSC_NUMBER, "annual_poa_shading_loss_percent", "POA shading loss", "%", "", "Loss", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_poa_soiling_loss_percent", "POA soiling loss", "%", "", "Loss", "*", "", "" },
	// annual_dc_nominal
	{ SSC_OUTPUT, SSC_NUMBER, "annual_dc_module_loss_percent", "DC module modeled loss", "%", "", "Loss", "*", "", "" },
	// annual_dc_gross
	{ SSC_OUTPUT, SSC_NUMBER, "annual_dc_snow_loss_percent", "DC snow loss", "%", "", "Loss", "*", "", "" },
	
	
	{ SSC_OUTPUT, SSC_NUMBER, "annual_dc_mppt_clip_loss_percent", "DC inverter MPPT clipping loss", "%", "", "Loss", "*", "", "" },

	
	{ SSC_OUTPUT, SSC_NUMBER, "annual_dc_mismatch_loss_percent", "DC mismatch loss", "%", "", "Loss", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_dc_diodes_loss_percent", "DC diodes and connections loss", "%", "", "Loss", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_dc_wiring_loss_percent", "DC wiring loss", "%", "", "Loss", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_dc_tracking_loss_percent", "DC tracking loss", "%", "", "Loss", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_dc_nameplate_loss_percent", "DC nameplate loss", "%", "", "Loss", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_dc_optimizer_loss_percent", "DC power optimizer loss", "%", "", "Loss", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_dc_perf_adj_loss_percent", "DC performance adjustment loss", "%", "", "Loss", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_dc_lifetime_loss_percent", "Lifetime daily DC loss- year 1", "%", "", "Loss", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_dc_battery_loss_percent", "DC connected battery loss- year 1", "%", "", "Loss", "*", "", "" },

	//annual_dc_net
	{ SSC_OUTPUT, SSC_NUMBER, "annual_ac_inv_clip_loss_percent", "AC inverter power clipping loss", "%", "", "Loss", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_ac_inv_pso_loss_percent", "AC inverter power consumption loss", "%", "", "Loss", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_ac_inv_pnt_loss_percent", "AC inverter night tare loss", "%", "", "Loss", "*", "", "" },
	// annual_ac_gross
	{ SSC_OUTPUT, SSC_NUMBER, "annual_ac_inv_eff_loss_percent", "AC inverter efficiency loss", "%", "", "Loss", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_ac_wiring_loss_percent", "AC wiring loss", "%", "", "Loss", "*", "", "" },
//	{ SSC_OUTPUT, SSC_NUMBER, "annual_ac_transformer_loss_percent", "AC step-up transformer loss", "%", "", "Loss", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_ac_lifetime_loss_percent", "Lifetime daily AC loss- year 1", "%", "", "Loss", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_ac_battery_loss_percent", "AC connected battery loss- year 1", "%", "", "Loss", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_xfmr_loss_percent", "Transformer loss percent", "%", "", "Loss", "", "", "" },


	// annual_ac_net
	{ SSC_OUTPUT, SSC_NUMBER, "annual_ac_perf_adj_loss_percent", "AC performance adjustment loss", "%", "", "Loss", "*", "", "" },
	// annual_energy

	/*
	{ SSC_OUTPUT, SSC_NUMBER, "annual_dc_after_mismatch_loss", "DC output after mismatch loss", "kWh", "", "Annual (Year 1)", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_dc_after_diodes_loss", "DC output after diodes and connections loss", "kWh", "", "Annual (Year 1)", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_dc_after_wiring_loss", "DC output after wiring loss", "kWh", "", "Annual (Year 1)", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_dc_after_tracking_loss", "DC output after tracking loss", "kWh", "", "Annual (Year 1)", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_dc_after_nameplate_loss", "DC output after nameplate loss", "kWh", "", "Annual (Year 1)", "*", "", "" },

	{ SSC_OUTPUT, SSC_NUMBER, "annual_ac_after_inv_cliploss", "AC output after inverter clipping loss", "kWh", "", "Annual (Year 1)", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_ac_after_inv_psoloss", "AC output after inverter power consumption loss", "kWh", "", "Annual (Year 1)", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_ac_after_inv_pntloss", "AC output after inverter night tare loss", "kWh", "", "Annual (Year 1)", "*", "", "" },
	*/
	{ SSC_OUTPUT, SSC_NUMBER, "annual_ac_wiring_loss", "AC wiring loss", "kWh", "", "Annual (Year 1)", "*", "", "" },
//	{ SSC_OUTPUT, SSC_NUMBER, "annual_ac_transformer_loss", "AC step-up transformer loss", "kWh", "", "Annual (Year 1)", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_dc_optimizer_loss", "DC power optimizer loss", "kWh", "", "Annual (Year 1)", "*", "", "" },

	/*
	{ SSC_OUTPUT, SSC_NUMBER, "annual_ac_after_wiring_loss", "AC output after wiring loss", "kWh", "", "Annual (Year 1)", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_ac_after_transformer_loss", "AC output after step-up transformer loss", "kWh", "", "Annual (Year 1)", "*", "", "" },
	*/

	//

	{ SSC_OUTPUT,        SSC_NUMBER,     "6par_a",                                      "CEC 6-parameter: a",        "",       "", "Module CEC 6-parameter model parameters",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "6par_Io",                                     "CEC 6-parameter: Io",       "",       "", "Module CEC 6-parameter model parameters",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "6par_Il",                                     "CEC 6-parameter: Il",       "",       "", "Module CEC 6-parameter model parameters",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "6par_Rs",                                     "CEC 6-parameter: Rs",       "",       "", "Module CEC 6-parameter model parameters",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "6par_Rsh",                                    "CEC 6-parameter: Rsh",      "",       "", "Module CEC 6-parameter model parameters",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "6par_Adj",                                    "CEC 6-parameter: Adj",      "",       "", "Module CEC 6-parameter model parameters",       "*",                    "",                              "" },
																												     
	{ SSC_OUTPUT,        SSC_NUMBER,     "performance_ratio",                           "Performance ratio",         "",       "",  "Annual (Year 1)",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "capacity_factor",                             "Capacity factor",           "%",      "",  "Annual (Year 1)", "*", "", "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "kwh_per_kw",                                  "First year kWh/kW",         "kWh/kW", "",	"Annual (Year 1)", "*", "", "" },

	//miscellaneous outputs
	{ SSC_OUTPUT,        SSC_NUMBER,      "ts_shift_hours",                            "Sun position time offset",   "hours",  "",  "Miscellaneous", "*",                       "",                          "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "nameplate_dc_rating",                        "System nameplate DC rating", "kW",     "",  "Miscellaneous",       "*",                    "",                              "" },



// test outputs
#ifdef SHADE_DB_OUTPUTS
	// ShadeDB validation

	{ SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray1_gpoa", "ShadeDB subarray 1 global poa input", "W/m2", "", "Time Series (Subarray 1)", "", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray1_dpoa", "ShadeDB subarray 1 diffuse poa input", "W/m2", "", "Time Series (Subarray 1)", "", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray1_pv_cell_temp", "ShadeDB subarray 1 pv cell temp input", "C", "", "Time Series (Subarray 1)", "", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray1_mods_per_str", "ShadeDB subarray 1 modules per string input", "", "", "Time Series (Subarray 1)", "", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray1_str_vmp_stc", "ShadeDB subarray 1 string Vmp at STC input", "V", "", "Time Series (Subarray 1)", "", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray1_mppt_lo", "ShadeDB subarray 1 MPPT low input", "V", "", "Time Series (Subarray 1)", "", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray1_mppt_hi", "ShadeDB subarray 1 MPPT high input", "V", "", "Time Series (Subarray 1)", "", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray2_gpoa", "ShadeDB subarray 2 global poa input", "W/m2", "", "Time Series (Subarray 2)", "", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray2_dpoa", "ShadeDB subarray 2 diffuse poa input", "W/m2", "", "Time Series (Subarray 2)", "", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray2_pv_cell_temp", "ShadeDB subarray 2 pv cell temp input", "C", "", "Time Series (Subarray 2)", "", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray2_mods_per_str", "ShadeDB subarray 2 modules per string input", "", "", "Time Series (Subarray 2)", "", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray2_str_vmp_stc", "ShadeDB subarray 2 string Vmp at STC input", "V", "", "Time Series (Subarray 2)", "", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray2_mppt_lo", "ShadeDB subarray 2 MPPT low input", "V", "", "Time Series (Subarray 2)", "", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray2_mppt_hi", "ShadeDB subarray 2 MPPT high input", "V", "", "Time Series (Subarray 2)", "", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray3_gpoa", "ShadeDB subarray 3 global poa input", "W/m2", "", "Time Series (Subarray 3)", "", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray3_dpoa", "ShadeDB subarray 3 diffuse poa input", "W/m2", "", "Time Series (Subarray 3)", "", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray3_pv_cell_temp", "ShadeDB subarray 3 pv cell temp input", "C", "", "Time Series (Subarray 3)", "", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray3_mods_per_str", "ShadeDB subarray 3 modules per string input", "", "", "Time Series (Subarray 3)", "", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray3_str_vmp_stc", "ShadeDB subarray 3 string Vmp at STC input", "V", "", "Time Series (Subarray 3)", "", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray3_mppt_lo", "ShadeDB subarray 3 MPPT low input", "V", "", "Time Series (Subarray 3)", "", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray3_mppt_hi", "ShadeDB subarray 3 MPPT high input", "V", "", "Time Series (Subarray 3)", "", "", "" },


	{ SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray4_gpoa", "ShadeDB subarray 4 global poa input", "W/m2", "", "Time Series (Subarray 4)", "", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray4_dpoa", "ShadeDB subarray 4 diffuse poa input", "W/m2", "", "Time Series (Subarray 4)", "", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray4_pv_cell_temp", "ShadeDB subarray 4 pv cell temp input", "C", "", "Time Series (Subarray 4)", "", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray4_mods_per_str", "ShadeDB subarray 4 modules per string input", "", "", "Time Series (Subarray 4)", "", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray4_str_vmp_stc", "ShadeDB subarray 4 string Vmp at STC input", "V", "", "Time Series (Subarray 4)", "", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray4_mppt_lo", "ShadeDB subarray 4 MPPT low input", "V", "", "Time Series (Subarray 4)", "", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray4_mppt_hi", "ShadeDB subarray 4 MPPT high input", "V", "", "Time Series (Subarray 4)", "", "", "" },

#endif

	// a couple debugging outputs
	/*
	{ SSC_OUTPUT,        SSC_ARRAY,      "p_nonlinear_dc_derate0",                      "SS1x dc derate",                                          "",    "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "p_nonlinear_derate_X",                        "SS1x X",                                          "",    "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "p_nonlinear_derate_S",                        "SS1x S",                                          "",    "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "p_nonlinear_shad1xf",                         "SS1x shade fraction",                                          "",    "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "p_nonlinear_Ee_ratio",                        "SS1x Ee ratio",                                          "",    "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "p_nonlinear_skyd1xf",                         "SS1x skydiff derate",                                          "",    "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "p_nonlinear_gndd1xf",                         "SS1x gnddiff derate",                                          "",    "",                      "pvsamv1",       "*",                    "",                              "" },
	*/



var_info_invalid };

struct subarray
{
	subarray()
	{
		enable = false;
		nstrings = 0;
		tilt_eq_lat = false;
		tilt = azimuth = 0;
		track_mode = 0;
		rotlim = 0;
		shade_mode = 0; // 0=none
		backtrack = 0;

		gcr = 0.3;

		derate = 1.0;
		
		for (size_t i=0;i<12;i++)
			soiling[i] = 1.0;
		
		poa.ibeam = 0;
		poa.iskydiff = 0;
		poa.ignddiff = 0;
		poa.ipoa = 0;
		poa.sunup = 0;
		poa.aoi = 0;
		poa.stilt = 0;
		poa.sazi = 0;
		poa.nonlinear_dc_shading_derate = 1.0;
		poa.usePOAFromWF = false;
		poa.poaShadWarningCount = 0;

		module.dcpwr = 0;
		module.dcv = 0;
		module.dceff = 0;
		module.tcell = 0;
		module.voc = 0;
		module.isc = 0;


	}

	bool enable;
	int nstrings;
	double tilt;
	bool tilt_eq_lat;
	ssc_number_t *monthly_tilt;
	double azimuth;
	int track_mode;
	double rotlim;
	double soiling[12];
	double derate;
	
	int shade_mode;
	bool backtrack;
	double gcr;

	ssinputs sscalc;
	ssoutputs ssout;
	
	shading_factor_calculator shad;

	pvsnowmodel sm;

	// calculated by irradiance processor
	struct {
		double ibeam;
		double iskydiff;
		double ignddiff;
		double ipoa;
		int sunup;
		double aoi;
		double stilt;
		double sazi;
		double nonlinear_dc_shading_derate;
		bool usePOAFromWF;
		int poaShadWarningCount;
		poaDecompReq poaAll;
	} poa;

	// calculated by module model
	struct {
		double dcpwr;
		double dcv;
		double voc;
		double isc;
		double dceff;
		double tcell;
	} module;
		
};


class cm_pvsamv1 : public compute_module
{
public:
	
	cm_pvsamv1()
	{
		add_var_info( _cm_vtab_pvsamv1 );
		add_var_info(vtab_adjustment_factors);
		add_var_info(vtab_dc_adjustment_factors);
		add_var_info(vtab_technology_outputs);
		add_var_info(vtab_battery_inputs);
		add_var_info(vtab_battery_outputs);
	}

	void setup_noct_model( const std::string &prefix, noct_celltemp_t &noct_tc )
	{		
		noct_tc.Tnoct = as_double(prefix+"_tnoct");
		noct_tc.ffv_wind = 0.51; // less than 22ft high (1 story)
		if ( as_integer(prefix+"_mounting") == 1 ) noct_tc.ffv_wind = 0.61;  // greater than 22ft high (2 story)
		
		int standoff = as_integer(prefix+"_standoff"); // bipv,3.5in,2.5-3.5in,1.5-2.5in,0.5-1.5in,ground/rack
		noct_tc.standoff_tnoct_adj = 0;
		switch( standoff )
		{
		case 2: noct_tc.standoff_tnoct_adj = 2; break; // between 2.5 and 3.5 inches
		case 3: noct_tc.standoff_tnoct_adj = 6; break; // between 1.5 and 2.5 inches
		case 4: noct_tc.standoff_tnoct_adj = 11; break; // between 0.5 and 1.5 inches
		case 5: noct_tc.standoff_tnoct_adj = 18; break; // less than 0.5 inches
		}
	}
	
	void exec( ) throw( general_error )
	{		
		std::auto_ptr<weather_data_provider> wdprov;
		if ( is_assigned( "solar_resource_file" ) )
		{
			const char *file = as_string("solar_resource_file");
			wdprov = std::auto_ptr<weather_data_provider>( new weatherfile( file ) );

			weatherfile *wfile = dynamic_cast<weatherfile*>(wdprov.get());
			if (!wfile->ok()) throw exec_error("pvsamv1", wfile->message());
			if( wfile->has_message() ) log( wfile->message(), SSC_WARNING);
		}
		else if ( is_assigned( "solar_resource_data" ) )
		{
			wdprov = std::auto_ptr<weather_data_provider>( new weatherdata( lookup("solar_resource_data") ) );
		}
		else
			throw exec_error("pvsamv1", "no weather data supplied");
		
								
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
			throw exec_error("pvsamv1", "subhourly weather files must specify the minute for each record" );

		assign( "ts_shift_hours", var_data( (ssc_number_t)ts_shift_hours ) );

		weather_header hdr;
		wdprov->header( &hdr );

		weather_record wf;		

		//total number of records in the weather file (i.e. 8760 * timestep)
		size_t nrec = wdprov->nrecords();
		size_t step_per_hour = nrec/8760;
		if ( step_per_hour < 1 || step_per_hour > 60 || step_per_hour*8760 != nrec )
			throw exec_error( "pvsamv1", util::format("invalid number of data records (%d): must be an integer multiple of 8760", (int)nrec ) );
		
		double ts_hour = 1.0/step_per_hour;
		// shading database if necessary
		std::auto_ptr<ShadeDB8_mpp>  p_shade_db; // (new ShadeDB8_mpp());

		bool en_snow_model = (as_integer("en_snow_model") > 0); // snow model activation
		double annual_snow_loss = 0;

		int modules_per_string = as_integer("modules_per_string");
		int strings_in_parallel = as_integer("strings_in_parallel");
		int num_inverters = as_integer("inverter_count");
//		double ac_derate = (1 - as_double("acwiring_loss") / 100) * (1 - as_double("transformer_loss") / 100);	//calculate using ac wiring and step up transformer losses
		double ac_derate = 1 - as_double("acwiring_loss") / 100;	//calculate using ac wiring 
		double ac_loss_percent = (1 - ac_derate) * 100;
		assign("ac_loss", var_data((ssc_number_t)ac_loss_percent));

		size_t alb_len = 0;
		ssc_number_t *alb_array = as_array("albedo", &alb_len); // monthly albedo array

		bool use_wf_alb = (as_integer("use_wf_albedo") > 0); // weather file albedo

		int radmode = as_integer("irrad_mode"); // 0=B&D, 1=G&B, 2=G&D, 3=POA-Ref, 4=POA-Pyra
		int skymodel = as_integer("sky_model"); // 0=isotropic, 1=hdkr, 2=perez

		// load the subarray parameter information
		subarray sa[4];		
		int num_subarrays = 1;


		// check to see if shading database needs to be created; updated 4/19/16
		bool create_shade_db = false;
		// loop over subarrays
		for ( size_t nn=0;nn<4;nn++ )
		{
			sa[nn].enable = true;
			sa[nn].nstrings = strings_in_parallel;
			std::string prefix = "subarray" + util::to_string( (int)(nn+1) ) + "_";

			if (nn > 0)
			{
				sa[nn].nstrings = 0;
				sa[nn].enable = as_boolean( prefix+"enable" );
				if (sa[nn].enable) sa[nn].nstrings = as_integer( prefix+"nstrings" );

				sa[0].nstrings -= sa[nn].nstrings;
				
				if (sa[nn].nstrings > 0 && sa[nn].enable)
					num_subarrays++;
			}

			// don't read in all the other variables
			// if the subarrays are disabled.
			if ( !sa[nn].enable )
				continue;
			
			size_t soil_len = 0;
			ssc_number_t *soiling = as_array(prefix+"soiling", &soil_len); // monthly soiling array
			if (soil_len != 12) throw exec_error( "pvsamv1", "soiling loss array must have 12 values: subarray " + util::to_string((int)(nn+1)) );
			for (int k=0;k<12;k++)
				sa[nn].soiling[k] = 1- (double) soiling[k]/100; //convert from % to derate
	
			sa[nn].derate =  /* combine all input losses into one derate, not a percentage */
				(1 - as_double(prefix + "mismatch_loss") / 100) *
				(1 - as_double(prefix + "diodeconn_loss") / 100) *
				(1 - as_double(prefix + "dcwiring_loss") / 100) *
				(1 - as_double(prefix + "tracking_loss") / 100) *
				(1 - as_double(prefix + "nameplate_loss") / 100) *
				(1 - as_double("dcoptimizer_loss") / 100);
			//assign output dc loss since we just calculated it
			double temploss = (1 - sa[nn].derate) * 100;
			assign(prefix + "dcloss", var_data((ssc_number_t)temploss));

			sa[nn].track_mode = as_integer( prefix+"track_mode"); // 0=fixed, 1=1axis, 2=2axis, 3=aziaxis, 4=timeseries

			sa[nn].tilt = fabs(hdr.lat);
			if (sa[nn].track_mode == 4) //timeseries tilt input
			{
				size_t monthly_tilt_count = 0;
				sa[nn].monthly_tilt = as_array(prefix + "monthly_tilt", &monthly_tilt_count);
			}
	
			if ( !lookup( prefix+"tilt_eq_lat" ) || !as_boolean( prefix+"tilt_eq_lat" ) )
				sa[nn].tilt = fabs( as_double( prefix+"tilt" ) );

			sa[nn].azimuth = as_double( prefix+"azimuth" );
			sa[nn].rotlim = as_double( prefix+"rotlim" );

			sa[nn].gcr = as_double(prefix + "gcr");
			if (sa[nn].gcr < 0.01)
				throw exec_error("pvsamv1", "array ground coverage ratio must obey 0.01 < gcr");
			
			if (!sa[nn].shad.setup( this, prefix ))
				throw exec_error("pvsamv1", prefix + "_shading: " + sa[nn].shad.get_error() );

			create_shade_db = (create_shade_db || sa[nn].shad.use_shade_db());

			// backtracking- only required if one-axis tracker
			if (sa[nn].track_mode == 1)
			{
				sa[nn].backtrack = as_boolean(prefix + "backtrack");
			}

			// Initialize snow model if activated
			if (en_snow_model)
			{
				if (sa[nn].track_mode == 4) //timeseries tilt input
					throw exec_error("pvsamv1", "Time-series tilt input may not be used with the snow model at this time: subarray " + util::to_string((int)(nn + 1)));
				if (!sa[nn].sm.setup(as_integer(prefix + "nmody"), (float)sa[nn].tilt)){
					if (sa[nn].sm.good)log(sa[nn].sm.msg, SSC_WARNING);
					else{
						log(sa[nn].sm.msg, SSC_ERROR);
						return;
					}
				}
			}

			sa[nn].poa.usePOAFromWF = false;
		}

		// create single instance of shading database if necessary
		if (create_shade_db)
		{
			p_shade_db = std::auto_ptr<ShadeDB8_mpp>(new ShadeDB8_mpp());
			p_shade_db->init();
		}


		// loop over subarrays AGAIN to calculate shading inputs because nstrings in subarray 1 isn't correct until AFTER the previous loop
		for (size_t nn = 0; nn < 4; nn++)
		{
			std::string prefix = "subarray" + util::to_string((int)(nn + 1)) + "_";

			// shading mode- only required for fixed tilt/timeseries tilt or one-axis, not backtracking systems
			if ( (sa[nn].track_mode == 0 || sa[nn].track_mode == 4) || (sa[nn].track_mode == 1 && sa[nn].backtrack == 0) )
			{
				sa[nn].shade_mode = as_integer(prefix + "shade_mode");
				if (!sa[nn].enable) continue; //skip disabled subarrays


				// shading inputs only required if shade mode is self-shaded
				if (sa[nn].shade_mode == 1 || sa[nn].shade_mode == 2)
				{
					sa[nn].sscalc.mod_orient = as_integer(prefix + "mod_orient");
					sa[nn].sscalc.nmody = as_integer(prefix + "nmody");
					sa[nn].sscalc.nmodx = as_integer(prefix + "nmodx");
					sa[nn].sscalc.nstrx = sa[nn].sscalc.nmodx / modules_per_string;

					// SELF-SHADING ASSUMPTIONS

					// Calculate the number of rows given the module dimensions of each row.
					sa[nn].sscalc.nrows = (int)floor((sa[nn].nstrings * modules_per_string) / (sa[nn].sscalc.nmodx * sa[nn].sscalc.nmody));
					//if nrows comes out to be zero, this will cause a divide by zero error. Give an error in this case.
					if (sa[nn].sscalc.nrows == 0 && sa[nn].nstrings != 0) //no need to give an error if the subarray has 0 strings
						throw exec_error("pvsamv1", "Self shading: Number of rows calculated for subarray " + util::to_string(to_double(nn + 1)) + " was zero. Please check your inputs.");
					// Otherwise, if self-shading configuration does not have equal number of modules as specified on system design page for that subarray,
					// compute dc derate using the self-shading configuration and apply it to the whole subarray. Give warning.
					if ((sa[nn].sscalc.nmodx * sa[nn].sscalc.nmody * sa[nn].sscalc.nrows) != (sa[nn].nstrings * modules_per_string))
						log(util::format("The product of number of modules along side and bottom for subarray %d is not equal to the number of modules in the subarray. Check your inputs for self shading.",
						(nn + 1)), SSC_WARNING);
					// assume aspect ratio of 1.7 (see variable "aspect_ratio" below to change this assumption)
					sa[nn].sscalc.str_orient = 1;	//assume horizontal wiring
					sa[nn].sscalc.mask_angle_calc_method = 0; //assume worst case mask angle calc method
					sa[nn].sscalc.ndiode = 3;	//assume 3 diodes- maybe update this assumption based on number of cells in the module?
				}
			}
		}
		
		double aspect_ratio = as_double("module_aspect_ratio");

		if (sa[0].nstrings < 0)
			throw exec_error("pvsamv1", "invalid string allocation between subarrays.  all subarrays must have zero or positive number of strings.");

		// run some preliminary checks on inputs

		int mod_type = as_integer("module_model");
		
		spe_module_t spe;
		sandia_celltemp_t spe_tc;

		cec6par_module_t cec;
		noct_celltemp_t noct_tc;
		mcsp_celltemp_t mcsp_tc;

		sandia_module_t snl;
		sandia_celltemp_t snl_tc;

		iec61853_module_t sd11; // 11 parameter single diode, uses noct_tc from above

		pvcelltemp_t *celltemp_model = 0;
		pvmodule_t *module_model = 0;		

		double ref_area_m2 = 0;

		double self_shading_fill_factor = 0;
		double ssVmp = 0;

		double module_watts_stc = -1.0;

		//"0=spe,1=cec,2=6par_user,3=snl,4=sd11-iec61853"
		bool enable_mismatch_vmax_calc = as_boolean("enable_mismatch_vmax_calc");
		if (enable_mismatch_vmax_calc 
			&& mod_type != 1 && mod_type != 2 && mod_type != 4 )
			throw exec_error( "pvsamv1", "String level subarray mismatch can only be calculated using a single-diode based module model.");


		bool speForceNoPOA = false;		// SEV 151002 - Set these flags to avoid calling as_integer(...) repeatedly later on
		bool mcspForceNoPOA = false;    //   These flags are used to ensure that the usePOAFromWF flag for each sub array will be force
										//   to false

		if ( mod_type == 0 )
		{
			spe.VmpNominal = as_double("spe_vmp");
			spe.VocNominal = as_double("spe_voc");
			spe.Area = as_double("spe_area");
			ref_area_m2 = spe.Area;
			for (int i=0;i<5;i++)
			{
				spe.Rad[i] = as_double( util::format("spe_rad%d", i));
				spe.Eff[i] = 0.01*as_double( util::format("spe_eff%d", i));
				if (i > 0 && spe.Rad[i] <= spe.Rad[i-1])
					throw exec_error( "pvsamv1", "SPE model radiation levels must increase monotonically");
			}
		
			spe.Gamma = as_double("spe_temp_coeff");
			spe.Reference = as_integer("spe_reference");
		
			switch(as_integer("spe_module_structure"))
			{
			case 0: //glass/cell/polymer sheet - open rack
				spe_tc.a = -3.56;
				spe_tc.b = -0.0750;
				spe_tc.DT0 = 3;
				break;
			case 1: //glass/cell/glass - open rack
				spe_tc.a = -3.47;
				spe_tc.b = -0.0594;
				spe_tc.DT0 = 3;
				break;
			case 2: //polymer/thin film/steel - open rack
				spe_tc.a = -3.58;
				spe_tc.b = -0.113;
				spe_tc.DT0 = 3;
				break;
			case 3: //Insulated back (building-integrated PV)
				spe_tc.a = -2.81;
				spe_tc.b = -0.0455;
				spe_tc.DT0 = 0;
				break;
			case 4: //close roof mount
				spe_tc.a = -2.98;
				spe_tc.b = -0.0471;
				spe_tc.DT0 = 1;
				break;
			case 5: //user defined
				spe_tc.a = as_double("spe_a");
				spe_tc.b = as_double("spe_b");
				spe_tc.DT0 = as_double("spe_dT");
				break;
			default:
				throw exec_error("pvsamv1", "invalid spe module structure and mounting");
			}

			spe.fd = as_double("spe_fd");
			spe_tc.fd = spe.fd;
			
			if(spe.fd < 1.0)
				speForceNoPOA = true;

			celltemp_model = &spe_tc;
			module_model = &spe;
			module_watts_stc = spe.WattsStc();
			ssVmp = spe.VmpNominal;
		}
		else if ( mod_type == 1 )
		{
			cec.Area = as_double("cec_area");
			ref_area_m2 = cec.Area;
			cec.Vmp = as_double("cec_v_mp_ref");
			cec.Imp = as_double("cec_i_mp_ref");
			cec.Voc = as_double("cec_v_oc_ref");
			cec.Isc = as_double("cec_i_sc_ref");
			cec.alpha_isc = as_double("cec_alpha_sc");
			cec.beta_voc = as_double("cec_beta_oc");
			cec.a = as_double("cec_a_ref");
			cec.Il = as_double("cec_i_l_ref");
			cec.Io = as_double("cec_i_o_ref");
			cec.Rs = as_double("cec_r_s");
			cec.Rsh = as_double("cec_r_sh_ref");
			cec.Adj = as_double("cec_adjust");

			self_shading_fill_factor = cec.Vmp * cec.Imp / cec.Voc / cec.Isc;
			ssVmp = cec.Vmp;

			if ( as_integer("cec_temp_corr_mode") == 0 )
			{
				noct_tc.Tnoct = as_double("cec_t_noct");
				int standoff = as_integer("cec_standoff");
				noct_tc.standoff_tnoct_adj = 0;
				switch(standoff)
				{
				case 2: noct_tc.standoff_tnoct_adj = 2; break; // between 2.5 and 3.5 inches
				case 3: noct_tc.standoff_tnoct_adj = 6; break; // between 1.5 and 2.5 inches
				case 4: noct_tc.standoff_tnoct_adj = 11; break; // between 0.5 and 1.5 inches
				case 5: noct_tc.standoff_tnoct_adj = 18; break; // less than 0.5 inches
					// note: all others, standoff_tnoct_adj = 0;
				}

				int height = as_integer("cec_height");
				noct_tc.ffv_wind = 0.51;
				if ( height == 1 )
					noct_tc.ffv_wind = 0.61;

				celltemp_model = &noct_tc;
			}
			else
			{
				/*	int MC; // Mounting configuration (1=rack,2=flush,3=integrated,4=gap)
					int HTD; // Heat transfer dimension (1=Module,2=Array)
					int MSO; // Mounting structure orientation (1=does not impede flow beneath, 2=vertical supports, 3=horizontal supports)
					int Nrows, Ncols; // number of modules in rows and columns, when using array heat transfer dimensions
					double Length; // module length, along horizontal dimension, (m)
					double Width; // module width, along vertical dimension, (m)
					double Wgap;  // gap width spacing (m)
					double TbackInteg; */

				mcsp_tc.DcDerate = sa[0].derate;  // TODO dc_derate needs to updated for each subarray
				mcsp_tc.MC = as_integer("cec_mounting_config")+1;
				mcsp_tc.HTD = as_integer("cec_heat_transfer")+1;
				mcsp_tc.MSO = as_integer("cec_mounting_orientation")+1;
				mcsp_tc.Wgap = as_double("cec_gap_spacing");
				mcsp_tc.Length = as_double("cec_module_length");
				mcsp_tc.Width = as_double("cec_module_width");
				mcsp_tc.Nrows = (int)as_integer("cec_array_rows");
				mcsp_tc.Ncols = (int)as_integer("cec_array_cols");
				mcsp_tc.TbackInteg = as_double("cec_backside_temp");

				celltemp_model = &mcsp_tc;
				mcspForceNoPOA = true;
			}
			
			module_model = &cec;
			module_watts_stc = cec.Vmp * cec.Imp;
		}
		else if ( mod_type == 3 )
		{
			snl.A0 = as_double("snl_a0");
			snl.A1 = as_double("snl_a1");
			snl.A2 = as_double("snl_a2");
			snl.A3 = as_double("snl_a3");
			snl.A4 = as_double("snl_a4");
			snl.aImp = as_double("snl_aimp");
			snl.aIsc = as_double("snl_aisc");
			snl.Area = as_double("snl_area");
			ref_area_m2 = snl.Area;
			snl.B0 = as_double("snl_b0");
			snl.B1 = as_double("snl_b1");
			snl.B2 = as_double("snl_b2");
			snl.B3 = as_double("snl_b3");
			snl.B4 = as_double("snl_b4");
			snl.B5 = as_double("snl_b5");
			snl.BVmp0 = as_double("snl_bvmpo");
			snl.BVoc0 = as_double("snl_bvoco");
			snl.C0 = as_double("snl_c0");
			snl.C1 = as_double("snl_c1");
			snl.C2 = as_double("snl_c2");
			snl.C3 = as_double("snl_c3");
			snl.C4 = as_double("snl_c4");
			snl.C5 = as_double("snl_c5");
			snl.C6 = as_double("snl_c6");
			snl.C7 = as_double("snl_c7");
			snl.fd = as_double("snl_fd");
			snl.Imp0 = as_double("snl_impo");
			snl.Isc0 = as_double("snl_isco");
			snl.Ix0 = as_double("snl_ixo");
			snl.Ixx0 = as_double("snl_ixxo");
			snl.mBVmp = as_double("snl_mbvmp");
			snl.mBVoc = as_double("snl_mbvoc");
			snl.DiodeFactor = as_double("snl_n");
			snl.NcellSer = as_integer("snl_series_cells");
			snl.Vmp0 = as_double("snl_vmpo");
			snl.Voc0 = as_double("snl_voco");

			self_shading_fill_factor = snl.Vmp0 * snl.Imp0 / snl.Voc0 / snl.Isc0;
			ssVmp = snl.Vmp0;

			// by default, use database values
			double A = as_double("snl_a");
			double B = as_double("snl_b");
			double DT = as_double("snl_dtc");
	
			switch(as_integer("snl_module_structure"))
			{
			case 1: //glass/cell/polymer sheet - open rack
				A = -3.56;
				B = -0.0750;
				DT = 3;
				break;
			case 2: //glass/cell/glass - open rack
				A = -3.47;
				B = -0.0594;
				DT = 3;
				break;
			case 3: //polymer/thin film/steel - open rack
				A = -3.58;
				B = -0.113;
				DT = 3;
				break;
			case 4: //Insulated back (building-integrated PV)
				A = -2.81;
				B = -0.0455;
				DT = 0;
				break;
			case 5: //close roof mount
				A = -2.98;
				B = -0.0471;
				DT = 1;
				break;
			case 6: //user defined
				A = as_double("snl_ref_a");
				B = as_double("snl_ref_b");
				DT = as_double("snl_ref_dT");
				break;

			default:
				break;
			}
		
			snl_tc.a = A;
			snl_tc.b = B;
			snl_tc.DT0 = DT;
			snl_tc.fd = snl.fd;
			
			celltemp_model = &snl_tc;
			module_model = &snl;
			module_watts_stc = snl.Vmp0 * snl.Imp0;
		}
		else if ( mod_type == 2 )
		{
			// calculate the 6 parameters
			// adjust TNOCT and FFV_wind

			int tech_id = module6par::monoSi;
			int type = as_integer("6par_celltech"); // "monoSi,multiSi,CdTe,CIS,CIGS,Amorphous"
			switch( type )
			{
			case 0: tech_id = module6par::monoSi; break;
			case 1: tech_id = module6par::multiSi; break;
			case 2: tech_id = module6par::CdTe; break;
			case 3: tech_id = module6par::CIS; break;
			case 4: tech_id = module6par::CIGS; break;
			case 5: tech_id = module6par::Amorphous; break;
			}

			double Vmp = as_double("6par_vmp");
			double Imp = as_double("6par_imp");
			double Voc = as_double("6par_voc");
			double Isc = as_double("6par_isc");
			double alpha = as_double("6par_aisc");
			double beta = as_double("6par_bvoc");
			double gamma = as_double("6par_gpmp");
			int nser = as_integer("6par_nser");
		
			module6par m(tech_id, Vmp, Imp, Voc, Isc, beta, alpha, gamma, nser, 298.15);
			int err = m.solve_with_sanity_and_heuristics<double>( 300, 1e-7 );

			if (err != 0)
				throw exec_error( "pvsamv1", "CEC 6 parameter model:  Could not solve for normalized coefficients.  Please check your inputs.");

			cec.Area = as_double("6par_area");
			ref_area_m2 = cec.Area;
			cec.Vmp = Vmp;
			cec.Imp = Imp;
			cec.Voc = Voc;
			cec.Isc = Isc;
			cec.alpha_isc = alpha;
			cec.beta_voc = beta;
			cec.a = m.a;
			cec.Il = m.Il;
			cec.Io = m.Io;
			cec.Rs = m.Rs;
			cec.Rsh = m.Rsh;
			cec.Adj = m.Adj;
		
			self_shading_fill_factor = cec.Vmp * cec.Imp / cec.Voc / cec.Isc;
			ssVmp = cec.Vmp;

			setup_noct_model( "6par", noct_tc );
			
			celltemp_model = &noct_tc;
			module_model = &cec;
			module_watts_stc = cec.Vmp * cec.Imp;
		}
		else if ( mod_type == 4 )
		{
			// IEC 61853 model
			sd11.NcellSer = as_integer("sd11par_nser");
			sd11.Area = as_double("sd11par_area");
			sd11.AMA[0] = as_double("sd11par_AMa0");
			sd11.AMA[1] = as_double("sd11par_AMa1");
			sd11.AMA[2] = as_double("sd11par_AMa2");    
			sd11.AMA[3] = as_double("sd11par_AMa3");    
			sd11.AMA[4] = as_double("sd11par_AMa4");  
			sd11.GlassAR = as_boolean("sd11par_glass");  

			setup_noct_model( "sd11par", noct_tc );
			
			sd11.Vmp0 = as_double( "sd11par_Vmp0" ); 
			sd11.Imp0 = as_double( "sd11par_Imp0" );
			sd11.Voc0 = as_double( "sd11par_Voc0" );
			sd11.Isc0 = as_double( "sd11par_Isc0" );
			sd11.alphaIsc = as_double( "sd11par_alphaIsc" );
			sd11.n = as_double("sd11par_n" );
			sd11.Il = as_double("sd11par_Il");
			sd11.Io = as_double("sd11par_Io");
			sd11.Egref = as_double("sd11par_Egref");
			sd11.D1 = as_double("sd11par_d1");
			sd11.D2 = as_double("sd11par_d2");
			sd11.D3 = as_double("sd11par_d3");
			sd11.C1 = as_double("sd11par_c1");
			sd11.C2 = as_double("sd11par_c2");
			sd11.C3 = as_double("sd11par_c3");

			celltemp_model = &noct_tc;
			module_model = &sd11;
			module_watts_stc = sd11.Vmp0 * sd11.Imp0;
			ref_area_m2 = sd11.Area;			
			self_shading_fill_factor = sd11.Vmp0 * sd11.Imp0 / sd11.Voc0 / sd11.Isc0;
			ssVmp = sd11.Vmp0;
		}
		else
			throw exec_error("pvsamv1", "invalid pv module model type");

		//boolean to determine if the sandia model is being used for CPV
		bool is_cpv = false;

		if (as_integer("module_model") == 3 // sandia model 
			&& as_double("snl_fd") == 0)
			is_cpv = true;

		// SELF-SHADING MODULE INFORMATION
		double width = sqrt((ref_area_m2 / aspect_ratio));
		for (size_t nn = 0; nn < 4; nn++)
		{
			sa[nn].sscalc.width = width;
			sa[nn].sscalc.length = width * aspect_ratio;
			sa[nn].sscalc.FF0 = self_shading_fill_factor;
			sa[nn].sscalc.Vmp = ssVmp;
			double b = 0;
			if (sa[nn].sscalc.mod_orient == 0)
				b = sa[nn].sscalc.nmody * sa[nn].sscalc.length;
			else
				b = sa[nn].sscalc.nmody * sa[nn].sscalc.width;
			sa[nn].sscalc.row_space = b / sa[nn].gcr;
		}
		
		double nameplate_kw = modules_per_string * strings_in_parallel * module_watts_stc * util::watt_to_kilowatt;

		::sandia_inverter_t snlinv;
		::partload_inverter_t plinv;

		int inv_type = as_integer("inverter_model");
		double V_mppt_lo_1module = as_double("mppt_low_inverter") / modules_per_string;
		double V_mppt_hi_1module = as_double("mppt_hi_inverter") / modules_per_string;
		bool clip_mppt_window = false;
		double ratedACOutput = 0;

		if ( V_mppt_lo_1module > 0 && V_mppt_hi_1module > V_mppt_lo_1module )
		{
			if ( mod_type == 1     // cec with database
				|| mod_type == 2   // cec with user specs
				|| mod_type == 4 ) // iec61853 single diode
			{
				clip_mppt_window = true;
			}
			else
			{
				log( "The simple efficiency and Sandia module models do not allow limiting module voltage to the MPPT tracking range of the inverter.", SSC_NOTICE );
			}
		}
		else
		{
			log( "Inverter MPPT voltage tracking window not defined - modules always operate at MPPT.", SSC_NOTICE );
		}

		if (inv_type == 0) // cec database
		{
			snlinv.Paco = as_double("inv_snl_paco");
			snlinv.Pdco = as_double("inv_snl_pdco");
			snlinv.Vdco = as_double("inv_snl_vdco");
			snlinv.Pso = as_double("inv_snl_pso");
			snlinv.Pntare = as_double("inv_snl_pnt");
			snlinv.C0 = as_double("inv_snl_c0");
			snlinv.C1 = as_double("inv_snl_c1");
			snlinv.C2 = as_double("inv_snl_c2");
			snlinv.C3 = as_double("inv_snl_c3");
			ratedACOutput = snlinv.Paco;
		}
		else if (inv_type == 1) // datasheet data
		{
			double eff_ds = as_double("inv_ds_eff") / 100.0;
			snlinv.Paco = as_double("inv_ds_paco");
			if (eff_ds != 0)
				snlinv.Pdco = snlinv.Paco / eff_ds;
			else
				snlinv.Pdco = 0;
			snlinv.Vdco = as_double("inv_ds_vdco");
			snlinv.Pso = as_double("inv_ds_pso");
			snlinv.Pntare = as_double("inv_ds_pnt");
			snlinv.C0 = 0;
			snlinv.C1 = 0;
			snlinv.C2 = 0;
			snlinv.C3 = 0;
			ratedACOutput = snlinv.Paco;
		}
		else if (inv_type == 2) // partload curve
		{
			plinv.Paco = as_double("inv_pd_paco");
			plinv.Pdco = as_double("inv_pd_pdco");
			plinv.Pntare = as_double("inv_pd_pnt");

			std::vector<double> pl_pd = as_doublevec("inv_pd_partload");
			std::vector<double> eff_pd = as_doublevec("inv_pd_efficiency");

			plinv.Partload = pl_pd;
			plinv.Efficiency = eff_pd;
			ratedACOutput = plinv.Paco;
		}
		else if (inv_type == 3) // coefficient generator
		{
			snlinv.Paco = as_double("inv_cec_cg_paco");
			snlinv.Pdco = as_double("inv_cec_cg_pdco");
			snlinv.Vdco = as_double("inv_cec_cg_vdco");
			snlinv.Pso = as_double("inv_cec_cg_psco");
			snlinv.Pntare = as_double("inv_cec_cg_pnt");
			snlinv.C0 = as_double("inv_cec_cg_c0");
			snlinv.C1 = as_double("inv_cec_cg_c1");
			snlinv.C2 = as_double("inv_cec_cg_c2");
			snlinv.C3 = as_double("inv_cec_cg_c3");
			ratedACOutput = snlinv.Paco;
		}
		else
		{
			throw exec_error("pvsamv1", "invalid inverter model type");
		}
		ratedACOutput *= num_inverters;



		// lifetime control variables - used to set array sizes
		int system_use_lifetime_output = as_integer("system_use_lifetime_output");
		size_t nyears = 1;
		if (system_use_lifetime_output == 1)
			nyears = as_integer("analysis_period");

		if ( __ARCHBITS__ == 32 && system_use_lifetime_output )
			throw exec_error( "pvsamv1", "Lifetime simulation of PV systems is only available in the 64 bit version of SAM.");

		ssc_number_t *p_dc_degrade_factor = 0;

		bool en_dc_lifetime_losses = as_boolean("en_dc_lifetime_losses");
		ssc_number_t *dc_lifetime_losses = 0; //daily losses over the lifetime of the system, optional
		bool en_ac_lifetime_losses = as_boolean("en_ac_lifetime_losses");
		ssc_number_t *ac_lifetime_losses = 0; //daily losses over the lifetime of the system, optional

		if (system_use_lifetime_output == 1)
		{
			size_t count_dc_degrad = 0;
			ssc_number_t *dc_degrad = 0;
			dc_degrad = as_array("dc_degradation", &count_dc_degrad);
			// setup output arrays
			p_dc_degrade_factor = allocate("dc_degrade_factor", nyears + 1);
			//ssc_number_t *p_ac_degrade_factor = allocate("ac_degrade_factor", nyears);

			p_dc_degrade_factor[0] = 1.0; // degradation assumed to start at year 2
			p_dc_degrade_factor[1] = 1.0; // degradation assumed to start at year 2

			if (count_dc_degrad == 1)
			{
				for (size_t i = 1; i < nyears; i++)
					p_dc_degrade_factor[i+1] = (ssc_number_t)pow((1.0 - dc_degrad[0] / 100.0), i);
			}
			else if (count_dc_degrad > 0)
			{
				for (size_t i = 1; i < nyears && i < count_dc_degrad; i++)
					p_dc_degrade_factor[i+1] = (ssc_number_t)(1.0 - dc_degrad[i] / 100.0);
			}

			//read in optional DC and AC lifetime daily losses, error check length of arrays
			if (en_dc_lifetime_losses)
			{
				size_t count_dc_lifetime = 0;
				dc_lifetime_losses = as_array("dc_lifetime_losses", &count_dc_lifetime);
				if (count_dc_lifetime != nyears * 365)
					throw exec_error("pvsamv1", "Length of the lifetime daily DC losses array must be equal to the analysis period * 365");
			}
			if (en_ac_lifetime_losses)
			{
				size_t count_ac_lifetime = 0;
				ac_lifetime_losses = as_array("ac_lifetime_losses", &count_ac_lifetime);
				if (count_ac_lifetime != nyears * 365)
					throw exec_error("pvsamv1", "Length of the lifetime daily AC losses array must be equal to the analysis period * 365");
			}
		}

		// output arrays for weather info- same for all four subarrays	
		ssc_number_t *p_glob = allocate( "gh", nrec );
		ssc_number_t *p_beam = allocate("dn", nrec);
		ssc_number_t *p_diff = allocate("df", nrec);
		ssc_number_t *p_wfpoa = allocate("wfpoa", nrec);    // POA irradiance from weather file
		ssc_number_t *p_sunpos_hour = allocate("sunpos_hour", nrec);
		ssc_number_t *p_wspd = allocate("wspd", nrec);
		ssc_number_t *p_tdry = allocate("tdry", nrec);
		ssc_number_t *p_albedo = allocate("alb", nrec);
		ssc_number_t *p_snowdepth = allocate("snowdepth", nrec);

		//set up the calculated components of irradiance such that they aren't reported if they aren't assigned
		//three possible calculated irradiance: gh, df, dn
		ssc_number_t *p_irrad_calc[3]; 
		if (radmode == DN_DF) p_irrad_calc[0] = allocate("gh_calc", nrec); //don't calculate global for POA models
		if (radmode == DN_GH || radmode == POA_R || radmode == POA_P) p_irrad_calc[1] = allocate("df_calc", nrec);
		if (radmode == GH_DF || radmode == POA_R || radmode == POA_P) p_irrad_calc[2] = allocate("dn_calc", nrec);

		//output arrays for solar position calculations- same for all four subarrays
		ssc_number_t *p_solzen = allocate("sol_zen", nrec);
		ssc_number_t *p_solalt = allocate("sol_alt", nrec);
		ssc_number_t *p_solazi = allocate("sol_azi", nrec);
		ssc_number_t *p_airmass = allocate("airmass", nrec);
		ssc_number_t *p_sunup = allocate("sunup", nrec);

		/*
		ssc_number_t *p_nonlinear_dc_derate0 = allocate("p_nonlinear_dc_derate0", nrec);
		ssc_number_t *p_nonlinear_derate_X = allocate("p_nonlinear_derate_X", nrec);
		ssc_number_t *p_nonlinear_derate_S = allocate("p_nonlinear_derate_S", nrec);
		ssc_number_t *p_nonlinear_Ee_ratio = allocate("p_nonlinear_Ee_ratio", nrec);
		ssc_number_t *p_nonlinear_shad1xf = allocate("p_nonlinear_shad1xf", nrec);
		ssc_number_t *p_nonlinear_skyd1xf = allocate("p_nonlinear_skyd1xf", nrec);
		ssc_number_t *p_nonlinear_gndd1xf = allocate("p_nonlinear_gndd1xf", nrec);
		*/
		
		//output arrays for subarray-specific parameters
		ssc_number_t *p_aoi[4];
		ssc_number_t *p_surftilt[4];  
		ssc_number_t *p_surfazi[4];   
		ssc_number_t *p_rot[4];       
		ssc_number_t *p_idealrot[4];
		ssc_number_t *p_poanom[4];
		ssc_number_t *p_poashaded[4];
		ssc_number_t *p_poaeffbeam[4];   
		ssc_number_t *p_poaeffdiff[4];   
		ssc_number_t *p_poaeff[4];  
		ssc_number_t *p_soiling[4];   
		ssc_number_t *p_shad[4];      	
		ssc_number_t *p_tcell[4];     
		ssc_number_t *p_modeff[4];    
		ssc_number_t *p_dcv[4];
		ssc_number_t *p_isc[4];
		ssc_number_t *p_voc[4];
		ssc_number_t *p_dcsubarray[4];
		
		// self-shading outputs- also sub-array specific
		ssc_number_t *p_ss_derate[4];
		ssc_number_t *p_linear_derate[4];
		ssc_number_t *p_ss_diffuse_derate[4];
		ssc_number_t *p_ss_reflected_derate[4];

		// Snow model specific
		ssc_number_t *p_snowloss[4];
		ssc_number_t *p_snowcoverage[4];


#ifdef SHADE_DB_OUTPUTS
		// ShadeDB validation
		ssc_number_t *p_shadedb_gpoa[4];
		ssc_number_t *p_shadedb_dpoa[4];
		ssc_number_t *p_shadedb_pv_cell_temp[4];
		ssc_number_t *p_shadedb_mods_per_str[4];
		ssc_number_t *p_shadedb_str_vmp_stc[4];
		ssc_number_t *p_shadedb_mppt_lo[4];
		ssc_number_t *p_shadedb_mppt_hi[4];
#endif
		// shading database fraction hourly output for year 1
		ssc_number_t *p_shadedb_shade_frac[4];


		// transformer loss outputs
		ssc_number_t *p_xfmr_nll_ts = allocate("xfmr_nll_ts", nrec);
		ssc_number_t *p_xfmr_ll_ts = allocate("xfmr_ll_ts", nrec);
		ssc_number_t *p_xfmr_loss_ts = allocate("xfmr_loss_ts", nrec);


		ssc_number_t xfmr_rating = ratedACOutput * util::watt_to_kilowatt; // W to kW
		ssc_number_t xfmr_ll_frac = as_number("transformer_load_loss") *0.01; // % to frac
		ssc_number_t xfmr_nll = as_number("transformer_no_load_loss") *0.01; // % to frac
		xfmr_nll *=  ts_hour * xfmr_rating; // kW

		// allocate output arrays for all subarray-specific parameters
		for (int nn=0;nn<4;nn++)
		{
			if ( sa[nn].enable )
			{
				std::string prefix = "subarray" + util::to_string( (int)(nn+1) ) + "_";
				p_aoi[nn]        = allocate( prefix+"aoi", nrec );
				p_surftilt[nn]   = allocate( prefix+"surf_tilt", nrec);
				p_surfazi[nn]    = allocate( prefix+"surf_azi", nrec);		
				p_rot[nn]        = allocate( prefix+"axisrot", nrec );
				p_idealrot[nn]   = allocate( prefix+"idealrot", nrec);
				p_poanom[nn]     = allocate( prefix+"poa_nom", nrec);
				p_poashaded[nn] = allocate(prefix + "poa_shaded", nrec);
				p_poaeffbeam[nn]    = allocate( prefix+"poa_eff_beam", nrec );
				p_poaeffdiff[nn]    = allocate( prefix+"poa_eff_diff", nrec );
				p_poaeff[nn]   = allocate( prefix+"poa_eff", nrec );		
				p_soiling[nn]    = allocate( prefix+"soiling_derate", nrec);
				p_shad[nn]       = allocate( prefix+"beam_shading_factor", nrec );
				p_tcell[nn]      = allocate( prefix+"celltemp", nrec );
				p_modeff[nn]     = allocate( prefix+"modeff", nrec );
				p_dcv[nn]        = allocate( prefix+"dc_voltage", nrec );
				p_voc[nn]        = allocate( prefix+"voc", nrec );
				p_isc[nn]        = allocate( prefix+"isc", nrec );
				p_dcsubarray[nn] = allocate( prefix+"dc_gross", nrec );
				p_linear_derate[nn] = allocate(prefix + "linear_derate", nrec);
				p_ss_derate[nn] = allocate(prefix + "ss_derate", nrec);
				p_ss_diffuse_derate[nn] = allocate(prefix + "ss_diffuse_derate", nrec);
				p_ss_reflected_derate[nn] = allocate(prefix + "ss_reflected_derate", nrec);

				if (en_snow_model){
					p_snowloss[nn] = allocate(prefix + "snow_loss", nrec);
					p_snowcoverage[nn] = allocate(prefix + "snow_coverage", nrec);
				}

#ifdef SHADE_DB_OUTPUTS
				// ShadeDB validation
				p_shadedb_gpoa[nn] = allocate("shadedb_" + prefix + "gpoa", nrec);
				p_shadedb_dpoa[nn] = allocate("shadedb_" + prefix + "dpoa", nrec);
				p_shadedb_pv_cell_temp[nn] = allocate("shadedb_" + prefix + "pv_cell_temp", nrec);
				p_shadedb_mods_per_str[nn] = allocate("shadedb_" + prefix + "mods_per_str", nrec);
				p_shadedb_str_vmp_stc[nn] = allocate("shadedb_" + prefix + "str_vmp_stc", nrec);
				p_shadedb_mppt_lo[nn] = allocate("shadedb_" + prefix + "mppt_lo", nrec);
				p_shadedb_mppt_hi[nn] = allocate("shadedb_" + prefix + "mppt_hi", nrec);

#endif
				p_shadedb_shade_frac[nn] = allocate("shadedb_" + prefix + "shade_frac", nrec);
			}
		}
		
		// outputs summed across all subarrays
		ssc_number_t *p_poanom_ts_total = allocate( "poa_nom", nrec );
		ssc_number_t *p_poabeamnom_ts_total = allocate( "poa_beam_nom", nrec );
		ssc_number_t *p_poashaded_ts_total = allocate("poa_shaded", nrec );
		ssc_number_t *p_poaeff_ts_total = allocate("poa_eff", nrec );
		ssc_number_t *p_poabeameff_ts_total = allocate("poa_beam_eff", nrec );
		
		ssc_number_t *p_dcsnowloss = allocate("dc_snow_loss", nrec);

		ssc_number_t *p_inv_dc_voltage = allocate( "inverter_dc_voltage", nrec * nyears );
		ssc_number_t *p_inveff = allocate("inv_eff", nrec);
		ssc_number_t *p_invcliploss = allocate( "inv_cliploss", nrec );
		ssc_number_t *p_invmpptloss = allocate("dc_invmppt_loss", nrec);
		
		ssc_number_t *p_invpsoloss = allocate( "inv_psoloss", nrec );
		ssc_number_t *p_invpntloss = allocate( "inv_pntloss", nrec );
		ssc_number_t *p_ac_wiringloss = allocate("ac_wiring_loss", nrec);

		// lifetime outputs
		ssc_number_t *p_dcpwr = allocate("dc_net", nrec * nyears);
		ssc_number_t *p_gen = allocate("gen", nrec * nyears);
		ssc_number_t *p_load_full = allocate("load_full", nrec* nyears);

		//dc hourly adjustment factors
		adjustment_factors dc_haf(this, "dc_adjust");
		if (!dc_haf.setup())
			throw exec_error("pvsamv1", "failed to setup DC adjustment factors: " + dc_haf.error());
		
		// hourly adjustment factors
		adjustment_factors haf(this, "adjust");
		if (!haf.setup())
			throw exec_error("pvsamv1", "failed to setup adjustment factors: " + haf.error());
		
		// setup battery model
		bool en_batt = as_boolean("en_batt");
		int batt_replacement_option = as_integer("batt_replacement_option");
		battstor batt(*this, en_batt, batt_replacement_option, nrec, ts_hour);
		
		int batt_dispatch = 0;
		int ac_or_dc = 0;
		bool look_ahead = false;
		bool look_behind = false;

		if (en_batt)
		{
			ac_or_dc = batt.batt_vars->batt_topology;
			batt_dispatch = batt.batt_vars->batt_dispatch;
			look_ahead = (batt_dispatch == dispatch_t::LOOK_AHEAD || batt_dispatch == dispatch_t::MAINTAIN_TARGET);
			look_behind = batt_dispatch == dispatch_t::LOOK_BEHIND;
		}
		
		// user replacement schedule
		size_t count_batt_replacement = 0;
		ssc_number_t *batt_replacement = 0;
		if (batt_replacement_option==2)
			batt_replacement = as_array("batt_replacement_schedule", &count_batt_replacement);
		
		double cur_load = 0.0;
		size_t nload = 0;
		ssc_number_t *p_load_in = 0;

		if ( is_assigned( "load" ) )
		{
			p_load_in = as_array( "load", &nload );
			if ( nload != nrec && nload != 8760 )
				throw exec_error("pvsamv1", "electric load profile must have same number of values as weather file, or 8760");
		}
		
		// for reporting status updates
		float percent_baseline = 0.;
		float percent_complete = 0.;
		size_t nreports = 50 * nyears;
		size_t ireport = 0;
		size_t ireplast = 0;
		size_t insteps = 3 * nyears * 8760;
		size_t irepfreq = insteps/nreports;

		size_t idx = 0;
		size_t hour = 0;

		// variables used to calculate loss diagram
		double annual_energy = 0, annual_ac_gross = 0, annual_ac_pre_avail = 0, dc_gross[4] = { 0, 0, 0, 0 }, annual_mppt_window_clipping = 0, annual_dc_adjust_loss = 0, annual_dc_lifetime_loss = 0, annual_ac_lifetime_loss = 0, annual_ac_battery_loss = 0, annual_xfmr_nll = 0, annual_xfmr_ll = 0, annual_xfmr_loss = 0;

		// Check if a POA model is used, if so load all POA data into the poaData struct
		if (radmode == POA_R || radmode == POA_P ){
			for (int nn = 0; nn < 4; nn++){
				if (!sa[nn].enable) continue;
				
				sa[nn].poa.poaAll.elev = hdr.elev;

				if( step_per_hour > 1) {
					sa[nn].poa.poaAll.stepScale = 'm';
					sa[nn].poa.poaAll.stepSize = 60.0 / step_per_hour;
				}

				sa[nn].poa.poaAll.POA = new double[ 8760*step_per_hour ];
				sa[nn].poa.poaAll.inc = new double[ 8760*step_per_hour ];
				sa[nn].poa.poaAll.tilt = new double[ 8760*step_per_hour ];
				sa[nn].poa.poaAll.zen = new double[ 8760*step_per_hour ];
				sa[nn].poa.poaAll.exTer = new double[ 8760*step_per_hour ];
					
				for (int h=0; h<8760; h++){
					for	(int m=0; m < step_per_hour; m++){
						int ii = h * step_per_hour + m;
						int month_idx = wf.month - 1;

						if (sa[nn].track_mode == 4) //timeseries tilt input
							sa[nn].tilt = sa[nn].monthly_tilt[month_idx]; //overwrite the tilt input with the current tilt to be used in calculations
						
						if (!wdprov->read( &wf ))
							throw exec_error("pvsamv1", "could not read data line " + util::to_string((int)(idx + 1)) + " in weather file while loading POA data");
						
						// save POA data
						if(wf.poa > 0)
							sa[nn].poa.poaAll.POA[ii] = wf.poa;
						else
							sa[nn].poa.poaAll.POA[ii] = -999;
						
						// Calculate incident angle
						double t_cur = wf.hour + wf.minute/60;
						
						// Calculate sunrise and sunset hours in local standard time for the current day
						double sun[9], angle[5];
						int tms[3];
						
						solarpos( wf.year, wf.month, wf.day, 12, 0.0, hdr.lat, hdr.lon, hdr.tz, sun );
					
						double t_sunrise = sun[4];
						double t_sunset = sun[5];
					
						if ( t_cur >= t_sunrise - ts_hour/2.0
							&& t_cur < t_sunrise + ts_hour/2.0 )
						{
							// time step encompasses the sunrise
							double t_calc = (t_sunrise + (t_cur+ts_hour/2.0))/2.0; // midpoint of sunrise and end of timestep
							int hr_calc = (int)t_calc;
							double min_calc = (t_calc-hr_calc)*60.0;
					
							tms[0] = hr_calc;
							tms[1] = (int)min_calc;
									
							solarpos( wf.year, wf.month, wf.day, hr_calc, min_calc, hdr.lat, hdr.lon, hdr.tz, sun );
					
							tms[2] = 2;				
						}
						else if (t_cur > t_sunset - ts_hour/2.0
							&& t_cur <= t_sunset + ts_hour/2.0 )
						{
							// timestep encompasses the sunset
							double t_calc = ( (t_cur-ts_hour/2.0) + t_sunset )/2.0; // midpoint of beginning of timestep and sunset
							int hr_calc = (int)t_calc;
							double min_calc = (t_calc-hr_calc)*60.0;
					
							tms[0] = hr_calc;
							tms[1] = (int)min_calc;
									
							solarpos( wf.year, wf.month, wf.day, hr_calc, min_calc, hdr.lat, hdr.lon, hdr.tz, sun );
					
							tms[2] = 3;
						}
						else if (t_cur >= t_sunrise && t_cur <= t_sunset)
						{
							// timestep is not sunrise nor sunset, but sun is up  (calculate position at provided t_cur)			
							tms[0] = wf.hour;
							tms[1] = (int)wf.minute;
							solarpos( wf.year, wf.month, wf.day, wf.hour, wf.minute, hdr.lat, hdr.lon, hdr.tz, sun );
							tms[2] = 1;
						}
						else
						{	
							// sun is down, assign sundown values
							sun[0] = -999; //avoid returning a junk azimuth angle
							sun[1] = -999; //avoid returning a junk zenith angle
							sun[2] = -999; //avoid returning a junk elevation angle
							tms[0] = -1;
							tms[1] = -1;
							tms[2] = 0;
						}


						if( tms[2] > 0){
							incidence( sa[nn].track_mode, sa[nn].tilt, sa[nn].azimuth, sa[nn].rotlim, sun[1], sun[0], sa[nn].backtrack, sa[nn].gcr, angle );
						} else {
							angle[0] = -999;
							angle[1] = -999;
							angle[2] = -999;
							angle[3] = -999;
							angle[4] = -999;	
						}


						sa[nn].poa.poaAll.inc[ii] = angle[ 0 ];
						sa[nn].poa.poaAll.tilt[ii] = angle[ 1 ];
						sa[nn].poa.poaAll.zen[ii] = sun[ 1 ];
						sa[nn].poa.poaAll.exTer[ii] = sun[ 8 ];

					}
				}
				wdprov->rewind();
			}
		}
		/* *********************************************************************************************
		PV DC calculation
		*********************************************************************************************** */
		for (size_t iyear = 0; iyear < nyears; iyear++)
		{
			for (hour = 0; hour < 8760; hour++)
			{
				// report progress updates to the caller	
				ireport++;
				if (ireport - ireplast > irepfreq)
				{
					percent_complete = percent_baseline + 100.0f *(float)(hour + iyear * 8760) / (float)(insteps);
					if (!update("", percent_complete))
						throw exec_error("pvsamv1", "simulation canceled at hour " + util::to_string(hour + 1.0) + " in year " + util::to_string((int)iyear + 1) + "in dc loop");
					ireplast = ireport;
				}

				// only hourly electric load, even
				// if PV simulation is subhourly.  load is assumed constant over the hour.
				// if no load profile supplied, load = 0
				if (p_load_in != 0 && nload == 8760)
					cur_load = p_load_in[hour];

				for (size_t jj = 0; jj < step_per_hour; jj++)
				{

					// electric load is subhourly
					// if no load profile supplied, load = 0
					if (p_load_in != 0 && nload == nrec)
						cur_load = p_load_in[hour*step_per_hour + jj];

					// log cur_load to check both hourly and sub hourly load data
					// load data over entrie lifetime period not currently supported.
					//					log(util::format("year=%d, hour=%d, step per hour=%d, load=%g",
					//						iyear, hour, jj, cur_load), SSC_WARNING, (float)idx);
					p_load_full[idx] = cur_load;

					if (!wdprov->read(&wf))
						throw exec_error("pvsamv1", "could not read data line " + util::to_string((int)(idx + 1)) + " in weather file");

					//update POA data structure indicies if radmode is POA model is enabled
					if (radmode == POA_R || radmode == POA_P){
						for (int nn = 0; nn < 4; nn++){
							if (!sa[nn].enable) continue;

							sa[nn].poa.poaAll.tDew = wf.tdew;
							sa[nn].poa.poaAll.i = idx;
							if (jj == 0 && wf.hour == 0) {
								sa[nn].poa.poaAll.dayStart = idx;
								sa[nn].poa.poaAll.doy += 1;
							}

						}
					}

					double solazi = 0, solzen = 0, solalt = 0;
					int sunup = 0;
					double dcpwr_net = 0.0, dc_string_voltage = 0.0;
					double alb = 0.2;

					// accumulators for radiation power (W) over this 
					// timestep from each subarray
					double ts_accum_poa_nom = 0.0;
					double ts_accum_poa_beam_nom = 0.0;
					double ts_accum_poa_shaded = 0.0;
					double ts_accum_poa_eff = 0.0;
					double ts_accum_poa_beam_eff = 0.0;

					int month_idx = wf.month - 1;

					if (use_wf_alb && std::isfinite(wf.alb) && wf.alb > 0 && wf.alb < 1)
						alb = wf.alb;
					else if (month_idx >= 0 && month_idx < 12)
						alb = alb_array[month_idx];
					else
						throw exec_error("pvsamv1",
						util::format("Error retrieving albedo value: Invalid month in weather file or invalid albedo value in weather file"));

					// calculate incident irradiance on each subarray
					for (int nn = 0; nn < 4; nn++)
					{
						if (!sa[nn].enable
							|| sa[nn].nstrings < 1)
							continue; // skip disabled subarrays
#define IRRMAX 1500
						// Sev 2015-09-15 Update check for bad irradiance values

						// Check for missing data
						// *note this method may not work for all compilers (lookin at you, MACs!)
						if ((wf.gh != wf.gh) && (radmode == DN_GH || radmode == GH_DF)){
							log(util::format("missing global irradiance %lg W/m2 at time [y:%d m:%d d:%d h:%d], exiting",
								wf.gh, wf.year, wf.month, wf.day, wf.hour), SSC_ERROR, (float)idx);
							return;
						}
						if ((wf.dn != wf.dn) && (radmode == DN_DF || radmode == DN_GH)){
							log(util::format("missing beam irradiance %lg W/m2 at time [y:%d m:%d d:%d h:%d], exiting",
								wf.dn, wf.year, wf.month, wf.day, wf.hour), SSC_ERROR, (float)idx);
							return;
						}
						if ((wf.df != wf.df) && (radmode == DN_DF || radmode == GH_DF)){
							log(util::format("missing diffuse irradiance %lg W/m2 at time [y:%d m:%d d:%d h:%d], exiting",
								wf.df, wf.year, wf.month, wf.day, wf.hour), SSC_ERROR, (float)idx);
							return;
						}
						if ((wf.poa != wf.poa) && (radmode == POA_R || radmode == POA_P)){
							log(util::format("missing POA irradiance %lg W/m2 at time [y:%d m:%d d:%d h:%d], exiting",
								wf.poa, wf.year, wf.month, wf.day, wf.hour), SSC_ERROR, (float)idx);
							return;
						}

						// Check for bad data
						if ((wf.gh < 0 || wf.gh > IRRMAX) && (radmode == DN_GH || radmode == GH_DF))
						{
							log(util::format("out of range global irradiance %lg W/m2 at time [y:%d m:%d d:%d h:%d], set to zero",
								wf.gh, wf.year, wf.month, wf.day, wf.hour), SSC_WARNING, (float)idx);
							wf.gh = 0;
						}
						if ((wf.dn < 0 || wf.dn > IRRMAX) && (radmode == DN_DF || radmode == DN_GH))
						{
							log(util::format("out of range beam irradiance %lg W/m2 at time [y:%d m:%d d:%d h:%d], set to zero",
								wf.dn, wf.year, wf.month, wf.day, wf.hour), SSC_WARNING, (float)idx);
							wf.dn = 0;
						}
						if ((wf.df < 0 || wf.df > IRRMAX) && (radmode == DN_DF || radmode == GH_DF))
						{
							log(util::format("out of range diffuse irradiance %lg W/m2 at time [y:%d m:%d d:%d h:%d], set to zero",
								wf.df, wf.year, wf.month, wf.day, wf.hour), SSC_WARNING, (float)idx);
							wf.df = 0;
						}
						if ((wf.poa < 0 || wf.poa > IRRMAX) && (radmode == POA_R || radmode == POA_P))
						{
							log(util::format("out of range POA irradiance %lg W/m2 at time [y:%d m:%d d:%d h:%d], set to zero",
								wf.poa, wf.year, wf.month, wf.day, wf.hour), SSC_WARNING, (float)idx);
							wf.poa = 0;
						}

						irrad irr;
						irr.set_time(wf.year, wf.month, wf.day, wf.hour, wf.minute,
							instantaneous ? IRRADPROC_NO_INTERPOLATE_SUNRISE_SUNSET : ts_hour);
						irr.set_location(hdr.lat, hdr.lon, hdr.tz);

						irr.set_sky_model(skymodel, alb);
						if (radmode == DN_DF) irr.set_beam_diffuse(wf.dn, wf.df);
						else if (radmode == DN_GH) irr.set_global_beam(wf.gh, wf.dn);
						else if (radmode == GH_DF) irr.set_global_diffuse(wf.gh, wf.df);
						else if (radmode == POA_R) irr.set_poa_reference(wf.poa, &sa[nn].poa.poaAll);
						else if (radmode == POA_P) irr.set_poa_pyranometer(wf.poa, &sa[nn].poa.poaAll);

						if (sa[nn].track_mode == 4) //timeseries tilt input
							sa[nn].tilt = sa[nn].monthly_tilt[month_idx]; //overwrite the tilt input with the current tilt to be used in calculations

						irr.set_surface(sa[nn].track_mode,
							sa[nn].tilt,
							sa[nn].azimuth,
							sa[nn].rotlim,
							sa[nn].backtrack == 1, // mode 1 is backtracking enabled
							sa[nn].gcr);

						int code = irr.calc();

						if (code != 0)
							throw exec_error("pvsamv1",
							util::format("failed to calculate irradiance incident on surface (POA) %d (code: %d) [y:%d m:%d d:%d h:%d]",
							nn + 1, code, wf.year, wf.month, wf.day, wf.hour));

						// p_irrad_calc is only weather file records long...
						if (iyear == 0)
						{
							if (radmode == POA_R || radmode == POA_P) {
								double gh_temp, df_temp, dn_temp;
								gh_temp = df_temp = dn_temp = 0;
								irr.get_irrad(&gh_temp, &dn_temp, &df_temp);
								p_irrad_calc[1][idx] = (ssc_number_t)df_temp;
								p_irrad_calc[2][idx] = (ssc_number_t)dn_temp;
							}
						}
						// beam, skydiff, and grounddiff IN THE PLANE OF ARRAY
						double ibeam, iskydiff, ignddiff;
						double ipoa; // Container for direct POA measurements
						double aoi, stilt, sazi, rot, btd;


						// Ensure that the usePOAFromWF flag is false unless a reference cell has been used. 
						//  This will later get forced to false if any shading has been applied (in any scenario)
						//  also this will also be forced to false if using the cec mcsp thermal model OR if using the spe module model with a diffuse util. factor < 1.0
						sa[nn].poa.usePOAFromWF = false;
						if (radmode == POA_R){
							ipoa = wf.poa;
							sa[nn].poa.usePOAFromWF = true;
						}
						else if (radmode == POA_P){
							ipoa = wf.poa;
						}

						if (speForceNoPOA && (radmode == POA_R || radmode == POA_P)){  // only will be true if using a poa model AND spe module model AND spe_fp is < 1
							sa[nn].poa.usePOAFromWF = false;
							if (idx == 0)
								log("The combination of POA irradiance as in input, single point efficiency module model, and module diffuse utilization factor less than one means that SAM must use a POA decomposition model to calculate the incident diffuse irradiance", SSC_WARNING);
						}

						if (mcspForceNoPOA && (radmode == POA_R || radmode == POA_P)){
							sa[nn].poa.usePOAFromWF = false;
							if (idx == 0)
								log("The combination of POA irradiance as input and heat transfer method for cell temperature means that SAM must use a POA decomposition model to calculate the beam irradiance required by the cell temperature model", SSC_WARNING);
						}


						// Get Incident angles and irradiances

						irr.get_sun(&solazi, &solzen, &solalt, 0, 0, 0, &sunup, 0, 0, 0);
						irr.get_angles(&aoi, &stilt, &sazi, &rot, &btd);
						irr.get_poa(&ibeam, &iskydiff, &ignddiff, 0, 0, 0);

						if (iyear == 0)
							p_sunpos_hour[idx] = (ssc_number_t)irr.get_sunpos_calc_hour();

						// save weather file beam, diffuse, and global for output and for use later in pvsamv1- year 1 only
						/*jmf 2016: these calculations are currently redundant with calculations in irrad.calc() because ibeam and idiff in that function are DNI and DHI, **NOT** in the plane of array
						we'll have to fix this redundancy in the pvsamv1 rewrite. it will require allowing irradproc to report the errors below
						and deciding what to do if the weather file DOES contain the third component but it's not being used in the calculations.*/
						if (iyear == 0)
						{
							// Apply all irradiance component data from weather file (if it exists)
							p_wfpoa[idx] = (ssc_number_t)wf.poa;
							p_beam[idx] = (ssc_number_t)wf.dn;
							p_glob[idx] = (ssc_number_t)(wf.gh);
							p_diff[idx] = (ssc_number_t)(wf.df);

							// calculate beam if global & diffuse are selected as inputs
							if (radmode == GH_DF)
							{
								p_irrad_calc[2][idx] = (ssc_number_t)((wf.gh - wf.df) / cos(solzen*3.1415926 / 180));
								if (p_irrad_calc[2][idx] < -1)
								{
									log(util::format("SAM calculated negative direct normal irradiance %lg W/m2 at time [y:%d m:%d d:%d h:%d], set to zero.",
										p_irrad_calc[2][idx], wf.year, wf.month, wf.day, wf.hour), SSC_WARNING, (float)idx);
									p_irrad_calc[2][idx] = 0;
								}
							}

							// calculate global if beam & diffuse are selected as inputs
							if (radmode == DN_DF)
							{
								p_irrad_calc[0][idx] = (ssc_number_t)(wf.df + wf.dn * cos(solzen*3.1415926 / 180));
								if (p_irrad_calc[0][idx] < -1)
								{
									log(util::format("SAM calculated negative global horizontal irradiance %lg W/m2 at time [y:%d m:%d d:%d h:%d], set to zero.",
										p_irrad_calc[0][idx], wf.year, wf.month, wf.day, wf.hour), SSC_WARNING, (float)idx);
									p_irrad_calc[0][idx] = 0;
								}
							}

							// calculate diffuse if total & beam are selected as inputs
							if (radmode == DN_GH)
							{
								p_irrad_calc[1][idx] = (ssc_number_t)(wf.gh - wf.dn * cos(solzen*3.1415926 / 180));
								if (p_irrad_calc[1][idx] < -1)
								{
									log(util::format("SAM calculated negative diffuse horizontal irradiance %lg W/m2 at time [y:%d m:%d d:%d h:%d], set to zero.",
										p_irrad_calc[1][idx], wf.year, wf.month, wf.day, wf.hour), SSC_WARNING, (float)idx);
									p_irrad_calc[1][idx] = 0;
								}
							}
						}

						// record sub-array plane of array output before computing shading and soiling
						if (iyear == 0)
						{
							if (radmode != POA_R)
								p_poanom[nn][idx] = (ssc_number_t)((ibeam + iskydiff + ignddiff));
							else
								p_poanom[nn][idx] = (ssc_number_t)((ipoa));
						}

						// note: ibeam, iskydiff, ignddiff are in units of W/m2

						// record sub-array contribution to total POA power for this time step  (W)
						if (radmode != POA_R)
							ts_accum_poa_nom += (ibeam + iskydiff + ignddiff) * ref_area_m2 * modules_per_string * sa[nn].nstrings;
						else
							ts_accum_poa_nom += (ipoa)* ref_area_m2 * modules_per_string * sa[nn].nstrings;

						// record sub-array contribution to total POA beam power for this time step (W)
						ts_accum_poa_beam_nom += ibeam * ref_area_m2 * modules_per_string * sa[nn].nstrings;

						// for non-linear shading from shading database
						if (sa[nn].shad.use_shade_db())
						{
							double shadedb_gpoa = ibeam + iskydiff + ignddiff;
							double shadedb_dpoa = iskydiff + ignddiff;

							// update cell temperature - unshaded value per Sara 1/25/16
							double tcell = wf.tdry;
							if (sunup > 0)
							{
								// calculate cell temperature using selected temperature model
								pvinput_t in(ibeam, iskydiff, ignddiff, ipoa,
									wf.tdry, wf.tdew, wf.wspd, wf.wdir, wf.pres,
									solzen, aoi, hdr.elev,
									stilt, sazi,
									((double)wf.hour) + wf.minute / 60.0,
									radmode, sa[nn].poa.usePOAFromWF);
								// voltage set to -1 for max power
								(*celltemp_model)(in, *module_model, -1.0, tcell);
							}
							double shadedb_str_vmp_stc = modules_per_string * ssVmp;
							double shadedb_mppt_lo = V_mppt_lo_1module * modules_per_string;;
							double shadedb_mppt_hi = V_mppt_hi_1module * modules_per_string;;

							if (!sa[nn].shad.fbeam_shade_db(p_shade_db, hour, solalt, solazi, jj, step_per_hour, shadedb_gpoa, shadedb_dpoa, tcell, modules_per_string, shadedb_str_vmp_stc, shadedb_mppt_lo, shadedb_mppt_hi))
							{
								throw exec_error("pvsamv1", util::format("Error calculating shading factor for subarray %d", nn));
							}
							if (iyear == 0)
							{
#ifdef SHADE_DB_OUTPUTS
								p_shadedb_gpoa[nn][idx] = (ssc_number_t)shadedb_gpoa;
								p_shadedb_dpoa[nn][idx] = (ssc_number_t)shadedb_dpoa;
								p_shadedb_pv_cell_temp[nn][idx] = (ssc_number_t)tcell;
								p_shadedb_mods_per_str[nn][idx] = (ssc_number_t)modules_per_string;
								p_shadedb_str_vmp_stc[nn][idx] = (ssc_number_t)shadedb_str_vmp_stc;
								p_shadedb_mppt_lo[nn][idx] = (ssc_number_t)shadedb_mppt_lo;
								p_shadedb_mppt_hi[nn][idx] = (ssc_number_t)shadedb_mppt_hi;
								log("shade db hour " + util::to_string((int)hour) +"\n" + p_shade_db->get_warning());
#endif
								// fraction shaded for comparison
								p_shadedb_shade_frac[nn][idx] = (ssc_number_t)(sa[nn].shad.dc_shade_factor());
							}
						}
						else
						{
							if (!sa[nn].shad.fbeam(hour, solalt, solazi, jj, step_per_hour))
							{
								throw exec_error("pvsamv1", util::format("Error calculating shading factor for subarray %d", nn));
							}
						}

						// apply hourly shading factors to beam (if none enabled, factors are 1.0) 
						// shj 3/21/16 - update to handle negative shading loss
						if (sa[nn].shad.beam_shade_factor() != 1.0){
							//							if (sa[nn].shad.beam_shade_factor() < 1.0){
							// Sara 1/25/16 - shading database derate applied to dc only
							// shading loss applied to beam if not from shading database
							ibeam *= sa[nn].shad.beam_shade_factor();
							if (radmode == POA_R || radmode == POA_P){
								sa[nn].poa.usePOAFromWF = false;
								if (sa[nn].poa.poaShadWarningCount == 0){
									log(util::format("Combining POA irradiance as input with the beam shading losses at time [y:%d m:%d d:%d h:%d] forces SAM to use a POA decomposition model to calculate incident beam irradiance",
										wf.year, wf.month, wf.day, wf.hour), SSC_WARNING, (float)idx);
								}
								else{
									log(util::format("Combining POA irradiance as input with the beam shading losses at time [y:%d m:%d d:%d h:%d] forces SAM to use a POA decomposition model to calculate incident beam irradiance",
										wf.year, wf.month, wf.day, wf.hour), SSC_NOTICE, (float)idx);
								}
								sa[nn].poa.poaShadWarningCount++;
							}
						}

						// apply sky diffuse shading factor (specified as constant, nominally 1.0 if disabled in UI)
						if (sa[nn].shad.fdiff() < 1.0){
							iskydiff *= sa[nn].shad.fdiff();
							if (radmode == POA_R || radmode == POA_P){
								if (idx == 0)
									log("Combining POA irradiance as input with the diffuse shading losses forces SAM to use a POA decomposition model to calculate incident diffuse irradiance", SSC_WARNING);
								sa[nn].poa.usePOAFromWF = false;
							}
						}

						double beam_shading_factor = sa[nn].shad.beam_shade_factor();

						//self-shading calculations
						if (((sa[nn].track_mode == 0 || sa[nn].track_mode == 4) && (sa[nn].shade_mode == 1 || sa[nn].shade_mode == 2)) //fixed tilt or timeseries tilt, self-shading (linear or non-linear) OR
							|| (sa[nn].track_mode == 1 && (sa[nn].shade_mode == 1 || sa[nn].shade_mode == 2) && sa[nn].backtrack == 0)) //one-axis tracking, self-shading, not backtracking
						{

							if (radmode == POA_R || radmode == POA_P){
								if (idx == 0)
									log("Combining POA irradiance as input with self shading forces SAM to employ a POA decomposition model to calculate incident beam irradiance", SSC_WARNING);
								sa[nn].poa.usePOAFromWF = false;
							}

							// info to be passed to self-shading function
							bool trackbool = (sa[nn].track_mode == 1);	// 0 for fixed tilt and timeseries tilt, 1 for one-axis
							bool linear = (sa[nn].shade_mode == 2); //0 for full self-shading, 1 for linear self-shading

							//geometric fraction of the array that is shaded for one-axis trackers.
							//USES A DIFFERENT FUNCTION THAN THE SELF-SHADING BECAUSE SS IS MEANT FOR FIXED ONLY. SHADE_FRACTION_1X IS FOR ONE-AXIS TRACKERS ONLY.
							//used in the non-linear self-shading calculator for one-axis tracking only
							double shad1xf = 0;
							if (trackbool)
								shad1xf = shade_fraction_1x(solazi, solzen, sa[nn].tilt, sa[nn].azimuth, sa[nn].gcr, rot);

							//execute self-shading calculations
							ssc_number_t beam_to_use; //some self-shading calculations require DNI, NOT ibeam (beam in POA). Need to know whether to use DNI from wf or calculated, depending on radmode
							if (radmode == DN_DF || radmode == DN_GH) beam_to_use = wf.dn; 
							else beam_to_use = p_irrad_calc[2][idx];

							if (linear && trackbool) //one-axis linear
							{
								ibeam *= (1 - shad1xf); //derate beam irradiance linearly by the geometric shading fraction calculated above per Chris Deline 2/10/16
								beam_shading_factor *= (1 - shad1xf);
								if (iyear == 0)
								{
									p_ss_derate[nn][idx] = (ssc_number_t)1;
									p_linear_derate[nn][idx] = (ssc_number_t)(1 - shad1xf);
									p_ss_diffuse_derate[nn][idx] = (ssc_number_t)1; //no diffuse derate for linear shading
									p_ss_reflected_derate[nn][idx] = (ssc_number_t)1; //no reflected derate for linear shading
								}
							}

							else if (ss_exec(sa[nn].sscalc, stilt, sazi, solzen, solazi, beam_to_use, ibeam, (iskydiff + ignddiff), alb, trackbool, linear, shad1xf, sa[nn].ssout))
							{
								if (linear) //fixed tilt linear
								{
									ibeam *= (1 - sa[nn].ssout.m_shade_frac_fixed);
									beam_shading_factor *= (1 - sa[nn].ssout.m_shade_frac_fixed);
									if (iyear == 0)
									{
										p_ss_derate[nn][idx] = (ssc_number_t)1;
										p_linear_derate[nn][idx] = (ssc_number_t)(1 - sa[nn].ssout.m_shade_frac_fixed);
										p_ss_diffuse_derate[nn][idx] = (ssc_number_t)1; //no diffuse derate for linear shading
										p_ss_reflected_derate[nn][idx] = (ssc_number_t)1; //no reflected derate for linear shading
									}
								}
								else //non-linear: fixed tilt AND one-axis
								{
									if (iyear == 0)
									{
										p_ss_diffuse_derate[nn][idx] = (ssc_number_t)sa[nn].ssout.m_diffuse_derate;
										p_ss_reflected_derate[nn][idx] = (ssc_number_t)sa[nn].ssout.m_reflected_derate;
										p_ss_derate[nn][idx] = (ssc_number_t)sa[nn].ssout.m_dc_derate;
										p_linear_derate[nn][idx] = (ssc_number_t)1;
									}

									// Sky diffuse and ground-reflected diffuse are derated according to C. Deline's algorithm
									iskydiff *= sa[nn].ssout.m_diffuse_derate;
									ignddiff *= sa[nn].ssout.m_reflected_derate;
									// Beam is not derated- all beam derate effects (linear and non-linear) are taken into account in the nonlinear_dc_shading_derate
									sa[nn].poa.nonlinear_dc_shading_derate = sa[nn].ssout.m_dc_derate;
								}
							}
							else
								throw exec_error("pvsamv1", util::format("Self-shading calculation failed at %d", (int)idx));
						}

						double poashad = (radmode == POA_R) ? ipoa : (ibeam + iskydiff + ignddiff);

						// determine sub-array contribution to total shaded plane of array for this hour
						ts_accum_poa_shaded += poashad * ref_area_m2 * modules_per_string * sa[nn].nstrings;


						// apply soiling derate to all components of irradiance
						double soiling_factor = 1.0;
						if (month_idx >= 0 && month_idx < 12)
						{
							soiling_factor = sa[nn].soiling[month_idx];
							ibeam *= soiling_factor;
							iskydiff *= soiling_factor;
							ignddiff *= soiling_factor;
							if (radmode == POA_R || radmode == POA_P){
								ipoa *= soiling_factor;
								if (soiling_factor < 1 && idx == 0)
									log("Soiling may already be accounted for in the input POA data. Please confirm that the input data does not contain soiling effects, or remove the additional losses on the Losses page.", SSC_WARNING);
							}
							beam_shading_factor *= soiling_factor;
						}

						if (iyear == 0)
						{
							// save sub-array level outputs			
							p_poashaded[nn][idx] = (ssc_number_t)poashad;
							p_poaeffbeam[nn][idx] = (ssc_number_t)ibeam;
							p_poaeffdiff[nn][idx] = (ssc_number_t)(iskydiff + ignddiff);
							p_poaeff[nn][idx] = (ssc_number_t)(radmode == POA_R) ? ipoa : (ibeam + iskydiff + ignddiff);
							p_shad[nn][idx] = (ssc_number_t)beam_shading_factor;
							p_rot[nn][idx] = (ssc_number_t)rot;
							p_idealrot[nn][idx] = (ssc_number_t)(rot - btd);
							p_aoi[nn][idx] = (ssc_number_t)aoi;
							p_surftilt[nn][idx] = (ssc_number_t)stilt;
							p_surfazi[nn][idx] = (ssc_number_t)sazi;
							p_soiling[nn][idx] = (ssc_number_t)soiling_factor;


						}

						// accumulate incident total radiation (W) in this timestep (all subarrays)
						ts_accum_poa_eff += ((radmode == POA_R) ? ipoa : (ibeam + iskydiff + ignddiff)) * ref_area_m2 * modules_per_string * sa[nn].nstrings;
						ts_accum_poa_beam_eff += ibeam * ref_area_m2 * modules_per_string * sa[nn].nstrings;

						// save the required irradiance inputs on array plane for the module output calculations.
						sa[nn].poa.ibeam = ibeam;
						sa[nn].poa.iskydiff = iskydiff;
						sa[nn].poa.ignddiff = ignddiff;
						sa[nn].poa.ipoa = ipoa;
						sa[nn].poa.aoi = aoi;
						sa[nn].poa.sunup = sunup;
						sa[nn].poa.stilt = stilt;
						sa[nn].poa.sazi = sazi;

					}

					// compute dc power output of one module in each subarray
					double module_voltage = -1;

					if (enable_mismatch_vmax_calc)
					{
						if (num_subarrays <= 1)
							throw exec_error("pvsamv1", "Subarray voltage mismatch calculation requires more than one subarray. Please check your inputs.");
						double vmax = module_model->VocRef()*1.3; // maximum voltage
						double vmin = 0.4 * vmax; // minimum voltage
						const int NP = 100;
						double V[NP], I[NP], P[NP];
						double Pmax = 0;
						// sweep voltage, calculating current for each subarray module, and adding
						for (int i = 0; i < NP; i++)
						{
							V[i] = vmin + (vmax - vmin)*i / ((double)NP);
							I[i] = 0;
							for (int nn = 0; nn < 4; nn++)
							{
								if (!sa[nn].enable || sa[nn].nstrings < 1) continue; // skip disabled subarrays

								pvinput_t in(sa[nn].poa.ibeam, sa[nn].poa.iskydiff, sa[nn].poa.ignddiff, sa[nn].poa.ipoa,
									wf.tdry, wf.tdew, wf.wspd, wf.wdir, wf.pres,
									solzen, sa[nn].poa.aoi, hdr.elev,
									sa[nn].poa.stilt, sa[nn].poa.sazi,
									((double)wf.hour) + wf.minute / 60.0,
									radmode, sa[nn].poa.usePOAFromWF);
								pvoutput_t out(0, 0, 0, 0, 0, 0, 0);
								if (sa[nn].poa.sunup > 0)
								{
									double tcell = wf.tdry;
									// calculate cell temperature using selected temperature model
									(*celltemp_model)(in, *module_model, V[i], tcell);
									// calculate module power output using conversion model previously specified
									(*module_model)(in, tcell, V[i], out);
								}
								I[i] += out.Current;
							}

							P[i] = V[i] * I[i];
							if (P[i] > Pmax)
							{
								Pmax = P[i];
								module_voltage = V[i];
							}
						}

						if (clip_mppt_window)
						{
							if (module_voltage < V_mppt_lo_1module) module_voltage = V_mppt_lo_1module;
							if (module_voltage > V_mppt_hi_1module) module_voltage = V_mppt_hi_1module;
						}

					}


					//  at this point we have 
					// a array maximum power module voltage

					// for averaging voltage in the case that mismatch calcs are disabled.
					int n_voltage_values = 0;
					double voltage_sum = 0.0;
					double mppt_clip_window = 0;

					for (int nn = 0; nn < 4; nn++)
					{
						if (!sa[nn].enable
							|| sa[nn].nstrings < 1)
							continue; // skip disabled subarrays

						pvinput_t in(sa[nn].poa.ibeam, sa[nn].poa.iskydiff, sa[nn].poa.ignddiff, sa[nn].poa.ipoa,
							wf.tdry, wf.tdew, wf.wspd, wf.wdir, wf.pres,
							solzen, sa[nn].poa.aoi, hdr.elev,
							sa[nn].poa.stilt, sa[nn].poa.sazi,
							((double)wf.hour) + wf.minute / 60.0,
							radmode, sa[nn].poa.usePOAFromWF);
						pvoutput_t out(0, 0, 0, 0, 0, 0, 0);

						double tcell = wf.tdry;
						if (sa[nn].poa.sunup > 0)
						{
							// calculate cell temperature using selected temperature model
							// calculate module power output using conversion model previously specified
							(*celltemp_model)(in, *module_model, module_voltage, tcell);
							(*module_model)(in, tcell, module_voltage, out);

							// if mismatch was enabled, the module voltage already was clipped to the inverter MPPT range if appropriate
							// here, if the module was running at mppt by default, and mppt window clipping is possible, recalculate
							// module power output to determine actual module power using the voltage window of the inverter
							if (iyear == 0) mppt_clip_window = out.Power;
							if (!enable_mismatch_vmax_calc && clip_mppt_window)
							{
								if (out.Voltage < V_mppt_lo_1module)
								{
									module_voltage = V_mppt_lo_1module;
									(*celltemp_model)(in, *module_model, module_voltage, tcell);
									(*module_model)(in, tcell, module_voltage, out);
								}
								else if (out.Voltage > V_mppt_hi_1module)
								{
									module_voltage = V_mppt_hi_1module;
									(*celltemp_model)(in, *module_model, module_voltage, tcell);
									(*module_model)(in, tcell, module_voltage, out);
								}
								// MPPT loss
							}
							if (iyear == 0)	mppt_clip_window -= out.Power;
						}

						if (out.Voltage > module_model->VocRef()*1.3)
							log(util::format("Module voltage is unrealistically high (exceeds 1.3*VocRef) at [mdhm: %d %d %d %lg]: %lg V\n", wf.month, wf.day, wf.hour, wf.minute, out.Voltage), SSC_NOTICE);

						if (!std::isfinite(out.Power))
						{
							out.Power = 0;
							out.Voltage = 0;
							out.Current = 0;
							out.Efficiency = 0;
							out.CellTemp = tcell;
							log(util::format("Non-finite power output calculated at [mdhm: %d %d %d %lg], set to zero.\n"
								"could be due to anomolous equation behavior at very low irradiances (poa: %lg W/m2)",
								wf.month, wf.day, wf.hour, wf.minute, sa[nn].poa.ipoa), SSC_NOTICE);
						}

						// save DC module outputs for this subarray
						sa[nn].module.dcpwr = out.Power;
						sa[nn].module.dceff = out.Efficiency * 100;
						sa[nn].module.dcv = out.Voltage;
						sa[nn].module.tcell = out.CellTemp;
						sa[nn].module.isc = out.Isc_oper;
						sa[nn].module.voc = out.Voc_oper;

						voltage_sum += out.Voltage;
						n_voltage_values++;
					}


					if (enable_mismatch_vmax_calc && num_subarrays > 1)
						dc_string_voltage = module_voltage * modules_per_string;
					else // when mismatch calculation is disabled and subarrays are enabled, simply average the voltages together for the inverter input
						dc_string_voltage = voltage_sum / n_voltage_values * modules_per_string;

					// sum up all DC power from the whole array
					for (int nn = 0; nn < 4; nn++)
					{
						if (!sa[nn].enable
							|| sa[nn].nstrings < 1)
							continue; // skip disabled subarrays

						// apply self-shading derate (by default it is 1.0 if disbled)
						sa[nn].module.dcpwr *= sa[nn].poa.nonlinear_dc_shading_derate;

						if (iyear == 0) mppt_clip_window *= sa[nn].poa.nonlinear_dc_shading_derate;

						// scale power and voltage to array dimensions
						sa[nn].module.dcpwr *= modules_per_string*sa[nn].nstrings;
						if (iyear == 0) mppt_clip_window *= modules_per_string*sa[nn].nstrings;

						// Calculate and apply snow coverage losses if activated
						if (en_snow_model)
						{
							float smLoss = 0.0f;

							if (!sa[nn].sm.getLoss((float)(sa[nn].poa.ibeam + sa[nn].poa.iskydiff + sa[nn].poa.ignddiff),
								(float)sa[nn].poa.stilt, (float)wf.wspd, (float)wf.tdry, (float)wf.snow, sunup, 1.0f / step_per_hour, &smLoss))
							{
								if (!sa[nn].sm.good)
									throw exec_error("pvsamv1", sa[nn].sm.msg);
							}

							if (iyear == 0)
							{
								p_snowloss[nn][idx] = (ssc_number_t)(util::watt_to_kilowatt*sa[nn].module.dcpwr*smLoss);
								p_dcsnowloss[idx] += (ssc_number_t)(util::watt_to_kilowatt*sa[nn].module.dcpwr*smLoss);
								p_snowcoverage[nn][idx] = (ssc_number_t)(sa[nn].sm.coverage);
								annual_snow_loss += (ssc_number_t)(util::watt_to_kilowatt*sa[nn].module.dcpwr*smLoss);
							}

							sa[nn].module.dcpwr *= (1 - smLoss);
						}

						// apply pre-inverter power derate
						// apply yearly degradation as necessary

						if (iyear == 0)
						{
							dc_gross[nn] += sa[nn].module.dcpwr*util::watt_to_kilowatt*ts_hour; //power W to	energy kWh
							annual_mppt_window_clipping += mppt_clip_window*util::watt_to_kilowatt*ts_hour; //power W to	energy kWh
							// save to SSC output arrays
							p_tcell[nn][idx] = (ssc_number_t)sa[nn].module.tcell;
							p_modeff[nn][idx] = (ssc_number_t)sa[nn].module.dceff;
							p_dcv[nn][idx] = (ssc_number_t)sa[nn].module.dcv * modules_per_string;
							p_voc[nn][idx] = (ssc_number_t)sa[nn].module.voc * modules_per_string;
							p_isc[nn][idx] = (ssc_number_t)sa[nn].module.isc;
							p_dcsubarray[nn][idx] = (ssc_number_t)(sa[nn].module.dcpwr * util::watt_to_kilowatt);
						}
						// Sara 1/25/16 - shading database derate applied to dc only
						// shading loss applied to beam if not from shading database
						sa[nn].module.dcpwr *= sa[nn].shad.dc_shade_factor();


						dcpwr_net += sa[nn].module.dcpwr * sa[nn].derate;

					}
					// bug fix jmf 12/13/16- losses that apply to ALL subarrays need to be applied OUTSIDE of the subarray summing loop
					// if they're applied WITHIN the loop, as they had been, then the power from subarrays 1-3 get the SAME derate/degradation applied nn-1 times, instead of just once!!

					//module degradation and lifetime DC losses apply to all subarrays
					if (system_use_lifetime_output == 1)
						dcpwr_net *= p_dc_degrade_factor[iyear + 1];

					//dc adjustment factors apply to all subarrays
					if (iyear == 0) annual_dc_adjust_loss += dcpwr_net * (1 - dc_haf(hour)) * util::watt_to_kilowatt * ts_hour; //only keep track of this loss for year 0, convert from power W to energy kWh
					dcpwr_net *= dc_haf(hour);

					//lifetime daily DC losses apply to all subarrays and should be applied last. Only applied if they are enabled.
					if (system_use_lifetime_output == 1 && en_dc_lifetime_losses)
					{
						//current index of the lifetime daily DC losses is the number of years that have passed (iyear, because it is 0-indexed) * the number of days + the number of complete days that have passed
						int dc_loss_index = iyear * 365 + (int)floor(hour / 24); //in units of days
						if (iyear == 0) annual_dc_lifetime_loss += dcpwr_net * (dc_lifetime_losses[dc_loss_index] / 100) * util::watt_to_kilowatt * ts_hour; //this loss is still in percent, only keep track of it for year 0, convert from power W to energy kWh
						dcpwr_net *= (100 - dc_lifetime_losses[dc_loss_index]) / 100;
					}

					// save other array-level environmental and irradiance outputs	- year 1 only outputs
					if (iyear == 0)
					{
						p_wspd[idx] = (ssc_number_t)wf.wspd;
						p_tdry[idx] = (ssc_number_t)wf.tdry;
						p_albedo[idx] = (ssc_number_t)alb;
						p_snowdepth[idx] = (ssc_number_t)wf.snow;

						p_solzen[idx] = (ssc_number_t)solzen;
						p_solalt[idx] = (ssc_number_t)solalt;
						p_solazi[idx] = (ssc_number_t)solazi;

						// absolute relative airmass calculation as f(zenith angle, site elevation)
						p_airmass[idx] = sunup > 0 ? (ssc_number_t)(exp(-0.0001184 * hdr.elev) / (cos(solzen*3.1415926 / 180) + 0.5057*pow(96.080 - solzen, -1.634))) : 0.0f;
						p_sunup[idx] = (ssc_number_t)sunup;

						// save radiation values.  the ts_accum_* variables are units of (W), 
						// and are sums of radiation power on each subarray for the current timestep
						p_poanom_ts_total[idx] = (ssc_number_t)(ts_accum_poa_nom * util::watt_to_kilowatt); // kW
						p_poabeamnom_ts_total[idx] = (ssc_number_t)(ts_accum_poa_beam_nom * util::watt_to_kilowatt); // kW
						p_poashaded_ts_total[idx] = (ssc_number_t)(ts_accum_poa_shaded * util::watt_to_kilowatt); // kW
						p_poaeff_ts_total[idx] = (ssc_number_t)(ts_accum_poa_eff * util::watt_to_kilowatt); // kW
						p_poabeameff_ts_total[idx] = (ssc_number_t)(ts_accum_poa_beam_eff * util::watt_to_kilowatt); // kW


						p_invmpptloss[idx] = (ssc_number_t)(mppt_clip_window * util::watt_to_kilowatt);

					}
					p_inv_dc_voltage[idx] = (ssc_number_t)dc_string_voltage;
					p_dcpwr[idx] = (ssc_number_t)(dcpwr_net * util::watt_to_kilowatt);

					idx++;
				}
			}
			// using single weather file initially - so rewind to use for next year
			wdprov->rewind();
		}

		// Initialize DC battery predictive controller
		if (en_batt && (ac_or_dc == charge_controller::DC_CONNECTED) && (look_ahead || look_behind))
			batt.initialize_automated_dispatch(p_dcpwr, p_load_full);

		/* *********************************************************************************************
		PV AC calculation
		*********************************************************************************************** */
		idx = 0; ireport = 0; ireplast = 0; percent_baseline = percent_complete;
		double annual_dc_power_before_battery = 0, annual_dc_power_after_battery = 0;
		for (size_t iyear = 0; iyear < nyears; iyear++)
		{
			for (hour = 0; hour < 8760; hour++)
			{
				// report progress updates to the caller	
				ireport++;
				if (ireport - ireplast > irepfreq)
				{
					percent_complete = percent_baseline + 100.0f *(float)(hour + iyear * 8760) / (float)(insteps);
					if (!update("", percent_complete))
						throw exec_error("pvsamv1", "simulation canceled at hour " + util::to_string(hour + 1.0) + " in year " + util::to_string((int)iyear + 1) + "in ac loop");
					ireplast = ireport;
				}

				for (size_t jj = 0; jj < step_per_hour; jj++)
				{



					// Battery replacement
					if (en_batt)
						batt.check_replacement_schedule(batt_replacement_option, count_batt_replacement, batt_replacement, iyear, hour, jj);

					// Iterative loop over DC battery
					size_t dc_count = 0; bool iterate_dc = false;
					double dcpwr_net = 0, acpwr_gross = 0, aceff = 0, pntloss = 0, psoloss = 0, cliploss = 0, ac_wiringloss = 0;
					do {

						cur_load = p_load_full[idx];
						dcpwr_net = util::kilowatt_to_watt * p_dcpwr[idx];
						double dc_string_voltage = p_inv_dc_voltage[idx];

						// DC Connected Battery
						bool battery_charging = false;
						if (en_batt && (ac_or_dc == charge_controller::DC_CONNECTED))
						{
							if (iyear == 0 && dc_count == 0)
								annual_dc_power_before_battery += p_dcpwr[idx] * ts_hour;

							batt.advance(*this, iyear, hour, jj, dcpwr_net*util::watt_to_kilowatt, cur_load);
							dcpwr_net = util::kilowatt_to_watt * batt.outGenPower[idx];

							// inverter can't handle negative dcpwr
							if (dcpwr_net < 0)
							{
								if (batt.outBatteryPower[idx] < 0)
								{
									battery_charging = true;
									dcpwr_net = fabs(dcpwr_net);
								}
								else
									dcpwr_net = 0;
							}
						}
						// inverter: runs at all hours of the day, even if no DC power.  important
						// for capturing tare losses			
						acpwr_gross = 0, aceff = 0, pntloss = 0, psoloss = 0, cliploss = 0, ac_wiringloss = 0;
						if ((inv_type == 0) || (inv_type == 1) || (inv_type == 3))
						{
							double _par, _plr;
							snlinv.acpower(dcpwr_net / num_inverters, dc_string_voltage,
								&acpwr_gross, &_par, &_plr, &aceff, &cliploss, &psoloss, &pntloss);

							acpwr_gross *= num_inverters;
							cliploss *= num_inverters;
							psoloss *= num_inverters;
							pntloss *= num_inverters;
							aceff *= 100;
						}
						else if (inv_type == 2) // partload
						{
							double _par, _plr;
							plinv.acpower(dcpwr_net / num_inverters, &acpwr_gross, &_par, &_plr, &aceff, &cliploss, &pntloss);
							acpwr_gross *= num_inverters;
							cliploss *= num_inverters;
							psoloss *= num_inverters;
							pntloss *= num_inverters;
							aceff *= 100;
						}

						// if dc connected battery, update post-inverted quantities
						if (en_batt && (ac_or_dc == charge_controller::DC_CONNECTED))
						{
							if (battery_charging)
							{
								// change sign back now that is inverted
								dcpwr_net *= -1;
								acpwr_gross *= -1;
							}
							batt.update_post_inverted(*this, iyear, hour, jj, acpwr_gross*util::watt_to_kilowatt);
							iterate_dc = batt.check_iterate(dc_count);
							acpwr_gross = batt.outGenPower[idx] * util::kilowatt_to_watt;
						}
						dc_count++;
					} while (iterate_dc);

					ac_wiringloss = fabs(acpwr_gross) * ac_loss_percent * 0.01;

					// accumulate first year annual energy
					if (iyear == 0)
					{
						if (en_batt && (ac_or_dc == charge_controller::DC_CONNECTED))
								annual_dc_power_after_battery += batt.outGenPower[idx] * ts_hour;

						annual_ac_gross += acpwr_gross * util::watt_to_kilowatt * ts_hour;
						p_inveff[idx] = (ssc_number_t)(aceff);
						p_invcliploss[idx] = (ssc_number_t)(cliploss * util::watt_to_kilowatt);
						p_invpsoloss[idx] = (ssc_number_t)(psoloss * util::watt_to_kilowatt);
						p_invpntloss[idx] = (ssc_number_t)(pntloss * util::watt_to_kilowatt);
						p_ac_wiringloss[idx] = (ssc_number_t)(ac_wiringloss * util::watt_to_kilowatt);
					}
					p_dcpwr[idx] = (ssc_number_t)(dcpwr_net * util::watt_to_kilowatt);
					
					//ac losses should always be subtracted, this means you can't just multiply by the derate because at nighttime it will add power
					p_gen[idx] = (ssc_number_t)((acpwr_gross - ac_wiringloss) * util::watt_to_kilowatt);

					// apply transformer loss
					// load loss
					ssc_number_t xfmr_ll = 0.0;
					if (xfmr_ll_frac != 0 && xfmr_rating != 0)
					{
						if (p_gen[idx] < xfmr_rating)
							xfmr_ll = xfmr_ll_frac * p_gen[idx] * p_gen[idx] / xfmr_rating;
						else // should really have user size transformer correctly!
							xfmr_ll = xfmr_ll_frac * p_gen[idx];
					} 
					// total load loss
					ssc_number_t xfmr_loss = xfmr_ll + xfmr_nll;
					// apply transformer loss
					p_gen[idx] -= xfmr_loss;

					// accumulate first year annual energy
					if (iyear == 0)
					{
						annual_xfmr_nll += xfmr_nll;
						annual_xfmr_ll += xfmr_ll;
						annual_xfmr_loss += xfmr_loss;
						p_xfmr_nll_ts[idx] = xfmr_nll;
						p_xfmr_ll_ts[idx] = xfmr_ll;
						p_xfmr_loss_ts[idx] = xfmr_loss;
					}

					idx++;
				}
			}
		}

		// Initialize AC connected battery predictive control
		if (en_batt && ac_or_dc == charge_controller::AC_CONNECTED && (look_ahead || look_behind))
			batt.initialize_automated_dispatch(p_gen, p_load_full);

		/* *********************************************************************************************
		Post PV AC 
		*********************************************************************************************** */
		idx = 0; ireport = 0; ireplast = 0; percent_baseline = percent_complete;
		double annual_energy_pre_battery = 0.; 
		for (size_t iyear = 0; iyear < nyears; iyear++)
		{
			for (hour = 0; hour < 8760; hour++)
			{
				// report progress updates to the caller	
				ireport++;
				if (ireport - ireplast > irepfreq)
				{
					percent_complete = percent_baseline + 100.0f *(float)(hour + iyear * 8760) / (float)(insteps);
					if (!update("", percent_complete))
						throw exec_error("pvsamv1", "simulation canceled at hour " + util::to_string(hour + 1.0) + " in year " + util::to_string((int)iyear + 1) + "in post ac loop");
					ireplast = ireport;
				}

				for (size_t jj = 0; jj < step_per_hour; jj++)
				{
					if (iyear == 0)
						annual_energy_pre_battery += p_gen[idx] * ts_hour;

					if (en_batt && ac_or_dc == charge_controller::AC_CONNECTED)
					{
						batt.advance(*this, iyear, hour, jj, p_gen[idx], p_load_full[idx]);
						p_gen[idx] = batt.outGenPower[idx];
					}

					// accumulate system generation before curtailment and availability
					if (iyear == 0)
						annual_ac_pre_avail += p_gen[idx] * ts_hour;
		

					//apply availability and curtailment
					p_gen[idx] *= haf(hour);

					//apply lifetime daily AC losses only if they are enabled
					if (system_use_lifetime_output == 1 && en_ac_lifetime_losses)
					{
						//current index of the lifetime daily AC losses is the number of years that have passed (iyear, because it is 0-indexed) * days in a year + the number of complete days that have passed
						int ac_loss_index = iyear * 365 + (int)floor(hour / 24); //in units of days
						if (iyear == 0) annual_ac_lifetime_loss += p_gen[idx] * (ac_lifetime_losses[ac_loss_index] / 100) * util::watt_to_kilowatt * ts_hour; //this loss is still in percent, only keep track of it for year 0, convert from power W to energy kWh
						p_gen[idx] *= (100 - ac_lifetime_losses[ac_loss_index]) / 100;
					}


					if (iyear == 0)
						annual_energy += (ssc_number_t)(p_gen[idx] * ts_hour);

					idx++;
				}
			} 

		} 
		// Check the snow models and if neccessary report a warning
		//  *This only needs to be done for subarray1 since all of the activated subarrays should 
		//   have the same number of bad values
		//  *Also accumulate monthly and annual loss values 

		if (en_snow_model){
			if (sa[0].sm.badValues > 0){
				log(util::format("The snow model has detected %d bad snow depth values (less than 0 or greater than 610 cm). These values have been set to zero.", sa[0].sm.badValues), SSC_WARNING);
			}
			
			// scale by ts_hour to convert power -> energy
			accumulate_monthly_for_year( "dc_snow_loss", "monthly_snow_loss", ts_hour , step_per_hour );			
			accumulate_annual_for_year( "dc_snow_loss", "annual_snow_loss", ts_hour, step_per_hour);
		}
		 
		if (hour != 8760)
			throw exec_error("pvsamv1", "failed to simulate all 8760 hours, error in weather file ?");


		accumulate_monthly_for_year("dc_net", "monthly_dc", ts_hour, step_per_hour);
		accumulate_monthly_for_year("gen", "monthly_energy", ts_hour, step_per_hour);
		
		// scale by ts_hour to convert power -> energy
		accumulate_annual_for_year("gh", "annual_gh", ts_hour, step_per_hour);
		
		// scale by ts_hour to convert power -> energy
		double annual_poa_nom = accumulate_annual_for_year("poa_nom", "annual_poa_nom", ts_hour, step_per_hour);
		double annual_poa_beam_nom = accumulate_annual_for_year("poa_beam_nom", "annual_poa_beam_nom", ts_hour, step_per_hour);
		double annual_poa_shaded = accumulate_annual_for_year("poa_shaded", "annual_poa_shaded", ts_hour, step_per_hour);
		double annual_poa_eff = accumulate_annual_for_year("poa_eff", "annual_poa_eff", ts_hour, step_per_hour);
		double annual_poa_beam_eff = accumulate_annual_for_year("poa_beam_eff", "annual_poa_beam_eff", ts_hour, step_per_hour);
		
		accumulate_monthly_for_year( "poa_nom", "monthly_poa_nom", ts_hour, step_per_hour );
		accumulate_monthly_for_year( "poa_beam_nom", "monthly_poa_beam_nom", ts_hour, step_per_hour );
		accumulate_monthly_for_year( "poa_eff", "monthly_poa_eff", ts_hour, step_per_hour );
		accumulate_monthly_for_year( "poa_beam_eff", "monthly_poa_beam_eff", ts_hour, step_per_hour );

		// scale by ts_hour to convert power -> energy
		double annual_dc_net = accumulate_annual_for_year("dc_net", "annual_dc_net", ts_hour, step_per_hour);
		double annual_ac_net = accumulate_annual_for_year("gen", "annual_ac_net", ts_hour, step_per_hour);
		double annual_inv_cliploss = accumulate_annual_for_year("inv_cliploss", "annual_inv_cliploss", ts_hour, step_per_hour);
		double annual_dc_invmppt_loss = accumulate_annual_for_year("dc_invmppt_loss", "annual_dc_invmppt_loss", ts_hour, step_per_hour);

		double annual_inv_psoloss = accumulate_annual_for_year("inv_psoloss", "annual_inv_psoloss", ts_hour, step_per_hour );
		double annual_inv_pntloss = accumulate_annual_for_year("inv_pntloss", "annual_inv_pntloss", ts_hour, step_per_hour);
	
		double nom_rad = is_cpv ? annual_poa_beam_nom : annual_poa_nom;
		double inp_rad = is_cpv ? annual_poa_beam_eff : annual_poa_eff;
		double ac_net = as_double("annual_ac_net");
		double mod_eff = module_eff( mod_type );

		// calculate system performance factor
		// reference: (http://files.sma.de/dl/7680/Perfratio-UEN100810.pdf)
		// additional reference: (http://www.nrel.gov/docs/fy05osti/37358.pdf)
		// PR = net_ac (kWh) / ( total input radiation (kWh) * stc efficiency (%) )
		// bug fix 6/15/15 jmf: total input radiation for PR should NOT including shading or soiling, hence use Nominal value.
		assign("performance_ratio", var_data((ssc_number_t)(ac_net / (nom_rad * mod_eff / 100.0))));

		// accumulate annual and monthly battery model outputs
		if ( en_batt ) batt.calculate_monthly_and_annual_outputs( *this );
		else assign( "average_cycle_efficiency", var_data( 0.0f ) ); // if battery disabled, since it's shown in the metrics table

		// calculate nominal dc input
		double annual_dc_nominal = (inp_rad * mod_eff / 100.0);
		assign( "annual_dc_nominal", var_data( (ssc_number_t) annual_dc_nominal) );

		assign( "nameplate_dc_rating", var_data( (ssc_number_t)nameplate_kw ) );

		inverter_vdcmax_check();
		inverter_size_check();

		assign("annual_energy", var_data((ssc_number_t)annual_energy));


		double annual_mismatch_loss = 0, annual_diode_loss = 0, annual_wiring_loss = 0, annual_tracking_loss = 0, annual_nameplate_loss = 0, annual_dcopt_loss = 0;
		double annual_dc_gross = 0;
		
		//dc optimizer losses are the same for all four subarrays, assign outside of subarray loop but calculate inside loop
		double dc_opt = as_double("dcoptimizer_loss");
		// loop over subarrays
		for (size_t nn = 0; nn < 4; nn++)
		{
			if ( sa[nn].enable )
			{
				std::string prefix = "subarray" + util::to_string((int)(nn + 1)) + "_";
				double mismatch = as_double(prefix + "mismatch_loss");
				double diodes = as_double(prefix + "diodeconn_loss");
				double wiring = as_double(prefix + "dcwiring_loss");
				double tracking = as_double(prefix + "tracking_loss");
				double nameplate = as_double(prefix + "nameplate_loss");
				double total_percent = mismatch + diodes + wiring + tracking + nameplate + dc_opt;

				double mismatch_loss = 0,diode_loss = 0,wiring_loss = 0,tracking_loss = 0, nameplate_loss = 0, dcopt_loss = 0;
				// dc derate for each sub array
				double dc_loss = dc_gross[nn] * (1.0 - sa[nn].derate);
				annual_dc_gross += dc_gross[nn];
				if (total_percent != 0)
				{
					mismatch_loss = mismatch / total_percent * dc_loss;
					diode_loss = diodes / total_percent * dc_loss;
					wiring_loss = wiring / total_percent * dc_loss;
					tracking_loss = tracking / total_percent * dc_loss;
					nameplate_loss = nameplate / total_percent * dc_loss;
					dcopt_loss = dc_opt / total_percent * dc_loss;
				}
				annual_mismatch_loss += mismatch_loss;
				annual_diode_loss += diode_loss;
				annual_wiring_loss += wiring_loss;
				annual_tracking_loss += tracking_loss;
				annual_nameplate_loss += nameplate_loss;
				annual_dcopt_loss += dcopt_loss;
			
				assign("annual_" + prefix + "dc_gross", var_data((ssc_number_t)dc_gross[nn]));
				assign("annual_" + prefix + "dc_mismatch_loss", var_data((ssc_number_t)mismatch_loss));
				assign("annual_" + prefix + "dc_diodes_loss", var_data((ssc_number_t)diode_loss));
				assign("annual_" + prefix + "dc_wiring_loss", var_data((ssc_number_t)wiring_loss));
				assign("annual_" + prefix + "dc_tracking_loss", var_data((ssc_number_t)tracking_loss));
				assign("annual_" + prefix + "dc_nameplate_loss", var_data((ssc_number_t)nameplate_loss));
			}
		}

		assign("annual_dc_gross", var_data((ssc_number_t)annual_dc_gross));
		assign("annual_ac_gross", var_data((ssc_number_t)annual_ac_gross));

		assign("xfmr_nll_year1", annual_xfmr_nll);
		assign("xfmr_ll_year1", annual_xfmr_ll);
		assign("xfmr_loss_year1", annual_xfmr_loss);

		assign("annual_dc_mismatch_loss", var_data((ssc_number_t)annual_mismatch_loss));
		assign("annual_dc_diodes_loss", var_data((ssc_number_t)annual_diode_loss));
		assign("annual_dc_wiring_loss", var_data((ssc_number_t)annual_wiring_loss));
		assign("annual_dc_tracking_loss", var_data((ssc_number_t)annual_tracking_loss));
		assign("annual_dc_nameplate_loss", var_data((ssc_number_t)annual_nameplate_loss));
		assign("annual_dc_optimizer_loss", var_data((ssc_number_t)annual_dcopt_loss));

		// dc user input losses
		// order taken from ui - meaningless if out of order - use percentages per 9/18/14 meeting
		double sys_output = annual_dc_gross;
		sys_output -= annual_mismatch_loss;
		assign("annual_dc_after_mismatch_loss", var_data((ssc_number_t)sys_output));
		sys_output -= annual_diode_loss;
		assign("annual_dc_after_diodes_loss", var_data((ssc_number_t)sys_output));
		sys_output -= annual_wiring_loss;
		assign("annual_dc_after_wiring_loss", var_data((ssc_number_t)sys_output));
		sys_output -= annual_tracking_loss;
		assign("annual_dc_after_tracking_loss", var_data((ssc_number_t)sys_output));
		sys_output -= annual_nameplate_loss;
		assign("annual_dc_after_nameplate_loss", var_data((ssc_number_t)sys_output));

//#define WITH_CHECKS

#ifdef WITH_CHECKS
		// check that sys_output=dc_net
		if ( fabs(annual_dc_net - sys_output)/annual_dc_net > 0.00001 )
			log(util::format("Internal discrepancy in calculated output dc_gross: %lg != %lg at DC1.  Please report to SAM support.", annual_dc_net, sys_output), SSC_WARNING);
#endif

		// dc to ac losses
		sys_output -= annual_inv_cliploss;
		assign("annual_ac_after_inv_cliploss", var_data((ssc_number_t)sys_output));
		sys_output -= annual_inv_psoloss;
		assign("annual_ac_after_inv_psoloss", var_data((ssc_number_t)sys_output));
		sys_output -= annual_inv_pntloss;
		assign("annual_ac_after_inv_pntloss", var_data((ssc_number_t)sys_output));

		double acwiring = as_double("acwiring_loss");
//		double transformer = as_double("transformer_loss");
		double total_percent = acwiring; // +transformer;
		double acwiring_loss = 0; // , transformer_loss = 0;
		sys_output = annual_ac_gross;
		double ac_loss = sys_output*(1.0 - ac_derate);

		if (total_percent != 0)
		{
			acwiring_loss = acwiring / total_percent * ac_loss;
//			transformer_loss = transformer / total_percent * ac_loss;
		}

		assign("annual_ac_wiring_loss", var_data((ssc_number_t)acwiring_loss));
//		assign("annual_ac_transformer_loss", var_data((ssc_number_t)transformer_loss));

		// ac losses
		sys_output -= acwiring_loss;
		assign("annual_ac_after_wiring_loss", var_data((ssc_number_t)sys_output));
//		sys_output -= transformer_loss;
//		assign("annual_ac_after_transformer_loss", var_data((ssc_number_t)sys_output));

		double percent = 0;
		if (annual_poa_nom > 0) percent = 100 * (annual_poa_nom - annual_poa_shaded) / annual_poa_nom;
		assign("annual_poa_shading_loss_percent", var_data((ssc_number_t)percent));
		percent = 0;
		if (annual_poa_shaded > 0) percent = 100 * (annual_poa_shaded - annual_poa_eff) / annual_poa_shaded;
		assign("annual_poa_soiling_loss_percent", var_data((ssc_number_t)percent));
		// annual_dc_nominal
		percent = 0;
		// SEV: Apply Snow loss to loss diagram 
		if (annual_dc_nominal > 0) percent = 100 * annual_snow_loss / annual_dc_nominal;
		assign("annual_dc_snow_loss_percent", var_data((ssc_number_t)percent));

		// apply clipping window loss
		if (annual_dc_nominal > 0) percent = 100 * annual_mppt_window_clipping / annual_dc_nominal;
		assign("annual_dc_mppt_clip_loss_percent", var_data((ssc_number_t)percent));

		// module loss depends on if MPPT clipping enabled.
		percent = 0;
		if (annual_dc_nominal > 0) percent = 100 * (annual_dc_nominal - (annual_dc_gross + annual_snow_loss + annual_mppt_window_clipping)) / annual_dc_nominal;
		assign("annual_dc_module_loss_percent", var_data((ssc_number_t)percent));


		// annual_dc_gross
		percent = 0;
		if (annual_dc_gross > 0) percent = 100 * annual_mismatch_loss / annual_dc_gross;
		assign("annual_dc_mismatch_loss_percent", var_data((ssc_number_t)percent));

		percent = 0;
		if (annual_dc_gross > 0) percent = 100 * annual_diode_loss / annual_dc_gross;
		assign("annual_dc_diodes_loss_percent", var_data((ssc_number_t)percent));

		percent = 0;
		if (annual_dc_gross > 0) percent = 100 * annual_wiring_loss / annual_dc_gross;
		assign("annual_dc_wiring_loss_percent", var_data((ssc_number_t)percent));

		percent = 0;
		if (annual_dc_gross > 0) percent = 100 * annual_tracking_loss / annual_dc_gross;
		assign("annual_dc_tracking_loss_percent", var_data((ssc_number_t)percent));

		percent = 0;
		if (annual_dc_gross > 0) percent = 100 * annual_nameplate_loss / annual_dc_gross;
		assign("annual_dc_nameplate_loss_percent", var_data((ssc_number_t)percent));

		percent = 0;
		if (annual_dc_gross > 0) percent = 100 * annual_dcopt_loss / annual_dc_gross;
		assign("annual_dc_optimizer_loss_percent", var_data((ssc_number_t)percent));

		percent = 0;
		if (annual_dc_gross > 0) percent = 100 * annual_dc_adjust_loss / annual_dc_gross;
		assign("annual_dc_perf_adj_loss_percent", var_data((ssc_number_t)percent));

		percent = 0;
		if (annual_dc_gross > 0) percent = 100 * (annual_dc_power_before_battery - annual_dc_power_after_battery) / annual_dc_gross;
		assign("annual_dc_battery_loss_percent", var_data((ssc_number_t)percent));

		percent = 0;
		if (annual_dc_gross > 0) percent = 100 * annual_dc_lifetime_loss / annual_dc_gross;
		assign("annual_dc_lifetime_loss_percent", var_data((ssc_number_t)percent));


		//annual_dc_net
		percent = 0;
		if (annual_dc_net > 0) percent = 100 *annual_inv_cliploss / annual_dc_net;
		assign("annual_ac_inv_clip_loss_percent", var_data((ssc_number_t)percent));

		percent = 0;
		if (annual_dc_net > 0) percent = 100 * annual_inv_psoloss / annual_dc_net;
		assign("annual_ac_inv_pso_loss_percent", var_data((ssc_number_t)percent));

		percent = 0;
		if (annual_dc_net > 0) percent = 100 * annual_inv_pntloss / annual_dc_net;
		assign("annual_ac_inv_pnt_loss_percent", var_data((ssc_number_t)percent));

		sys_output = annual_dc_net;
		sys_output -= (annual_inv_cliploss + annual_inv_pntloss + annual_inv_psoloss);
		percent = 0;
		if (sys_output > 0) percent = 100 * (sys_output - annual_ac_gross) / sys_output;
		assign("annual_ac_inv_eff_loss_percent", var_data((ssc_number_t)percent));


		// annual_ac_gross
		sys_output *= (1.0 - percent / 100.0);

#ifdef WITH_CHECKS
		// check that ac_gross = sys_output at this point
		if (fabs(annual_ac_gross - sys_output)/ annual_ac_gross > 0.00001)
			log(util::format("Internal discrepancy in calculated output ac_gross: %lg != %lg at AC1.  Please report to SAM support.", annual_ac_gross, sys_output), SSC_WARNING);
#endif

		percent = 0;
		annual_ac_battery_loss = (annual_energy_pre_battery - annual_ac_pre_avail);
		if (annual_ac_gross > 0) percent = 100.0 * annual_ac_battery_loss / annual_ac_gross;
		assign("annual_ac_battery_loss_percent", var_data((ssc_number_t)percent));
		sys_output -= annual_ac_battery_loss;

		percent = 0;
		if (annual_ac_gross > 0) percent = 100.0 * acwiring_loss / annual_ac_gross;
		assign("annual_ac_wiring_loss_percent", var_data((ssc_number_t)percent));
		sys_output -= acwiring_loss;

//		percent = 0;
//		if (annual_ac_gross > 0) percent = 100.0 * transformer_loss / annual_ac_gross;
//		assign("annual_ac_transformer_loss_percent", var_data((ssc_number_t)percent));
//		sys_output -= transformer_loss;
		// annual_ac_pre_avail

		percent = 0;
		if (annual_ac_gross > 0) percent = 100 * annual_ac_lifetime_loss / annual_ac_gross;
		assign("annual_ac_lifetime_loss_percent", var_data((ssc_number_t)percent));
		sys_output -= annual_ac_lifetime_loss;

		percent = 0;
		if (annual_xfmr_loss > 0) percent = 100 * annual_xfmr_loss / annual_ac_gross;
		assign("annual_xfmr_loss_percent", var_data((ssc_number_t)percent));
		sys_output -= annual_ac_lifetime_loss;


#ifdef WITH_CHECKS
		// check that ac_net = sys_output at this point
		if (fabs(annual_ac_pre_avail - sys_output)/ annual_ac_pre_avail > 0.00001)
			log(util::format("Internal discrepancy in calculated output ac_net: %lg != %lg at AC2.  Please report to SAM support.", annual_ac_pre_avail, sys_output), SSC_WARNING);
#endif



		percent = 0;
		if (annual_ac_pre_avail > 0) percent = 100.0 * (annual_ac_pre_avail - annual_energy) / annual_ac_pre_avail;
		assign("annual_ac_perf_adj_loss_percent", var_data((ssc_number_t)percent));
		sys_output *= (1.0 - percent / 100.0);


		// annual_ac_net = system_output

#ifdef WITH_CHECKS
		// check that ac_net = sys_output at this point
		if (fabs(annual_ac_net - sys_output) / annual_ac_net > 0.00001)
			log(util::format("Internal discrepancy in calculated output ac_net: %lg != %lg at AC3.  Please report to SAM support.", annual_ac_net, sys_output), SSC_WARNING);
#endif


		// end of losses

		
		assign( "6par_a", var_data((ssc_number_t) cec.a) );
		assign( "6par_Io", var_data((ssc_number_t) cec.Io) );
		assign( "6par_Il", var_data((ssc_number_t) cec.Il) );
		assign( "6par_Rs", var_data((ssc_number_t) cec.Rs) );
		assign( "6par_Rsh", var_data((ssc_number_t) cec.Rsh) );
		assign( "6par_Adj", var_data((ssc_number_t) cec.Adj) );


		double kWhperkW = 0.0;
		double nameplate = as_double("system_capacity");
		if (nameplate > 0) kWhperkW = annual_energy / nameplate;
		// adjustment for timestep values
		kWhperkW *= ts_hour;
		assign("capacity_factor", var_data((ssc_number_t)(kWhperkW / 87.6)));
		assign("kwh_per_kw", var_data((ssc_number_t)kWhperkW));
	}
	
	bool check_azal_monotonic_increase( const util::matrix_t<double> &azal )
	{
		if (azal.nrows() < 3 || azal.ncols() < 3) return false;

		for (size_t i=2;i<azal.nrows();i++)
			if (azal.at(i,0) < azal.at(i-1,0))				
				return false;

		for (size_t i=2;i<azal.ncols();i++)
			if (azal.at(0,i) < azal.at(0,i-1))
				return false;

		return true;
	}


	double module_eff(int mod_type)
	{
		double eff = -1;
	
		switch (mod_type)
		{
		case 0: // SPE
			eff = as_double( util::format("spe_eff%d", as_integer("spe_reference")) );
			break;
		case 1: // CEC
			{
				double a_c = as_double("cec_area");
				double i_noct = 1000; // as_double("cec_i_noct");
				double v_mp_ref = as_double("cec_v_mp_ref");
				double i_mp_ref = as_double("cec_i_mp_ref");

				if (a_c == 0) a_c = -1;
			//	if (i_noct == 0) i_noct = 1000.0;

				eff = 100.0 * (( v_mp_ref * i_mp_ref ) / a_c) / i_noct;
			}
			break;
		case 2: // 6par user entered
			{
				double area = as_double("6par_area");
				double vmp = as_double("6par_vmp");
				double imp = as_double("6par_imp");
				if (area == 0) area = 1;
				eff = 100.0 * ((vmp*imp)/area)/1000.0;
			}
			break;
		case 3: // Sandia
			{
				double area = as_double("snl_area");
				double vmpo = as_double("snl_vmpo");
				double impo = as_double("snl_impo");

				eff = vmpo*impo;
				if (area > 0)
					eff = eff/area;
				eff = eff / 1000.0;
				eff = eff * 100;
			}
			break;
		case 4: // IEC 61853
			{
				double area = as_double("sd11par_area");
				double vmp = as_double("sd11par_Vmp0");
				double imp = as_double("sd11par_Imp0");
				if (area == 0) area = 1;
				eff = 100.0 * ((vmp*imp)/area)/1000.0;
			}
			break;
		}

		if (eff == 0.0) eff = -1;
		return eff;
	}

	void inverter_vdcmax_check()
	{
		// check that no hourly vmp values exceed Vdcmax
		// add max value and number of times > Vdcmax
		int numVmpGTVdcmax = 0;
		double maxVmp=0;
		int maxVmpHour=0;
		int invType = as_integer("inverter_model");
		double vdcmax;
		switch (invType)
		{
			case 0: // cec
				vdcmax = as_double("inv_snl_vdcmax");
				break;
			case 1: // datasheet
				vdcmax = as_double("inv_ds_vdcmax");
				break;
			case 2: // partload curve
				vdcmax = as_double("inv_pd_vdcmax");
				break;
			case 3: // coefficient generator
				vdcmax = as_double("inv_cec_cg_vdcmax");
				break;
			default:
				// message
				return;
		}

		// warning on inverter page
		if (vdcmax <=0) return;

		size_t count;
		ssc_number_t *da = as_array("inverter_dc_voltage", &count);
		if (count == 8760)
		{
			for (size_t i=0; i < count;i++)
			{
				if (da[i] > vdcmax)
				{
					numVmpGTVdcmax++;
					if (da[i] > maxVmp) 
					{
						maxVmp = da[i];
						maxVmpHour = i;
					}
				}
			}
		}

		if (numVmpGTVdcmax > 0) 
		{
			log( util::format( "Module array voltage Vmp exceeds the Vdcmax (%.2lfV) of inverter %d times.\n"
					"   The maximum Vmp value is %.2lfV at hour %d.\n"
					"   It is recommended that you reduce the number of modules per string.", vdcmax, numVmpGTVdcmax, maxVmp, maxVmpHour ),
					SSC_WARNING );
		}
	}

	void inverter_size_check()
	{
		// undersized - check that no hourly output exceeds the rated output of the inverter
		// 9/26/10 note that e_net automatically clipped - must look at derated dc power
		// oversized - add max output > 75% of inverter ourput
		ssc_number_t *acPower;
		size_t acCount;
		ssc_number_t *dcPower;
		size_t dcCount;
		int numHoursClipped = 0;
		double maxACOutput=0;
		int invType = as_integer("inverter_model");
		int numInv = as_integer("inverter_count");

		double ratedACOutput = 0;
		double ratedDCOutput = 0;
		switch (invType)
		{
			case 0: // cec
				ratedACOutput = as_double("inv_snl_paco");
				ratedDCOutput = as_double("inv_snl_pdco");
				break;
			case 1: // datasheet
				ratedACOutput = as_double("inv_ds_paco");
				ratedDCOutput = as_double("inv_ds_eff")/100.0;
				if (ratedDCOutput != 0) ratedDCOutput = ratedACOutput/ratedDCOutput;
				break;
			case 2: // partload curve
				ratedACOutput = as_double("inv_pd_paco");
				ratedDCOutput = as_double("inv_pd_pdco");
				break;
			case 3: // coefficient generator
				ratedACOutput = as_double("inv_cec_cg_paco");
				ratedDCOutput = as_double("inv_cec_cg_pdco");
				break;
			default:
				// message
				return;
		}
		ratedACOutput *= numInv;
		ratedDCOutput *= numInv;

		if ((ratedACOutput <= 0) || (ratedDCOutput <= 0)) return;

		ratedACOutput = ratedACOutput * util::watt_to_kilowatt; // W to kW to compare to hourly output
		ratedDCOutput = ratedDCOutput * util::watt_to_kilowatt; // W to kW to compare to hourly output

		acPower = as_array("gen", &acCount);
		dcPower = as_array("dc_net", &dcCount);
		if ((acCount == 8760) && (dcCount == 8760))
		{
			for (size_t i=0; i < acCount;i++)
			{
				if (dcPower[i] > ratedDCOutput) numHoursClipped++;
				if (acPower[i] > maxACOutput) maxACOutput = acPower[i]; 
			}
		}
		if (numHoursClipped >= 2190) //more than one quarter of the entire year (8760) is clipped
			log( util::format("Inverter undersized: The array output exceeded the inverter rating %.2lf kWdc for %d hours.", 
				ratedDCOutput, numHoursClipped), 
				SSC_WARNING );

		if ((maxACOutput < 0.75 * ratedACOutput) && (maxACOutput > 0))
			log( util::format("Inverter oversized: The maximum inverter output was %.2lf%% of the rated value %lg kWac.", 
				100 * maxACOutput / ratedACOutput, ratedACOutput), 
				SSC_WARNING);
	}
};

DEFINE_MODULE_ENTRY( pvsamv1, "Photovoltaic performance model, SAM component models V.1", 1 )

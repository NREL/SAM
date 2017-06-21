#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include "sscapi.h"

#ifndef MAX_PATH
#define MAX_PATH 1024
#endif

typedef struct { 
	const char *var;
	const char *shortname;
	const char *label;
	const char *units;
	ssc_number_t value;
	char file[MAX_PATH];
	ssc_number_t min;
	ssc_number_t max;
} varinfo;

varinfo variables[] = {
    { "solar_resource_file", "wf",      "Solar weather data file",    "tm2,tm3,epw,smw,csv",                                                                         -99,    "",    0,        0         },
	{ "system_capacity",     "size",    "System size (DC nameplate)", "kW",                                                                                          1.0f,   "",    0.05f,    500000.0f },
	{ "module_type",         "module",  "Module type",                "0=standard,1=premium,2=thin film",                                                            0,      "",    0,        2         },
	{ "dc_ac_ratio",         "ratio",   "DC to AC ratio",             "ratio",                                                                                       1.1f,   "",    0.1f,     10        },
	{ "inv_eff",             "inveff",  "Inverter efficiency",        "%",                                                                                           96.0f,  "",    90.0f,    99.5f     },
	{ "losses",              "loss",    "System losses",              "%",                                                                                           14.0f,  "",    -5.0f,    99.0f     },
	{ "array_type",          "array",   "Array type",                 "0=fixed open rack,1=fixed roof mount,2=one axis tracking,3=backtracking,4=two axis tracking", 0,      "",    0,        4         },
	{ "tilt",                "tilt",    "Tilt",                       "degrees",                                                                                     20.0f,  "",    0.0f,     90.0f     },
	{ "azimuth",             "azi",     "Azimuth",                    "degrees",                                                                                     180.0f, "",    0.0f,     360.0f    },
	{ "gcr",                 "gcr",     "Ground coverage ratio",      "ratio",                                                                                       0.4f,   "",    0.0f,     3.0f      },
	{ "adjust:constant",     "adj",     "Availability losses",        "%",                                                                                           0.0f,   "",    -5.0f,    100.0f    },
	{ "print",               "prn",     "Print annual results",       "0/1/2",                                                                                       1.0f,   "",    0.0f,     2.0f      },
	{ "output",              "out",     "Output file name",           "path",                                                                                        -99,    "",    0,        0         },
	{ 0, 0, 0, 0, 0, "", 0, 0 } };

varinfo *getvar(const char*sn)
{
	int i=0;
	while( variables[i].var )
	{
		if ( strcmp(variables[i].shortname,sn)==0) return &variables[i];
		i++;
	}
	return &variables[i];
}
	
int main(int argc, char *argv[]) 
{
	ssc_data_t pdata;
	size_t i,j,ierr;
	const char *serr;
	FILE *fp;
	const char *outfile;
	int prn;
	
	if ( argc > 1 && strcmp(argv[1],"-help")==0 )
	{
		i=0;
		printf("PVWatts V5 Command Line\n(using SAM SDK version %d %s)\n\n", ssc_version(), ssc_build_info());
		printf("  Option               Shortcut   Default  Name                          Units\n" );
		while( variables[i].var )
		{
			printf(" -%-20s  -%-6s   %-6.1f  %-30s  %-s\n", 
				variables[i].var, variables[i].shortname, variables[i].value, variables[i].label, variables[i].units );
			i++;
		}
				
		return 0;
	}
	
	ierr = 0;
	
	for( j=1;j<argc;j++)
	{
		int found;
		char arg[256],sval[MAX_PATH];
		if ( argv[j][0] == '-' )
		{
			const char *eq = strchr((char*)argv[j], '=');
			if ( !eq ) continue;
			strncpy( arg, argv[j]+1, eq-argv[j]-1 );
			arg[ eq-argv[j]-1 ] = 0;			
			strcpy( (char*)sval, (char*)(eq+1) );
		}
		
		found = 0;
		i=0;
		while( variables[i].var )
		{
			if ( strcmp(arg,variables[i].var)==0 || strcmp(arg,variables[i].shortname) == 0 )
			{
				found = 1;
				if ( variables[i].min == variables[i].max
					&& variables[i].value == -99 )
				{
					strcpy( (char*)variables[i].file, (char*)sval );
				}
				else
				{
					ssc_number_t value = (ssc_number_t)atof(sval);
					if ( value >= variables[i].min && value <= variables[i].max )
						variables[i].value = value;
					else
					{
						printf( "Parameter '%s'=%g out of range:   [%g, %g]\n", 
							variables[i].var,
							value, variables[i].min, variables[i].max );
						ierr = 1;
					}
				}				
			}			
			i++;
		}

		
		if ( !found )
		{
			printf("unrecognized parameter: '%s' with value '%s'\n", arg, sval );
			ierr = 1;
		}
	}
	
	if ( ierr != 0 ) return -ierr;
	
	pdata = ssc_data_create();
	i=0;
	while( variables[i].var )
	{
		if ( variables[i].value == -99 ) ssc_data_set_string( pdata, variables[i].var, variables[i].file );
		else ssc_data_set_number( pdata, variables[i].var, variables[i].value );
		i++;
	}
	
	prn = (int)getvar("prn")->value;
	
	ssc_module_exec_set_print( prn > 1 );
	serr = ssc_module_exec_simple_nothread( "pvwattsv5", pdata );
	
	if ( serr != 0 )
	{
		ssc_data_free( pdata );
		printf("error: %s\n", serr );
		return -1;
	}
	
	
	if ( prn )
	{
		ssc_number_t ackwh, capfac;
		ssc_data_get_number( pdata, "annual_energy", &ackwh );
		ssc_data_get_number( pdata, "capacity_factor", &capfac );
		printf("ac=%g kWh cf=%g %%\n", ackwh, capfac);
	}
	
	outfile = getvar("out")->file;
	if ( strlen(outfile) > 0 )
	{
		ssc_number_t lat, lon, tz, elev;
		int count;
		
		fp = fopen( outfile, "w" );
		if ( !fp )
		{
			ssc_data_free( pdata );
			printf("could not open output file for writing: %s\n", outfile);
			return -2;
		}
		
		i=0;
		while( variables[i].var )
		{
			fputs( variables[i].shortname, fp );
			fputc( ',', fp );
			i++;
		}
		
		fputs( ",LAT,LON,TZ,ELEV,PVWATTS V5 SSC VERSION\n", fp );
		i=0;
		while( variables[i].var )
		{
			if ( variables[i].value == -99 )  fputs( variables[i].file, fp );
			else fprintf(fp, "%g", variables[i].value );
			fputc(',',fp);
			i++;
		}
		
		ssc_data_get_number( pdata, "lat", &lat );
		ssc_data_get_number( pdata, "lon", &lon );
		ssc_data_get_number( pdata, "tz", &tz );
		ssc_data_get_number( pdata, "elev", &elev );
		
		fprintf( fp, ",%g,%g,%g,%g,ver.%d %s\n", lat, lon, tz, elev, ssc_version(), ssc_build_info() );
		
		fputs( "POA,DC,AC,ENERGY\n", fp );
		fputs( "W/m2,W,W,Wh\n", fp );
				
		ssc_number_t *poa = ssc_data_get_array( pdata, "poa", &count );
		ssc_number_t *dc = ssc_data_get_array( pdata, "dc", &count );
		ssc_number_t *ac = ssc_data_get_array( pdata, "ac", &count );
		ssc_number_t *net = ssc_data_get_array( pdata, "energy", &count );
		
		for( i=0;i<count;i++ )
			fprintf( fp, "%g,%g,%g,%g\n",
				poa[i],dc[i],ac[i],net[i] );
		
		fclose(fp);
	}
	
	
	ssc_data_free( pdata );
	
	return 0;
}

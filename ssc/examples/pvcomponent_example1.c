#include <stdio.h>
#include <stdlib.h>

#include "sscapi.h"

const char *weather_file = "TX Abilene.tm2";


ssc_bool_t my_handler( ssc_module_t p_mod, ssc_handler_t p_handler, int action, 
	float f0, float f1, const char *s0, const char *s1, void *user_data )
{
	if (action == SSC_LOG)
	{
		// print log message to console
		switch( (int)f0 )
		{
		case SSC_NOTICE: printf("Notice: %s\n", s0); break;
		case SSC_WARNING: printf("Warning: %s\n", s0); break;
		case SSC_ERROR: printf("Error: %s\n", s0); break;
		}
		return 1;
	}
	else if (action == SSC_UPDATE)
	{
		// print status update to console
		printf("(%.2f %%) %s\n",f0,s0);
		return 1; // return 0 to abort simulation as needed.
	}
	else if (action == SSC_EXECUTE)
	{
		// run the executable, pipe the output, and return output to p_mod
		// **TODO** (not necessary for basic PVwatts)
		return 0;
	}
	else
		return 0;
}

int main(int argc, char *argv[])
{
	
	// create an empty SSC data container
	ssc_data_t p_data = ssc_data_create();
	
	
	/*******************************
		create weather file reader module and setup inputs
		************************************/
		
	ssc_module_t p_mod = ssc_module_create( "wfreader" );
	if ( p_mod == 0 )
	{
		printf("could not create 'wfreader' model\n");
		return -2;
	
	}
	
	// set the weather file name
	ssc_data_set_string( p_data, "file_name", weather_file );
	// this flag will synthesize the albedo (ground reflectance)
	// from the snow depth data in the weather file, if it exists
	// this is used to produce an albedo that is consistent with 
	// what the PVWatts Version 1 online tool does.  Basically,
	// if there is snow, albedo is 0.6, otherwise 0.2.  This assumes
	// that panels are cleaned off immediately when there is snow fall
	// - an assumption that is certainly questionable.
	ssc_data_set_number( p_data, "syn_albedo_from_snow", 1 ); 
	
	
	
	if ( !ssc_module_exec_with_handler( p_mod, p_data, my_handler, 0 ) )
	{
		printf("failed to read weather file...\n");
		return -1;
	}
	
	// at this point, the data container 'p_data' will be populated with
	// all the data arrays from the weather file
	
	ssc_module_free( p_mod );
	
	
	/*******************************
		create irradiance processor module and setup inputs
		************************************/
		
		
	p_mod = ssc_module_create( "irradproc" );
	if ( p_mod == 0 )
	{
		printf("could not create 'irradproc' model\n");
		return -2;
	}
	
	// set up inputs for the irradiance processor
	// the inputs below are the minimum required, there are many more advanced options
	ssc_data_set_number( p_data, "track_mode", 0);
	ssc_data_set_number( p_data, "azimuth", 180);
	ssc_data_set_number( p_data, "tilt", 40);
	
	
	if ( !ssc_module_exec_with_handler( p_mod, p_data, my_handler, 0 ) )
	{
		printf("failed to process irradiance data...\n");
		return -1;
	}
	
	
	ssc_module_free( p_mod );
	
	
	/*******************************
		create pvwatts module and setup inputs
		************************************/
	
	p_mod = ssc_module_create( "pv6parmod" );
	if ( p_mod == 0 )
	{
		printf("could not create 'pv6parmod' model\n");
		return -2;
	}
	
	// set up inputs for the 6 parameter pv module model
	// uses all the data outputted by the weather reader and irradiance processor
	// to calculate the energy production
	ssc_data_set_number( p_data, "area", 1.3 );
	ssc_data_set_number( p_data, "tnoct", 46 );
	ssc_data_set_number( p_data, "vmp", 30);
	ssc_data_set_number( p_data, "imp", 6);
	ssc_data_set_number( p_data, "voc", 37);
	ssc_data_set_number( p_data, "isc", 7);
	ssc_data_set_number( p_data, "beta_voc", -0.11);
	ssc_data_set_number( p_data, "alpha_isc", 0.004);
	ssc_data_set_number( p_data, "gamma_pmp", -0.41);
	ssc_data_set_number( p_data, "a", 1.56708);
	ssc_data_set_number( p_data, "Il", 7.06232);
	ssc_data_set_number( p_data, "Io", 3.44881e-010);
	ssc_data_set_number( p_data, "rs", 0.377915);
	ssc_data_set_number( p_data, "rsh", 42.4459);
	ssc_data_set_number( p_data, "adj", 25.3809);
	
		
	if ( !ssc_module_exec_with_handler( p_mod, p_data, my_handler, 0 ) )
	{
		printf("failed to run pv6parmod...\n");
		return -1;
	}
	
	ssc_module_free( p_mod );
	
	
	// dc output is only for one module.
	// assume we have 7 modules in a string, feeding 1 inverter
	
	int mod_per_string = 7;
	int strings_in_parallel = 1;
	
	int i, count = 0;
	ssc_number_t *dcpwr = ssc_data_get_array( p_data, "dc", &count );
	ssc_number_t *dcvol = ssc_data_get_array( p_data, "dc_voltage", &count );
	
	if ( !dcpwr || !dcvol || count == 0)
	{
		printf("could not obtain dc power and voltage\n");
		return -1;
	}
	
	for (i=0;i<count;i++)
	{
		dcpwr[i] *= mod_per_string * strings_in_parallel;
		dcvol[i] *= mod_per_string;
	}
	
	// now dcpower and dcvoltage are representative of the larger array, feed to inverter.
	
	p_mod = ssc_module_create( "pvsandiainv" );
	if ( p_mod == 0 )
	{
		printf("could not create 'pvsandiainv' model\n");
		return -2;
	}
	
	// set inverter model parameters
	ssc_data_set_number( p_data, "paco", 1100);
	ssc_data_set_number( p_data, "pdco", 1214.68);
	ssc_data_set_number( p_data, "vdco", 199.33);
	ssc_data_set_number( p_data, "pso", 14.67);
	ssc_data_set_number( p_data, "pntare", 0.13);
	ssc_data_set_number( p_data, "c0", -4.443e-5);
	ssc_data_set_number( p_data, "c1", 4.4787e-5);
	ssc_data_set_number( p_data, "c2", -0.000131677);
	ssc_data_set_number( p_data, "c3", -0.00221685);
			
	if ( !ssc_module_exec_with_handler( p_mod, p_data, my_handler, 0 ) )
	{
		printf("failed to run pvsandiainv...\n");
		return -1;
	}
	
	ssc_module_free( p_mod );
	
	
	/*******************************
		show some outputs from pvwatts
		************************************/
	
	const ssc_number_t *p_ac = ssc_data_get_array( p_data, "ac", &count );
	
	double ac_sum = 0;
	for (i=0;i<count;i++)
		ac_sum += p_ac[i];
		
	printf("success: total ac=%lg\n", ac_sum );
		
	ssc_data_free( p_data );
	
	return 0;
}




	/*
	
	// extract and print out information about the variables
	// this model knows about
	
	
	int i=0;
	ssc_info_t p_inf = NULL;
	while ( (p_inf = ssc_module_var_info( p_mod, i++ )) )
	{
		int var_type = ssc_info_var_type( p_inf );   // SSC_INPUT, SSC_OUTPUT, SSC_INOUT
		int data_type = ssc_info_data_type( p_inf ); // SSC_STRING, SSC_NUMBER, SSC_ARRAY, SSC_MATRIX
		
		const char *name = ssc_info_name( p_inf );
		const char *label = ssc_info_label( p_inf );
		const char *units = ssc_info_units( p_inf );
		const char *meta = ssc_info_meta( p_inf );
		const char *group = ssc_info_group( p_inf );
		
		if ( var_type == SSC_INPUT )
			printf("[%s]:%d   %s  %s %s\n", name, data_type, label, units, meta);
	}
	*/
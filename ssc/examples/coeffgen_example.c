#include <stdio.h>
#include <stdlib.h>

#include "sscapi.h"


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
	else
		return 0;
}

int main(int argc, char *argv[])
{
	
	// create an empty SSC data container
	ssc_data_t p_data = ssc_data_create();
	
	
	/*******************************
		create 6 parameter solver module and setup inputs
		************************************/
		
	ssc_module_t p_mod = ssc_module_create( "6parsolve" );
	if ( p_mod == 0 )
	{
		printf("could not create '6parsolve' model\n");
		return -2;
	
	}
	
	// cell type:monoSi,multiSi,cis,cigs,cdte,amorphous
	ssc_data_set_string( p_data, "celltype", "multiSi" );
	ssc_data_set_number( p_data, "Vmp", 30 );
	ssc_data_set_number( p_data, "Imp", 6 );
	ssc_data_set_number( p_data, "Voc", 37 );
	ssc_data_set_number( p_data, "Isc", 7 );
	ssc_data_set_number( p_data, "alpha_isc", 0.004 );
	ssc_data_set_number( p_data, "beta_voc", -0.11 );
	ssc_data_set_number( p_data, "gamma_pmp", -0.41 );
	ssc_data_set_number( p_data, "Nser", 60 );
	
	if ( !ssc_module_exec_with_handler( p_mod, p_data, my_handler, 0 ) )
	{
		printf("failed to solve for coefficients...\n");
		return -1;
	}
	
	// at this point, the data container 'p_data' will be populated with
	// all the calculated coefficients
	
	ssc_module_free( p_mod );
	
	
	ssc_number_t a, Il, Io, Rs, Rsh, Adj;
	
	ssc_data_get_number( p_data, "a", &a );
	ssc_data_get_number( p_data, "Il", &Il );
	ssc_data_get_number( p_data, "Io", &Io );
	ssc_data_get_number( p_data, "Rs", &Rs );
	ssc_data_get_number( p_data, "Rsh", &Rsh );
	ssc_data_get_number( p_data, "Adj", &Adj );
	
	printf("solved coeffs: a=%lg Il=%lg Io=%lg Rs=%lg Rsh=%lg Adj=%lg\n",
		(double)a, (double)Il, (double)Io, (double)Rs, (double)Rsh, (double)Adj);
	
	ssc_data_free( p_data );
	
	return 0;
}

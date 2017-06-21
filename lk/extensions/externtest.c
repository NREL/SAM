#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "../lk_invoke.h"

// DLL functions look like
LK_FUNCTION( externtest )
{
	LK_DOCUMENT("externtest", "Tests basic external calling of dynamically loaded dll functions.", "(string, string):number" );
	
	if (lk_arg_count() < 2)
	{
		lk_error( "too few arguments passed to externtest. two strings required!" );
		return;
	}
	
	const char *s1 = lk_as_string( lk_arg( 0 ) );
	if (s1 == 0)
	{
		lk_error( "s1 null" );
		return;
	}
	int len1 = strlen(s1);
	
	const char *s2 = lk_as_string( lk_arg(1) );
	if (s2 == 0)
	{
		lk_error( "s2 null" );
		return;
	}
	int len2 = strlen( s2 );
		
	lk_return_number( len1 + len2 );
}


LK_FUNCTION( sumify )
{
   LK_DOCUMENT( "sumify", "Sums up all the parameters passed.", "(...):number" );

   double val = 0;
   int i;
   int count = lk_arg_count();
   if (count < 1)
   {
      lk_error("sum() must be provided more than zero arguments");
      return;
   }

   for (i=0;i<count;i++)
   {
      lk_var_t x = lk_arg( i );
      val = val + lk_as_number( x );
   }

   lk_var_t ret = lk_result();
   lk_set_number( ret, val );
}

LK_FUNCTION( tabulate )
{
	LK_DOCUMENT( "tabulate", "Converts an array to a table.", "(array):table");
	
	lk_var_t arr, ret;
	int len, i;
	char key[64];
	
	if (lk_arg_count() != 1
		|| lk_type( lk_arg(0) ) != LK_ARRAY)
	{
		lk_error("tabulate() requires one array parameter");
		return;
	}
	
	ret = lk_result();
	lk_make_table(ret);
	
	arr = lk_arg(0);
	len = lk_length( arr );
	
	for (i=0;i<len;i++)
	{
		sprintf(key, "item%d", i);
		
		lk_var_t item = lk_index(arr, i);
		int ty = lk_type(item);
		switch(ty)
		{
		case LK_NUMBER:
			lk_table_set_number( ret, key, lk_as_number( item ) );
			break;
		case LK_STRING:
			lk_table_set_string( ret, key, lk_as_string( item ) );
			break;
			
		case LK_ARRAY:
		case LK_TABLE:
			lk_error("arrays and tables not currently copied. exercise for the user!");
			return;
		}
	}		
}

void steam_psat( struct __lk_invoke_t *lk )
{
	LK_DOCUMENT("steam_psat", "Returns the saturation pressure (kPa) of steam at a given temperature in deg C", "(number:Tc):number");
	
	if ( lk_arg_count() < 1 )
	{
		lk_error( "insufficient arguments provided");
		return;
	}
	
	double T = lk_as_number( lk_arg(0) ) + 273.15;
	
	double Tc = 647.096;
	double Pc = 22.064;
	
	double a1 = -7.85954783;
	double a2 = 1.84408259;
	double a3 = -11.7866497;
	double a4 = 22.6807411;
	double a5 = -15.9618719;
	double a6 = 1.80122502;
	double tau = 1 - T/Tc;
	double tauh = sqrt(tau);
	
	double P = Pc * exp( Tc/T*(a1*tau 
		+ a2*tau*tauh 
		+ a3*tau*tau*tau 
		+ a4*tau*tau*tau*tauh 
		+ a5*tau*tau*tau*tau 
		+ a6*tau*tau*tau*tau*tau*tau*tau*tauh) );
		
	lk_return_number( P*1000 );
}


LK_BEGIN_EXTENSION()
	externtest,
	sumify,
	tabulate,
	steam_psat
LK_END_EXTENSION()
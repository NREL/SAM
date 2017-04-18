#include <math.h>
#include <cmath>
#include <limits>
#include "lib_pvinv.h"



partload_inverter_t::partload_inverter_t( )
{
	Paco = Pdco = Pntare = std::numeric_limits<double>::quiet_NaN();
}

bool partload_inverter_t::acpower(
	/* inputs */
	double Pdc,     /* Input power to inverter (Wdc) */

	/* outputs */
	double *Pac,    /* AC output power (Wac) */
	double *Ppar,   /* AC parasitic power consumption (Wac) */
	double *Plr,    /* Part load ratio (Pdc_in/Pdc_rated, 0..1) */
	double *Eff,	    /* Conversion efficiency (0..1) */
	double *Pcliploss, /* Power loss due to clipping loss (Wac) */
	double *Pntloss /* Power loss due to night time tare loss (Wac) */
	)
{

	if ( Pdco <= 0 ) return false;

	// handle limits - can send error back or record out of range values
//	if ( Pdc < 0 ) Pdc = 0;
//	if ( Pdc > Pdco ) Pdc = Pdco;

	// linear interpolation based on Pdc/Pdco and *Partload and *Efficiency arrays
	double x = 100.0 * Pdc / Pdco; // percentages in partload ratio

	int n = Partload.size();

	bool ascnd = (Partload[n-1] > Partload[0]); // check ascending order
	int ndx;
	int nu = n;
	int nl = 0;

	// Numerical Recipes in C p.117
	while ( (nu-nl) > 1 )
	{
		ndx = (nu + nl) >> 1; // divide by 2
		if ( x >= Partload[ndx] == ascnd )
			nl = ndx;
		else 
			nu = ndx;
	}
	if ( x == Partload[0] )
		ndx = 0;
	else if ( x == Partload[n-1] )
		ndx = n-1;
	else
		ndx = nl;

	// check in range
	if (ndx >= (n-1))
		ndx = n-2;
	if ( ndx < 0 ) 
		ndx =0;

	// x between Partload[ndx] and Partload[ndx-1]
	if ( x > Partload[ndx] )
		*Eff = Efficiency[ndx] + ((Efficiency[ndx+1] - Efficiency[ndx]) / 
									(Partload[ndx+1] - Partload[ndx] )) * (x - Partload[ndx]);
	else
		*Eff = Efficiency[ndx];

	if ( *Eff < 0.0 ) *Eff = 0.0;

	*Eff /= 100.0; // user data in percentages

	*Pac = *Eff * Pdc;
	*Ppar = 0.0;

	// night time power loss Wac
	*Pntloss = 0.0;
	if (Pdc <= 0.0)
	{
		*Pac = -Pntare;
		*Ppar = Pntare;
		*Pntloss = Pntare;
	}

	// clipping loss Wac
	*Pcliploss = 0.0;
	double PacNoClip = *Pac;
	if ( *Pac > Paco )	
	{
		*Pac = Paco;
		*Pcliploss = PacNoClip - *Pac;
	}

	*Plr = Pdc / Pdco;

	return true;
}




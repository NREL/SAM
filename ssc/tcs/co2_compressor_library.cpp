#include "co2_compressor_library.h"

double compressor_psi_polynomial_fit(int comp_type, double phi)
{
	switch( comp_type )
	{
	case 1:				// Sandia compressor from John Dyreby's work
		return (((-498626.0*phi + 53224.0)*phi - 2505.0)*phi + 54.6)*phi + 0.04049;		// from performance map curve fit

	default:
		return (((-498626.0*phi + 53224.0)*phi - 2505.0)*phi + 54.6)*phi + 0.04049;		// from performance map curve fit

	}
}

double compressor_eta_polynomial_fit(int comp_type, double phi)
{
	switch( comp_type )
	{
	case 1:
		return (((-1.638E6*phi + 182725.0)*phi - 8089.0)*phi + 168.6)*phi - 0.7069;		// from performance map curve fit

	default:
		return (((-1.638E6*phi + 182725.0)*phi - 8089.0)*phi + 168.6)*phi + 0.7069;		// from performance map curve fit
	}
}

void get_compressor_parameters(int comp_type, double & phi_design, double & phi_min, double & phi_max)
{
	switch( comp_type )
	{
	case 1:				// Sandia compressor from John Dyreby's work
		phi_design = 0.0297;
		phi_min = 0.021;
		phi_max = 0.05;
		return;

	default:
		phi_design = 0.0297;
		phi_min = 0.021;
		phi_max = 0.05;
		return;
	}
}
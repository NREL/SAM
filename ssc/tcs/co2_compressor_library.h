#ifndef __CO2_lib_
#define __CO2_lib_

double compressor_psi_polynomial_fit(int comp_type, double phi);

double compressor_eta_polynomial_fit(int comp_type, double phi);

void get_compressor_parameters(int comp_type, double & phi_design, double & phi_min, double & phi_max);

#endif

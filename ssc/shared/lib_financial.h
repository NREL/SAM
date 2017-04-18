#ifndef __lib_financial_h
#define __lib_financial_h

#include <vector>

namespace libfin {

double irr(double tolerance, int maxIterations, const std::vector<double> &CashFlows, int Count);
double npv(double Rate, const std::vector<double> &CashFlows, int Count);
double payback(const std::vector<double> &CumulativePayback, const std::vector<double> &Payback, int Count);

double pow1pm1 (double x, double y);
double pow1p (double x, double y); 
double fvifa (double rate, double nper); 
double pvif (double rate, double nper); 
double pmt (double rate, double nper, double pv, double fv, int type); 
double ipmt (double rate, double per, double nper, double pv, double fv, int type) ;
double ppmt (double rate, double per, double nper, double pv, double fv, int type) ;

long round_dhf(double number);

}

#endif

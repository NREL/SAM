#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <vector>
#include "nlopt.hpp"

using namespace std;

class testoptclass
{
	int call_count;
public:
	testoptclass();

	void reset_counter();

	void random_start(vector<double> &x, vector<vector<double> > &range);

	double memfunc(unsigned n, const double *x, double *grad, void *my_func_data);
	
	double styb_tang_test(unsigned n, const double *x, double *grad, void *data);

	double rosenbrock_test(unsigned n, const double *x, double *grad, void *data);

	double matyas_test(unsigned n, const double *x, double *grad, void *data);

	int get_call_count();
};
//
//double nlopt_callback(unsigned n, const double *x, double *grad, void *data){
//	testoptclass *frame = static_cast<testoptclass*>( data );
//	if(frame != NULL) return frame->styb_tang_test(n, x, grad, data);
//}
//
//
//void test_optimization(){
//	int ndim = 4;
//
//	/* 
//	GN_ORIG_DIRECT 
//	GN_ORIG_DIRECT_L  1
//	GN_DIRECT
//	GN_DIRECT_L
//	GN_CRS2_LM
//	GN_ISRES
//	GN_ESCH
//	LN_COBYLA
//	LN_BOBYQA
//	LN_PRAXIS
//	LN_NELDERMEAD 1
//	LN_SBPLX
//	*/
//
//	nlopt::opt opt(nlopt::GN_ORIG_DIRECT_L, ndim); /* Algorithm and dimension */
//	
//
//	vector<double> 
//		ub(ndim),
//		lb(ndim);
//	for(int i=0; i<ndim; i++){
//		ub.at(i) = 10.;
//		lb.at(i) = -10.;
//	}
//	opt.set_lower_bounds(lb);
//	opt.set_upper_bounds(ub);
//
//	testoptclass P;
//		
//	opt.set_min_objective(nlopt_callback, &P);// NULL);
//	//opt.set_min_objective(myfunc, NULL);
//
//	/*my_constraint_data data[2] = { {2,0}, {-1,1} };
//	opt.add_inequality_constraint(myconstraint, &data[0], 1e-8);
//	opt.add_inequality_constraint(myconstraint, &data[1], 1e-8);*/
//
//	opt.set_xtol_rel(.001);
//	opt.set_xtol_abs(.001);
//	
//	std::vector<double> x(ndim);
//	vector<vector<double> > xrange;
//	for(int i=0; i<ndim; i++){
//		vector<double> trange(2);
//		trange.at(0) = lb.at(i);
//		trange.at(1) = ub.at(i);
//		xrange.push_back(trange);
//	}
//	double minf;
//
//	int nstart = 20;
//
//	vector<vector<double> > xopts;
//	vector<double> fopts;
//	vector<int> ncalls;
//
//	double fminall=9e9;
//	int iminall=0;
//	int countall=0;
//
//	for(int i=0; i<nstart; i++){
//		P.reset_counter();
//
//		P.random_start(x, xrange);
//		
//		nlopt::result result = opt.optimize(x, minf);
//
//		int ncall = P.get_call_count();
//
//		xopts.push_back(x);
//		fopts.push_back(minf);
//		ncalls.push_back(ncall);
//
//		countall += ncall;
//
//		//keep track of overall min
//		if(minf < fminall){
//			fminall=minf;
//			iminall = i;
//		}
//	}
//
//	fminall *= 1./(double)ndim;
//	double *xstar = new double[ndim];
//	for(int i=0; i<ndim; i++){
//		xstar[i] = xopts.at(iminall).at(i);
//	}
//}


//typedef struct {
//    double a, b;
//} my_constraint_data;
//
//double myconstraint(unsigned n, const double *x, double *grad, void *data)
//{
//    my_constraint_data *d = (my_constraint_data *) data;
//    double a = d->a, b = d->b;
//    if (grad) {
//        grad[0] = 3 * a * (a*x[0] + b) * (a*x[0] + b);
//        grad[1] = -1.0;
//    }
//    return ((a*x[0] + b) * (a*x[0] + b) * (a*x[0] + b) - x[1]);
// }

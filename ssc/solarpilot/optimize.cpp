#include "optimize.h"



testoptclass::testoptclass(){
	call_count = 0;
	srand( (unsigned int)time(NULL) );
}

void testoptclass::reset_counter(){call_count = 0;}

void testoptclass::random_start(vector<double> &x, vector<vector<double> > &range){
	int n = (int)x.size();
	for(int i=0; i<n; i++){
		double 
			rmax = range.at(i).at(1),
			rmin = range.at(i).at(0);
		int r = rand();
		double fr = (double)r/(double)RAND_MAX;
		x.at(i) = rmin + fr*(rmax - rmin);
	}
}

double testoptclass::memfunc(unsigned n, const double *x, double *grad, void *my_func_data){
	call_count ++;
	return sqrt(x[1]);
};


double testoptclass::styb_tang_test(unsigned n, const double *x, double *grad, void *data){
	/* x* = {-2.903534, .....}, f(x*) = -39.16599*n */
	double y=0.;
	for(unsigned i=0; i<n; i++){
		y+= pow(x[i],4)-16.*pow(x[i],2) + 5*x[i];
	}
	y *= 0.5;
	call_count ++;
	return y;
}

double testoptclass::rosenbrock_test(unsigned n, const double *x, double *grad, void *data){
	double y=0.;

	for(unsigned i=1; i<n; i++){
		//y += 100. * pow(x[i] - pow(x[i-1],2),2) + pow(x[i-1]-1.,2);
		y += pow(x[i] - pow(x[i-1],2),2) + pow(x[i-1]-1.,2);
	}
	call_count ++;
	return y;
};

double testoptclass::matyas_test(unsigned n, const double *x, double *grad, void *data){
	/* Convex.. Valid from -10..10. */
	call_count++;
	double y=0.;
	for(unsigned i=0; i<n; i++){
		y+= 0.26 * pow(fabs(x[i]),(double)n);
	}
	double xx=0.48;
	for(unsigned i=0; i<n; i++){
		xx *= x[i];
	}
	y += - xx;
	return y;

}

int testoptclass::get_call_count(){return call_count;}


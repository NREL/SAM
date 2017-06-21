#include "lib_pvmodel.h"
#include <math.h>
#include <limits>
#include <iostream>

#ifndef M_PI
#define M_PI 3.14159265358979323846264338327
#endif


pvinput_t::pvinput_t()
{
	Ibeam = Idiff = Ignd = Tdry = poaIrr= Tdew = Wspd = Wdir = Patm
		= Zenith = IncAng = Elev 
		= Tilt = Azimuth = HourOfDay = std::numeric_limits<double>::quiet_NaN();

	radmode = 0;
	usePOAFromWF = false;
}


pvinput_t::pvinput_t( double ib, double id, double ig, double ip,
		double ta, double td, double ws, double wd, double patm,
		double zen, double inc, 
		double elv, double tlt, double azi,
		double hrday, int rmode, bool up )
{
	Ibeam = ib;
	Idiff = id;
	Ignd = ig;
	poaIrr = ip;
	Tdry = ta;
	Tdew = td;
	Wspd = ws;
	Wdir = wd;
	Patm = patm;
	Zenith = zen;
	IncAng = inc;
	Elev = elv;
	Tilt = tlt;
	Azimuth = azi;
	HourOfDay = hrday;
	radmode = rmode;
	usePOAFromWF = up;
}

std::string pvcelltemp_t::error()
{
	return m_err;
}

pvoutput_t::pvoutput_t()
{
	Power = Voltage = Current = Efficiency
		= Voc_oper = Isc_oper = CellTemp = std::numeric_limits<double>::quiet_NaN();
}


pvoutput_t::pvoutput_t( double p, double v,
		double c, double e, 
		double voc, double isc, double t )
{
	Power = p;
	Voltage = v;
	Current = c;
	Efficiency = e;
	Voc_oper = voc;
	Isc_oper = isc;
	CellTemp = t;
}


std::string pvmodule_t::error()
{
	return m_err;
}

spe_module_t::spe_module_t( )
{
	VmpNominal = 0;
	VocNominal = 0;
	Area = 0;
	Gamma = 0;
	Reference = 0;
	fd = 1;
	for (int i=0;i<5;i++)
		Eff[i] = Rad[i] = 0;
}

	
double spe_module_t::eff_interpolate( double irrad, double rad[5], double eff[5] )
{
	if ( irrad < rad[0] )
		return eff[0];
	else if ( irrad > rad[4] )
		return eff[4];

	int i = 1;
	for ( i=1;i<5;i++ )
		if ( irrad < rad[i] ) break;
      
	int i1 = i-1;

	double wx=(irrad-rad[i1])/(rad[i]-rad[i1]);
	return (1-wx)*eff[i1]+wx*eff[i];
}

bool spe_module_t::operator() ( pvinput_t &input, double TcellC, double opvoltage, pvoutput_t &output)
{
	double idiff = fd*(input.Idiff + input.Ignd);

	// Sev 2015-09-14: Changed to allow POA data directly
	double dceff, dcpwr;
	if( input.radmode != 3  || !input.usePOAFromWF){
		dceff = eff_interpolate( input.Ibeam + idiff, Rad, Eff );
		dcpwr = dceff*(input.Ibeam+idiff)*Area;	
	}
	else{
		dceff = eff_interpolate( input.poaIrr, Rad, Eff );
		dcpwr = dceff*(input.poaIrr)*Area;
	}

	dcpwr += dcpwr*(Gamma/100.0)*(TcellC - 25.0);
	if (dcpwr < 0) dcpwr = 0;

	output.CellTemp = TcellC;
	output.Efficiency = dceff;	
	output.Power = dcpwr;
	output.Voltage = VmpRef();
	output.Current = output.Power / output.Voltage;
	output.Isc_oper = IscRef();
	output.Voc_oper = VocRef();
	return true;
}




/******** BEGIN GOLDEN METHOD CODE FROM NR3 *********/

#define GOLD 1.618034
#define GLIMIT 100.0
#define TINY 1.0e-20
#define SHFT(a,b,c,d) (a)=(b);(b)=(c);(c)=(d);
#define FMAX(a,b) ((a)>(b)?(a):(b))
#define SIGN(a,b) ((b) >= 0.0 ? fabs(a) : -fabs(a))

static void mnbrak(double *ax, double *bx, double *cx, double *fa, double *fb, double *fc,
	double (*func)(double, void *), void *data)
{
	double ulim,u,r,q,fu,dum;

	*fa=(*func)(*ax, data);
	*fb=(*func)(*bx, data);
	if (*fb > *fa) {
		SHFT(dum,*ax,*bx,dum)
		SHFT(dum,*fb,*fa,dum)
	}
	*cx=(*bx)+GOLD*(*bx-*ax);
	*fc=(*func)(*cx,data);
	while (*fb > *fc) {
		r=(*bx-*ax)*(*fb-*fc);
		q=(*bx-*cx)*(*fb-*fa);
		u=(*bx)-((*bx-*cx)*q-(*bx-*ax)*r)/
			(2.0*SIGN(FMAX(fabs(q-r),TINY),q-r));
		ulim=(*bx)+GLIMIT*(*cx-*bx);
		if ((*bx-u)*(u-*cx) > 0.0) {
			fu=(*func)(u,data);
			if (fu < *fc) {
				*ax=(*bx);
				*bx=u;
				*fa=(*fb);
				*fb=fu;
				return;
			} else if (fu > *fb) {
				*cx=u;
				*fc=fu;
				return;
			}
			u=(*cx)+GOLD*(*cx-*bx);
			fu=(*func)(u,data);
		} else if ((*cx-u)*(u-ulim) > 0.0) {
			fu=(*func)(u,data);
			if (fu < *fc) {
				SHFT(*bx,*cx,u,*cx+GOLD*(*cx-*bx))
				SHFT(*fb,*fc,fu,(*func)(u,data))
			}
		} else if ((u-ulim)*(ulim-*cx) >= 0.0) {
			u=ulim;
			fu=(*func)(u,data);
		} else {
			u=(*cx)+GOLD*(*cx-*bx);
			fu=(*func)(u,data);
		}
		SHFT(*ax,*bx,*cx,u)
		SHFT(*fa,*fb,*fc,fu)
	}
}
#undef GOLD
#undef GLIMIT
#undef TINY
#undef SHFT
#undef NRANSI

#define R 0.61803399
#define C (1.0-R)
#define SHFT2(a,b,c) (a)=(b);(b)=(c);
#define SHFT3(a,b,c,d) (a)=(b);(b)=(c);(c)=(d);

static bool golden(double ax, double bx, double (*f)(double,void*), void *data, double tol, double *xmin, double *Result, int maxiter )
{
	double f1,f2,x0,x1,x2,x3,cx, fa, fb, fc;
	int ni = 0;
	double ax0(ax), bx0(bx);
	mnbrak(&ax, &bx, &cx, &fa, &fb, &fc, f, data );

	// in rare cases mnbrak returns values beyond original bounds???
	if ( ax < ax0 ) ax = ax0;
	if ( ax > bx0 ) ax = bx0;
	if ( bx < ax0 ) bx = ax0;
	if ( bx > bx0 ) bx = bx0;

	x0=ax;
	x3=cx;
	if (fabs(cx-bx) > fabs(bx-ax)) {
		x1=bx;
		x2=bx+C*(cx-bx);
	} else {
		x2=bx;
		x1=bx-C*(bx-ax);
	}
	f1=(*f)(x1,data);
	f2=(*f)(x2,data);
	while (fabs(x3-x0) > tol*(fabs(x1)+fabs(x2))) {
		if (f2 < f1) {
			SHFT3(x0,x1,x2,R*x1+C*x3)
			SHFT2(f1,f2,(*f)(x2,data))
		} else {
			SHFT3(x3,x2,x1,R*x2+C*x0)
			SHFT2(f2,f1,(*f)(x1,data))
		}

		if (ni++ > maxiter) return false;
	}
	if (f1 < f2) {
		*xmin=x1;
		*Result = f1;
		return true;
	} else {
		*xmin=x2;
		*Result = f2;
		return true;
	}
}
#undef C
#undef R
#undef SHFT2
#undef SHFT3

/******** END GOLDEN METHOD CODE FROM NR2 *********/

#define max(a,b) ((a)>(b)?(a):(b))

double current_5par( double V, double IMR, double A, double IL, double IO, double RS, double RSH )
{
/*
	C     Iterative solution for current as a function of voltage using
	C     equations from the five-parameter model.  Newton's method is used
	C     to converge on a value.  Max power at reference conditions is initial
	C     guess. 
*/
	double IOLD = 0.0;
	double V_MODULE = V;	
	
	//C**** first guess is max.power point current
	double INEW = IMR;
	const int maxit = 4000;
	int it = 0;
	while( fabs(INEW-IOLD) > 0.0001)
	{
		IOLD = INEW;
		double F = IL-IOLD-IO*(exp((V_MODULE+IOLD*RS)/A)-1.0) - (V_MODULE+IOLD*RS)/RSH;
		double FPRIME = -1.0-IO*(RS/A)*exp((V_MODULE+IOLD*RS)/A)-(RS/RSH);
		INEW = max(0.0,(IOLD-(F/FPRIME)));
		if ( it++ == maxit ) 
			return -1.0;
	}
	
	return INEW;
}

double openvoltage_5par( double Voc0, double a, double IL, double IO, double Rsh )
{
/*
	C     Iterative solution for open-circuit voltage.  Explicit algebraic solution
	C     not possible in 5-parameter model
*/	
	double VocLow = 0;
	double VocHigh = Voc0 * 1.5;
	
	double Voc = Voc0; // initial guess
	
	int niter = 0;
	while( fabs(VocHigh-VocLow) > 0.001 )
	{
		double I = IL - IO*(exp(Voc/a)-1) - Voc/Rsh;
		if (I < 0) VocHigh = Voc;
		if (I > 0) VocLow = Voc;
		Voc = (VocHigh+VocLow)/2;

		if (++niter > 5000)
			return -1.0;
	}
	return Voc;	
}


struct refparm { double a, Il, Io, Rs, Rsh; };

static double powerfunc( double V, void *_d )
{
	struct refparm *r = (struct refparm*)_d;
	return -V*current_5par( V, 0.9*r->Il, r->a, r->Il, r->Io, r->Rs, r->Rsh );
}

double maxpower_5par( double Voc_ubound, double a, double Il, double Io, double Rs, double Rsh, double *__Vmp, double *__Imp )
{
	double P, V, I;
	struct refparm refdata;
	refdata.a = a;
	refdata.Il = Il;
	refdata.Io = Io;
	refdata.Rs = Rs;
	refdata.Rsh = Rsh;

	int maxiter = 5000;
				
	if (golden( 0, Voc_ubound, powerfunc, &refdata, 1e-4, &V, &P, maxiter))
	{
		P = -P;				
		I = 0;
		if (V != 0) I=P/V;
	}
	else
		P = V = I = -999;

	if ( __Vmp ) *__Vmp = V;
	if ( __Imp ) *__Imp = I;
	return P;
}

double transmittance( double theta1_deg, /* incidence angle of incoming radiation (deg) */
		double n_cover,  /* refractive index of cover material, n_glass = 1.586 */
		double n_incoming, /* refractive index of incoming material, typically n_air = 1.0 */
		double k,        /* proportionality constant assumed to be 4 (1/m) for derivation of Bouguer's law (set to zero to skip bougeur's law */
		double l_thick,  /* material thickness (set to zero to skip Bouguer's law */
		double *_theta2_deg ) /* thickness of cover material (m), usually 2 mm for typical module */
{
	// reference: duffie & beckman, Ch 5.3
	
	double theta1 = theta1_deg * M_PI/180.0;
	double theta2 = asin( n_incoming / n_cover * sin(theta1 ) ); // snell's law, assuming n_air = 1.0
	// fresnel's equation for non-reflected unpolarized radiation as an average of perpendicular and parallel components
	double tr = 1 - 0.5 *
			( pow( sin(theta2-theta1), 2 )/pow( sin(theta2+theta1), 2)
			+ pow( tan(theta2-theta1), 2 )/pow( tan(theta2+theta1), 2 ) );
	
	if ( _theta2_deg ) *_theta2_deg = theta2 * 180/M_PI;

	return tr * exp( -k * l_thick / cos(theta2) );
}

double iam( double theta, bool ar_glass )
{
	if ( theta < AOI_MIN ) theta = AOI_MIN;
	if ( theta > AOI_MAX ) theta = AOI_MAX;

	double normal = iam_nonorm( 1, ar_glass );
	double actual = iam_nonorm( theta, ar_glass );
	return actual/normal;	
}

double iam_nonorm( double theta, bool ar_glass )
{
	double n_air = 1.0;

	double n_g = 1.526;
	double k_g = 4;
	double l_g = 0.002;

	double n_arc = 1.3;
	double k_arc = 4;
	double l_arc = l_g*0.01;  // assume 1/100th thickness of glass for AR coating

	if ( theta < AOI_MIN ) theta = AOI_MIN;
	if ( theta > AOI_MAX ) theta = AOI_MAX;

	if ( ar_glass )
	{
		double theta2 = 1;
		double tau_coating = transmittance( theta, n_arc, n_air, k_arc, l_arc, &theta2 );
		double tau_glass = transmittance( theta2, n_g, n_arc, k_g, l_g );
		return tau_coating*tau_glass;
	}
	else
	{
		return transmittance(theta, n_g, n_air, k_g, l_g );
	}
}
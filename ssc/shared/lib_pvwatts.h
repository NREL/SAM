#ifndef __lib_pvwatts_h
#define __lib_pvwatts_h

#define PVWATTS_INOCT (45.0+273.15)
#define PVWATTS_HEIGHT 5.0
#define PVWATTS_REFTEM 25.0
#define PVWATTS_PWRDGR -0.005
#define PVWATTS_EFFFP 0.92
#define PVWATTS_ROTLIM 45.0
#define PVWATTS_ALBEDO 0.2

double transpoa( double poa,double dn,double inc, bool ar_glass );
double dcpowr(double reftem,double refpwr,double pwrdgr,double tmloss,double poa,double pvt, double iref);
double dctoac(double pcrate,double efffp,double dc);

class pvwatts_celltemp
{
	/*  
	This class was converted from the original PVWatts subroutine
	to store the previous irradiance and cell temp to make the model
	give consistent results with the online PVWatts V1.  Previously
	all calculations were done over the course of a day, so the
	previous module/sun values were saved, but in the single time stamp
	version, these changes were not tracked.  apd 2/24/2012

	Defines function to calculate cell temperature, changed 8/22/2007 to
	 work with single time stamp data, also see pvsubs2.c
	
	This function was converted from a PVFORM version 3.3 subroutine
c     this routine estimates the array temperature given the poa radiation,
c     ambient temperature, and wind speed.  it uses an advanced cell temp
c     model developed by m fuentes at snla.  if the poa insolation is eq
c     zero then set cell temp = 999.
c
	passed variables:
		inoct = installed nominal operating cell temperature (deg K)
		height = average array height (meters)
		poa2 = plane of array irradiances (W/m2)
		ws2 = wind speeds (m/s)
		ambt2 = ambient temperatures (deg C)

c  local variables :
c     absorb = absorbtivity
c     backrt = ratio of actual backside heat xfer to theoretical of rack mount
c     boltz = boltzmann's constant
c     cap = capacitance per unit area of module
c     capo = capacitance per unit area of rack mounted module
c     conair = conductivity of air
c     convrt = ratio of total convective heat xfer coef to topside hxc
c     denair = density of air
c     dtime = time step
c     eigen = product of eigen value and time step
c     emmis = emmisivity
		ex = ?
c     grashf = grashoffs number
c     hconv = convective coeff of module (both sides)
c     hforce = forced convective coeff of top side
c     hfree = free convective coeff of top side
c     hgrnd = radiative heat xfer coeff from module to ground
		hsky = ?
c     iflagc = flag to check if routine has been executed
c     reynld = reynolds number
c     suun = insolation at start of time step
c     suno = previous hours insolation
c     tamb = ambient temp
c     tave = average of amb and cell temp
c     tgrat = ratio of grnd temp above amb to cell temp above amb
c     tgrnd = temperature of ground
c     tmod = computed cell temp
c     tmodo = cell temp for previous time step
c     tsky = sky temp
c     visair = viscosity of air
c     windmd = wind speed at module height
c     xlen = hydrodynamic length of module              */

private:
	int j;
	double height, inoct;
	double absorb,backrt,boltz,cap,capo,conair,convrt,denair;
	double dtime,eigen,emmis,grashf,hconv,hforce,hfree,hgrnd,reynld,suun;
	double suno,tamb,tave,tgrat,tgrnd,tmod,tmodo,tsky,visair,windmd,xlen;
	double hsky,ex;
public:
	pvwatts_celltemp( double _inoct, double _height, double _dTimeHrs);
	double operator() ( double poa2, double ws2, double ambt2, double fhconv = 1.0 );
	void set_last_values( double Tc, double poa );
};

#endif

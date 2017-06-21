#ifndef cec6par_h
#define cec6par_h

#include "lib_pvmodel.h"



/*
   Implementation of CEC 5 (6) parameter model as presented by
   DeSoto, Klein, and Beckman, Solar Energy Journal 2005   
   http://minds.wisconsin.edu/handle/1793/7602
*/

class cec6par_module_t : public pvmodule_t
{
public:	
	double Area;
	double Vmp;
	double Imp;
	double Voc;
	double Isc;
	double alpha_isc;
	double beta_voc;
	double a;
	double Il;
	double Io;
	double Rs;
	double Rsh;
	double Adj;

	cec6par_module_t();

	virtual double AreaRef() { return Area; }
	virtual double VmpRef() { return Vmp; }
	virtual double ImpRef() { return Imp; }
	virtual double VocRef() { return Voc; }
	virtual double IscRef() { return Isc; }
	virtual bool operator() ( pvinput_t &input, double TcellC, double opvoltage, pvoutput_t &output );
};


class noct_celltemp_t : public pvcelltemp_t
{
public:
	double standoff_tnoct_adj;
	double ffv_wind;
	double Tnoct;

	virtual bool operator() ( pvinput_t &input, pvmodule_t &module, double opvoltage, double &Tcell );
};


class mcsp_celltemp_t : public pvcelltemp_t
{
public:
	double DcDerate; // DC derate factor (0..1)
	int MC; // Mounting configuration (1=rack,2=flush,3=integrated,4=gap)
	int HTD; // Heat transfer dimension (1=Module,2=Array)
	int MSO; // Mounting structure orientation (1=does not impede flow beneath, 2=vertical supports, 3=horizontal supports)
	int Nrows, Ncols; // number of modules in rows and columns, when using array heat transfer dimensions
	double Length; // module length, along horizontal dimension, (m)
	double Width; // module width, along vertical dimension, (m)
	double Wgap;  // gap width spacing (m)
	double TbackInteg;  // back surface temperature for integrated modules ('C)
	
	virtual bool operator() ( pvinput_t &input, pvmodule_t &module, double opvoltage, double &Tcell );
};

#endif


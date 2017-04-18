#ifndef _LAND_H_
#define _LAND_H_ 1

#include <vector>
#include "mod_base.h"
#include "definitions.h"

//class Land : public mod_base
class Land : public mod_base
{
	var_land* _var_land;

    double _bound_area;

public:
    void Create(var_map &V);
    void updateCalculatedParameters(var_map &V);

    double getLandBoundArea();

	static void getExtents(var_map &V, double rval[]);
	//static void getExtents(var_map &V, double rval[], double tht=1.);
	static void getRadialExtents(var_map &V, double rval[2], double tht=1.);
	//static bounds_array *getInclusions();
	//static bounds_array *getExclusions();
	static std::vector<Point> *getLayoutPositions();
	static double calcPolyLandArea(var_land &V);
	
	void calcLandArea(var_land &V, vector<Point> &layout);

	//methods for containment testing
	static bool InBounds(var_land &V, Point &H, double tht=1.0);

 } ;

#endif
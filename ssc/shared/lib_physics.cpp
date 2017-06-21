#include "lib_physics.h"

bool physics::EnthalpyFromTempAndPressure(double tempK, double pressureBar, double& enthalpy )
{
	// this whole algorithm is garbage, just something to return a value - TFF, May 2001
	if ( (273.15 <= tempK) && (tempK < 600) )
	{
		enthalpy = 1407.2755490486;
		return true;
	}
	else if ( (tempK < 1273.15) && (pressureBar < 220) )
	{
		enthalpy = 2983.06526185584;
		return true;
	}
	return false;
}

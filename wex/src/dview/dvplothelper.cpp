
#include "wex/dview/dvplothelper.h"


namespace wxDVPlotHelper
{
	void MouseWheelZoom(double* worldMin, double* worldMax, wxCoord center, wxCoord physMin, wxCoord physMax, int step)
	{
		//Center zooming on the location of the mouse.
		center -= physMin;
		physMax -= physMin;
	
		double locationPercent = double(center) / double(physMax);
		locationPercent -= 0.5;

		if (step > 0)
		{
			ZoomFactor(worldMin, worldMax, 1.5 * step, locationPercent);
		}
		else
		{
			ZoomFactor(worldMin, worldMax, 1 / (-1.5 * step));
		}
	}

	void ZoomFactor(double* worldMin, double* worldMax, double factor, double shiftPercent)
	{
		//A factor of 2 would zoom in twice as far as current level.
		if (factor == 1)
			return;
	
		double oldRange = *worldMax - *worldMin;
		double newRange = oldRange / factor;
		double newMin = *worldMin + (oldRange - newRange)/2.0;
		double newMax = *worldMax - (oldRange - newRange)/2.0;

		newMin += shiftPercent * (newRange < oldRange ? newRange : oldRange);
		newMax += shiftPercent * (newRange < oldRange ? newRange : oldRange);

		*worldMin = newMin;
		*worldMax = newMax;
	}

	void SetRangeEndpointsToDays(double* min, double* max)
	{
		//This function doesn't really set endpoints to days if the range is sufficiently small.
		//Choose an appropriate interval based on range.
		int intMin = int(*min);
		int intMax = int(*max);

		int range = intMax - intMin;
		int interval; //hours to set endpoint to
		if (range <= 6)
			return;
		else if (range <= 12)
			interval = 3;
		else if (range <= 24)
			interval = 3; //Set endpoints to eighth-days.
		else if (range <= 48)
			interval = 12;
		else
			interval = 24; //Actually set endpoints to days.

	
		int oldIntMin = intMin;
		if (intMin % interval > interval / 2 && intMin / interval != intMax / interval)
			intMin += interval;
		intMin -= intMin % interval;

		if (intMax % interval > interval / 2 || oldIntMin / interval == intMax / interval)
			intMax += interval;
		intMax -= intMax % interval;

		*min = double(intMin);
		*max = double(intMax);
	}

	
}

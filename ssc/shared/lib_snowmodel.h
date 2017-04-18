#ifndef __lib_snowmodel_h
#define __lib_snowmodel_h

#include <string>

class pvsnowmodel
{
public:
	pvsnowmodel();
	bool setup(int, float);

	bool getLoss(float poa, float tilt, float wspd, float tdry, float snowDepth, int sunup, float dt, float *returnLoss);

	float	tilt,		// Surface tilt, degrees
		baseTilt,		// The default tilt for 1-axis tracking systems
		mSlope,			// This is a value given by fig. 4 in [1]
		sSlope,			// This is a value given by fig. 7 in [1]
		deltaThreshold,	// The minimum change in snow depth required to trigger a snow fall detection
		depthThreshold,	// The minimum snow depth required to trigger a snow fall detection
		previousDepth,	// The snow depth from the previous iteration
		coverage,		// Snow coverage ( 0...1 )
		pCvg;			// Snow coverage from previous iteration ( 0...1 )

	int nmody,			// number of modules in a row
		badValues,		// keeps track of the number of detected bad snow depth values
		maxBadValues;	// The number of maximum bad snow depth values that is acceptable

	std::string msg;		// This is a string used to return error messages
	bool good;				// This an error flag that will be set to false
							//  if an error has occured
};

#endif
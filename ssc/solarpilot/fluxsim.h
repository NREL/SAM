#ifndef _FLUXSIM_
#define _FLUXSIM_ 1

//#include "mod_base.h"
#include "definitions.h"

class FluxSimData 
{
    
public:

    /*struct CLOUD_SHAPE { enum A {ELLIPTICAL,RECTANGULAR,FRONT}; };
    struct FLUX_DIST { enum A {TRIANGULAR, NORMAL, UNIFORM};};
    struct FLUX_MODEL { enum A {HERMITE, SOLTRACE}; };
    struct FLUX_TIME { enum A {POSITION, CALENDAR}; };
	struct AIM_STRATEGY { enum A { SIMPLE, SIGMA, PROBABILITY, IMAGE_SIZE, EXISTING, FREEZE }; };*/

    void Create(var_map &V);
    void updateCalculatedParameters(var_map &V);

};


#endif
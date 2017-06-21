#ifndef _PLANT_H_
#define _PLANT_H_ 1

#include "definitions.h"

class Plant : public mod_base
 {
     double _power_gross;
     double _power_net;

     var_plant* _var_plant;

 public:

     double getPowerGross();
     double getPowerNet();
     var_plant *getVarMap();

     void Create(var_map &V);
     void updateCalculatedParameters(var_map &V);
 } ;

#endif
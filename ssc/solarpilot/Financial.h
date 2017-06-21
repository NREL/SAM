#ifndef _FINANCIAL_H_
#define _FINANCIAL_H_ 1
#include "math.h"
#include <vector>
#include "mod_base.h"

class SolarField;

class Financial : public mod_base
 {
     double _tower_cost;
     double _rec_cost;
     double _plant_cost;
     double _tes_cost;
     double _site_cost;
     double _heliostat_cost;
     double _wiring_cost;
     double _contingency_cost;
     double _total_direct_cost;
     double _total_indirect_cost;
     double _land_cost;
     double _sales_tax_cost;
     double _total_installed_cost;
     double _cost_per_capacity;

     std::vector< double > _pricing_array;
     std::vector< int > _schedule_array;

     var_financial *_var_fin;

public:
    void Create(var_map &V);
    void updateCalculatedParameters(var_map &V);

    std::vector< double >* getPricingArray();
    std::vector< int >* getScheduleArray();

    void CreateHourlyTODSchedule(var_map &V);
	void calcPlantCapitalCost(var_map &V);
	//void calcSimpleCOE(double *enet /*8760 MWh*/, int nval = 8760); //Calculates _simple_coe and _weighted_coe

 } ;

#endif
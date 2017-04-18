#include <vector>
#include <math.h>

#include "definitions.h"
#include "Financial.h"
#include "SolarField.h"

using namespace std;

void Financial::Create(var_map &V)
{
    _var_fin = &V.fin;

    updateCalculatedParameters(V);
}

void Financial::updateCalculatedParameters(var_map &V)
{
    //if schedules are not provided in the var_map, create them now
    if( V.fin.pricing_array.Val().size() < 2 )
    {
        //Assign the hourly schedules
	    CreateHourlyTODSchedule(V);
        V.fin.pricing_array.Setval( _pricing_array );
        V.fin.schedule_array.Setval( _schedule_array );
    }
    else
    {
        //the arrays have been created previously and we should copy now.
        
        _pricing_array = V.fin.pricing_array.Val();
        _schedule_array = V.fin.schedule_array.Val();
    }

    calcPlantCapitalCost(V);

    _var_fin->schedule_array.Setval( _schedule_array );
    _var_fin->pricing_array.Setval( _pricing_array );
    _var_fin->tower_cost.Setval( _tower_cost );
    _var_fin->rec_cost.Setval( _rec_cost );
    _var_fin->plant_cost.Setval( _plant_cost );
    _var_fin->tes_cost.Setval( _tes_cost );
    _var_fin->site_cost.Setval( _site_cost );
    _var_fin->heliostat_cost.Setval( _heliostat_cost );
    _var_fin->wiring_cost.Setval( _wiring_cost );
    _var_fin->contingency_cost.Setval( _contingency_cost );
    _var_fin->total_direct_cost.Setval( _total_direct_cost );
    _var_fin->total_indirect_cost.Setval( _total_indirect_cost );
    _var_fin->land_cost.Setval( _land_cost );
    _var_fin->sales_tax_cost.Setval( _sales_tax_cost );
    _var_fin->total_installed_cost.Setval( _total_installed_cost );
    _var_fin->cost_per_capacity.Setval( _cost_per_capacity );
}

std::vector< double >* Financial::getPricingArray()
{
    return &_pricing_array;
}

std::vector< int >* Financial::getScheduleArray()
{
    return &_schedule_array;
}

void Financial::CreateHourlyTODSchedule(var_map &V){
	/* 
	Take a schedule (12x24 = 288) of the TOD factors in string form and convert them into 
	an 8760 schedule of integers indicating the TOD factor for each hour of the year.

	Assume the year starts on a Sunday
	*/

	int nwd = V.fin.weekday_sched.val.size();
	int nwe = V.fin.weekend_sched.val.size();

	if(nwd != 288 || nwe != 288) return;

	int monthlength[] = {31,28,31,30,31,30,31,31,30,31,30,31};

    _schedule_array.resize(8760);
    _pricing_array.resize(8760);

	int h=0, tod;
	int dow = 6;	//M=0, T=1; W=2; Th=3; Fr=4, Sa=5; Su=6. Start on a Sunday
	string ss;
	for(int i=0; i<12; i++){
		for(int j=0; j<monthlength[i]; j++){
			for(int k=0; k<24; k++){
				ss = dow<5 ? V.fin.weekday_sched.val.at(i*24+k) : V.fin.weekend_sched.val.at(i*24+k);
				to_integer(ss, &tod);
				_schedule_array[h] = tod;
                _pricing_array[h] = V.fin.pmt_factors.val.at(tod-1);
				h++;
			}
			dow==6 ? dow = 0 : dow++ ;
		}
	}

}

void Financial::calcPlantCapitalCost(var_map &V){
	    
	double Asf = V.sf.sf_area.Val(); 
    double Arec = V.sf.rec_area.Val(); 

    double power_gross = V.plt.power_gross.Val(); 
    double power_net = V.plt.power_net.Val();
	double Aland = V.land.land_area.Val(); 

	_tower_cost =  V.fin.tower_fixed_cost.val * exp(V.sf.tht.val * V.fin.tower_exp.val ) ;
	
	_rec_cost =  V.fin.rec_ref_cost.val * pow( Arec / V.fin.rec_ref_area.val, V.fin.rec_cost_exp.val ) ;
	
	_plant_cost =  V.fin.plant_spec_cost.val * power_gross * 1000. ;
	
    double tescap = power_gross / V.plt.eta_cycle.val * V.plt.hours_tes.val;
	_tes_cost =  tescap * 1000. * V.fin.tes_spec_cost.val ;
	
	
	_site_cost =  V.fin.site_spec_cost.val * Asf ;
	_heliostat_cost =  V.fin.heliostat_spec_cost.val * Asf ;
	_wiring_cost =  V.fin.wiring_user_spec.val * Asf ;

    double tdc =
        _tower_cost + 
        _rec_cost +
        _heliostat_cost +
        _wiring_cost +
        _plant_cost +
        _tes_cost +
        V.fin.fixed_cost.val ;

	_contingency_cost =  V.fin.contingency_rate.val/100. * tdc ;

	_total_direct_cost =  tdc + _contingency_cost ;

	_land_cost =  V.land.land_area.Val() * V.fin.land_spec_cost.val ;		

	_sales_tax_cost =  
        V.fin.sales_tax_rate.val * V.fin.sales_tax_frac.val * _total_direct_cost / 1.e4 ;

	_total_indirect_cost =  
        _sales_tax_cost + _land_cost ;

	_total_installed_cost =  
        _total_direct_cost + _total_indirect_cost ;

	_cost_per_capacity =  
        _total_installed_cost / power_net / 1000. ;

}

//void Financial::calcSimpleCOE(double *enet, int nval){
//	/* 
//	Calculate the cost of energy on a simple capital cost to energy production ratio.
//	This number is not representative of an actual LCOE.
//	
//	enet :: double[nval]
//	*/
//	if(nval > (int)_schedule_array.ncells()) return;
//
//	double
//		prod = 0.,
//		prod_w = 0.;
//
//	for(int i=0; i<nval; i++){
//		double e = enet[i];
//		prod_w += _schedule_array[i] * e;
//		prod += e;
//	}
//
//
//	_simple_coe = _total_installed_cost/prod;
//	_weighted_coe = _total_installed_cost/prod_w;
//
//}

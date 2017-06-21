#include "Plant.h"
//#include "math.h"
#include <vector>
using namespace std;

void Plant::Create(var_map &V)
{
    _var_plant = &V.plt;

    updateCalculatedParameters(V);
}

void Plant::updateCalculatedParameters(var_map &V)
{
    _power_gross = V.sf.q_des.val / V.plt.solar_mult.val * V.plt.eta_cycle.val;
    _power_net =  _power_gross * V.plt.par_factor.val ;

    V.plt.power_gross.Setval( _power_gross );
    V.plt.power_net.Setval( _power_net );

}

double Plant::getPowerGross()
{
    return _power_gross;
}

double Plant::getPowerNet()
{
    return _power_net;
}

var_plant* Plant::getVarMap()
{
    return _var_plant;
}
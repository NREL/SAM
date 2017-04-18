#include <iostream>
#include "LandBOS.hpp"

int main(){


    double machineRating = 2000.0;
    double rotorDiameter = 110.0;
    double hubHeight = 100.0;
    double nTurbines = 100.0;
    double interconnectVoltage = 137.0;
    double distToInterconnect = 5.0;
    C::SiteTerrain terrain = C::FLAT_TO_ROLLING;
    C::TurbineLayout layout = C::COMPLEX;
    C::SoilCondition soil = C::STANDARD;
    double TCC = 1000.0;
    double towerTopMass = 88.0;

    BOS bos(machineRating, rotorDiameter, hubHeight, nTurbines, interconnectVoltage,
            distToInterconnect, TCC, towerTopMass, terrain, layout, soil);

    std::cout.precision(9);
    std::cout << bos.totalCost() << std::endl;

}
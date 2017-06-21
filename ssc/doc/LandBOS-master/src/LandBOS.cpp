//
//  LandBOS.cpp
//  LandBOS
//
//  Created by Andrew Ning on 3/10/14.
//  Copyright (c) 2014 NREL. All rights reserved.
//

#include "LandBOS.hpp"

BOS::BOS(double machineRating, double rotorDiameter, double hubHeight,
         int nTurbines, double interconnectVoltage, double distToInterconnect,
         double turbineCapitalCosts, double towerTopMass,
         C::SiteTerrain siteTerrain, C::TurbineLayout turbineLayout,
         C::SoilCondition soilCondition){

    rating = machineRating;
    diameter = rotorDiameter;
    hubHt = hubHeight;
    nTurb = nTurbines;
    voltage = interconnectVoltage;
    distInter = distToInterconnect;
    tcc = turbineCapitalCosts;
    topMass = towerTopMass;
    terrain = siteTerrain;
    layout = turbineLayout;
    soil = soilCondition;

    // Wind Farm Size (MW)
    farmSize = C::farmSize(rating, nTurb);

    // Construction Time (months)
    constructionTime = C::defaultConstructionTime(nTurb);

    // Access road entrances
    accessRoadEntrances = C::defaultAccessRoadEntrances(nTurb);

    // O&M Building Size (ft2)
    buildingSize = C::defaultBuildingSize(farmSize);

    // Quantity of Temporary Meteorological Towers for Testing
    temporary = C::defaultTempMetTowers(farmSize);

    // Quantity of Permanent Meteorological Towers for Testing
    permanent = C::defaultPermanentMetTowers(farmSize);

    // Wind/Weather delay days
    weatherDelayDays = C::defaultWeatherDelayDays(nTurb);

    // Crane breakdowns
    craneBreakdowns = C::defaultCraneBreakdowns(nTurb);

}


void BOS::setConstructionTime(int months){
    constructionTime = months;
}

void BOS::setAccessRoadEntrances(int number){
    accessRoadEntrances = number;
}

void BOS::setBuildingSize(double sqft){
    buildingSize = sqft;
}

void BOS::setTempTowers(int towers){
    temporary = towers;
}

void BOS::setPermanentTowers(int towers){
    permanent = towers;
}

void BOS::setWeatherDelays(int days){
    weatherDelayDays = days;
}

void BOS::setCraneBreakdowns(int number){
    craneBreakdowns = number;
}



double BOS::transportationCost(double transportationDistance) const{

    return C::transportationCost(tcc, rating, nTurb, hubHt, transportationDistance);
}


double BOS::engineeringCost() const{

    return C::engineeringCost(nTurb, farmSize);
}


double BOS::powerPerformanceCost() const{

    return C::powerPerformanceCost(hubHt, permanent, temporary);
}


double BOS::accessRoadsCost() const{

    return C::accessRoadsCost(terrain, layout, nTurb, diameter, constructionTime, accessRoadEntrances);
}


double BOS::siteCompoundCost() const{

    return C::siteCompoundCost(accessRoadEntrances, constructionTime, farmSize);
}


double BOS::buildingCost() const{

    return C::buildingCost(buildingSize);
}

double BOS::foundationCost() const{

    return C::foundationCost(rating, diameter, topMass, hubHt, soil, nTurb);
}


double BOS::erectionCost(bool deliveryAssistRequired) const{

    return C::erectionCost(rating, hubHt, nTurb, weatherDelayDays, craneBreakdowns, deliveryAssistRequired);
}


double BOS::electricalMaterialsCost(bool padMountTransformer, double thermalBackfill) const{

    return C::electricalMaterialsCost(terrain, layout, farmSize, diameter, nTurb,
                                      padMountTransformer, thermalBackfill);
}


double BOS::electricalInstallationCost(double rockTrenchingLength, double overheadCollector) const{

    return C::electricalInstallationCost(terrain, layout, farmSize, diameter, nTurb,
                                         rockTrenchingLength, overheadCollector);
}


double BOS::substationCost() const{

    return C::substationCost(voltage, farmSize);
}


double BOS::transmissionCost(bool newSwitchyardRequired) const{

    return C::transmissionCost(voltage, distInter, newSwitchyardRequired);
}



double BOS::projectMgmtCost() const{

    return C::projectMgmtCost(constructionTime);
}


double BOS::developmentCost(double developmentFee) const{

    return C::developmentCost(developmentFee);
}


C::MultCost BOS::insuranceMultiplierAndCost(double foundationCost,
    bool performanceBond) const{

    return C::insuranceMultiplierAndCost(tcc, farmSize, foundationCost, performanceBond);
}


C::MultCost BOS::markupMultiplierAndCost(double transportationCost,
    double contingency, double warranty, double useTax, double overhead,
    double profitMargin) const{

    return C::markupMultiplierAndCost(transportationCost, contingency, warranty,
                                      useTax, overhead, profitMargin);

}


double BOS::totalCost(bool deliveryAssistRequired, bool padMountTransformer,
                      bool newSwitchyardRequired, double rockTrenchingLength,
                      double thermalBackfill, double overheadCollector,
                      bool performanceBond, double contingency, double warranty,
                      double useTax, double overhead, double profitMargin,
                      double developmentFee, double transportationDistance) const{

    return C::totalCost(rating, diameter, hubHt, nTurb, voltage, distInter, terrain,
                        layout, soil, farmSize, tcc, topMass, constructionTime, buildingSize,
                        temporary, permanent, weatherDelayDays, craneBreakdowns,
                        accessRoadEntrances, deliveryAssistRequired, padMountTransformer,
                        newSwitchyardRequired, rockTrenchingLength, thermalBackfill,
                        overheadCollector, performanceBond, contingency, warranty, useTax,
                        overhead, profitMargin, developmentFee, transportationDistance);

}
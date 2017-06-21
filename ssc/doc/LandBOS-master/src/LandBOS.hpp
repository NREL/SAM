//
//  LandBOS.hpp
//  LandBOS
//
//  Created by Andrew Ning on 3/10/14.
//  Copyright (c) 2014 NREL. All rights reserved.
//

#ifndef __LandBOS_HPP__
#define __LandBOS_HPP__

#include <math.h>

extern "C" {
    namespace C{
        #include "LandBOS.h"
    }
}



class BOS {
    double rating;  // machine rating (kW)
    double diameter;  // rotor diameter (m)
    double hubHt;  // hub height (m)
    int nTurb;  // number of turbines
    double voltage;  // interconnect voltage (kV)
    double distInter;  // distance to interconnect (mi)
    C::SiteTerrain terrain;
    C::TurbineLayout layout;
    C::SoilCondition soil;

    double tcc;  // Turbine Capital Cost ($/kW)
    double topMass;  // Tower Top Mass (Tonnes)

    double farmSize;  // wind farm size (MW)

    // --- values that can be overridden ---
    // Note: some values that are really integer values are defined here as doubles so
    //       that they can be overriden with continuous values for the "smooth"
    //       version of this code.

    int constructionTime;  // Construction Time (months)
    double buildingSize;  // O&M Building Size (ft2)
    int temporary;  // Quantity of Temporary Meteorological Towers for Testing
    int permanent;  // Quantity of Permanent Meteorological Towers for Testing
    int weatherDelayDays;  // Wind/Weather delay days
    int craneBreakdowns;  // Crane breakdowns
    int accessRoadEntrances;  // Access road entrances


    // ---------------------------------


public:

    // constructor
    BOS(double machineRating, double rotorDiameter, double hubHeight, int nTurbines,
        double interconnectVoltage, double distToInterconnect,
        double turbineCapitalCosts, double towerTopMass,
        C::SiteTerrain terrain, C::TurbineLayout layout, C::SoilCondition soil);

    // override any of these parameters
    void setConstructionTime(int months);
    void setAccessRoadEntrances(int number);
    void setBuildingSize(double sqft);
    void setTempTowers(int towers);
    void setPermanentTowers(int towers);
    void setWeatherDelays(int days);
    void setCraneBreakdowns(int number);

    // transportation
    double transportationCost(double transportationDistance=0.0) const;

    // engineering
    double engineeringCost() const;

    // met masts and power performance
    double powerPerformanceCost() const;

    // access roads and site improvement
    double accessRoadsCost() const;

    // site compound and security
    double siteCompoundCost() const;

    // control O&M building
    double buildingCost() const;

    // foundation
    double foundationCost() const;

    // erection
    double erectionCost(bool deliveryAssistRequired=false) const;

    // electrical materials
    double electricalMaterialsCost(bool padMountTransformer=true,
        double thermalBackfill=0.0) const;

    // electrical installation
    double electricalInstallationCost(double rockTrenchingLength=10.0,
        double overheadCollector=0.0) const;

    // collector substation
    double substationCost() const;

    // transmission line and interconnect
    double transmissionCost(bool newSwitchyardRequired=true) const;

    // project management
    double projectMgmtCost() const;

    // development
    double developmentCost(double developmentFee=5.0) const;

    // insurance
    C::MultCost insuranceMultiplierAndCost(double foundationCost,
        bool performanceBond=false) const;

    // markup and contingency
    C::MultCost markupMultiplierAndCost(double transportationCost,
        double contingency=3.0, double warranty=0.02, double useTax=0.0,
        double overhead=5.0, double profitMargin=5.0) const;


    // total
    double totalCost(bool deliveryAssistRequired=false, bool padMountTransformer=true,
                     bool newSwitchyardRequired=true, double rockTrenchingLength=10.0,
                     double thermalBackfill=0.0, double overheadCollector=0.0,
                     bool performanceBond=false, double contingency=3.0, double warranty=0.02,
                     double useTax=0.0, double overhead=5.0, double profitMargin=5.0,
                     double developmentFee=5.0, double transportationDistance=0.0) const;
};


#endif
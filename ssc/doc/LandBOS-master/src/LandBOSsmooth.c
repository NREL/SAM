//
//  LandBOS.c
//  LandBOS
//
//  Created by Andrew Ning on 3/12/14.
//  Copyright (c) 2014 NREL. All rights reserved.
//

#include "LandBOS.h"

/*
Continuously differentiable versions of several functions are added
for use in gradient-based optimization.
nTurb, terrain, layout, soil and any optional function inputs
are assumed to be constant during an optimization
so any dependence on them need not be smooth.
*/

double farmSize(double rating, int nTurb){
    return rating * nTurb / 1000.0;
}

// Construction Time (months)
int defaultConstructionTime(int nTurb){
    return round(0.0001*nTurb*nTurb + 0.0963*nTurb + 2.7432);
}

// Access road entrances
int defaultAccessRoadEntrances(int nTurb){
    return fmax(1, round(nTurb/20.0));
}

// O&M Building Size (ft2)
double defaultBuildingSize(double farmSize){

    double c3 = -6.13534135936e-06;
    double c2 = 0.0124193928834;
    double c1 = 1.06594513037;
    double c0 = 3000.0;

    double x2 = 1391.1224868;
    double f2 = 12000.0;

    double buildingSize;

    if (farmSize >= x2){
        buildingSize = f2;
    } else{
        buildingSize = c3*pow(farmSize, 3) + c2*pow(farmSize, 2) + c1*farmSize + c0;
    }

    return buildingSize;
}

// Quantity of Temporary Meteorological Towers for Testing
double defaultTempMetTowers(double farmSize){

    return farmSize/75.0;
}

// Quantity of Permanent Meteorological Towers for Testing
double defaultPermanentMetTowers(double farmSize){

    return farmSize/100.0;
}

// Wind/Weather delay days
int defaultWeatherDelayDays(int nTurb){
    return round(nTurb/5.0);
}

// Crane breakdowns
int defaultCraneBreakdowns(int nTurb){
    return round(nTurb/20.0);
}




// this one only for greater than 2.5 MW machines
double transportationCost(double tcc, double rating, int nTurb,
        double hubHt, double transportDist){

    double cost = tcc * rating * nTurb;

    cost += 1867*pow(transportDist, 0.726) * nTurb;

    return cost;
}


double engineeringCost(int nTurb, double farmSize){

    double cost = 7188.5 * nTurb;

    cost += (3.4893*log(nTurb)-7.3049)*16800;

    double c3 = -2e-06;
    double c2 = 0.0012;
    double c1 = -0.225;
    double c0 = 14.5;

    double multiplier;
    if (farmSize <= 150.0){
        multiplier = 1.0;
    }
    else if (farmSize >= 250.0){
        multiplier = 2.0;
    }
    else{
        multiplier = c3*pow(farmSize, 3) + c2*pow(farmSize, 2) + c1*farmSize + c0;
    }

    cost += multiplier * 161675;

    cost += 4000;

    return cost;
}


double powerPerformanceCost(double hubHt, double permanent,
        double temporary){

    double multiplier1;
    double multiplier2;

    double hL = 85.0;
    double hU = 95.0;



    double c3 = -114.8;
    double c2 = 30996.0;
    double c1 = -2781030.0;
    double c0 = 83175600.0;

    double mL1 = 232600.0;
    double mU1 = 290000.0;
    if (hubHt <= hL){
        multiplier1 = mL1;
    }
    else if (hubHt >= hU){
        multiplier1 = mU1;
    }
    else{
        multiplier1 = c3*pow(hubHt, 3) + c2*pow(hubHt, 2) + c1*hubHt + c0;
    }


    c3 = -48.4;
    c2 = 13068.0;
    c1 = -1172490.0;
    c0 = 35061600.0;

    double mL2 = 92600;
    double mU2 = 116800;

    if (hubHt <= hL){
        multiplier2 = mL2;
    }
    else if (hubHt >= hU){
        multiplier2 = mU2;
    }
    else{
        multiplier2 = c3*pow(hubHt, 3) + c2*pow(hubHt, 2) + c1*hubHt + c0;
    }



    double cost = 200000 + permanent*multiplier1 + temporary*multiplier2;

    return cost;
}




double accessRoadsCost(SiteTerrain terrain, TurbineLayout layout,
        int nTurb, double diameter, int constructionTime,
        int accessRoadEntrances){

    double factor1 = 0.0;
    double factor2 = 0.0;

    if (layout == SIMPLE){
        if (terrain == FLAT_TO_ROLLING){
            factor1 = 49962.5;
            factor2 = 24.8;
        } else if (terrain == RIDGE_TOP){
            factor1 = 59822.0;
            factor2 = 26.8;
        } else if (terrain == MOUNTAINOUS){
            factor1 = 66324.0;
            factor2 = 26.8;
        }

    } else if (layout == COMPLEX){
        if (terrain == FLAT_TO_ROLLING){
            factor1 = 62653.6;
            factor2 = 30.9;
        } else if (terrain == RIDGE_TOP){
            factor1 = 74213.3;
            factor2 = 33.0;
        } else if (terrain == MOUNTAINOUS){
            factor1 = 82901.1;
            factor2 = 33.0;
        }
    }

    double cost = (nTurb*factor1 + nTurb*diameter*factor2
                   + constructionTime*55500
                   + accessRoadEntrances*3800)*1.05;

    return cost;
}


double siteCompoundCost(int accessRoadEntrances, int constructionTime,
        double farmSize){

    double cost = 9825.0*accessRoadEntrances + 29850.0*constructionTime;

    double x1 = 10;
    double x2 = 50;
    double x1_2 = 80;
    double x2_2 = 120;
    double f1 = 3.0;
    double f2 = 5.0;
    double f3 = 10.0;

    double c3 = -6.25e-05;
    double c2 = 0.005625;
    double c1 = -0.09375;
    double c0 = 3.4375;

    double c3_2 = -0.00015625;
    double c2_2 = 0.046875;
    double c1_2 = -4.5;
    double c0_2 = 145.0;

    double multiplier;
    if (farmSize <= x1){
        multiplier = f1;
    }
    else if (farmSize > x1 && farmSize < x2){
        multiplier = c3*pow(farmSize, 3) + c2*pow(farmSize, 2) + c1*farmSize + c0;
    }
    else if (farmSize >= x2 && farmSize <= x1_2){
        multiplier = f2;
    }
    else if (farmSize > x1_2 && farmSize < x2_2){
        multiplier = c3_2*pow(farmSize, 3) + c2_2*pow(farmSize, 2) + c1_2*farmSize + c0_2;
    }
    else{
        multiplier = f3;
    }

    cost += multiplier * 30000;


    c3 = -22.5;
    c2 = 2025.0;
    c1 = -54000.0;
    c0 = 450000.0;

    if (farmSize <= 20.0){
        cost += 0.0;
    }
    else if (farmSize >= 40.0){
        cost += 90000.0;
    }
    else{
        cost += c3*pow(farmSize, 3) + c2*pow(farmSize, 2) + c1*farmSize + c0;
    }


    cost += farmSize*60 + 62400;

    return cost;
}


double buildingCost(double buildingSize){

    double cost = buildingSize*125 + 176125;

    return cost;

}

double foundationCost(double rating, double diameter, double topMass,
        double hubHt, SoilCondition soil, int nTurb){

    double cost = rating*diameter*topMass/1000.0
        + 163421.5*pow(nTurb, -0.1458) + (hubHt-80)*500;

    if (soil == BOUYANT){
        cost += 20000;
    }

    cost *= nTurb;

    return cost;
}


double erectionCost(double rating, double hubHt, int nTurb, int weatherDelayDays,
        int craneBreakdowns, int deliveryAssistRequired){

    double cost = (37*rating + 27000*pow(nTurb, -0.42145) + (hubHt-80)*500)*nTurb;

    if (deliveryAssistRequired){
        cost += 60000*nTurb;
    }

    cost += 20000*weatherDelayDays + 35000*craneBreakdowns + 181*nTurb + 1834;

    return cost;
}


double electricalMaterialsCost(SiteTerrain terrain, TurbineLayout layout,
        double farmSize, double diameter, int nTurb, int padMountTransformer,
        double thermalBackfill){

    double factor1 = 0.0;
    double factor2 = 0.0;
    double factor3 = 0.0;

    if (layout == SIMPLE){
        if (terrain == FLAT_TO_ROLLING){
            factor1 = 66733.4;
            factor2 = 27088.4;
            factor3 = 545.4;
        } else if (terrain == RIDGE_TOP){
            factor1 = 67519.4;
            factor2 = 27874.4;
            factor3 = 590.8;
        } else if (terrain == MOUNTAINOUS){
            factor1 = 68305.4;
            factor2 = 28660.4;
            factor3 = 590.8;
        }

    } else if (layout == COMPLEX){
        if (terrain == FLAT_TO_ROLLING){
            factor1 = 67519.4;
            factor2 = 27874.4;
            factor3 = 681.7;
        } else if (terrain == RIDGE_TOP){
            factor1 = 68305.4;
            factor2 = 28660.4;
            factor3 = 727.2;
        } else if (terrain == MOUNTAINOUS){
            factor1 = 69484.4;
            factor2 = 29839.4;
            factor3 = 727.2;
        }
    }

    double cost;
    if (padMountTransformer){
        cost = nTurb*factor1;
    } else{
        cost = nTurb*factor2;
    }
    cost += farmSize/25.0*35375 + farmSize/100.0*50000
        + diameter*nTurb*factor3 + thermalBackfill*5 + 41945;

    return cost;
}


double electricalInstallationCost(SiteTerrain terrain, TurbineLayout layout,
        double farmSize, double diameter, int nTurb,
        double rockTrenchingLength, double overheadCollector){

    double factor1 = 0.0;
    double factor2 = 0.0;
    double factor3 = 0.0;

    if (layout == SIMPLE){
        if (terrain == FLAT_TO_ROLLING){
            factor1 = 7059.3;
            factor2 = 352.4;
            factor3 = 297.0;
        } else if (terrain == RIDGE_TOP){
            factor1 = 7683.5;
            factor2 = 564.3;
            factor3 = 483.0;
        } else if (terrain == MOUNTAINOUS){
            factor1 = 8305.0;
            factor2 = 682.6;
            factor3 = 579.0;
        }

    } else if (layout == COMPLEX){
        if (terrain == FLAT_TO_ROLLING){
            factor1 = 7683.5;
            factor2 = 564.9;
            factor3 = 446.0;
        } else if (terrain == RIDGE_TOP){
            factor1 = 8305.0;
            factor2 = 866.8;
            factor3 = 713.0;
        } else if (terrain == MOUNTAINOUS){
            factor1 = 9240.0;
            factor2 = 972.8;
            factor3 = 792.0;
        }
    }

    double cost = farmSize/25.0*14985;

    double c3 = -4.53125;
    double c2 = 2718.75;
    double c1 = -538312.5;
    double c0 = 35390000.0;

    double x1 = 180;
    double x2 = 220;
    double f1 = 155000;
    double f2 = 300000;

    if (farmSize <= x1){
        cost += f1;
    }
    else if (farmSize >= x2){
        cost += f2;
    }
    else{
        cost += c3*pow(farmSize, 3) + c2*pow(farmSize, 2) + c1*farmSize + c0;
    }

    cost += nTurb*(factor1 + diameter*(factor2 + factor3*rockTrenchingLength/100.0))
        + overheadCollector*200000 + 10000;

    return cost;
}


double substationCost(double voltage, double farmSize){

    double cost = 11652*(voltage+farmSize) + 11795*pow(farmSize, 0.3549) + 1526800;

    return cost;
}


double transmissionCost(double voltage, double distInter,
        int newSwitchyardRequired){

    double cost = (1176*voltage + 218257)*pow(distInter, 0.8937);

    if (newSwitchyardRequired){
        cost += 18115*voltage + 165944;
    }

    return cost;
}



double projectMgmtCost(int constructionTime){

    double cost;
    if (constructionTime < 28){
        cost = (53.333*constructionTime*constructionTime - 3442*constructionTime
                + 209542)*(constructionTime + 2);
    } else{
        cost = (constructionTime + 2)*155000;
    }

    return cost;
}


double developmentCost(double developmentFee){

    return developmentFee*1000000;
}


MultCost insuranceMultiplierAndCost(double tcc, double farmSize,
        double foundationCost, int performanceBond){

    MultCost result;

    result.alpha = 3.5 + 0.7 + 0.4 + 1.0;
    result.cost = (0.7 + 0.4 + 1.0) * tcc * farmSize;

    if (performanceBond){
        result.alpha += 10.0;
        result.cost += 10.0 * tcc * farmSize;
    }

    result.alpha /= 1000.0;
    result.cost += 0.02*foundationCost + 20000;

    return result;
}


MultCost markupMultiplierAndCost(double transportationCost, double contingency,
        double warranty, double useTax, double overhead, double profitMargin){

    MultCost result;

    result.alpha = (contingency + warranty + useTax + overhead + profitMargin)/100.0;

    result.cost = -result.alpha * transportationCost;


    return result;
}


double totalCost(double rating, double diameter, double hubHt,
        int nTurb, double voltage, double distInter,
        SiteTerrain terrain, TurbineLayout layout, SoilCondition soil,
        double farmSize, double tcc, double topMass,
        int constructionTime, double buildingSize, double temporary,
        double permanent, int weatherDelayDays, int craneBreakdowns,
        int accessRoadEntrances,
        int deliveryAssistRequired, int padMountTransformer,
        int newSwitchyardRequired, double rockTrenchingLength,
        double thermalBackfill, double overheadCollector,
        int performanceBond, double contingency, double warranty,
        double useTax, double overhead, double profitMargin,
        double developmentFee, double transportDist){

    double cost = 0.0;
    double alpha = 0.0;

    double transCost = transportationCost(tcc, rating, nTurb,
        hubHt, transportDist);
    cost += transCost;
    cost += engineeringCost(nTurb, farmSize);
    cost += powerPerformanceCost(hubHt, permanent, temporary);
    cost += siteCompoundCost(accessRoadEntrances, constructionTime, farmSize);
    cost += buildingCost(buildingSize);
    cost += transmissionCost(voltage, distInter, newSwitchyardRequired);
    cost += developmentCost(developmentFee);
    cost += accessRoadsCost(terrain, layout, nTurb, diameter, constructionTime, accessRoadEntrances);
    double foundCost = foundationCost(rating, diameter, topMass, hubHt, soil, nTurb);
    cost += foundCost;
    cost += erectionCost(rating, hubHt, nTurb, weatherDelayDays, craneBreakdowns, deliveryAssistRequired);
    cost += electricalMaterialsCost(terrain, layout, farmSize, diameter, nTurb, padMountTransformer, thermalBackfill);
    cost += electricalInstallationCost(terrain, layout, farmSize, diameter, nTurb, rockTrenchingLength, overheadCollector);
    cost += substationCost(voltage, farmSize);
    cost += projectMgmtCost(constructionTime);


    MultCost result;
    result = insuranceMultiplierAndCost(tcc, farmSize, foundCost, performanceBond);
    cost += result.cost;
    alpha += result.alpha;

    result = markupMultiplierAndCost(transCost, contingency, warranty, useTax, overhead, profitMargin);
    cost += result.cost;
    alpha += result.alpha;

    // multiplier
    cost /= (1.0 - alpha);

    return cost;

}
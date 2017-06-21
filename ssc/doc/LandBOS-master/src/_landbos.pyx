# distutils: language = c

cimport c_landbos



def farmSize(double rating, int nTurb):
    return c_landbos.farmSize(rating, nTurb)


def defaultConstructionTime(int nTurb):
    return c_landbos.defaultConstructionTime(nTurb)


def defaultAccessRoadEntrances(int nTurb):
    return c_landbos.defaultAccessRoadEntrances(nTurb)


def defaultBuildingSize(double farmSize):
    return c_landbos.defaultBuildingSize(farmSize)


def defaultTempMetTowers(double farmSize):
    return c_landbos.defaultTempMetTowers(farmSize)


def defaultPermanentMetTowers(double farmSize):
    return c_landbos.defaultPermanentMetTowers(farmSize)


def defaultWeatherDelayDays(int nTurb):
    return c_landbos.defaultWeatherDelayDays(nTurb)


def defaultCraneBreakdowns(int nTurb):
    return c_landbos.defaultCraneBreakdowns(nTurb)



def transportationCost(double tcc, double rating, int nTurb,
        double hubHt, double transportDist=0.0):
    return c_landbos.transportationCost(tcc, rating, nTurb, hubHt, transportDist)


def engineeringCost(int nTurb, double farmSize):
    return c_landbos.engineeringCost(nTurb, farmSize)


def powerPerformanceCost(double hubHt, double permanent, double temporary):
    return c_landbos.powerPerformanceCost(hubHt, permanent, temporary)


def accessRoadsCost(c_landbos.SiteTerrain terrain, c_landbos.TurbineLayout layout,
        int nTurb, double diameter, int constructionTime, int accessRoadEntrances):
    return c_landbos.accessRoadsCost(terrain, layout,
        nTurb, diameter, constructionTime, accessRoadEntrances)


def siteCompoundCost(int accessRoadEntrances, int constructionTime, double farmSize):
    return c_landbos.siteCompoundCost(accessRoadEntrances, constructionTime, farmSize)


def buildingCost(double buildingSize):
    return c_landbos.buildingCost(buildingSize)


def foundationCost(double rating, double diameter, double topMass,
        double hubHt, c_landbos.SoilCondition soil, int nTurb):
    return c_landbos.foundationCost(rating, diameter, topMass,
        hubHt, soil, nTurb)


def erectionCost(double rating, double hubHt, int nTurb, int weatherDelayDays,
        int craneBreakdowns, bint deliveryAssistRequired=False):
    return c_landbos.erectionCost(rating, hubHt, nTurb, weatherDelayDays,
        craneBreakdowns, deliveryAssistRequired)


def electricalMaterialsCost(c_landbos.SiteTerrain terrain, c_landbos.TurbineLayout layout,
        double farmSize, double diameter, int nTurb, bint padMountTransformer=True,
        double thermalBackfill=0.0):
    return c_landbos.electricalMaterialsCost(terrain, layout,
        farmSize, diameter, nTurb, padMountTransformer, thermalBackfill)


def electricalInstallationCost(c_landbos.SiteTerrain terrain, c_landbos.TurbineLayout layout,
        double farmSize, double diameter, int nTurb,
        double rockTrenchingLength=10.0, double overheadCollector=0.0):
    return c_landbos.electricalInstallationCost(terrain, layout,
        farmSize, diameter, nTurb, rockTrenchingLength, overheadCollector)


def substationCost(double voltage, double farmSize):
    return c_landbos.substationCost(voltage, farmSize)


def transmissionCost(double voltage, double distInter,
        bint newSwitchyardRequired=True):
    return c_landbos.transmissionCost(voltage, distInter, newSwitchyardRequired)


def projectMgmtCost(int constructionTime):
    return c_landbos.projectMgmtCost(constructionTime)


def developmentCost(double developmentFee=5):
    return c_landbos.developmentCost(developmentFee)


def insuranceMultiplierAndCost(double tcc, double farmSize,
        double foundationCost, bint performanceBond=True):
    return c_landbos.insuranceMultiplierAndCost(tcc, farmSize,
        foundationCost, performanceBond)


def markupMultiplierAndCost(double transportationCost, double contingency=3.0,
        double warranty=0.02, double useTax=0.0, double overhead=5.0,
        double profitMargin=5.0):
    return c_landbos.markupMultiplierAndCost(transportationCost, contingency,
        warranty, useTax, overhead, profitMargin)


def totalCost(double rating, double diameter, double hubHt,
        int nTurb, double voltage, double distInter,
        c_landbos.SiteTerrain terrain, c_landbos.TurbineLayout layout,
        c_landbos.SoilCondition soil,
        double farmSize, double tcc, double topMass,
        int constructionTime, double buildingSize, double temporary,
        double permanent, int weatherDelayDays, int craneBreakdowns,
        int accessRoadEntrances,
        bint deliveryAssistRequired=False, bint padMountTransformer=True,
        bint newSwitchyardRequired=True, double rockTrenchingLength=10.0,
        double thermalBackfill=0.0, double overheadCollector=0.0,
        bint performanceBond=False, double contingency=3.0, double warranty=0.02,
        double useTax=0.0, double overhead=5.0, double profitMargin=5.0,
        double developmentFee=5.0, double transportDist=0.0):
    return c_landbos.totalCost(rating, diameter, hubHt,
        nTurb, voltage, distInter, terrain, layout, soil,
        farmSize, tcc, topMass, constructionTime, buildingSize,
        temporary, permanent, weatherDelayDays, craneBreakdowns,
        accessRoadEntrances, deliveryAssistRequired, padMountTransformer,
        newSwitchyardRequired, rockTrenchingLength, thermalBackfill,
        overheadCollector, performanceBond, contingency, warranty,
        useTax, overhead, profitMargin, developmentFee, transportDist)


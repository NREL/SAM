cdef extern from "LandBOS.h":

    ctypedef enum SiteTerrain:
        FLAT_TO_ROLLING
        RIDGE_TOP
        MOUNTAINOUS

    ctypedef enum TurbineLayout:
        SIMPLE
        COMPLEX

    ctypedef enum SoilCondition:
        STANDARD
        BOUYANT

    ctypedef struct MultCost:
        double alpha
        double cost

    double farmSize(double rating, int nTurb)

    int defaultConstructionTime(int nTurb)

    int defaultAccessRoadEntrances(int nTurb)

    double defaultBuildingSize(double farmSize)

    double defaultTempMetTowers(double farmSize)

    double defaultPermanentMetTowers(double farmSize)

    int defaultWeatherDelayDays(int nTurb)

    int defaultCraneBreakdowns(int nTurb)


    double transportationCost(double tcc, double rating, int nTurb,
            double hubHt, double transportDist)

    double engineeringCost(int nTurb, double farmSize)

    double powerPerformanceCost(double hubHt, double permanent,
            double temporary)

    double accessRoadsCost(SiteTerrain terrain, TurbineLayout layout,
            int nTurb, double diameter, int constructionTime,
            int accessRoadEntrances)

    double siteCompoundCost(int accessRoadEntrances, int constructionTime,
            double farmSize)

    double buildingCost(double buildingSize)

    double foundationCost(double rating, double diameter, double topMass,
            double hubHt, SoilCondition soil, int nTurb)

    double erectionCost(double rating, double hubHt, int nTurb, int weatherDelayDays,
            int craneBreakdowns, bint deliveryAssistRequired)

    double electricalMaterialsCost(SiteTerrain terrain, TurbineLayout layout,
            double farmSize, double diameter, int nTurb, bint padMountTransformer,
            double thermalBackfill)

    double electricalInstallationCost(SiteTerrain terrain, TurbineLayout layout,
            double farmSize, double diameter, int nTurb,
            double rockTrenchingLength, double overheadCollector)

    double substationCost(double voltage, double farmSize)

    double transmissionCost(double voltage, double distInter,
            bint newSwitchyardRequired)

    double projectMgmtCost(int constructionTime)

    double developmentCost(double developmentFee)

    MultCost insuranceMultiplierAndCost(double tcc, double farmSize,
            double foundationCost, bint performanceBond)

    MultCost markupMultiplierAndCost(double transportationCost, double contingency,
            double warranty, double useTax, double overhead, double profitMargin)

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
        double developmentFee, double transportDist)



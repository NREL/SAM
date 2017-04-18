#include "core.h"
#include "lib_wind_obos.h"

//#include <iostream>
//#include <cmath>
//#include <math.h>
//#include <vector>
//#include <algorithm>
//#include <map>
//#include <string>
//#include <array>
//#include <fstream>

//using namespace std;

//substructure type
enum { MONOPILE, JACKET, SPAR, SEMISUBMERSIBLE };
//anchor types
enum  { DRAGEMBEDMENT, SUCTIONPILE };
//turbine installation methods
enum  { INDIVIDUAL, BUNNYEARS, ROTORASSEMBLED };
//turbine tower installation methods
enum  { ONEPIECE, TWOPIECE };
//installation vessel strategy
enum  { PRIMARYVESSEL, FEEDERBARGE };
//toggle cable cost optimizer on or off
enum  { ON, OFF };

static var_info _cm_vtab_wind_obos[] = {
/*  VARTYPE           DATATYPE         NAME                              LABEL                                                      UNITS                 META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/

//Main inputs
   { SSC_INPUT,        SSC_NUMBER,      "turbCapEX",                      "Turbine Capital Cost",                                     "$/kW",               "",                       "wobos",            "?=1605",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "nTurb",                          "Number of Turbines",                                       "",                   "",                       "wobos",            "?=20",                    "MIN=2,Max=200",                 ""},
   { SSC_INPUT,        SSC_NUMBER,      "turbR",                          "Turbine Rating",                                           "MW",                 "",                       "wobos",            "?=5",                     "MIN=1,MAX=10",                  ""},
   { SSC_INPUT,        SSC_NUMBER,      "rotorD",                         "Rotor Diameter",                                           "m",                  "",                       "wobos",            "?=120",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "hubH",                           "Hub Height",                                               "m",                  "",                       "wobos",            "?=90",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "waterD",                         "Max Water Depth",                                          "m",                  "",                       "wobos",            "?=30",                    "MIN=3,MAX=1000",                ""},
   { SSC_INPUT,        SSC_NUMBER,      "distShore",                      "Distance to Landfall",                                     "km",                 "",                       "wobos",            "?=90",                    "MIN=5,MAX=1000",                ""},
   { SSC_INPUT,        SSC_NUMBER,      "distPort",                       "Distance from Installation Port to Site",                  "km",                 "",                       "wobos",            "?=90",                    "MIN=5,MAX=1000",                ""},
   { SSC_INPUT,        SSC_NUMBER,      "distPtoA",                       "Distance from Installation Port to Inshore Assembly Area", "km",                 "",                       "wobos",            "?=90",                    "MIN=5,MAX=1000",                ""},
   { SSC_INPUT,        SSC_NUMBER,      "distAtoS",                       "Distance form Inshore Assembly Area to Site",              "km",                 "",                       "wobos",            "?=90",                    "MIN=5,MAX=1000",                ""},
   { SSC_INPUT,        SSC_NUMBER,      "substructure",                   "Substructure Type",                                        "",                   "",                       "wobos",            "?=0",                     "INTEGER",                       ""},
   { SSC_INPUT,        SSC_NUMBER,      "anchor",                         "Anchor Type",                                              "",                   "",                       "wobos",            "?=0",                     "INTEGER",                       ""},
   { SSC_INPUT,        SSC_NUMBER,      "turbInstallMethod",              "Turbine Installation Method",                              "",                   "",                       "wobos",            "?=0",                     "INTEGER",                       ""},
   { SSC_INPUT,        SSC_NUMBER,      "towerInstallMethod",             "Tower Installation Method",                                "",                   "",                       "wobos",            "?=0",                     "INTEGER",                       ""},
   { SSC_INPUT,        SSC_NUMBER,      "installStrategy",                "Installation Vessel Strategy",                             "",                   "",                       "wobos",            "?=0",                     "INTEGER",                       ""},
   { SSC_INPUT,        SSC_NUMBER,      "cableOptimizer",                 "Electrical Cable Cost Optimization",                       "",                   "",                       "wobos",            "?=0",                     "INTEGER",                       ""},
   { SSC_INPUT,        SSC_NUMBER,      "moorLines",                      "Number Of Mooring Lines",                                  "",                   "",                       "wobos",            "?=3",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "buryDepth",                      "Electrical Cable Burial Depth",                            "m",                  "",                       "wobos",            "?=2",                     "MIN=0,MAX=15",                  ""},
   { SSC_INPUT,        SSC_NUMBER,      "arrayY",                         "Spacing Between Turbines in Rows",                         "rotor diameters",    "",                       "wobos",            "?=9",                     "MIN=1",                         ""},
   { SSC_INPUT,        SSC_NUMBER,      "arrayX",                         "Spacing Between Turbine Rows",                             "rotor diameters",    "",                       "wobos",            "?=9",                     "MIN=1",                         ""},
   //{ SSC_INPUT,        SSC_ARRAY,       "arrVoltage",                     "Array Cable System Voltage",                              "kV",                 "",                       "wobos",            "*",                       "",                              ""},
   //{ SSC_INPUT,        SSC_ARRAY,       "expVoltage",                     "Export Cable System Voltage",                             "kV",                 "",                       "wobos",            "*",                       "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "substructCont",                  "Substructure Install Weather Contingency",                 "%",                  "",                       "wobos",            "?=30",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "turbCont",                       "Turbine Install Weather Contingency",                      "%",                  "",                       "wobos",            "?=30",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "elecCont",                       "Electrical Install Weather Contingency",                   "%",                  "",                       "wobos",            "?=30",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "interConVolt",                   "Grid Interconnect Voltage",                                "kV",                 "",                       "wobos",            "?=345",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "distInterCon",                   "Distance Over Land to Grid Interconnect",                  "miles",              "",                       "wobos",            "?=3",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "scrapVal",                       "Total Scrap Value of Decommissioned Components",           "$",                  "",                       "wobos",            "?=0",                     "",                              ""},

//General
   { SSC_INPUT,        SSC_NUMBER,      "projLife",                       "Project Economic Life",                                    "years",              "",                       "wobos",            "?=20",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "inspectClear",                   "Inspection Clearance",                                     "m",                  "",                       "wobos",            "?=2",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "plantComm",                      "Plant Commissioning Cost Factor",                          "%",                  "",                       "wobos",            "?=0.01",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "procurement_contingency",        "Procurement Contingency",                                  "%",                  "",                       "wobos",            "?=5",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "install_contingency",            "Installation Contingency",                                 "%",                  "",                       "wobos",            "?=30",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "construction_insurance",         "Insurance During Construction (% of ICC)",                 "%",                  "",                       "wobos",            "?=1",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "capital_cost_year_0",            "Capital cost spent in year 0",                             "%",                  "",                       "wobos",            "?=20",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "capital_cost_year_1",            "Capital cost spent in year 1",                             "%",                  "",                       "wobos",            "?=60",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "capital_cost_year_2",            "Capital cost spent in year 2",                             "%",                  "",                       "wobos",            "?=10",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "capital_cost_year_3",            "Capital cost spent in year 3",                             "%",                  "",                       "wobos",            "?=10",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "capital_cost_year_4",            "Capital cost spent in year 4",                             "%",                  "",                       "wobos",            "?=0",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "capital_cost_year_5",            "Capital cost spent in year 5",                             "%",                  "",                       "wobos",            "?=0",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "tax_rate",                       "Effective Tax Rate",                                       "%",                  "",                       "wobos",            "?=40",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "interest_during_construction",   "Interest During Construction",                             "%",                  "",                       "wobos",            "?=8",                  "",                              ""},

//Substructure & Foundation
   { SSC_INPUT,        SSC_NUMBER,      "mpileCR",                        "Monopile Cost Rate",                                       "$/tonne",            "",                       "wobos",            "?=2250",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "mtransCR",                       "Monopile Transition Piece Cost Rate",                      "$/tonne",            "",                       "wobos",            "?=3230",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "mpileD",                         "Monopile Diameter",                                        "m",                  "",                       "wobos",            "",                        "MIN=0.01",                      ""},
   { SSC_INPUT,        SSC_NUMBER,      "mpileL",                         "Monopile Length",                                          "m",                  "",                       "wobos",            "",                        "MIN=0.01",                      ""},
   { SSC_INPUT,        SSC_NUMBER,      "mpEmbedL",                       "Monopile Embedment Length",                                "m",                  "",                       "wobos",            "?=30",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "jlatticeCR",                     "Jacket Main Lattice Cost Rate",                            "$/tonne",            "",                       "wobos",            "?=4680",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "jtransCR",                       "Jacket Transition Piece Cost Rate",                        "$/tonne",            "",                       "wobos",            "?=4500",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "jpileCR",                        "Jacket Pile Cost Rate",                                    "$/tonne",            "",                       "wobos",            "?=2250",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "jlatticeA",                      "Jacket Main Lattice Footprint Area",                       "m^2",                "",                       "wobos",            "?=26",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "jpileL",                         "Jacket Pile Length",                                       "m",                  "",                       "wobos",            "?=47.5",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "jpileD",                         "Jacket Pile Diameter",                                     "m",                  "",                       "wobos",            "?=1.6",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "spStifColCR",                    "Spar Stiffened Column Cost Rate",                          "$/tonne",            "",                       "wobos",            "?=3120",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "spTapColCR",                     "Spar Tapered Column Cost Rate",                            "$/tonne",            "",                       "wobos",            "?=4220",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "ballCR",                         "Floating Ballast Cost Rate",                               "$/tonne",            "",                       "wobos",            "?=100",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "deaFixLeng",                     "Fixed Mooring Length for Drag Embedment Anchors",          "m",                  "",                       "wobos",            "?=500",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "ssStifColCR",                    "Semi-submersible Stiffened Column Cost Rate",              "$/tonne",            "",                       "wobos",            "?=3120",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "ssTrussCR",                      "Semi-submersible Truss Cost Rate",                         "$/tonne",            "",                       "wobos",            "?=6250",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "ssHeaveCR",                      "Semi-submersible Heave Plate Cost Rate",                   "$/tonne",            "",                       "wobos",            "?=6250",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "sSteelCR",                       "Secondary/Outfitting Steel Cost Rate",                     "$/tonne",            "",                       "wobos",            "?=7250",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "moorDia",                        "Mooring Line Diameter",                                    "m",                  "",                       "wobos",            "",                        "MIN=0.09",                      ""},
   { SSC_INPUT,        SSC_NUMBER,      "moorCR",                         "Mooring Line Cost Rate",                                   "$/m",                "",                       "wobos",            "",                        "MIN=399",                       ""},
   { SSC_INPUT,        SSC_NUMBER,      "scourMat",                       "Scour Protection Material Cost",                           "$/location",         "",                       "wobos",            "?=250000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "number_install_seasons",         "Number of Installation Seasons",                           "",                   "",                       "wobos",            "?=1",                     "",                              ""},


//Electrical Infrastructure
   { SSC_INPUT,        SSC_NUMBER,      "pwrFac",                         "Power Transfer Efficiency Factor",                         "%",                  "",                       "wobos",            "?=0.95",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "buryFac",                        "Cable Burial Depth Factor",                                "%/m",                "",                       "wobos",            "?=0.1",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "catLengFac",                     "Catenary Cable Length Factor",                             "%",                  "",                       "wobos",            "?=0.04",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "exCabFac",                       "Excess Cable Factor",                                      "%",                  "",                       "wobos",            "?=0.1",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "subsTopFab",                     "Offshore Substation Fabrication Cost",                     "$/tonne",            "",                       "wobos",            "?=14500",                 "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "subsTopDes",                     "Offshore Substation Design Cost",                          "$",                  "",                       "wobos",            "?=4500000",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "topAssemblyFac",                 "Offshore Substation Land-based Assembly Factor",           "%",                  "",                       "wobos",            "?=0.075",                 "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "subsJackCR",                     "Offshore Substation Jacket Lattice Cost Rate",             "$/tonne",            "",                       "wobos",            "?=6250",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "subsPileCR",                     "Offshore Substation Jacket Pile Cost Rate",                "$/tonne",            "",                       "wobos",            "?=2250",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "dynCabFac",                      "Dynamic Cable Cost Premium Factor",                        "",                   "",                       "wobos",            "?=2",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "shuntCR",                        "Shunt Reactor Cost Rate",                                  "$/MVA",              "",                       "wobos",            "?=35000",                 "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "highVoltSG",                     "High Voltage Switchgear Cost",                             "$",                  "",                       "wobos",            "?=950000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "medVoltSG",                      "Medium Voltage Switchgear Cost",                           "$",                  "",                       "wobos",            "?=500000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "backUpGen",                      "Back up Diesel Generator Cost",                            "$",                  "",                       "wobos",            "?=1000000",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "workSpace",                      "Offshore Substation Workspace & Accommodations Cost",      "$",                  "",                       "wobos",            "?=2000000",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "otherAncillary",                 "Other Ancillary Systems Costs",                            "$",                  "",                       "wobos",            "?=3000000",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "mptCR",                          "Main Power Transformer Cost Rate",                         "$/MVA",              "",                       "wobos",            "?=12500",                 "",                              ""},
   //{ SSC_INPUT,        SSC_ARRAY,       "arrCables",                      "Array Cables and Specifications",                          "",                   "",                       "wobos",            "*",                       "",                              ""},
   //{ SSC_INPUT,        SSC_ARRAY,       "expCables",                      "Export Cables and Specifications",                         "",                   "",                       "wobos",            "*",                       "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "arrVoltage",                     "Array cable voltage",                                      "kV",                 "",                       "wobos",            "?=33",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "cab1CR",                         "Array cable 1 Cost Rate",                                  "$/m",                "",                       "wobos",            "?=185.889",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "cab2CR",                         "Array cable 2 Cost Rate",                                  "$/m",                "",                       "wobos",            "?=202.788",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "cab1CurrRating",                 "Array cable 1 current rating",                             "A",                  "",                       "wobos",            "?=300",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "cab2CurrRating",                 "Array cable 2 current rating",                             "A",                  "",                       "wobos",            "?=340",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "arrCab1Mass",                    "Array cable 1 mass",                                       "kg/m",               "",                       "wobos",            "?=20.384",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "arrCab2Mass",                    "Array cable 2 mass",                                       "kg/m",               "",                       "wobos",            "?=21.854",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "cab1TurbInterCR",                "Cable 1 turbine interface cost",                           "$/interface",        "",                       "wobos",            "?=8410",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "cab2TurbInterCR",                "Cable 2 turbine interface cost",                           "$/interface",        "",                       "wobos",            "?=8615",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "cab2SubsInterCR",                "Cable 2 substation interface cost",                        "$/interface",        "",                       "wobos",            "?=19815",                 "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "expVoltage",                     "Export cable voltage",                                     "kV",                 "",                       "wobos",            "?=220",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "expCurrRating",                  "Export cable current rating",                              "A",                  "",                       "wobos",            "?=530",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "expCabMass",                     "Export cable mass",                                        "kg/m",               "",                       "wobos",            "?=71.9",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "expCabCR",                       "Export cable cost rate",                                   "$/m",                "",                       "wobos",            "?=495.411",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "expSubsInterCR",                 "Export cable substation interface cost",                   "$/interface",        "",                       "wobos",            "?=57500",                 "",                              ""},



//Assembly & Installation
   { SSC_INPUT,        SSC_NUMBER,      "moorTimeFac",                    "Anchor & Mooring Water Depth Time Factor",                 "",                   "",                       "wobos",            "?=0.005",                 "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "moorLoadout",                    "Anchor & Mooring Loadout Time",                            "hours",              "",                       "wobos",            "?=5",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "moorSurvey",                     "Survey Mooring Lines & Anchor Positions Time",             "hours",              "",                       "wobos",            "?=4",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "prepAA",                         "Prepare Inshore Assembly Area For Turbine Installation",   "hours",              "",                       "wobos",            "?=168",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "prepSpar",                       "Prepare Spar for Tow to Inshore Assembly Area",            "hours",              "",                       "wobos",            "?=18",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "upendSpar",                      "Upend and Ballast Spar",                                   "hours",              "",                       "wobos",            "?=36",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "prepSemi",                       "Prepare Semi-submersible for Turbine Installation",        "hours",              "",                       "wobos",            "?=12",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "turbFasten",                     "Prepare and Fasten Turbine for Transport",                 "hours/turbine",      "",                       "wobos",            "?=8",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "boltTower",                      "Lift and Bolt Tower Section",                              "hours",              "",                       "wobos",            "?=7",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "boltNacelle1",                   "Lift and Bolt Nacelle Individual Components Method",       "hours",              "",                       "wobos",            "?=7",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "boltNacelle2",                   "Lift and Bolt Nacelle Bunny Ears Method",                  "hours",              "",                       "wobos",            "?=7",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "boltNacelle3",                   "Lift and Bolt Nacelle Fully Assembled Rotor Method",       "hours",              "",                       "wobos",            "?=7",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "boltBlade1",                     "Lift and Bolt Blade Individual Components Method",         "hours",              "",                       "wobos",            "?=3.5",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "boltBlade2",                     "Lift and Bolt Blade Bunny Ears Method",                    "hours",              "",                       "wobos",            "?=3.5",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "boltRotor",                      "Lift and Bolt Rotor Fully Assembled Rotor Method",         "hours",              "",                       "wobos",            "?=7",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "vesselPosTurb",                  "Vessel Positioning Time Turbine Installation",             "hours",              "",                       "wobos",            "?=2",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "vesselPosJack",                  "Vessel Positioning Time Jacket Installation",              "hours",              "",                       "wobos",            "?=8",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "vesselPosMono",                  "Vessel Positioning Time Monopile Installation",            "hours",              "",                       "wobos",            "?=3",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "subsVessPos",                    "Vessel Positioning Time Offshore Substation Installation", "hours",              "",                       "wobos",            "?=6",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "monoFasten",                     "Prepare and Fasten Monopile for Transport",                "hours/unit",         "",                       "wobos",            "?=12",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "jackFasten",                     "Prepare and Fasten Jacket for Transport",                  "hours/unit",         "",                       "wobos",            "?=20",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "prepGripperMono",                "Prepare Monopile Gripper and Upender",                     "hours",              "",                       "wobos",            "?=1.5",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "prepGripperJack",                "Prepare Jacket Gripper and Upender",                       "hours",              "",                       "wobos",            "?=8",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "placePiles",                     "Place Jacket Piles",                                       "hours",              "",                       "wobos",            "?=12",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "prepHamMono",                    "Prepare Hammer for Monopile Installation",                 "hours",              "",                       "wobos",            "?=2",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "prephamJack",                    "Prepare Hammer for jacket Piles Installation",             "hours",              "",                       "wobos",            "?=2",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "removeHamMono",                  "Remove Hammer for Monopile Installation",                  "hours",              "",                       "wobos",            "?=2",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "removeHamJack",                  "Remove Hammer for Jacket Piles Installation",              "hours",              "",                       "wobos",            "?=4",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "placeTemplate",                  "Place Jacket Pile Template on Seabed",                     "hours",              "",                       "wobos",            "?=4",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "placeJack",                      "Place Jacket Main Lattice onto Piles",                     "hours",              "",                       "wobos",            "?=12",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "levJack",                        "Level Jacket Main Lattice",                                "hours",              "",                       "wobos",            "?=24",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "hamRate",                        "Pile Hammer Rate",                                         "m/hour",             "",                       "wobos",            "?=20",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "placeMP",                        "Lift and Place Monopile for Hammering",                    "hours",              "",                       "wobos",            "?=3",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "instScour",                      "Install Scour Protection Around Monopile Base",            "hours",              "",                       "wobos",            "?=6",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "placeTP",                        "Place Transition Piece onto Monopile",                     "hours",              "",                       "wobos",            "?=3",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "groutTP",                        "Grout Transition Piece/Monopile Interface",                "hours",              "",                       "wobos",            "?=8",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "tpCover",                        "Install Transition Piece Cover",                           "hours",              "",                       "wobos",            "?=1.5",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "prepTow",                        "Prepare Floating Substructure for Tow to Site",            "hours",              "",                       "wobos",            "?=12",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "spMoorCon",                      "Connect Mooring Lines to Spar",                            "hours",              "",                       "wobos",            "?=20",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "ssMoorCon",                      "Connect Mooring Lines to Semi-Submersible",                "hours",              "",                       "wobos",            "?=22",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "spMoorCheck",                    "Survey Spar Mooring Lines and Connections",                "hours",              "",                       "wobos",            "?=16",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "ssMoorCheck",                    "Survey Semi-submersible Mooing Lines and Connections",     "hours",              "",                       "wobos",            "?=12",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "ssBall",                         "Ballast Semi-submersible",                                 "hours",              "",                       "wobos",            "?=6",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "surflayRate",                    "Cable Surface Lay Rate",                                   "m/hour",             "",                       "wobos",            "?=375",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "cabPullIn",                      "Array Cable Pull in to Interfaces",                        "hours",              "",                       "wobos",            "?=5.5",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "cabTerm",                        "Cable Termination and Testing",                            "hours",              "",                       "wobos",            "?=5.5",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "cabLoadout",                     "Array Cable Loadout for Installation",                     "hours",              "",                       "wobos",            "?=14",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "buryRate",                       "Cable Burial Rate",                                        "m/hour",             "",                       "wobos",            "?=125",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "subsPullIn",                     "Cable Pull in to Offshore Substation",                     "hours",              "",                       "wobos",            "?=48",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "shorePullIn",                    "Cable Pull in to Onshore Infrastructure",                  "hours",              "",                       "wobos",            "?=96",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "landConstruct",                  "Onshore Infrastructure Construction",                      "days",               "",                       "wobos",            "?=7",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "expCabLoad",                     "Export Cable Loadout for Installation",                    "hours",              "",                       "wobos",            "?=24",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "subsLoad",                       "Offshore Substation Loadout for Installation",             "hours",              "",                       "wobos",            "?=60",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "placeTop",                       "Lift and Place Offshore Substation Topside",               "hours",              "",                       "wobos",            "?=24",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "nFeederBarge",                   "Number of Feeder Barges (Feeder Barge Strategy)",          "",                   "",                       "wobos",            "?=2",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "pileSpreadDR",                   "Piling Spread Day Rate",                                   "$/day",              "",                       "wobos",            "?=2500",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "pileSpreadMob",                  "Piling Spread Mobilization Cost",                          "$",                  "",                       "wobos",            "?=750000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "groutSpreadDR",                  "Grouting Spread Day Rate",                                 "$/day",              "",                       "wobos",            "?=3000",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "groutSpreadMob",                 "Grouting Spread Mobilization Cost",                        "$",                  "",                       "wobos",            "?=1000000",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "seaSpreadDR",                    "Suction Pile Anchor Spread Day Rate",                      "$/day",              "",                       "wobos",            "?=165000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "seaSpreadMob",                   "Suction Pile Anchor Spread Mobilization Cost",             "$",                  "",                       "wobos",            "?=4500000",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "compRacks",                      "Component Racks Cost",                                     "$",                  "",                       "wobos",            "?=1000000",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "cabSurveyCR",                    "Cable Route Survey Cost",                                  "$/m",                "",                       "wobos",            "?=240",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "cabDrillDist",                   "Horizontal Drilling distance for Cable Landfall",          "m",                  "",                       "wobos",            "?=500",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "cabDrillCR",                     "Cost Rate for Horizontal Drilling",                        "$/m",                "",                       "wobos",            "?=3200",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "mpvRentalDR",                    "MPV Rental Day Rate",                                      "$/day",              "",                       "wobos",            "?=72000",                 "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "diveTeamDR",                     "Cable Landfall Dive Team Day Rate",                        "$/day",              "",                       "wobos",            "?=3200",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "winchDR",                        "Cable Landfall Winch Day Rate",                            "$/day",              "",                       "wobos",            "?=1000",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "civilWork",                      "Onshore Infrastructure Civil Work Cost",                   "$",                  "",                       "wobos",            "?=40000",                 "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "elecWork",                       "Onshore Infrastructure Electrical Work Cost",              "$",                  "",                       "wobos",            "?=25000",                 "",                              ""},
   /*{ SSC_INPUT,        SSC_ARRAY,       "turbInstVessel",                 "Turbine Install Vessel Specifications",                    "",                   "",                       "wobos",            "*",                       "",                              ""},
   { SSC_INPUT,        SSC_ARRAY,       "turbFeederBarge",                "Turbine Install Feeder Barge Specifications",              "",                   "",                       "wobos",            "*",                       "",                              ""},
   { SSC_INPUT,        SSC_ARRAY,       "turbSupportVessels",             "Turbine Install Support Vessels",                          "",                   "",                       "wobos",            "*",                       "",                              ""},
   { SSC_INPUT,        SSC_ARRAY,       "subInstVessel",                  "Substructure Install Vessel Specifications",               "",                   "",                       "wobos",            "*",                       "",                              ""},
   { SSC_INPUT,        SSC_ARRAY,       "subFeederBarge",                 "Substructure Install Feeder Barge Specifications",         "",                   "",                       "wobos",            "*",                       "",                              ""},
   { SSC_INPUT,        SSC_ARRAY,       "subSupportVessels",              "Substructure Install Support Vessels",                     "",                   "",                       "wobos",            "*",                       "",                              ""},
   { SSC_INPUT,        SSC_ARRAY,       "arrCabInstVessel",               "Array Cable Install Vessel Specifications",                "",                   "",                       "wobos",            "*",                       "",                              ""},
   { SSC_INPUT,        SSC_ARRAY,       "expCabInstVessel",               "Export Cable Install Vessel Specifications",               "",                   "",                       "wobos",            "*",                       "",                              ""},
   { SSC_INPUT,        SSC_ARRAY,       "substaInstVessel",               "Offshore Substation Install Vessel Specifications",        "",                   "",                       "wobos",            "*",                       "",                              ""},
   { SSC_INPUT,        SSC_ARRAY,       "elecSupportVessels",             "Electrical Infrastructure Install Support Vessels",        "",                   "",                       "wobos",            "*",                       "",                              ""},
   */
//Port & Staging
   { SSC_INPUT,        SSC_NUMBER,      "nCrane600",                      "Number of 600 tonne Crawler Cranes",                       "",                   "",                       "wobos",            "",                        "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "nCrane1000",                     "Number of 1000 tonne Crawler Cranes",                      "",                   "",                       "wobos",            "",                        "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "crane600DR",                     "600 tonne Crawler Crane Day Rate",                         "$/day",              "",                       "wobos",            "?=5000",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "crane1000DR",                    "1000 tonne Crawler Crane Day Rate",                        "$/day",              "",                       "wobos",            "?=8000",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "craneMobDemob",                  "Port Crane Mobilization/Demobilization Cost",              "$",                  "",                       "wobos",            "?=150000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "entranceExitRate",               "Port Entrance and Exit Cost Rate",                         "$/occurrence",       "",                       "wobos",            "?=0.525",                 "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "dockRate",                       "Quayside Docking Cost Rate",                               "$/day",              "",                       "wobos",            "?=3000",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "wharfRate",                      "Wharf Loading and Unloading Cost Rate",                    "$/tonne",            "",                       "wobos",            "?=2.75",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "laydownCR",                      "Laydown and Storage Cost Rate",                            "$/m^2/day",          "",                       "wobos",            "?=0.25",                  "",                              ""},

//Engineering & Management
   { SSC_INPUT,        SSC_NUMBER,      "estEnMFac",                      "Estimated Engineering & Management Cost Factor",           "%",                  "",                       "wobos",            "?=0.04",                  "",                              ""},

//Development
   { SSC_INPUT,        SSC_NUMBER,      "preFEEDStudy",                   "Pre-FEED study Cost",                                      "$",                  "",                       "wobos",            "?=5000000",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "feedStudy",                      "FEED Study Cost",                                          "$",                  "",                       "wobos",            "?=10000000",              "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "stateLease",                     "State Leasing and Permitting Cost",                        "$",                  "",                       "wobos",            "?=250000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "outConShelfLease",               "Outer Continental Shelf Lease Cost",                       "$",                  "",                       "wobos",            "?=1000000",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "saPlan",                         "Site Assessment Plan Cost",                                "$",                  "",                       "wobos",            "?=500000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "conOpPlan",                      "Construction Operations Plan Cost",                        "$",                  "",                       "wobos",            "?=1000000",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "nepaEisMet",                     "NEPA Environmental Impact Statement Met Tower Cost",       "$",                  "",                       "wobos",            "?=2000000",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "physResStudyMet",                "Physical Resource Study Met Tower Cost",                   "$",                  "",                       "wobos",            "?=1500000",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "bioResStudyMet",                 "Biological Resource Study Met Tower Cost",                 "$",                  "",                       "wobos",            "?=1500000",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "socEconStudyMet",                "Socioeconomic and Land use Study Met Tower Cost",          "$",                  "",                       "wobos",            "?=500000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "navStudyMet",                    "Navigation and Transport Study Met Tower Cost",            "$",                  "",                       "wobos",            "?=500000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "nepaEisProj",                    "NEPA Environmental Impact Study Project Cost",             "$",                  "",                       "wobos",            "?=5000000",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "physResStudyProj",               "Physical Resource Study Project Cost",                     "$",                  "",                       "wobos",            "?=500000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "bioResStudyProj",                "Biological Resource Study Porject Cost",                   "$",                  "",                       "wobos",            "?=500000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "socEconStudyProj",               "Socioeconomic and Land use Study Project Cost",            "$",                  "",                       "wobos",            "?=200000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "navStudyProj",                   "Navigation and Transport Study Project Cost",              "$",                  "",                       "wobos",            "?=250000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "coastZoneManAct",                "Coastal Zone Management Act Compliance Cost",              "$",                  "",                       "wobos",            "?=100000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "rivsnHarbsAct",                  "Rivers & Harbors Act, Section 10 Compliance Cost",         "$",                  "",                       "wobos",            "?=100000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "cleanWatAct402",                 "Clean Water Act, Section 402 Compliance Cost",             "$",                  "",                       "wobos",            "?=100000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "cleanWatAct404",                 "Clean Water Act, Section 404 Compliance Cost",             "$",                  "",                       "wobos",            "?=100000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "faaPlan",                        "Federal Aviation Administration Plans & Mitigation Cost",  "$",                  "",                       "wobos",            "?=10000",                 "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "endSpecAct",                     "Endangered Species Act Compliance Cost",                   "$",                  "",                       "wobos",            "?=500000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "marMamProtAct",                  "Marine Mammal Protection Act Compliance Cost",             "$",                  "",                       "wobos",            "?=500000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "migBirdAct",                     "Migratory Bird Treaty Compliance Cost",                    "$",                  "",                       "wobos",            "?=500000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "natHisPresAct",                  "National Historic Preservation Act Compliance Cost",       "$",                  "",                       "wobos",            "?=250000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "addLocPerm",                     "Additional State and Local Permitting Cost",               "$",                  "",                       "wobos",            "?=200000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "metTowCR",                       "Meteorological (Met) Tower Fabrication & Install Cost",    "$/MW",               "",                       "wobos",            "?=11518",                 "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "decomDiscRate",                  "Decommissioning Cost Discount Rate",                       "%",                  "",                       "wobos",            "?=0.03",                  "",                              ""},
   
   //OUTPUTS
    //General outputs
   {SSC_OUTPUT,        SSC_NUMBER,      "hubD",                           "Hub Diameter",                                             "m",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "bladeL",                         "Blade Length",                                             "m",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "chord",                          "Blade Max Chord",                                          "m",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "nacelleW",                       "Nacelle Width",                                            "m",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "nacelleL",                       "Nacelle Length",                                           "m",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "rnaM",                           "Rotor-Nacelle Assembly Mass"                               "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "towerD",                         "Tower Diameter",                                           "m",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "towerM",                         "Tower Mass",                                               "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "construction_finance_factor",    "Construction Finance Factor",                              "",                   "",                       "wobos",            "",                        "",                              ""},

    //Substructure & Foundation outputs
   {SSC_OUTPUT,        SSC_NUMBER,      "mpileM",                         "Monopile Pile Mass",                                       "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "mtransM",                        "Monopile Transition Piece Mass",                           "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "mPileCost",                      "Monopile Pile Cost",                                       "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "mTransCost",                     "Monopile Transition Piece Cost",                           "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "jlatticeM",                      "Jacket Main Lattice Mass",                                 "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "jtransM",                        "Jacket Transtion Piece Mass",                              "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "jpileM",                         "Jacket Piles Mass (total for 4 piles)",                    "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "jLatticeCost",                   "Jacket Main Lattice Cost",                                 "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "jTransCost",                     "Jacket Transition Piece Cost",                             "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "jPileCost",                      "Jacket Piles Cost (total for 4 piles)",                    "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "spStifColM",                     "Spar Stiffened Column Mass",                               "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "spTapColM",                      "Spar Tapered Column Mass",                                 "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "spStifColCost",                  "Spar Stiffened Column Cost",                               "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "spTapColCost",                   "Spar Tapered Column Cost",                                 "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "ballM",                          "Spar Ballast Mass",                                        "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "ballCost",                       "Spar Ballast Cost",                                        "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "ssStifColM",                     "Semi-submersible Stiffened Column Mass",                   "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "ssTrussM",                       "Semi-submersible Truss Mass",                              "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "ssHeaveM",                       "Semi-submersible Heave Plate Mass",                        "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "ssStifColCost",                  "Semi-submersible Stiffened Column Cost",                   "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "ssTrussCost",                    "Semi-submersible Truss Cost",                              "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "ssHeaveCost",                    "Semi-submersible Heave Plate Cost",                        "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "moorSysCost",                    "Mooring and Anchor System Cost",                           "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "sSteelM",                        "Secondary/Outfitting Steel Mass",                          "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "sSteelCost",                     "Secondary/Outfitting Steel Cost",                          "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "subTotM",                        "Total Substructure Mass per Turbine",                      "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "subTotCost",                     "Substructure & Foundation Total Cost",                     "$",                  "",                       "wobos",            "",                        "",                              ""},

    //Electrical Infrastructure outputs
   {SSC_OUTPUT,        SSC_NUMBER,      "systAngle",                      "Floating System Angle",                                    "degrees",            "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "freeCabLeng",                    "Free Hanging Cable Length",                                "m",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "fixCabLeng",                     "Fixed Cable Length",                                       "m",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "nExpCab",                        "Number of Export Cables",                                  "",                   "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "nSubstation",                    "Number Of Substations",                                    "",                   "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "fullStrings",                    "Full Array Strings",                                       "",                   "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "nTurbPS",                        "Number of Turbines per Partial Array String",              "",                   "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "nTurbCab1",                      "Number of Turbines per Array Cable #1",                    "",                   "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "nTurbCab2",                      "Number of Turbines per Array Cable #2",                    "",                   "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "nTurbInter1",                    "Number of Turbine Interfaces per Array Cable #1",          "",                   "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "nTurbInter2",                    "Number of Turbine Interfaces per Array Cable #2",          "",                   "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "nSubsInter",                     "Number of Array Cable Substation Interfaces",              "",                   "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "cab1Leng",                       "Array Cable #1 Length",                                    "m",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "cab2Leng",                       "Array Cabel #2 Length",                                    "m",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "expCabLeng",                     "Export Cable Length",                                      "m",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "nMPT",                           "Number of Main Power Transformers (MPTs)",                 "",                   "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "mptRating",                      "MPT Rating",                                               "MVA",                "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "mptCost",                        "MPT Cost",                                                 "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "subsTopM",                       "Substation Topside Mass",                                  "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "subsTopCost",                    "Substation Topside Cost",                                  "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "arrCab1Cost",                    "Array Cable #1 and Ancillary Cost",                        "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "arrCab2Cost",                    "Array Cable #2 and Ancillary Cost",                        "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "expCabCost",                     "Export Cable and Ancillary Cost",                          "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "shuntReactors",                  "Shunt Reactor Cost",                                       "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "switchGear",                     "Switchgear Cost",                                          "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "ancillarySys",                   "Offshore Substation Ancillary Systems Cost",               "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "subsSubM",                       "Offshore Substation Substructure Mass",                    "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "subsPileM",                      "Offshore Substation Jacket Piles Mass",                    "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "subsLandAssembly",               "Offshore Substation Land-based Assembly Cost",             "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "subsSubCost",                    "Offshore Substation Substructure Cost,"                    "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "switchYard",                     "Onshore Switch Yard Cost",                                 "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "onShoreSubs",                    "Onshore Substation Cost",                                  "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "onshoreMisc",                    "Onshore Misc. Costs",                                      "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "transLine",                      "Overhead Transmission Line Cost",                          "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "subCabCost",                     "Total Subsea Cable and Ancillary Cost",                    "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "offSubsCost",                    "Total Offshore Substation Cost",                           "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "onshoreTransCost",               "Total Onshore Transmission System Cost",                   "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "totElecCost",                    "Total Electrical Infrastructure Cost"                      "$",                  "",                       "wobos",            "",                        "",                              ""},

    //Assembly & Installation outputs
   {SSC_OUTPUT,        SSC_NUMBER,      "moorTime",                       "Mooring and Anchor System Installation Time",              "days",               "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "turbDeckArea",                   "Deck Area Required per Turbine",                           "m^2",                "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "nTurbPerTrip",                   "Maximum Number of Turbines per Vessel Trip",               "",                   "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "turbInstTime",                   "Turbine Installation Time",                                "days",               "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "subDeckArea",                    "Deck Area Required per Substructure",                      "m^2",                "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "nSubPerTrip",                    "Maximum Number of Substructures per Vessel Trip",          "",                   "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "subInstTime",                    "Substructure Installation Time",                           "days",               "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "cab1SecM",                       "Array Cable #1 Section Mass",                              "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "cab2SecM",                       "Array Cable #2 Section Mass",                              "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "cab1SecPerTrip",                 "Array Cable #1 Sections per Vessel Trip",                  "",                   "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "cab2SecPerTrip",                 "Array Cable #2 Sections per Vessel Trip",                  "",                   "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "arrInstTime",                    "Array Cable System Installation Time",                     "days",               "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "expCabSecM",                     "Export Cable Section Mass",                                "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "expCabSecPerTrip",               "Export Cable Sections per Vessel Trip",                    "",                   "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "expInstTime",                    "Export Cable Installation Time",                           "days",               "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "subsInstTime",                   "Offshore Substation Installation Time",                    "days",               "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "totInstTime",                    "Total Installation Time",                                  "days",               "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "totAnICost",                     "Total Assembly & Installation Cost",                       "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_ARRAY,       "turbCostsByVessel",              "Turbine Installation Vessel Costs",                        "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_ARRAY,       "subCostsByVessel",               "Substructure Installation Vessel Costs",                   "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_ARRAY,       "elecCostsByVessel",              "Electrical Infrastructure Installation Vessel Costs",      "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_ARRAY,       "mobDemobCostByVessel",           "Vessel Mobilization and Demobilization Cost",              "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "cabSurvey",                      "Cable Route Survey Cost",                                  "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "turbine_install_cost",           "Turbine Install Cost",                                     "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "substructure_install_cost",      "Substructure Install Cost",                                "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "electrical_install_cost",        "Electrical Install Cost",                                  "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "mob_demob_cost",                 "Mobilization/Demobilization Cost",                         "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "array_cable_install_cost",       "Array Cable Installation Cost",                            "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "export_cable_install_cost",      "Export Cable Installation Cost",                           "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "substation_install_cost",        "Substation Installation Cost",                             "$",                  "",                       "wobos",            "",                        "",                              ""},


    //Port & Staging outputs
   {SSC_OUTPUT,        SSC_NUMBER,      "entrExitCost",                   "Port Entrance and Exit Cost",                              "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "wharfCost",                      "Port Wharf Cost",                                          "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "dockCost",                       "Port Docking Cost",                                        "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "subLaydownA",                    "Substructure Laydown and Storage Area",                    "m^2",                "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "subLayCost",                     "Substructure Laydown and Storage Cost",                    "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "turbLaydownA",                   "Turbine Laydown and Storage Area",                         "m^2",                "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "turbLayCost",                    "Turbine Laydown and Storage Cost",                         "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "craneCost",                      "Port Craneage Cost",                                       "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "totPortCost",                    "Total Port Cost",                                          "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "totStageCost",                   "Total Staging Cost",                                       "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "totPnSCost",                     "Total Port & Staging Cost",                                "$",                  "",                       "wobos",            "",                        "",                              ""},

    //Engineering & Management outputs
   {SSC_OUTPUT,        SSC_NUMBER,      "totEnMCost",                     "Total Engineering & Management Cost",                      "$",                  "",                       "wobos",            "",                        "",                              ""},

    //Development outputs
   {SSC_OUTPUT,        SSC_NUMBER,      "feedCost",                       "Front End Engineering Design (FEED) Cost",                 "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "permStudyComp",                  "Permitting, Studies, and Compliance Cost",                 "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "metFabCost",                     "Meteorological Tower Fabrication and Installation Cost",   "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "decomCost",                      "Decommissioning Expense",                                  "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "totDevCost",                     "Total Development Cost",                                   "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "commissioning",                  "Plant Commissioning Cost",                                 "$",                  "",                       "wobos",            "",                        "",                              ""},
   
   //Total BOS Cost
   {SSC_OUTPUT,        SSC_NUMBER,      "totalBOScost",                   "Total Balance of System Cost",                             "$",                  "",                       "wobos",            "",                        "",                              ""},

   var_info_invalid};

class cm_wind_obos : public compute_module
{
public:
	cm_wind_obos()
	{
		add_var_info(_cm_vtab_wind_obos);
	}

	// BEGIN WRAPPER OF LIB_OBOS 
	// **********************************************************************

	void exec() throw(general_error)
	{
		//Create an instance of the wobos class********************************************************************************************************
		wobos obos;

		//Assign inputs***********************************************************************************************************************************
		//Basic inputs
		obos.turbCapEx = (double)as_number("turbCapEx");
		obos.nTurb = (double)as_number("nTurb"); //number of turbines
		obos.rotorD = (double)as_number("rotorD");//rotor diameter (m)
		obos.turbR = (double)as_number("turbR");//turbine rating (MW)
		obos.hubH = (double)as_number("hubH");//hub height (m)
		obos.waterD = (double)as_number("waterD");// water depth (m)
		obos.distShore = (double)as_number("distShore");//distance to shore from install site (km)
		obos.distPort = (double)as_number("distPort");//distance to install site from install port (km)
		obos.distPtoA = (double)as_number("distPtoA");//distance from install port to inshore assembly area (km) (spar only)
		obos.distAtoS = (double)as_number("distAtoS");//distance from inshore assembly area to install site (km) (spar Only)
		obos.substructure = (int)as_number("substructure"); //substructure type
		obos.anchor = (int)as_number("anchor"); //anchor type
		obos.turbInstallMethod = (int)as_number("towerInstallMethod"); //tower installation method
		obos.installStrategy = (int)as_number("installStrategy"); //installation vessel strategy
		obos.cableOptimizer = (int)as_number("cableOptimizer"); //switch to turn cable optimizer on or off
		obos.moorLines = (double)as_number("moorLines");//number of mooring lines for floating substructures
		obos.buryDepth = (double)as_number("buryDepth");//array and export cable burial depth (m)
		obos.arrayY = (double)as_number("arrayY");//turbine array spacing between turbines on same row (rotor diameters)
		obos.arrayX = (double)as_number("arrayX");// turbine array spacing between turbine rows (rotor diameters)
		obos.substructCont = (double)as_number("substructCont") / 100;//substructure install weather contingency, convert from percentage to decimal
		obos.turbCont = (double)as_number("turbCont") / 100;//turbine install weather contingency, convert from percentage to decimal
		obos.elecCont = (double)as_number("elecCont") / 100;//turbine install weather contingency, convert from percentage to decimal
		obos.interConVolt = (double)as_number("interConVolt");//grid interconnect voltage (kV)
		obos.distInterCon = (double)as_number("distInterCon");//distance from onshore substation to grid interconnect (miles)
		obos.scrapVal = (double)as_number("scrapVal");//scrap value of decommissioned components ($)
        obos.number_install_seasons = (double)as_number("number_install_seasons");//number of installation seasons

		//Detailed inputs
		//General
		obos.projLife = (double)as_number("projLife");//economic lifetime of the project (years)
		obos.inspectClear = (double)as_number("inspectClear");//inspection clearance for substructure and turbine components (m)
        obos.plantComm = (double)as_number("plantComm");//plant commissioning cost factor
        obos.procurement_contingency = (double)as_number("procurement_contingency") / 100; //convert from percentage to decimal
        obos.install_contingency = (double)as_number("install_contingency") / 100; //convert from percentage to decimal
        obos.construction_insurance = (double)as_number("construction_insurance") / 100; //convert from percentage to decimal
        obos.capital_cost_year_0 = (double)as_number("capital_cost_year_0") /100; //convert from percentage to decimal
		obos.capital_cost_year_1 = (double)as_number("capital_cost_year_1") / 100; //convert from percentage to decimal
		obos.capital_cost_year_2 = (double)as_number("capital_cost_year_2") / 100; //convert from percentage to decimal
		obos.capital_cost_year_3 = (double)as_number("capital_cost_year_3") / 100; //convert from percentage to decimal
		obos.capital_cost_year_4 = (double)as_number("capital_cost_year_4") / 100; //convert from percentage to decimal
		obos.capital_cost_year_5 = (double)as_number("capital_cost_year_5") / 100; //convert from percentage to decimal
		obos.tax_rate = (double)as_number("tax_rate") / 100; //convert from percentage to decimal
		obos.interest_during_construction = (double)as_number("interest_during_construction") / 100; //convert from percentage to decimal
		
		//Substructure & Foundation
		obos.mpileCR = (double)as_number("mpileCR");//monopile pile cost rate ($/tonne)
		obos.mtransCR = (double)as_number("mtransCR");//monopile transition piece cost rate ($/tonne)
		if (is_assigned("mpileD")) obos.mpileD = (double)as_number("mpileD"); else obos.mpileD = -999; //monopile diameter(m), negative filtered by wobos::run()
		if (obos.mpileD <= 0 && obos.mpileD != -999)
			throw exec_error("wind_obos", "Monopile diameter (mpileD) must be > 0, or -999 to allow the model to calculate the diameter.");
		if (is_assigned("mpileL")) obos.mpileL = (double)as_number("mpileL"); else obos.mpileL = -999; //monopile length (m), negative filtered by wobos::run()
		if (obos.mpileL <= 0 && obos.mpileL != -999)
			throw exec_error("wind_obos", "Monopile length (mpileL) must be > 0, or -999 to allow the model to calculate the length.");
		obos.jlatticeCR = (double)as_number("jlatticeCR");//jacket lattice cost rate ($/tonne)
		obos.jtransCR = (double)as_number("jtransCR");//jacket transition piece cost rate ($/tonne)
		obos.jpileCR = (double)as_number("jpileCR");//jacket pile cost rate ($/tonne)
		obos.jlatticeA = (double)as_number("jlatticeA");//jacket lattice footprint area
		obos.jpileL = (double)as_number("jpileL");//jacket pile length
		obos.jpileD = (double)as_number("jpileD");//jacket pile diameter
		obos.spStifColCR = (double)as_number("spStifColCR");//spar stiffened column cost rate ($/tonne)
		obos.spTapColCR = (double)as_number("spTapColCR");//spar tapered column cost rate ($/tonne)
		obos.ballCR = (double)as_number("ballCR");//ballast cost rate ($/tonne)
		obos.deaFixLeng = (double)as_number("deaFixLeng");//drag embedment anchor fixed mooring line length
		obos.ssStifColCR = (double)as_number("ssStifColCR");//semisubmersible stiffened column cost rate ($/tonne)
		obos.ssTrussCR = (double)as_number("ssTrussCR");// semisubmersible truss cost rate ($/tonne)
		obos.ssHeaveCR = (double)as_number("ssHeaveCR");//semisubmersible heave plate cost rate ($/tonne)
		obos.sSteelCR = (double)as_number("sSteelCR");//secondary steel cost rate ($/tonne)
		if (is_assigned("moorDia")) obos.moorDia = (double)as_number("moorDia"); else obos.moorDia = -999; //this gets filtered in the MooringSys function
		if (obos.moorDia < 0.09 && obos.moorDia != -999)
			throw exec_error("wind_obos", "Mooring line diameter (moorDia) must be >= 0.09, or -999 to allow the model to calculate the diameter.");
		if (is_assigned("moorCR")) obos.moorCR = (double)as_number("moorCR"); else obos.moorCR = -999; //this gets filtered in the MooringSys function
		if (obos.moorCR < 399 && obos.moorCR != -999)
			throw exec_error("wind_obos", "Mooring line cost rate (moorCR) must be >= 399, or -999 to allow the model to calculate the diameter.");
		obos.mpEmbedL = (double)as_number("mpEmbedL");//monopile embedment length (m)
		obos.scourMat = (double)as_number("scourMat");

		//Electrical Infrastructure
		obos.pwrFac = (double)as_number("pwrFac");//power factor to estimate losses
		obos.buryFac = (double)as_number("buryFac");//cable burial factor
		obos.catLengFac = (double)as_number("catLengFac");//free hanging or catenary cable length factor
		obos.exCabFac = (double)as_number("exCabFac");// excess cable factor
		obos.subsTopFab = (double)as_number("subsTopFab");//substation topside fabrication cost ($/tonne)
		obos.subsTopDes = (double)as_number("subsTopDes");//substation topside design cost ($)
		obos.topAssemblyFac = (double)as_number("topAssemblyFac");//land based substation topside assembly factor
		obos.subsJackCR = (double)as_number("subsJackCR");//substation jacket substructure cost rate ($/tonne)
		obos.subsPileCR = (double)as_number("subsPileCR");//substation jacket pile cost rate ($/tonne)
		obos.dynCabFac = (double)as_number("dynCabFac");//dynamic/free hanging cable cost premium
		obos.shuntCR = (double)as_number("shuntCR");//shunt reactor cost rate ($/MVA)
		obos.highVoltSG = (double)as_number("highVoltSG");//high voltage switchgear cost ($)
		obos.medVoltSG = (double)as_number("medVoltSG");//medium voltage switchgear cost ($)
		obos.backUpGen = (double)as_number("backUpGen");//back up generator cost ($)
		obos.workSpace = (double)as_number("workSpace");//substation workshop and accommodations cost ($)
		obos.otherAncillary = (double)as_number("otherAncillary");//substation other ancillary costs ($)
		obos.mptCR = (double)as_number("mptCR");//main power transformer cost rate ($/MVA)
		obos.arrVoltage = (double)as_number("arrVoltage");//main power transformer cost rate ($/MVA)
		obos.cab1CR = (double)as_number("cab1CR");//main power transformer cost rate ($/MVA)
		obos.cab2CR = (double)as_number("cab2CR");//main power transformer cost rate ($/MVA)
		obos.cab1CurrRating = (double)as_number("cab1CurrRating");//main power transformer cost rate ($/MVA)
		obos.cab2CurrRating = (double)as_number("cab2CurrRating");//main power transformer cost rate ($/MVA)
		obos.arrCab1Mass = (double)as_number("arrCab1Mass");//main power transformer cost rate ($/MVA)
		obos.arrCab2Mass = (double)as_number("arrCab2Mass");//main power transformer cost rate ($/MVA)
		obos.cab1TurbInterCR = (double)as_number("cab1TurbInterCR");//main power transformer cost rate ($/MVA)
		obos.cab2TurbInterCR = (double)as_number("cab2TurbInterCR");//main power transformer cost rate ($/MVA)
		obos.cab2SubsInterCR = (double)as_number("cab2SubsInterCR");//main power transformer cost rate ($/MVA)
		obos.expVoltage = (double)as_number("expVoltage");//main power transformer cost rate ($/MVA)
		obos.expCurrRating = (double)as_number("expCurrRating");//main power transformer cost rate ($/MVA)
		obos.expCabMass = (double)as_number("expCabMass");//main power transformer cost rate ($/MVA)
		obos.expCabCR = (double)as_number("expCabCR");//main power transformer cost rate ($/MVA)
		obos.expSubsInterCR = (double)as_number("expSubsInterCR");//main power transformer cost rate ($/MVA)

		//Assembly & Installation
		obos.moorTimeFac = (double)as_number("moorTimeFac");//mooring installation timing factor (hrs/m)
		obos.moorLoadout = (double)as_number("moorLoadout");//mooring system loadout timing (hrs)
		obos.moorSurvey = (double)as_number("moorSurvey");//mooring system anchor position survey timing (hrs)
		obos.prepAA = (double)as_number("prepAA");//prep inshore assembly area timing (hrs)
		obos.prepSpar = (double)as_number("prepSpar");//prep spare for tow out to assembly area timing (hrs)
		obos.upendSpar = (double)as_number("upendSpar");//upend and ballast the spar timing (hrs)
		obos.prepSemi = (double)as_number("prepSemi");//prep semisubmersible for turbine install timing (hrs)
		obos.turbFasten = (double)as_number("turbFasten");//fasten turbine for transport timing (hrs)
		obos.boltTower = (double)as_number("boltTower");// bolt tower to substructure timing (hrs)
		obos.boltNacelle1 = (double)as_number("boltNacelle1");//bolt nacelle to tower timing individual components method (hrs)
		obos.boltNacelle2 = (double)as_number("boltNacelle2");//bolt nacelle to tower timing bunny ears method (hrs)
		obos.boltNacelle3 = (double)as_number("boltNacelle3");//bolt nacelle to tower timing assembled rotor method (hrs)
		obos.boltBlade1 = (double)as_number("boltBlade1");//bolt blade to rotor timing individual components method (hrs)
		obos.boltBlade2 = (double)as_number("boltBlade2");//bolt blade to rotor timing bunny ears method (hrs)
		obos.boltRotor = (double)as_number("boltRotor");//bolt rotor to nacelle timing assembled rotor method (hrs)
		obos.vesselPosTurb = (double)as_number("vesselPosTurb");//vessel positioning timing turbine install (hrs)
		obos.vesselPosJack = (double)as_number("vesselPosJack");//vessel positioning timing jacket install (hrs)
		obos.vesselPosMono = (double)as_number("vesselPosMono");//vessel positioning timing monopile install (hrs)
		obos.subsVessPos = (double)as_number("subsVessPos");//vessel positioning timing offshore substation install (hrs)
		obos.monoFasten = (double)as_number("monoFasten");//fasten monopile for transport timing (hrs)
		obos.jackFasten = (double)as_number("jackFasten");//fasten jacket for transport timing (hrs)
		obos.prepGripperMono = (double)as_number("prepGripperMono");//prepare pile gripper and upender timing monopile install (hrs)
		obos.prepGripperJack = (double)as_number("prepGripperJack");//prepare pile gripper and upender timing iacket install (hrs)
		obos.placePiles = (double)as_number("placePiles");//lift and place jacket piles timing (hrs)
		obos.prepHamMono = (double)as_number("prepHamMono");//prepare pile hammer timing monopile install (hrs)
		obos.removeHamMono = (double)as_number("removeHamMono");//remove hammer timing monopile install (hrs)
		obos.prepHamJack = (double)as_number("prepHamJack");//prepare pile hammer timing iacket install (hrs)
		obos.removeHamJack = (double)as_number("removeHamJack");//remove hammer timing iacket install (hrs)
		obos.placeJack = (double)as_number("placeJack");//place  jacket timing (hrs)
		obos.levJack = (double)as_number("levJack");//level jacket timing (hrs)
		obos.placeTemplate = (double)as_number("placeTemplate");//place jacket template timing (hrs)
		obos.hamRate = (double)as_number("hamRate");//pile hammer rate (m/hr)
		obos.placeMP = (double)as_number("placeMP");//place monopile pile timing (hrs)
		obos.instScour = (double)as_number("instScour");//install scour protection (hrs)
		obos.placeTP = (double)as_number("placeTP");//place transition piece on monopile timing (hrs)
		obos.groutTP = (double)as_number("groutTP");//grout transition piece (hrs)
		obos.tpCover = (double)as_number("tpCover");//install transition piece cover timing (hrs)
		obos.prepTow = (double)as_number("prepTow");//prep floating substructure for towing timing (hrs)
		obos.spMoorCon = (double)as_number("spMoorCon");//connect spar to mooring system timing (hrs)
		obos.ssMoorCon = (double)as_number("ssMoorCon");//connect semisubmersible to mooring system (hrs)
		obos.spMoorCheck = (double)as_number("spMoorCheck");//check mooring connections to spar timing (hrs)
		obos.ssMoorCheck = (double)as_number("ssMoorCheck");//check mooring connections to semisubmersible timing (hrs)
		obos.ssBall = (double)as_number("ssBall");//ballast semisubmersible timing (hrs)
		obos.surfLayRate = (double)as_number("surfLayRate");//electrical cable surface lay rate (m/hr)
		obos.cabPullIn = (double)as_number("cabPullIn");//array cable pull in to interfaces timing (hrs)
		obos.cabTerm = (double)as_number("cabTerm");//cable termination and testing timing (hrs)
		obos.cabLoadout = (double)as_number("cabLoadout");//array cable loadout timing (hrs)
		obos.buryRate = (double)as_number("buryRate");//cable bury rate (m/hr)
		obos.subsPullIn = (double)as_number("subsPullIn");//cable pull in to substation timing (hrs)
		obos.shorePullIn = (double)as_number("shorePullIn");//cable pull in to shore timing (hrs)
		obos.landConstruct = (double)as_number("landConstruct");//land construction of required onshore electrical systems timing (days)
		obos.expCabLoad = (double)as_number("expCabLoad");//export cable loadout timing (hrs)
		obos.subsLoad = (double)as_number("subsLoad");//substation loadout timing (hrs)
		obos.placeTop = (double)as_number("placeTop");//lift and place substation topside timing (hrs)
		obos.pileSpreadDR = (double)as_number("pileSpreadDR");//piling equipment spread day rate ($/day)
		obos.pileSpreadMob = (double)as_number("pileSpreadMob");//piling equipment spread mobilization/demobilization cost ($)
		obos.groutSpreadDR = (double)as_number("groutSpreadDR");//grouting equipment spread day rate ($/day)
		obos.groutSpreadMob = (double)as_number("groutSpreadMob");//grouting equipment spread mobilization/demobilization cost ($)
		obos.seaSpreadDR = (double)as_number("seaSpreadDR");//suction pile anchor vessel and equipment spread day rate ($/day)
		obos.seaSpreadMob = (double)as_number("seaSpreadMob");//suction pile anchor vessel and equipment spread mobilization/demobilization cost ($)
		obos.compRacks = (double)as_number("compRacks");//component racks cost ($)
		obos.cabSurveyCR = (double)as_number("cabSurveyCR");//cost rate of surveying and verifying electrical cable installation ($/)
		obos.cabDrillDist = (double)as_number("cabDrillDist");
		obos.cabDrillCR = (double)as_number("cabDrillCR");
		obos.mpvRentalDR = (double)as_number("mpvRentalDR");
		obos.diveTeamDR = (double)as_number("diveTeamDR");
		obos.winchDR = (double)as_number("winchDR");
		obos.civilWork = (double)as_number("civilWork");
		obos.elecWork = (double)as_number("elecWork");

		//Port & Staging
		if (is_assigned("nCrane600")) obos.nCrane600 = (double)as_number("nCrane600"); else obos.nCrane600 = -999; //this gets filtered in the NumCranes function
		if (is_assigned("nCrane1000")) obos.nCrane1000 = (double)as_number("nCrane1000"); else obos.nCrane1000 = -999; //this gets filtered in the NumCranes function
		obos.crane600DR = (double)as_number("crane600DR");//600 tonne capacity crawler crane day rate ($/day)
		obos.crane1000DR = (double)as_number("crane1000DR");//1000 tonne capacity crawler crane day rate ($/day)
		obos.craneMobDemob = (double)as_number("craneMobDemob");//crane mobilization and demobilization cost ($)
		obos.entranceExitRate = (double)as_number("entranceExitRate");//port entrance and exit cost ($/m^2/occurrence)
		obos.dockRate = (double)as_number("dockRate");//port docking cost ($/day)
		obos.wharfRate = (double)as_number("wharfRate");//port wharf loading and unloading cost ($/tonne)
		obos.laydownCR = (double)as_number("laydownCR");//port laydown and storage cost ($/m/day)

		//Engineering & Management
		obos.estEnMFac = (double)as_number("estEnMFac");//estimated engineering and management cost factor

		//Development
		obos.preFEEDStudy = (double)as_number("preFEEDStudy");//pre-fornt end engineering design (FEED) study cost ($)
		obos.feedStudy = (double)as_number("feedStudy");// FEED study cost ($)
		obos.stateLease = (double)as_number("stateLease");//state leasing cost ($)
		obos.outConShelfLease = (double)as_number("outConShelfLease");//outer continental shelf lease cost ($)
		obos.saPlan = (double)as_number("saPlan");//site assessment plan cost ($)
		obos.conOpPlan = (double)as_number("conOpPlan");//construction operations plan cost ($)
		obos.nepaEisMet = (double)as_number("nepaEisMet");//national environmental protection agency (NEPA) environmental impact (EIS) meteorological (met) tower study cost ($)
		obos.physResStudyMet = (double)as_number("physResStudyMet");//physical resource met tower study cost ($)
		obos.bioResStudyMet = (double)as_number("bioResStudyMet");//biological resource met tower study ($)
		obos.socEconStudyMet = (double)as_number("socEconStudyMet");//socioeconomic met tower study cost ($)
		obos.navStudyMet = (double)as_number("navStudyMet");//navigation met tower study ($)
		obos.nepaEisProj = (double)as_number("nepaEisProj");// NEPA EIS project site study cost ($)
		obos.physResStudyProj = (double)as_number("physResStudyProj");//physical resource project site study cost ($)
		obos.bioResStudyProj = (double)as_number("bioResStudyProj");//biological resource project site study cost ($)
		obos.socEconStudyProj = (double)as_number("socEconStudyProj");//socioeconomic project site study cost ($)
		obos.navStudyProj = (double)as_number("navStudyProj");//navigation project site study cost ($)
		obos.coastZoneManAct = (double)as_number("coastZoneManAct");//coastal zone management act compliance cost ($)
		obos.rivsnHarbsAct = (double)as_number("rivsnHarbsAct");//rivers & harbors act section 10 compliance cost ($)
		obos.cleanWatAct402 = (double)as_number("cleanWatAct402");//clean water act section 402 compliance cost ($)
		obos.cleanWatAct404 = (double)as_number("cleanWatAct404");//clean water act section 404 compliance cost ($)
		obos.faaPlan = (double)as_number("faaPlan");//federal aviation administration (FAA) plans and mitigation cost ($)
		obos.endSpecAct = (double)as_number("endSpecAct");//endangered species act compliance cost ($)
		obos.marMamProtAct = (double)as_number("marMamProtAct");//marine mammal protection act compliance cost ($)
		obos.migBirdAct = (double)as_number("migBirdAct");//migratory bird act compliance ($)
		obos.natHisPresAct = (double)as_number("natHisPresAct");//national historic preservation act compliance cost ($)
		obos.addLocPerm = (double)as_number("addLocPerm");//additional local and state permissions and compliance cost ($)
		obos.metTowCR = (double)as_number("metTowCR");//meteorological tower fabrication, design, and install cost rate ($/MW)
		obos.decomDiscRate = (double)as_number("decomDiscRate");//decommissioning expense discount rate

		//CABLE VECTORS****************************************************************************************************************************************
		//DEFAULT VECTOR SIZES
		double nArrVolts = 2;
		double nExpVolts = 2;
		double nArrCables = 11;
		double nExpCables = 10;
		double nCableAttributes = 6;
		double nVesselAttributes = 27;
		double nSupportVessels = 27;
		double nTugs = 4;

		obos.arrayVolt.resize(nArrVolts);
		obos.arrCables.resize(nArrVolts);

		for (int i = 0; i < obos.arrayVolt.size(); i++)
		{
			obos.arrayVolt[i].resize(nArrVolts);
		}
		for (int i = 0; i < obos.arrCables.size(); i++)
		{
			obos.arrCables[i].resize(nArrCables);

			for (int j = 0; j < obos.arrCables[i].size(); j++)
			{
				obos.arrCables[i][j].resize(nCableAttributes);
			}
		}

		obos.expCabVolt.resize(nExpVolts);
		obos.expCables.resize(nExpVolts);

		for (int i = 0; i < obos.expCabVolt.size(); i++)
		{
			obos.expCabVolt[i].resize(nExpVolts);
		}
		for (int i = 0; i < obos.expCables.size(); i++)
		{
			obos.expCables[i].resize(nExpCables);

			for (int j = 0; j < obos.expCables[i].size(); j++)
			{
				obos.expCables[i][j].resize(nCableAttributes);
			}
		}

		//vessel vectors
		obos.turbInstVessel.resize(nVesselAttributes);
		obos.turbFeederBarge.resize(nVesselAttributes);
		obos.turbSupportVessels.resize(nSupportVessels);
		obos.subInstVessel.resize(nVesselAttributes);
		obos.scourProtVessel.resize(nVesselAttributes);
		obos.subFeederBarge.resize(nVesselAttributes);
		obos.subSupportVessels.resize(nSupportVessels);
		obos.arrCabInstVessel.resize(nVesselAttributes);
		obos.expCabInstVessel.resize(nVesselAttributes);
		obos.substaInstVessel.resize(nVesselAttributes);
		obos.elecSupportVessels.resize(nSupportVessels);
		obos.elecTugs.resize(nTugs);

		for(int i = 0; i < nTugs; i++)
		{
		    obos.elecTugs[i].resize(nVesselAttributes);
		}
		for (int i = 0; i < nSupportVessels; i++)
		{
			obos.turbSupportVessels[i].resize(nVesselAttributes);
			obos.subSupportVessels[i].resize(nVesselAttributes);
			obos.elecSupportVessels[i].resize(nVesselAttributes);
		}

		/*
		array and export voltage
		array voltage vector rows by index value
		0: 33
		1: 66
		export voltage vector rows by index value
		0: 132
		1: 220

		voltage vector columns by index value
		0: voltage value in kV
		1: voltage identifier value: corresponds to the row index for each voltage
		*/
		//defaults
		obos.arrayVolt[0][0] = 33;
		obos.arrayVolt[1][0] = 66;
		obos.arrayVolt[0][1] = 0;
		obos.arrayVolt[1][1] = 1;

		obos.expCabVolt[0][0] = 132;
		obos.expCabVolt[1][0] = 220;
		obos.expCabVolt[0][1] = 0;
		obos.expCabVolt[1][1] = 1;

		/*
		cable data
		dimension 1's index refers to a voltage value identifier in vector obos.arrayVolt
		for array cables:
		0: 0 - corresponds to 33 kV
		1: 1 - corresponds to 66 kV

		for export cables:
		0: 0 - corresponds to 132 kV
		1: 1 - corresponds to 220 kV

		dimension 2 is the rows required for each cable size
		array cable rows by index value:
		0: 95 mm2
		1: 120 mm2
		2: 150 mm2
		3: 185 mm2
		4: 240 mm2
		5: 300 mm2
		6: 400 mm2
		7: 500 mm2
		8: 630 mm2
		9: 800 mm2
		10: 1000 mm2

		export cable rows by index value:
		0: 300 mm2
		1: 400 mm2
		2: 500 mm2
		3: 630 mm2
		4: 800 mm2
		5: 1000 mm2
		6: 1200 mm2
		7: 1600 mm2
		8: 2000 mm2
		9: 2500 mm2

		dimension 3 is columns for storing cable attributes
		cable columns by index value:
		0: index from wrapper
		1: cost ($/m)
		2: mass (kg/m)
		3: current rating (amps)
		4: turbine interface cost ($/interface)
		5: substation interface cost ($/interface)
		*/
		//defaults
		//33 kV
		//identifiers
		obos.arrCables[0][0][0] = 0;
		obos.arrCables[0][1][0] = 1;
		obos.arrCables[0][2][0] = 2;
		obos.arrCables[0][3][0] = 3;
		obos.arrCables[0][4][0] = 4;
		obos.arrCables[0][5][0] = 5;
		obos.arrCables[0][6][0] = 6;
		obos.arrCables[0][7][0] = 7;
		obos.arrCables[0][8][0] = 8;
		obos.arrCables[0][9][0] = 9;
		obos.arrCables[0][10][0] = 10;
		//cost per meter
		obos.arrCables[0][0][1] = 185.889;
		obos.arrCables[0][1][1] = 202.788;
		obos.arrCables[0][2][1] = 208.421;
		obos.arrCables[0][3][1] = 236.586;
		obos.arrCables[0][4][1] = 270.384;
		obos.arrCables[0][5][1] = 315.448;
		obos.arrCables[0][6][1] = 360.512;
		obos.arrCables[0][7][1] = 422.475;
		obos.arrCables[0][8][1] = 478.805;
		obos.arrCables[0][9][1] = 585.832;
		obos.arrCables[0][10][1] = 698.492;
		//mass per meter
		obos.arrCables[0][0][2] = 20.384;
		obos.arrCables[0][1][2] = 21.854;
		obos.arrCables[0][2][2] = 23.912;
		obos.arrCables[0][3][2] = 25.676;
		obos.arrCables[0][4][2] = 28.910;
		obos.arrCables[0][5][2] = 32.242;
		obos.arrCables[0][6][2] = 37.142;
		obos.arrCables[0][7][2] = 42.336;
		obos.arrCables[0][8][2] = 48.706;
		obos.arrCables[0][9][2] = 57.428;
		obos.arrCables[0][10][2] = 66.738;
		//current rating
		obos.arrCables[0][0][3] = 300;
		obos.arrCables[0][1][3] = 340;
		obos.arrCables[0][2][3] = 375;
		obos.arrCables[0][3][3] = 420;
		obos.arrCables[0][4][3] = 480;
		obos.arrCables[0][5][3] = 530;
		obos.arrCables[0][6][3] = 590;
		obos.arrCables[0][7][3] = 655;
		obos.arrCables[0][8][3] = 715;
		obos.arrCables[0][9][3] = 775;
		obos.arrCables[0][10][3] = 825;
		//cost per turbine interface
		obos.arrCables[0][0][4] = 8410;
		obos.arrCables[0][1][4] = 8615;
		obos.arrCables[0][2][4] = 8861;
		obos.arrCables[0][3][4] = 9149;
		obos.arrCables[0][4][4] = 9600;
		obos.arrCables[0][5][4] = 10092;
		obos.arrCables[0][6][4] = 10913;
		obos.arrCables[0][7][4] = 11733;
		obos.arrCables[0][8][4] = 12800;
		obos.arrCables[0][9][4] = 14195;
		obos.arrCables[0][10][4] = 15836;
		//cost per substation interface
		obos.arrCables[0][0][5] = 19610;
		obos.arrCables[0][1][5] = 19815;
		obos.arrCables[0][2][5] = 20062;
		obos.arrCables[0][3][5] = 20349;
		obos.arrCables[0][4][5] = 20800;
		obos.arrCables[0][5][5] = 21292;
		obos.arrCables[0][6][5] = 22113;
		obos.arrCables[0][7][5] = 22933;
		obos.arrCables[0][8][5] = 24000;
		obos.arrCables[0][9][5] = 25395;
		obos.arrCables[0][10][5] = 27036;
		//66 kV
		//identifiers
		obos.arrCables[1][0][0] = 0;
		obos.arrCables[1][1][0] = 1;
		obos.arrCables[1][2][0] = 2;
		obos.arrCables[1][3][0] = 3;
		obos.arrCables[1][4][0] = 4;
		obos.arrCables[1][5][0] = 5;
		obos.arrCables[1][6][0] = 6;
		obos.arrCables[1][7][0] = 7;
		obos.arrCables[1][8][0] = 8;
		obos.arrCables[1][9][0] = 9;
		obos.arrCables[1][10][0] = 10;
		//cost per meter
		obos.arrCables[1][0][1] = 225.320;
		obos.arrCables[1][1][1] = 242.219;
		obos.arrCables[1][2][1] = 253.485;
		obos.arrCables[1][3][1] = 281.650;
		obos.arrCables[1][4][1] = 326.714;
		obos.arrCables[1][5][1] = 383.044;
		obos.arrCables[1][6][1] = 433.741;
		obos.arrCables[1][7][1] = 506.970;
		obos.arrCables[1][8][1] = 574.566;
		obos.arrCables[1][9][1] = 704.125;
		obos.arrCables[1][10][1] = 844.950;
		//mass per meter
		obos.arrCables[1][0][2] = 21.6;
		obos.arrCables[1][1][2] = 23.8;
		obos.arrCables[1][2][2] = 25.7;
		obos.arrCables[1][3][2] = 28.0;
		obos.arrCables[1][4][2] = 31.3;
		obos.arrCables[1][5][2] = 34.3;
		obos.arrCables[1][6][2] = 39.2;
		obos.arrCables[1][7][2] = 45.4;
		obos.arrCables[1][8][2] = 52.0;
		obos.arrCables[1][9][2] = 60.1;
		obos.arrCables[1][10][2] = 70.7;
		//current rating
		obos.arrCables[1][0][3] = 300;
		obos.arrCables[1][1][3] = 340;
		obos.arrCables[1][2][3] = 375;
		obos.arrCables[1][3][3] = 420;
		obos.arrCables[1][4][3] = 480;
		obos.arrCables[1][5][3] = 530;
		obos.arrCables[1][6][3] = 590;
		obos.arrCables[1][7][3] = 655;
		obos.arrCables[1][8][3] = 715;
		obos.arrCables[1][9][3] = 775;
		obos.arrCables[1][10][3] = 825;
		//cost per turbine interface
		obos.arrCables[1][0][4] = 8830.50;
		obos.arrCables[1][1][4] = 9045.75;
		obos.arrCables[1][2][4] = 9304.05;
		obos.arrCables[1][3][4] = 9606.45;
		obos.arrCables[1][4][4] = 10080.00;
		obos.arrCables[1][5][4] = 10596.60;
		obos.arrCables[1][6][4] = 11458.65;
		obos.arrCables[1][7][4] = 12319.65;
		obos.arrCables[1][8][4] = 13440.00;
		obos.arrCables[1][9][4] = 14904.75;
		obos.arrCables[1][10][4] = 16627.80;
		//cost per substation interface
		obos.arrCables[1][0][5] = 20590.50;
		obos.arrCables[1][1][5] = 20805.75;
		obos.arrCables[1][2][5] = 21065.10;
		obos.arrCables[1][3][5] = 21366.45;
		obos.arrCables[1][4][5] = 21840.00;
		obos.arrCables[1][5][5] = 22356.60;
		obos.arrCables[1][6][5] = 23218.65;
		obos.arrCables[1][7][5] = 24079.65;
		obos.arrCables[1][8][5] = 25200.00;
		obos.arrCables[1][9][5] = 26664.75;
		obos.arrCables[1][10][5] = 28387.80;

		//export cables
		//132 kV
		//identifiers
		obos.expCables[0][0][0] = 0;
		obos.expCables[0][1][0] = 1;
		obos.expCables[0][2][0] = 2;
		obos.expCables[0][3][0] = 3;
		obos.expCables[0][4][0] = 4;
		obos.expCables[0][5][0] = 5;
		obos.expCables[0][6][0] = 6;
		obos.expCables[0][7][0] = 7;
		obos.expCables[0][8][0] = 8;
		obos.expCables[0][9][0] = 9;
		//cost per meter
		obos.expCables[0][0][1] = 433.504;
		obos.expCables[0][1][1] = 520.489;
		obos.expCables[0][2][1] = 596.388;
		obos.expCables[0][3][1] = 689.479;
		obos.expCables[0][4][1] = 843.823;
		obos.expCables[0][5][1] = 1006.054;
		obos.expCables[0][6][1] = 1168.284;
		obos.expCables[0][7][1] = 1492.745;
		obos.expCables[0][8][1] = 1818.332;
		obos.expCables[0][9][1] = 2223.908;
		//mass per meter
		obos.expCables[0][0][2] = 48.000;
		obos.expCables[0][1][2] = 51.100;
		obos.expCables[0][2][2] = 58.000;
		obos.expCables[0][3][2] = 65.200;
		obos.expCables[0][4][2] = 74.000;
		obos.expCables[0][5][2] = 85.400;
		obos.expCables[0][6][2] = 113.147;
		obos.expCables[0][7][2] = 131.387;
		obos.expCables[0][8][2] = 149.627;
		obos.expCables[0][9][2] = 172.427;
		//current rating
		obos.expCables[0][0][3] = 530;
		obos.expCables[0][1][3] = 590;
		obos.expCables[0][2][3] = 655;
		obos.expCables[0][3][3] = 715;
		obos.expCables[0][4][3] = 775;
		obos.expCables[0][5][3] = 825;
		obos.expCables[0][6][3] = 990;
		obos.expCables[0][7][3] = 1061;
		obos.expCables[0][8][3] = 1299;
		obos.expCables[0][9][3] = 1375;
		//cost per substation interface
		obos.expCables[0][0][5] = 57500;
		obos.expCables[0][1][5] = 60000;
		obos.expCables[0][2][5] = 62500;
		obos.expCables[0][3][5] = 65000;
		obos.expCables[0][4][5] = 67500;
		obos.expCables[0][5][5] = 70000;
		obos.expCables[0][6][5] = 72500;
		obos.expCables[0][7][5] = 75000;
		obos.expCables[0][8][5] = 77500;
		obos.expCables[0][9][5] = 80000;
		//220 kV
		//identifiers
		obos.expCables[1][0][0] = 0;
		obos.expCables[1][1][0] = 1;
		obos.expCables[1][2][0] = 2;
		obos.expCables[1][3][0] = 3;
		obos.expCables[1][4][0] = 4;
		obos.expCables[1][5][0] = 5;
		obos.expCables[1][6][0] = 6;
		obos.expCables[1][7][0] = 7;
		obos.expCables[1][8][0] = 8;
		obos.expCables[1][9][0] = 9;
		//cost per meter
		obos.expCables[1][0][1] = 495.411;
		obos.expCables[1][1][1] = 578.187;
		obos.expCables[1][2][1] = 681.863;
		obos.expCables[1][3][1] = 788.620;
		obos.expCables[1][4][1] = 966.623;
		obos.expCables[1][5][1] = 1159.271;
		obos.expCables[1][6][1] = 1336.148;
		obos.expCables[1][7][1] = 1676.499;
		obos.expCables[1][8][1] = 2042.784;
		obos.expCables[1][9][1] = 2498.703;
		//mass per meter
		obos.expCables[1][0][2] = 71.900;
		obos.expCables[1][1][2] = 76.500;
		obos.expCables[1][2][2] = 81.300;
		obos.expCables[1][3][2] = 86.700;
		obos.expCables[1][4][2] = 95.300;
		obos.expCables[1][5][2] = 104.000;
		obos.expCables[1][6][2] = 113.147;
		obos.expCables[1][7][2] = 131.387;
		obos.expCables[1][8][2] = 149.627;
		obos.expCables[1][9][2] = 172.427;
		//current rating
		obos.expCables[1][0][3] = 530;
		obos.expCables[1][1][3] = 590;
		obos.expCables[1][2][3] = 655;
		obos.expCables[1][3][3] = 715;
		obos.expCables[1][4][3] = 775;
		obos.expCables[1][5][3] = 825;
		obos.expCables[1][6][3] = 960;
		obos.expCables[1][7][3] = 1025;
		obos.expCables[1][8][3] = 1181;
		obos.expCables[1][9][3] = 1248;
		//cost per substation interface
		obos.expCables[1][0][5] = 57500;
		obos.expCables[1][1][5] = 60000;
		obos.expCables[1][2][5] = 62500;
		obos.expCables[1][3][5] = 65000;
		obos.expCables[1][4][5] = 67500;
		obos.expCables[1][5][5] = 70000;
		obos.expCables[1][6][5] = 72500;
		obos.expCables[1][7][5] = 75000;
		obos.expCables[1][8][5] = 77500;
		obos.expCables[1][9][5] = 80000;

		//Vessel vector***************************************************************************************************************************************
		//vessel defaults
		/*
		turbine and substructure install vessels by identifier value
		1: leg stabilized crane vessel
		2: mid height, mid sized jack-up vessel
		3: mid height, large sized jack-up vessel
		4: high height, mid sized jack-up vessel
		5: high height, large sized jack-up vessel
		6: shear leg crane vessel
		7: derrick crane vessel
		8: semi-submersible crane vessel
		9: heavy lift cargo vessel
		10: small AHST
		11: medium AHST
		12: large AHST

		array cable install vessels by identifier value
		13: medium array cable lay barge
		14: large array cable lay barge
		15: medium array cable lay vessel
		16: large array cable lay vessel

		export cable install vessels by identifier value
		17: medium export cable lay barge
		18: large export cable lay barge
		19: medium export cable lay vessel
		20: large export cable lay vessel

		offshore substation install vessels by identifier value
		8: semi-submersible crane vessel
		9: heavy lift cargo vessel
		10: small AHST
		11: medium AHST
		12: large AHST

		feeder barges by identifier value
		21: medium jack-up barge
		22: large jack-up barge
		23: medium jack-up barge with crane
		24: large jack-up barge with crane
		25: small barge
		26: medium barge
		27: large barge

		support vessels by identifier value
		10: small AHST
		11: medium AHST
		12: large AHST
		21: medium jack-up barge
		22: large jack-up barge
		23: medium jack-up barge with crane
		24: large jack-up barge with crane
		25: small barge
		26: medium barge
		27: large barge
		28: sea going support tug
		29: hotel vessel
		30: mother ship
		31: personnel transport vessel
		32: dive support vessel
		33: guard vessel
		34: semi-submersible cargo barge
		35: semi-submersible cargo barge
		36: backhoe dredger
		37: grab or clamshell dredger
		38: fall pipe or trailing suction dredger
		39: side rock dumper vessel
		40: ballasting vessel
		41: ballast hopper vessel
		42: environmental survey vessel
		43: geophysical survey vessel
		44: geotechnical survey vessel

		vessel vector columns by index value:
		0: identifier value to/from wrapper
		1: length
		2: breadth
		3: max draft
		4: max operational water depth
		5: leg length
		6: jack up/down speed
		7: deck space
		8: max payload
		9: max lift capacity
		10: max lift height
		11: max transit speed
		12: max operational wind speed
		13: max operational wave height
		14: day rate
		15: mob/demob time
		16: number of vessels required
		17: accommodation capacity
		18: crew capacity
		19: passenger capacity
		20: bollard pull
		21: tow speed
		22: carousel weight
		23: max spud depth
		24: max dredge depth
		25: bucket size
		26: grabber size
		27: hopper size
		*/
		//install vessel defaults
		
		if (obos.substructure == MONOPILE || obos.substructure == JACKET)
		{
			//turbine install vessel if monopile or jacket substructure
			obos.turbInstVessel[0] = 5;
			obos.turbInstVessel[1] = 150.3;
			obos.turbInstVessel[2] = 47;
			obos.turbInstVessel[3] = 6.2;
			obos.turbInstVessel[4] = 57.8;
			obos.turbInstVessel[5] = 99;
			obos.turbInstVessel[6] = 1.2;
			obos.turbInstVessel[7] = 5030;
			obos.turbInstVessel[8] = 8000;
			obos.turbInstVessel[9] = 1375;
			obos.turbInstVessel[10] = 105;
			obos.turbInstVessel[11] = 12.0;
			obos.turbInstVessel[12] = 18.0;
			obos.turbInstVessel[13] = 2.7;
			obos.turbInstVessel[14] = 242000;
			obos.turbInstVessel[15] = 3;
			obos.turbInstVessel[16] = 1;
			obos.turbInstVessel[17] = 0;
			obos.turbInstVessel[18] = 0;
			obos.turbInstVessel[19] = 0;
			obos.turbInstVessel[20] = 0;
			obos.turbInstVessel[21] = 0;
			obos.turbInstVessel[22] = 0;
			obos.turbInstVessel[23] = 0;
			obos.turbInstVessel[24] = 0;
			obos.turbInstVessel[25] = 0;
			obos.turbInstVessel[26] = 0;
			//substructure install vessel if monopile or jacket substructure
			obos.subInstVessel[0] = 5;
			obos.subInstVessel[1] = 150.3;
			obos.subInstVessel[2] = 47;
			obos.subInstVessel[3] = 6.2;
			obos.subInstVessel[4] = 57.8;
			obos.subInstVessel[5] = 99;
			obos.subInstVessel[6] = 1.2;
			obos.subInstVessel[7] = 5030;
			obos.subInstVessel[8] = 8000;
			obos.subInstVessel[9] = 1375;
			obos.subInstVessel[10] = 105;
			obos.subInstVessel[11] = 12.0;
			obos.subInstVessel[12] = 18.0;
			obos.subInstVessel[13] = 2.7;
			obos.subInstVessel[14] = 242000;
			obos.subInstVessel[15] = 3;
			obos.subInstVessel[16] = 1;
			obos.subInstVessel[17] = 0;
			obos.subInstVessel[18] = 0;
			obos.subInstVessel[19] = 0;
			obos.subInstVessel[20] = 0;
			obos.subInstVessel[21] = 0;
			obos.subInstVessel[22] = 0;
			obos.subInstVessel[23] = 0;
			obos.subInstVessel[24] = 0;
			obos.subInstVessel[25] = 0;
			obos.subInstVessel[26] = 0;
			//offshore substation installation vessel if monopile or jacket substructure
			obos.substaInstVessel[0] = 8;
			obos.substaInstVessel[1] = 166;
			obos.substaInstVessel[2] = 72.6;
			obos.substaInstVessel[3] = 24.3;
			obos.substaInstVessel[4] = 0;
			obos.substaInstVessel[5] = 0;
			obos.substaInstVessel[6] = 0;
			obos.substaInstVessel[7] = 0;
			obos.substaInstVessel[8] = 0;
			obos.substaInstVessel[9] = 7826;
			obos.substaInstVessel[10] = 0;
			obos.substaInstVessel[11] = 7.3;
			obos.substaInstVessel[12] = 20;
			obos.substaInstVessel[13] = 1.5;
			obos.substaInstVessel[14] = 525000;
			obos.substaInstVessel[15] = 3;
			obos.substaInstVessel[16] = 1;
			obos.substaInstVessel[17] = 0;
			obos.substaInstVessel[18] = 0;
			obos.substaInstVessel[19] = 0;
			obos.substaInstVessel[20] = 0;
			obos.substaInstVessel[21] = 0;
			obos.substaInstVessel[22] = 0;
			obos.substaInstVessel[23] = 0;
			obos.substaInstVessel[24] = 0;
			obos.substaInstVessel[25] = 0;
			obos.substaInstVessel[26] = 0;
			//turbine install support vessels if monopile or jacket substructure
			obos.turbSupportVessels[0][0] = 31;
			obos.turbSupportVessels[1][0] = 33;
			obos.turbSupportVessels[0][1] = 19;
			obos.turbSupportVessels[1][1] = 20.6;
			obos.turbSupportVessels[0][2] = 0;
			obos.turbSupportVessels[1][2] = 5.4;
			obos.turbSupportVessels[0][3] = 1.5;
			obos.turbSupportVessels[1][3] = 3.1;
			obos.turbSupportVessels[0][11] = 23;
			obos.turbSupportVessels[1][11] = 15;
			obos.turbSupportVessels[0][12] = 15;
			obos.turbSupportVessels[0][13] = 1.75;
			obos.turbSupportVessels[1][13] = 1.75;
			obos.turbSupportVessels[0][14] = 3000;
			obos.turbSupportVessels[1][14] = 3000;
			obos.turbSupportVessels[0][15] = 1;
			obos.turbSupportVessels[1][15] = 1;
			obos.turbSupportVessels[0][16] = 1;
			obos.turbSupportVessels[1][16] = 1;
			obos.turbSupportVessels[0][18] = 3;
			obos.turbSupportVessels[0][19] = 13;
			//substructure install support vessels if monopile or jacket substructure
			obos.subSupportVessels[0][0] = 31;
			obos.subSupportVessels[1][0] = 33;
			obos.subSupportVessels[0][1] = 19;
			obos.subSupportVessels[1][1] = 20.6;
			obos.subSupportVessels[0][2] = 0;
			obos.subSupportVessels[1][2] = 5.4;
			obos.subSupportVessels[0][3] = 1.5;
			obos.subSupportVessels[1][3] = 3.1;
			obos.subSupportVessels[0][11] = 23;
			obos.subSupportVessels[1][11] = 15;
			obos.subSupportVessels[0][12] = 15;
			obos.subSupportVessels[0][13] = 1.75;
			obos.subSupportVessels[1][13] = 1.75;
			obos.subSupportVessels[0][14] = 3000;
			obos.subSupportVessels[1][14] = 3000;
			obos.subSupportVessels[0][15] = 1;
			obos.subSupportVessels[1][15] = 1;
			obos.subSupportVessels[0][16] = 1;
			obos.subSupportVessels[1][16] = 1;
			obos.subSupportVessels[0][18] = 3;
			obos.subSupportVessels[0][19] = 13;
			//electrical infrastructure install support vessels
            obos.elecSupportVessels[1][0] = 33;
            obos.elecSupportVessels[3][0] = 31;
            obos.elecSupportVessels[1][1] = 20.6;
            obos.elecSupportVessels[1][2] = 5.4;
            obos.elecSupportVessels[1][3] = 3.1;
            obos.elecSupportVessels[1][11] = 15;
            obos.elecSupportVessels[1][13] = 1.75;
            obos.elecSupportVessels[1][14] = 3000;
            obos.elecSupportVessels[1][15] = 1;
            obos.elecSupportVessels[1][16] = 1;
            obos.elecSupportVessels[3][1] = 19;
            obos.elecSupportVessels[3][2] = 0;
            obos.elecSupportVessels[3][3] = 1.5;
            obos.elecSupportVessels[3][11] = 23;
            obos.elecSupportVessels[3][12] = 15;
            obos.elecSupportVessels[3][13] = 1.75;
            obos.elecSupportVessels[3][14] = 3000;
            obos.elecSupportVessels[3][15] = 1;
            obos.elecSupportVessels[3][16] = 1;
            obos.elecSupportVessels[3][18] = 3;
            obos.elecSupportVessels[3][19] = 13;
			if(obos.substructure == MONOPILE)
			{
				//scour protection vessel
				obos.scourProtVessel[0] = 39;
				obos.scourProtVessel[1] = 71;
				obos.scourProtVessel[2] = 17.7;
				obos.scourProtVessel[3] = 3.3;
				obos.scourProtVessel[8] = 1490;
				obos.scourProtVessel[11] = 7.8;
				obos.scourProtVessel[13] = 2.5;
				obos.scourProtVessel[14] = 84000;
				obos.scourProtVessel[15] = 3;
				obos.scourProtVessel[16] = 1;
			}
            obos.elecTugs[0][0] = 12;
            obos.elecTugs[0][1] = 87;
            obos.elecTugs[0][2] = 20.9;
            obos.elecTugs[0][3] = 7.4;
            obos.elecTugs[0][4] = 0;
            obos.elecTugs[0][5] = 0;
            obos.elecTugs[0][6] = 0;
            obos.elecTugs[0][7] = 637;
            obos.elecTugs[0][8] = 1440;
            obos.elecTugs[0][9] = 0;
            obos.elecTugs[0][10] = 0;
            obos.elecTugs[0][11] = 15.2;
            obos.elecTugs[0][12] = 0;
            obos.elecTugs[0][13] = 2.0;
            obos.elecTugs[0][14] = 90000;
            obos.elecTugs[0][15] = 3;
            obos.elecTugs[0][16] = 1;
            obos.elecTugs[0][17] = 0;
            obos.elecTugs[0][18] = 0;
            obos.elecTugs[0][19] = 0;
            obos.elecTugs[0][20] = 237;
            obos.elecTugs[0][21] = 5;
            obos.elecTugs[0][22] = 0;
            obos.elecTugs[0][23] = 0;
            obos.elecTugs[0][24] = 0;
            obos.elecTugs[0][25] = 0;
            obos.elecTugs[0][26] = 0;

		}
		else if (obos.substructure == SPAR)
		{
			//turbine install vessel if spar substructure
			obos.turbInstVessel[0] = 12;
			obos.turbInstVessel[1] = 87;
			obos.turbInstVessel[2] = 20.9;
			obos.turbInstVessel[3] = 7.4;
			obos.turbInstVessel[4] = 0;
			obos.turbInstVessel[5] = 0;
			obos.turbInstVessel[6] = 0;
			obos.turbInstVessel[7] = 637;
			obos.turbInstVessel[8] = 1440;
			obos.turbInstVessel[9] = 0;
			obos.turbInstVessel[10] = 0;
			obos.turbInstVessel[11] = 15.2;
			obos.turbInstVessel[12] = 0;
			obos.turbInstVessel[13] = 2.0;
			obos.turbInstVessel[14] = 90000;
			obos.turbInstVessel[15] = 3;
			obos.turbInstVessel[16] = 1;
			obos.turbInstVessel[17] = 0;
			obos.turbInstVessel[18] = 0;
			obos.turbInstVessel[19] = 0;
			obos.turbInstVessel[20] = 237;
			obos.turbInstVessel[21] = 5;
			obos.turbInstVessel[22] = 0;
			obos.turbInstVessel[23] = 0;
			obos.turbInstVessel[24] = 0;
			obos.turbInstVessel[25] = 0;
			obos.turbInstVessel[26] = 0;
			//substructure install vessel if spar or semisubmersible substructure
			obos.subInstVessel[0] = 11;
			obos.subInstVessel[1] = 68.2;
			obos.subInstVessel[2] = 15.9;
			obos.subInstVessel[3] = 6.3;
			obos.subInstVessel[4] = 0;
			obos.subInstVessel[5] = 0;
			obos.subInstVessel[6] = 0;
			obos.subInstVessel[7] = 441;
			obos.subInstVessel[8] = 738;
			obos.subInstVessel[9] = 0;
			obos.subInstVessel[10] = 0;
			obos.subInstVessel[11] = 12.7;
			obos.subInstVessel[12] = 0;
			obos.subInstVessel[13] = 2.0;
			obos.subInstVessel[14] = 60000;
			obos.subInstVessel[15] = 3;
			obos.subInstVessel[16] = 1;
			obos.subInstVessel[17] = 0;
			obos.subInstVessel[18] = 0;
			obos.subInstVessel[19] = 0;
			obos.subInstVessel[20] = 152;
			obos.subInstVessel[21] = 5;
			obos.subInstVessel[22] = 0;
			obos.subInstVessel[23] = 0;
			obos.subInstVessel[24] = 0;
			obos.subInstVessel[25] = 0;
			obos.subInstVessel[26] = 0;
			//offshore substation installation vessel if spar or semisubmersible substructure
			obos.substaInstVessel[0] = 12;
			obos.substaInstVessel[1] = 87;
			obos.substaInstVessel[2] = 20.9;
			obos.substaInstVessel[3] = 7.4;
			obos.substaInstVessel[4] = 0;
			obos.substaInstVessel[5] = 0;
			obos.substaInstVessel[6] = 0;
			obos.substaInstVessel[7] = 637;
			obos.substaInstVessel[8] = 1440;
			obos.substaInstVessel[9] = 0;
			obos.substaInstVessel[10] = 0;
			obos.substaInstVessel[11] = 15.2;
			obos.substaInstVessel[12] = 0;
			obos.substaInstVessel[13] = 2.0;
			obos.substaInstVessel[14] = 90000;
			obos.substaInstVessel[15] = 3;
			obos.substaInstVessel[16] = 1;
			obos.substaInstVessel[17] = 0;
			obos.substaInstVessel[18] = 0;
			obos.substaInstVessel[19] = 0;
			obos.substaInstVessel[20] = 237;
			obos.substaInstVessel[21] = 5;
			obos.substaInstVessel[22] = 0;
			obos.substaInstVessel[23] = 0;
			obos.substaInstVessel[24] = 0;
			obos.substaInstVessel[25] = 0;
			obos.substaInstVessel[26] = 0;
			//turbine install support vessels if spar substructure
			obos.turbSupportVessels[0][0] = 11;
			obos.turbSupportVessels[1][0] = 21;
			obos.turbSupportVessels[3][0] = 28;
			obos.turbSupportVessels[4][0] = 31;
			obos.turbSupportVessels[5][0] = 33;
			obos.turbSupportVessels[6][0] = 40;
			obos.turbSupportVessels[7][0] = 41;
			obos.turbSupportVessels[0][1] = 68.2;
			obos.turbSupportVessels[0][2] = 15.9;
			obos.turbSupportVessels[0][3] = 6.3;
			obos.turbSupportVessels[0][7] = 441;
			obos.turbSupportVessels[0][8] = 738;
			obos.turbSupportVessels[0][11] = 12.7;
			obos.turbSupportVessels[0][13] = 2.5;
			obos.turbSupportVessels[0][14] = 60000;
			obos.turbSupportVessels[0][15] = 3;
			obos.turbSupportVessels[0][16] = 1;
			obos.turbSupportVessels[1][1] = 34.9;
			obos.turbSupportVessels[1][2] = 20.5;
			obos.turbSupportVessels[1][3] = 2.3;
			obos.turbSupportVessels[1][4] = 25;
			obos.turbSupportVessels[1][5] = 45;
			obos.turbSupportVessels[1][6] = 0.6667;
			obos.turbSupportVessels[1][7] = 600;
			obos.turbSupportVessels[1][8] = 395;
			obos.turbSupportVessels[1][11] = 4;
			obos.turbSupportVessels[1][12] = 10;
			obos.turbSupportVessels[1][13] = 1.5;
			obos.turbSupportVessels[1][14] = 50000;
			obos.turbSupportVessels[1][15] = 3;
			obos.turbSupportVessels[1][16] = 3;
			obos.turbSupportVessels[3][1] = 28.2;
			obos.turbSupportVessels[3][2] = 8.8;
			obos.turbSupportVessels[3][3] = 3.1;
			obos.turbSupportVessels[3][7] = 39.3;
			obos.turbSupportVessels[3][8] = 8;
			obos.turbSupportVessels[3][11] = 9.6;
			obos.turbSupportVessels[3][13] = 2.5;
			obos.turbSupportVessels[3][14] = 27500;
			obos.turbSupportVessels[3][15] = 1;
			obos.turbSupportVessels[3][16] = 2;
			obos.turbSupportVessels[3][20] = 37;
			obos.turbSupportVessels[4][1] = 19;
			obos.turbSupportVessels[4][2] = 0;
			obos.turbSupportVessels[4][3] = 1.5;
			obos.turbSupportVessels[4][11] = 23;
			obos.turbSupportVessels[4][12] = 15;
			obos.turbSupportVessels[4][13] = 1.75;
			obos.turbSupportVessels[4][14] = 3000;
			obos.turbSupportVessels[4][15] = 1;
			obos.turbSupportVessels[4][16] = 1;
			obos.turbSupportVessels[4][18] = 3;
			obos.turbSupportVessels[4][19] = 13;
			obos.turbSupportVessels[5][1] = 20.6;
			obos.turbSupportVessels[5][2] = 5.4;
			obos.turbSupportVessels[5][3] = 3.1;
			obos.turbSupportVessels[5][11] = 15;
			obos.turbSupportVessels[5][13] = 1.75;
			obos.turbSupportVessels[5][14] = 3000;
			obos.turbSupportVessels[5][15] = 1;
			obos.turbSupportVessels[5][16] = 1;
			obos.turbSupportVessels[6][14] = 11500;
			obos.turbSupportVessels[7][14] = 20500;
			obos.turbSupportVessels[6][15] = 3;
			obos.turbSupportVessels[7][15] = 3;
			obos.turbSupportVessels[6][16] = 1;
			obos.turbSupportVessels[7][16] = 1;
			//substructure install support vessels if spar substructure
			obos.subSupportVessels[0][0] = 21;
			obos.subSupportVessels[2][0] = 28;
			obos.subSupportVessels[3][0] = 31;
			obos.subSupportVessels[4][0] = 33;
			obos.subSupportVessels[5][0] = 40;
			obos.subSupportVessels[6][0] = 41;
			obos.subSupportVessels[0][1] = 34.9;
			obos.subSupportVessels[0][2] = 20.5;
			obos.subSupportVessels[0][3] = 2.3;
			obos.subSupportVessels[0][4] = 25;
			obos.subSupportVessels[0][5] = 45;
			obos.subSupportVessels[0][6] = 0.6667;
			obos.subSupportVessels[0][7] = 600;
			obos.subSupportVessels[0][8] = 395;
			obos.subSupportVessels[0][11] = 4;
			obos.subSupportVessels[0][12] = 10;
			obos.subSupportVessels[0][13] = 1.5;
			obos.subSupportVessels[0][14] = 50000;
			obos.subSupportVessels[0][15] = 3;
			obos.subSupportVessels[0][16] = 3;
			obos.subSupportVessels[2][1] = 28.2;
			obos.subSupportVessels[2][2] = 8.8;
			obos.subSupportVessels[2][3] = 3.1;
			obos.subSupportVessels[2][7] = 39.3;
			obos.subSupportVessels[2][8] = 8;
			obos.subSupportVessels[2][11] = 9.6;
			obos.subSupportVessels[2][13] = 2.5;
			obos.subSupportVessels[2][14] = 27500;
			obos.subSupportVessels[2][15] = 1;
			obos.subSupportVessels[2][16] = 2;
			obos.subSupportVessels[2][20] = 37;
			obos.subSupportVessels[3][1] = 19;
			obos.subSupportVessels[3][2] = 0;
			obos.subSupportVessels[3][3] = 1.5;
			obos.subSupportVessels[3][11] = 23;
			obos.subSupportVessels[3][12] = 15;
			obos.subSupportVessels[3][13] = 1.75;
			obos.subSupportVessels[3][14] = 3000;
			obos.subSupportVessels[3][15] = 1;
			obos.subSupportVessels[3][16] = 1;
			obos.subSupportVessels[3][18] = 3;
			obos.subSupportVessels[3][19] = 13;
			obos.subSupportVessels[4][1] = 20.6;
			obos.subSupportVessels[4][2] = 5.4;
			obos.subSupportVessels[4][3] = 3.1;
			obos.subSupportVessels[4][11] = 15;
			obos.subSupportVessels[4][13] = 1.75;
			obos.subSupportVessels[4][14] = 3000;
			obos.subSupportVessels[4][15] = 1;
			obos.subSupportVessels[4][16] = 1;
			obos.subSupportVessels[5][14] = 11500;
			obos.subSupportVessels[6][14] = 20500;
			obos.subSupportVessels[5][15] = 3;
			obos.subSupportVessels[6][15] = 3;
			obos.subSupportVessels[5][16] = 1;
			obos.subSupportVessels[6][16] = 1;
            obos.elecSupportVessels[1][0] = 33;
            obos.elecSupportVessels[3][0] = 31;
            obos.elecSupportVessels[1][1] = 20.6;
            obos.elecSupportVessels[1][2] = 5.4;
            obos.elecSupportVessels[1][3] = 3.1;
            obos.elecSupportVessels[1][11] = 15;
            obos.elecSupportVessels[1][13] = 1.75;
            obos.elecSupportVessels[1][14] = 3000;
            obos.elecSupportVessels[1][15] = 1;
            obos.elecSupportVessels[1][16] = 1;
            obos.elecSupportVessels[3][1] = 19;
            obos.elecSupportVessels[3][2] = 0;
            obos.elecSupportVessels[3][3] = 1.5;
            obos.elecSupportVessels[3][11] = 23;
            obos.elecSupportVessels[3][12] = 15;
            obos.elecSupportVessels[3][13] = 1.75;
            obos.elecSupportVessels[3][14] = 3000;
            obos.elecSupportVessels[3][15] = 1;
            obos.elecSupportVessels[3][16] = 1;
            obos.elecSupportVessels[3][18] = 3;
            obos.elecSupportVessels[3][19] = 13;
            obos.elecTugs[0][0] = 12;
            obos.elecTugs[1][0] = 28;
            obos.elecTugs[0][1] = 87;
            obos.elecTugs[0][2] = 20.9;
            obos.elecTugs[0][3] = 7.4;
            obos.elecTugs[0][4] = 0;
            obos.elecTugs[0][5] = 0;
            obos.elecTugs[0][6] = 0;
            obos.elecTugs[0][7] = 637;
            obos.elecTugs[0][8] = 1440;
            obos.elecTugs[0][9] = 0;
            obos.elecTugs[0][10] = 0;
            obos.elecTugs[0][11] = 15.2;
            obos.elecTugs[0][12] = 0;
            obos.elecTugs[0][13] = 2.0;
            obos.elecTugs[0][14] = 90000;
            obos.elecTugs[0][15] = 3;
            obos.elecTugs[0][16] = 1;
            obos.elecTugs[0][17] = 0;
            obos.elecTugs[0][18] = 0;
            obos.elecTugs[0][19] = 0;
            obos.elecTugs[0][20] = 237;
            obos.elecTugs[0][21] = 5;
            obos.elecTugs[0][22] = 0;
            obos.elecTugs[0][23] = 0;
            obos.elecTugs[0][24] = 0;
            obos.elecTugs[0][25] = 0;
            obos.elecTugs[0][26] = 0;
			obos.elecTugs[1][1] = 28.2;
			obos.elecTugs[1][2] = 8.8;
			obos.elecTugs[1][3] = 3.1;
			obos.elecTugs[1][7] = 39.3;
			obos.elecTugs[1][8] = 8;
			obos.elecTugs[1][11] = 9.6;
			obos.elecTugs[1][13] = 2.5;
			obos.elecTugs[1][14] = 27500;
			obos.elecTugs[1][15] = 1;
			obos.elecTugs[1][16] = 2;
			obos.elecTugs[1][20] = 37;
		}
		//turbine install vessel if semisubmersible substructure
		else if (obos.substructure == SEMISUBMERSIBLE)
		{
			obos.turbInstVessel[0] = 11;
			obos.turbInstVessel[1] = 68.2;
			obos.turbInstVessel[2] = 15.9;
			obos.turbInstVessel[3] = 6.3;
			obos.turbInstVessel[4] = 0;
			obos.turbInstVessel[5] = 0;
			obos.turbInstVessel[6] = 0;
			obos.turbInstVessel[7] = 441;
			obos.turbInstVessel[8] = 738;
			obos.turbInstVessel[9] = 0;
			obos.turbInstVessel[10] = 0;
			obos.turbInstVessel[11] = 12.7;
			obos.turbInstVessel[12] = 0;
			obos.turbInstVessel[13] = 2.0;
			obos.turbInstVessel[14] = 60000;
			obos.turbInstVessel[15] = 3;
			obos.turbInstVessel[16] = 1;
			obos.turbInstVessel[17] = 0;
			obos.turbInstVessel[18] = 0;
			obos.turbInstVessel[19] = 0;
			obos.turbInstVessel[20] = 152;
			obos.turbInstVessel[21] = 5;
			obos.turbInstVessel[22] = 0;
			obos.turbInstVessel[23] = 0;
			obos.turbInstVessel[24] = 0;
			obos.turbInstVessel[25] = 0;
			obos.turbInstVessel[26] = 0;
			//substructure install vessel if spar or semisubmersible substructure
			obos.subInstVessel[0] = 11;
			obos.subInstVessel[1] = 68.2;
			obos.subInstVessel[2] = 15.9;
			obos.subInstVessel[3] = 6.3;
			obos.subInstVessel[4] = 0;
			obos.subInstVessel[5] = 0;
			obos.subInstVessel[6] = 0;
			obos.subInstVessel[7] = 441;
			obos.subInstVessel[8] = 738;
			obos.subInstVessel[9] = 0;
			obos.subInstVessel[10] = 0;
			obos.subInstVessel[11] = 12.7;
			obos.subInstVessel[12] = 0;
			obos.subInstVessel[13] = 2.0;
			obos.subInstVessel[14] = 60000;
			obos.subInstVessel[15] = 3;
			obos.subInstVessel[16] = 1;
			obos.subInstVessel[17] = 0;
			obos.subInstVessel[18] = 0;
			obos.subInstVessel[19] = 0;
			obos.subInstVessel[20] = 152;
			obos.subInstVessel[21] = 5;
			obos.subInstVessel[22] = 0;
			obos.subInstVessel[23] = 0;
			obos.subInstVessel[24] = 0;
			obos.subInstVessel[25] = 0;
			obos.subInstVessel[26] = 0;
			//offshore substation installation vessel if spar or semisubmersible substructure
			obos.substaInstVessel[0] = 12;
			obos.substaInstVessel[1] = 87;
			obos.substaInstVessel[2] = 20.9;
			obos.substaInstVessel[3] = 7.4;
			obos.substaInstVessel[4] = 0;
			obos.substaInstVessel[5] = 0;
			obos.substaInstVessel[6] = 0;
			obos.substaInstVessel[7] = 637;
			obos.substaInstVessel[8] = 1440;
			obos.substaInstVessel[9] = 0;
			obos.substaInstVessel[10] = 0;
			obos.substaInstVessel[11] = 15.2;
			obos.substaInstVessel[12] = 0;
			obos.substaInstVessel[13] = 2.0;
			obos.substaInstVessel[14] = 90000;
			obos.substaInstVessel[15] = 3;
			obos.substaInstVessel[16] = 1;
			obos.substaInstVessel[17] = 0;
			obos.substaInstVessel[18] = 0;
			obos.substaInstVessel[19] = 0;
			obos.substaInstVessel[20] = 237;
			obos.substaInstVessel[21] = 5;
			obos.substaInstVessel[22] = 0;
			obos.substaInstVessel[23] = 0;
			obos.substaInstVessel[24] = 0;
			obos.substaInstVessel[25] = 0;
			obos.substaInstVessel[26] = 0;
			//turbine install support vessels if semisubmersible substructure
			obos.turbSupportVessels[0][0] = 28;
			obos.turbSupportVessels[1][0] = 33;
			obos.turbSupportVessels[0][1] = 28.2;
			obos.turbSupportVessels[0][2] = 8.8;
			obos.turbSupportVessels[0][3] = 3.1;
			obos.turbSupportVessels[0][7] = 39.3;
			obos.turbSupportVessels[0][8] = 8;
			obos.turbSupportVessels[0][11] = 9.6;
			obos.turbSupportVessels[0][13] = 2.5;
			obos.turbSupportVessels[0][14] = 27500;
			obos.turbSupportVessels[0][15] = 1;
			obos.turbSupportVessels[0][16] = 2;
			obos.turbSupportVessels[0][20] = 37;
			obos.turbSupportVessels[1][1] = 20.6;
			obos.turbSupportVessels[1][2] = 5.4;
			obos.turbSupportVessels[1][3] = 3.1;
			obos.turbSupportVessels[1][11] = 15;
			obos.turbSupportVessels[1][13] = 1.75;
			obos.turbSupportVessels[1][14] = 3000;
			obos.turbSupportVessels[1][15] = 1;
			obos.turbSupportVessels[1][16] = 1;
			//substructure install support vessels is semisubmersible substructure
			obos.subSupportVessels[0][0] = 28;
			obos.subSupportVessels[1][0] = 33;
			obos.subSupportVessels[0][1] = 28.2;
			obos.subSupportVessels[0][2] = 8.8;
			obos.subSupportVessels[0][3] = 3.1;
			obos.subSupportVessels[0][7] = 39.3;
			obos.subSupportVessels[0][8] = 8;
			obos.subSupportVessels[0][11] = 9.6;
			obos.subSupportVessels[0][13] = 2.5;
			obos.subSupportVessels[0][14] = 27500;
			obos.subSupportVessels[0][15] = 1;
			obos.subSupportVessels[0][16] = 2;
			obos.subSupportVessels[0][20] = 37;
			obos.subSupportVessels[1][1] = 20.6;
			obos.subSupportVessels[1][2] = 5.4;
			obos.subSupportVessels[1][3] = 3.1;
			obos.subSupportVessels[1][11] = 15;
			obos.subSupportVessels[1][13] = 1.75;
			obos.subSupportVessels[1][14] = 3000;
			obos.subSupportVessels[1][15] = 1;
			obos.subSupportVessels[1][16] = 1;
            obos.elecSupportVessels[1][0] = 33;
            obos.elecSupportVessels[3][0] = 31;
            obos.elecSupportVessels[1][1] = 20.6;
            obos.elecSupportVessels[1][2] = 5.4;
            obos.elecSupportVessels[1][3] = 3.1;
            obos.elecSupportVessels[1][11] = 15;
            obos.elecSupportVessels[1][13] = 1.75;
            obos.elecSupportVessels[1][14] = 3000;
            obos.elecSupportVessels[1][15] = 1;
            obos.elecSupportVessels[1][16] = 1;
            obos.elecSupportVessels[3][1] = 19;
            obos.elecSupportVessels[3][2] = 0;
            obos.elecSupportVessels[3][3] = 1.5;
            obos.elecSupportVessels[3][11] = 23;
            obos.elecSupportVessels[3][12] = 15;
            obos.elecSupportVessels[3][13] = 1.75;
            obos.elecSupportVessels[3][14] = 3000;
            obos.elecSupportVessels[3][15] = 1;
            obos.elecSupportVessels[3][16] = 1;
            obos.elecSupportVessels[3][18] = 3;
            obos.elecSupportVessels[3][19] = 13;
            obos.elecTugs[0][0] = 12;
            obos.elecTugs[1][0] = 28;
            obos.elecTugs[0][1] = 87;
            obos.elecTugs[0][2] = 20.9;
            obos.elecTugs[0][3] = 7.4;
            obos.elecTugs[0][4] = 0;
            obos.elecTugs[0][5] = 0;
            obos.elecTugs[0][6] = 0;
            obos.elecTugs[0][7] = 637;
            obos.elecTugs[0][8] = 1440;
            obos.elecTugs[0][9] = 0;
            obos.elecTugs[0][10] = 0;
            obos.elecTugs[0][11] = 15.2;
            obos.elecTugs[0][12] = 0;
            obos.elecTugs[0][13] = 2.0;
            obos.elecTugs[0][14] = 90000;
            obos.elecTugs[0][15] = 3;
            obos.elecTugs[0][16] = 1;
            obos.elecTugs[0][17] = 0;
            obos.elecTugs[0][18] = 0;
            obos.elecTugs[0][19] = 0;
            obos.elecTugs[0][20] = 237;
            obos.elecTugs[0][21] = 5;
            obos.elecTugs[0][22] = 0;
            obos.elecTugs[0][23] = 0;
            obos.elecTugs[0][24] = 0;
            obos.elecTugs[0][25] = 0;
            obos.elecTugs[0][26] = 0;
			obos.elecTugs[1][1] = 28.2;
			obos.elecTugs[1][2] = 8.8;
			obos.elecTugs[1][3] = 3.1;
			obos.elecTugs[1][7] = 39.3;
			obos.elecTugs[1][8] = 8;
			obos.elecTugs[1][11] = 9.6;
			obos.elecTugs[1][13] = 2.5;
			obos.elecTugs[1][14] = 27500;
			obos.elecTugs[1][15] = 1;
			obos.elecTugs[1][16] = 2;
			obos.elecTugs[1][20] = 37;
		}


		//turbine install feeder barge
		obos.turbFeederBarge[0] = 22;
		obos.turbFeederBarge[1] = 75.6;
		obos.turbFeederBarge[2] = 32.5;
		obos.turbFeederBarge[3] = 4.2;
		obos.turbFeederBarge[4] = 32.5;
		obos.turbFeederBarge[5] = 50.4;
		obos.turbFeederBarge[6] = 0.9;
		obos.turbFeederBarge[7] = 2000;
		obos.turbFeederBarge[8] = 1930;
		obos.turbFeederBarge[9] = 510;
		obos.turbFeederBarge[10] = 89.5;
		obos.turbFeederBarge[11] = 4;
		obos.turbFeederBarge[12] = 10;
		obos.turbFeederBarge[13] = 2;
		obos.turbFeederBarge[14] = 70000;
		obos.turbFeederBarge[15] = 3;
		obos.turbFeederBarge[16] = 1;
		//substructure feeder barge
		obos.subFeederBarge[0] = 22;
		obos.subFeederBarge[1] = 75.6;
		obos.subFeederBarge[2] = 32.5;
		obos.subFeederBarge[3] = 4.2;
		obos.subFeederBarge[4] = 32.5;
		obos.subFeederBarge[5] = 50.4;
		obos.subFeederBarge[6] = 0.9;
		obos.subFeederBarge[7] = 2000;
		obos.subFeederBarge[8] = 1930;
		obos.subFeederBarge[9] = 510;
		obos.subFeederBarge[10] = 89.5;
		obos.subFeederBarge[11] = 4;
		obos.subFeederBarge[12] = 10;
		obos.subFeederBarge[13] = 2;
		obos.subFeederBarge[14] = 70000;
		obos.subFeederBarge[15] = 3;
		obos.subFeederBarge[16] = 1;
		//array cable install vessel
		obos.arrCabInstVessel[0] = 16;
		obos.arrCabInstVessel[1] = 111.1;
		obos.arrCabInstVessel[2] = 23.6;
		obos.arrCabInstVessel[3] = 6.3;
		obos.arrCabInstVessel[4] = 0;
		obos.arrCabInstVessel[5] = 0;
		obos.arrCabInstVessel[6] = 0;
		obos.arrCabInstVessel[7] = 1520;
		obos.arrCabInstVessel[8] = 0;
		obos.arrCabInstVessel[9] = 0;
		obos.arrCabInstVessel[10] = 0;
		obos.arrCabInstVessel[11] = 10.2;
		obos.arrCabInstVessel[12] = 25;
		obos.arrCabInstVessel[13] = 1;
		obos.arrCabInstVessel[14] = 144000;
		obos.arrCabInstVessel[15] = 3;
		obos.arrCabInstVessel[16] = 1;
		obos.arrCabInstVessel[17] = 0;
		obos.arrCabInstVessel[18] = 0;
		obos.arrCabInstVessel[19] = 0;
		obos.arrCabInstVessel[20] = 72;
		obos.arrCabInstVessel[21] = 0;
		obos.arrCabInstVessel[22] = 5900;
		obos.arrCabInstVessel[23] = 0;
		obos.arrCabInstVessel[24] = 0;
		obos.arrCabInstVessel[25] = 0;
		obos.arrCabInstVessel[26] = 0;
		//export cable install vessel
		obos.expCabInstVessel[0] = 20;
		obos.expCabInstVessel[1] = 102.7;
		obos.expCabInstVessel[2] = 25.2;
		obos.expCabInstVessel[3] = 5.6;
		obos.expCabInstVessel[4] = 0;
		obos.expCabInstVessel[5] = 0;
		obos.expCabInstVessel[6] = 0;
		obos.expCabInstVessel[7] = 1640;
		obos.expCabInstVessel[8] = 0;
		obos.expCabInstVessel[9] = 0;
		obos.expCabInstVessel[10] = 0;
		obos.expCabInstVessel[11] = 10.1;
		obos.expCabInstVessel[12] = 25;
		obos.expCabInstVessel[13] = 1;
		obos.expCabInstVessel[14] = 173000;
		obos.expCabInstVessel[15] = 3;
		obos.expCabInstVessel[16] = 1;
		obos.expCabInstVessel[17] = 0;
		obos.expCabInstVessel[18] = 0;
		obos.expCabInstVessel[19] = 0;
		obos.expCabInstVessel[20] = 0;
		obos.expCabInstVessel[21] = 0;
		obos.expCabInstVessel[22] = 5980;
		obos.expCabInstVessel[23] = 0;
		obos.expCabInstVessel[24] = 0;
		obos.expCabInstVessel[25] = 0;
		obos.expCabInstVessel[26] = 0;


		//RUN COMPUTE MODULE***********************************************************************************************************************************
		obos.run();
		
		//Assign outputs***************************************************************************************************************************************
		/*
		assign("subTotCost", var_data(obos.subTotCost));
		assign("totElecCost", var_data(obos.totElecCost));
		assign("totAnICost", var_data(obos.totAnICost));
		assign("totPnSCost", var_data(obos.totPnSCost));
		assign("totEnMCost", var_data(obos.totEnMCost));
		assign("totDevCost", var_data(obos.totDevCost));
        assign("totInstTime", var_data(obos.totInstTime));
        assign("entrExitCost", var_data(obos.entrExitCost));
        assign("wharfCost", var_data(obos.wharfCost));
        assign("dockCost", var_data(obos.dockCost));
        assign("subLayCost", var_data(obos.subLayCost));
        assign("turbLayCost", var_data(obos.turbLayCost));
        assign("craneCost", var_data(obos.craneCost));
		assign("turbInstTime", var_data(obos.turbInstTime));
		assign("subInstTime", var_data(obos.subInstTime));
		assign("arrInstTime", var_data(obos.arrInstTime));
		assign("expInstTime", var_data(obos.expInstTime));
		assign("subsInstTime", var_data(obos.subsInstTime));
		assign("commissioning", var_data(obos.commissioning));

		assign("decomCost", var_data(obos.decomCost));
		assign("construction_insurance_cost", var_data(obos.construction_insurance_cost));
		assign("total_contingency_cost", var_data(obos.total_contingency_cost));
		assign("construction_finance_cost", var_data(obos.construction_finance_cost));
		assign("soft_costs", var_data(obos.soft_costs));*/

		//Total OBOS output
		assign("totalBOScost", var_data(obos.total_bos_cost));

		//General outputs
		assign("hubD", var_data(obos.hubD));
		assign("bladeL", var_data(obos.bladeL));
		assign("chord", var_data(obos.chord));
		assign("nacelleW", var_data(obos.nacelleW));
		assign("nacelleL", var_data(obos.nacelleL));
		assign("rnaM", var_data(obos.rnaM));
		assign("towerD", var_data(obos.towerD));
		assign("towerM", var_data(obos.towerM));
		assign("construction_finance_factor", var_data(obos.construction_finance_factor));

		//Substructure & foundation outputs
		assign("mpileM", var_data(obos.mpileM));
		assign("mtransM", var_data(obos.mtransM));
		assign("mPileCost", var_data(obos.mPileCost));
		assign("mTransCost", var_data(obos.mTransCost));
		assign("jlatticeM", var_data(obos.jlatticeM));
		assign("jtransM", var_data(obos.jtransM));
		assign("jpileM", var_data(obos.jpileM));
		assign("jLatticeCost", var_data(obos.jLatticeCost));
		assign("jTransCost", var_data(obos.jTransCost));
		assign("jPileCost", var_data(obos.jPileCost));
		assign("spStifColM", var_data(obos.spStifColM));
		assign("spTapColM", var_data(obos.spTapColM));
		assign("spStifColCost", var_data(obos.spStifColCost));
		assign("spTapColCost", var_data(obos.spTapColCost));
		assign("ballM", var_data(obos.ballM));
		assign("ballCost", var_data(obos.ballCost));
		assign("ssStifColM", var_data(obos.ssStifColM));
		assign("ssTrussM", var_data(obos.ssTrussM));
		assign("ssHeaveM", var_data(obos.ssHeaveM));
		assign("ssStifColCost", var_data(obos.ssStifColCost));
		assign("ssTrussCost", var_data(obos.ssTrussCost));
		assign("ssHeaveCost", var_data(obos.ssHeaveCost));
		assign("moorSysCost", var_data(obos.moorSysCost));
		assign("sSteelM", var_data(obos.sSteelM));
		assign("sSteelCost", var_data(obos.sSteelCost));
		assign("subTotM", var_data(obos.subTotM));
		assign("subTotCost", var_data(obos.subTotCost));

		//Electrical infrastructure outputs
		assign("systAngle", var_data(obos.systAngle));
		assign("freeCabLeng", var_data(obos.freeCabLeng));
		assign("fixCabLeng", var_data(obos.fixCabLeng));
		assign("nExpCab", var_data(obos.nExpCab));
		assign("nSubstation", var_data(obos.nSubstation));
		assign("fullStrings", var_data(obos.fullStrings));
		assign("nTurbPS", var_data(obos.nTurbPS));
		assign("nTurbCab1", var_data(obos.nTurbCab1));
		assign("nTurbCab2", var_data(obos.nTurbCab2));
		assign("nTurbInter1", var_data(obos.nTurbInter1));
		assign("nTurbInter2", var_data(obos.nTurbInter2));
		assign("nSubsInter", var_data(obos.nSubsInter));
		assign("cab1Leng", var_data(obos.cab1Leng));
		assign("cab2Leng", var_data(obos.cab2Leng));
		assign("expCabLeng", var_data(obos.expCabLeng));
		assign("nMPT", var_data(obos.nMPT));
		assign("mptRating", var_data(obos.mptRating));
		assign("mptCost", var_data(obos.mptCost));
		assign("subsTopM", var_data(obos.subsTopM));
		assign("subsTopCost", var_data(obos.subsTopCost));
		assign("arrCab1Cost", var_data(obos.arrCab1Cost));
		assign("arrCab2Cost", var_data(obos.arrCab2Cost));
		assign("expCabCost", var_data(obos.expCabCost));
		assign("shuntReactors", var_data(obos.shuntReactors));
		assign("switchGear", var_data(obos.switchGear));
		assign("ancillarySys", var_data(obos.ancillarySys));
		assign("subsSubM", var_data(obos.subsSubM));
		assign("subsPileM", var_data(obos.subsPileM));
		assign("subsLandAssembly", var_data(obos.subsLandAssembly));
		assign("subsSubCost", var_data(obos.subsSubCost));
		assign("switchYard", var_data(obos.switchYard));
		assign("onShoreSubs", var_data(obos.onShoreSubs));
		assign("onshoreMisc", var_data(obos.onshoreMisc));
		assign("transLine", var_data(obos.transLine));
		assign("subCabCost", var_data(obos.subCabCost));
		assign("offSubsCost", var_data(obos.offSubsCost));
		assign("onshoreTransCost", var_data(obos.onshoreTransCost));
		assign("totElecCost", var_data(obos.totElecCost));

		//Assembly & infrastructure outputs
		assign("moorTime", var_data(obos.moorTime));
		assign("turbDeckArea", var_data(obos.turbDeckArea));
		assign("nTurbPerTrip", var_data(obos.nTurbPerTrip));
		assign("turbInstTime", var_data(obos.turbInstTime));
		assign("subDeckArea", var_data(obos.subDeckArea));
		assign("nSubPerTrip", var_data(obos.nSubPerTrip));
		assign("subInstTime", var_data(obos.subInstTime));
		assign("cab1SecM", var_data(obos.cab1SecM));
		assign("cab2SecM", var_data(obos.cab2SecM));
		assign("cab1SecPerTrip", var_data(obos.cab1SecPerTrip));
		assign("cab2SecPerTrip", var_data(obos.cab2SecPerTrip));
		assign("arrInstTime", var_data(obos.arrInstTime));
		assign("expCabSecM", var_data(obos.expCabSecM));
		assign("expCabSecPerTrip", var_data(obos.expCabSecPerTrip));
		assign("expInstTime", var_data(obos.expInstTime));
		assign("subsInstTime", var_data(obos.subsInstTime));
		assign("totInstTime", var_data(obos.totInstTime));
		assign("totAnICost", var_data(obos.totAnICost));
		//assign("turbCostsByVessel", var_data(obos.turbCostsByVessel));
		//assign("subCostsByVessel", var_data(obos.subCostsByVessel));
		//assign("elecCostsByVessel", var_data(obos.elecCostsByVessel));
		//assign("mobDemobCostByVessel", var_data(obos.mobDemobCostByVessel));
		assign("cabSurvey", var_data(obos.cabSurvey));
        assign("turbine_install_cost", var_data(obos.turbine_install_cost));
        assign("substructure_install_cost", var_data(obos.substructure_install_cost));
        assign("electrical_install_cost", var_data(obos.electrical_install_cost));
        assign("mob_demob_cost", var_data(obos.mob_demob_cost));
		assign("array_cable_install_cost", var_data(obos.array_cable_install_cost));
		assign("export_cable_install_cost", var_data(obos.export_cable_install_cost));
		assign("substation_install_cost", var_data(obos.substation_install_cost));

		//Port & staging outputs
		assign("entrExitCost", var_data(obos.entrExitCost));
		assign("wharfCost", var_data(obos.wharfCost));
		assign("dockCost", var_data(obos.dockCost));
		assign("subLaydownA", var_data(obos.subLaydownA));
		assign("subLayCost", var_data(obos.subLayCost));
		assign("turbLaydownA", var_data(obos.turbLaydownA));
		assign("turbLayCost", var_data(obos.turbLayCost));
		assign("craneCost", var_data(obos.craneCost));
		assign("totPortCost", var_data(obos.totPortCost));
		assign("totStageCost", var_data(obos.totStageCost));
		assign("totPnSCost", var_data(obos.totPnSCost));

		//Engineering & management outputs
		assign("totEnMCost", var_data(obos.totEnMCost));

		//Development outputs
		assign("feedCost", var_data(obos.feedCost));
		assign("permStudyComp", var_data(obos.permStudyComp));
		assign("metFabCost", var_data(obos.metFabCost));
		assign("decomCost", var_data(obos.decomCost));
		assign("totDevCost", var_data(obos.totDevCost));
		assign("commissioning", var_data(obos.commissioning));



	}
};

DEFINE_MODULE_ENTRY(wind_obos, "Wind Offshore Balance of System cost model", 1)

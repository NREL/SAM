#######################################################################
## SDK Example: PVWatts + Utility Rate
## Severin Ryberg
## April 27th, 2015
##
##  The following is an example of accessing SAM's compute modules
## through the python SDK. This script queries Open EI for 
## utility rates available in a user-defined zip code. It then  
## downloads the selected rate data and applies it in SAM. Finally
## a simulations are performed using the 'pvwattsv5' and 'utilityrate3'
## compute modules using a user-provided weatherfile and electric load
## file. 
## 
## * The simulation will accept any of the weatherfile formats allowed 
##   in SAM
## * The electric load file is simply a file with 8760 lines of hourly 
##   load data
##  

import httplib, json, sys

## Include SAM libraries
sys.path.insert(0, 'C:\\SAM\\SDK\\languages\\python\\')
import sscapi

########################################
## Runtime Variables

_key = '<Obtain a key from http://en.openei.org/services/api/signup/>' ## Had to register for this
_version = 'latest'
_format = 'json'
_sector='Residential'
decoder = json.JSONDecoder()

tierSize = 6
periodSize = 12

monthNames = ['jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec']

########################################
## Construct PVWatts simulation

ssc = sscapi.PySSC()

data = ssc.data_create()
ssc.data_set_number(data, 'system_capacity', 4)
ssc.data_set_number(data, 'module_type', 0)
ssc.data_set_number(data, 'dc_ac_ratio', 1.1)
ssc.data_set_number(data, 'inv_eff', 96)
ssc.data_set_number(data, 'losses', 14.0757)
ssc.data_set_number(data, 'array_type', 0)
ssc.data_set_number(data, 'tilt', 20)
ssc.data_set_number(data, 'azimuth', 180)
ssc.data_set_number(data, 'gcr', 0.4)
ssc.data_set_number(data, 'adjust:constant', 0)


########################################
## open HTTP connection with OpenEI
con = httplib.HTTPConnection('api.openei.org')

########################################
## Get User's ZIP code and display local rates
rateFound = False
while( not rateFound):
    _zip = raw_input("Please enter your ZIP code: ")
    print("Downloading utility rates near " + _zip + "...")

    reqURL = '/utility_rates?version=%s&format=%s&api_key=%s&detail=full&address=%s&sector=%s' %(
            _version,
            _format,
            _key,
            _zip,
            _sector)
    
    con.request('GET', reqURL)
    r = con.getresponse()
    print((r.status,r.reason))

    rateDataRaw = r.read()
    
    if(len(rateDataRaw)<100):
        print("OpenEI did not return any rates for the ZIP code you provided. Please try again.\n")
        continue
    
    rateData = decoder.decode(rateDataRaw)
   
    print("\nUtility rates in this area:")
    for i in range(len(rateData['items'])):
        print("\t%d: %s - %s" % (i+1,rateData['items'][i]['utility'], rateData['items'][i]['name']))
    
    while(True):
        try:
            rateSelection = int(raw_input("\nChoose which rate you would like to use, or 0 to try again: "))
        except ValueError:
            print('Could not understand input. Please enter a number corresponding to one of the rates above.')
            continue

        if(rateSelection <0 or rateSelection>len(rateData['items'])):
            print('Please enter a number corresponding to one of the rates above, or 0.')
            continue
        else: break

    if(rateSelection>0):
        rate = rateData['items'][rateSelection-1]
        rateFound = True

con.close()

########################################
## Apply rate data to variables

print("\nParsing rate data for \'%s\'" %rate['utility'])

def tryRead(data, var, default):
    try:
        output = data[var]
        good = True
    except KeyError:
        output = default
        good = False
    return (good, output)    

## Set default values for volatile data

monthlyFixedCharge = 0.0

energyChargesEnable = 0

energyChargesWeekdaySchedule = [[ 1 for i in range(24)] for j in range(12)]
energyChargesWeekendSchedule = [[ 1 for i in range(24)] for j in range(12)]

energyChargesMaxUsageDefault = 1e38
energyChargesMaxUsage = [[ energyChargesMaxUsageDefault for i in range(tierSize)] for j in range(periodSize)]

energyChargesBuyRateDefault = 0
energyChargesBuyRate  = [[ energyChargesBuyRateDefault for i in range(tierSize)] for j in range(periodSize)]

energyChargesSellRateDefault = 0
energyChargesSellRate  = [[ energyChargesSellRateDefault for i in range(tierSize)] for j in range(periodSize)]

demandChargesEnable = 0

fixedDemandChargesMaxUsageDefault = 1e38
fixedDemandChargesMaxUsage = [[ fixedDemandChargesMaxUsageDefault for i in range(tierSize)] for j in range(periodSize)]

fixedDemandChargesChargeDefault = 0
fixedDemandChargesCharge = [[ fixedDemandChargesChargeDefault for i in range(tierSize)] for j in range(periodSize)]

touDemandChargesWeekdaySchedule = [[ 1 for i in range(24)] for j in range(12)]
touDemandChargesWeekendSchedule = [[ 1 for i in range(24)] for j in range(12)]

touDemandChargesMaxUsageDefault = 1e38
touDemandChargesMaxUsage = [[ touDemandChargesMaxUsageDefault for i in range(tierSize)] for j in range(periodSize)]

touDemandChargesChargeDefault = 0
touDemandChargesCharge = [[ touDemandChargesChargeDefault for i in range(tierSize)] for j in range(periodSize)]

## Looking for a flat monthly charge and unit

(goodRead, monthlyFixedCharge) = tryRead(rate, 'fixedmonthlycharge', monthlyFixedCharge)
if(goodRead): print('\tFound a fixed monthly charge')

(goodRead, unit) = tryRead(rate, 'flatdemandunit', 'kWh')

if(unit=='kWh' or unit=='kW'): scale = 1.0
elif(unit=='Wh' or unit=='W'): scale = 0.001
else: 
    print('\t\tUnit not recognised. Assuming kWh')
    scale = 1.0

monthlyFixedCharge *= scale

## Look for an energy rate structure

(goodRead, energyStructure) = tryRead(rate, 'energyratestructure', [{}])
if(goodRead): 
    print('\tFound an energy rate structure with %d period%s' %(len(energyStructure), ('s' if len(energyStructure)>1 else '') ))
    energyChargesEnable = 1

    for pi in range(len(energyStructure)):
        for ti in range(len(energyStructure[pi])):
            (goodread, periodRate) = tryRead(energyStructure[pi][ti], 'rate', energyChargesBuyRateDefault)
            (goodread, periodAdj) = tryRead(energyStructure[pi][ti], 'adj', 0.0)
            (goodread, periodMax) = tryRead(energyStructure[pi][ti], 'Max', energyChargesMaxUsageDefault)
            (goodread, periodUnit) = tryRead(energyStructure[pi][ti], 'unit', 'kWh')
    
            if(periodUnit=='kWh' or periodUnit=='kW'): scale = 1.0
            elif(periodUnit=='Wh' or periodUnit=='W'): scale = 0.001
            else: 
                print('\t\t\tUnit not recognised. Assuming kWh')
                scale = 1.0
            
            energyChargesMaxUsage[pi][ti] = periodMax
            energyChargesBuyRate[pi][ti] = (periodRate+periodAdj)*scale

    (goodRead, energyChargesWeekdaySchedule) = tryRead(rate, 'energyweekdayschedule', energyChargesWeekdaySchedule)
    if(goodRead):
        for mi in range(12):
            for hi in range(24):
                energyChargesWeekdaySchedule[mi][hi] += 1
    
    (goodRead, energyChargesWeekendSchedule) = tryRead(rate, 'energyweekendschedule', energyChargesWeekendSchedule)
    if(goodRead):
        for mi in range(12):
            for hi in range(24):
                energyChargesWeekendSchedule[mi][hi] += 1


## Look for a demand rate structure

(goodRead, demandStructure) = tryRead(rate, 'demandratestructure', [{}])
if(goodRead): 
    print('\tFound a demand rate structure with %d period%s' %(len(demandStructure), ('s' if len(demandStructure)>1 else '') ))
    demandChargesEnable = 1

    for pi in range(len(demandStructure)):
        for ti in range(len(demandStructure[pi])):
            (goodread, periodRate) = tryRead(demandStructure[pi][ti], 'rate', touDemandChargesChargeDefault)
            (goodread, periodAdj) = tryRead(demandStructure[pi][ti], 'adj', 0.0)
            (goodread, periodMax) = tryRead(demandStructure[pi][ti], 'Max', touDemandChargesMaxUsageDefault)
            (goodread, periodUnit) = tryRead(demandStructure[pi][ti], 'unit', 'kWh')
                        
            if(periodUnit=='kWh' or periodUnit=='kW'): scale = 1.0
            elif(periodUnit=='Wh' or periodUnit=='W'): scale = 0.001
            else: 
                print('\t\t\tUnit not recognised. Assuming kWh')
                scale = 1.

            touDemandChargesMaxUsage[pi][ti] = periodMax
            touDemandChargesCharge[pi][ti] = (periodRate+periodAdj)*scale

    (goodRead, touDemandChargesWeekdaySchedule) = tryRead(rate, 'demandweekdayschedule', touDemandChargesWeekdaySchedule)
    if(goodRead):
        for mi in range(12):
            for hi in range(24):
                touDemandChargesWeekdaySchedule[mi][hi] += 1
    
    (goodRead, touDemandChargesWeekendSchedule) = tryRead(rate, 'demandweekendschedule', touDemandChargesWeekendSchedule)
    if(goodRead):
        for mi in range(12):
            for hi in range(24):
                touDemandChargesWeekendSchedule[mi][hi] += 1


    ## Need to add reads for flat demand rates (need to find an example)


########################################
## Write variables into SAM
print("\nWriting rate data into SAM")

## Write Constant Variables
ssc.data_set_number(data, 'analysis_period', 25)
ssc.data_set_number(data, 'inflation_rate', 2.5)
ssc.data_set_array(data, 'degradation', [ 0.5 ])
ssc.data_set_array(data, 'load_escalation', [ 0 ])
ssc.data_set_array(data, 'rate_escalation', [ 0 ])
ssc.data_set_number(data, 'ur_enable_net_metering', 1)
ssc.data_set_number(data, 'ur_nm_yearend_sell_rate', 0.02789)
ssc.data_set_number(data, 'ur_flat_buy_rate', 0 )
ssc.data_set_number(data, 'ur_flat_sell_rate', 0 )
ssc.data_set_number(data, 'ur_monthly_min_charge', 0 )
ssc.data_set_number(data, 'ur_annual_min_charge', 0 )

## Write volatile variables

ssc.data_set_number(data, 'ur_monthly_fixed_charge', monthlyFixedCharge )

ssc.data_set_number(data, 'ur_ec_enable', energyChargesEnable )
ssc.data_set_matrix(data, 'ur_ec_sched_weekday', energyChargesWeekdaySchedule)
ssc.data_set_matrix(data, 'ur_ec_sched_weekend', energyChargesWeekendSchedule)

for pi in range(periodSize):
    for ti in range(tierSize):
        root = ("ur_ec_p%d_t%d_" %(pi+1, ti+1))
        ssc.data_set_number(data, root +"br", energyChargesBuyRate[pi][ti])
        ssc.data_set_number(data, root +"sr", energyChargesSellRate[pi][ti])
        ssc.data_set_number(data, root +"ub", energyChargesMaxUsage[pi][ti])

ssc.data_set_number(data, 'ur_dc_enable', demandChargesEnable )
ssc.data_set_matrix(data, 'ur_dc_sched_weekday', touDemandChargesWeekdaySchedule)
ssc.data_set_matrix(data, 'ur_dc_sched_weekend', touDemandChargesWeekendSchedule)

for pi in range(periodSize):
    for ti in range(tierSize):
        root = ("ur_dc_p%d_t%d_" %(pi+1, ti+1))
        ssc.data_set_number(data, root +"dc", touDemandChargesCharge[pi][ti])
        ssc.data_set_number(data, root +"ub", touDemandChargesMaxUsage[pi][ti])

        root = ("ur_dc_%s_t%d_" %(monthNames[pi], ti+1))
        ssc.data_set_number(data, root +"dc", fixedDemandChargesCharge[pi][ti])
        ssc.data_set_number(data, root +"ub", fixedDemandChargesMaxUsage[pi][ti])


##########################################
## Get weather file and load file from user

weatherFile = raw_input('\nEnter location of weather file: ')
loadFile = raw_input('Enter location of electric load file: ')

ssc.data_set_string(data, "solar_resource_file", weatherFile)

## Read in electric load data from csv file

electricLoad = []
for line in open(loadFile):
    try:
        electricLoad.append( float(line[:-1]) )
    except ValueError:
        continue

## for pre-gen utilityrate3 before 4/15/15
##ssc.data_set_array(data, 'p_load', electricLoad )
##ssc.data_set_array(data, 'e_load', electricLoad )
ssc.data_set_array(data, 'load', electricLoad )

########################################
## Running SAM Simulation

print("\nExecuting SAM Simulations...")

pvModule = ssc.module_create("pvwattsv5")
utilityModule = ssc.module_create("utilityrate3")

ssc.module_exec_set_print( 0 )

print( "\nPVWatts Sim: " + str( ssc.module_exec( pvModule, data )))
print( "Utility Sim: " + str( ssc.module_exec( utilityModule, data )))

########################################
## Report Results

monthlyBillWithSystem = ssc.data_get_array( data, 'year1_monthly_ec_charge_with_system')
monthlyBillWithoutSystem = ssc.data_get_array(data, 'year1_monthly_ec_charge_without_system')

print("\n\nEstimated Monthly Bill ($):")
print('\tMonth\tWith System\tWithout System')

for i in range(12):
    print( "\t %s \t  %6.5f  \t  %6.5f" %(monthNames[i], monthlyBillWithSystem[i], monthlyBillWithoutSystem[i]) )


raw_input("\n\nPress Enter to exit...")

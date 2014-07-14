#ifndef __openeiapi_h
#define __openeiapi_h

#include <wx/string.h>
#include <vector>

class OpenEI
{
public:
	struct RateInfo
	{
		wxString GUID;
		wxString Name;
		wxString Utility;
		wxString Sector;
		wxString Description;
		wxString Source;
		wxString uri;
		int Version;
	};

	struct RateData
	{
		RateData();
		void Reset();

		RateInfo Header;

		wxString StartDate;
		wxString EndDate;

		bool NetMetering;
		bool HasFlatRate;
		double FlatRateBuy;
		double FlatRateSell;
		double FlatRateAdj;
		double FixedMonthlyCharge;


		bool HasDemandCharge;
		wxString DemandRateUnit; // kW, kVA or hp
		double DemandReactivePower;
		// month based
		int FlatDemandMonth[12];
		double FlatDemandMax[12][6]; 
		double FlatDemandCharge[12][6]; 
		double FlatDemandAdj[12][6]; 
		// diurnal based
		double DemandMax[12][6]; 
		double DemandCharge[12][6]; 
		double DemandAdj[12][6]; 
//		char DemandWeekdaySchedule[289];
//		char DemandWeekendSchedule[289];
		double DemandWeekdaySchedule[12][24];
		double DemandWeekendSchedule[12][24];

		bool HasEnergyCharge;	
		// diurnal based
		wxString EnergyRateUnit; // kWh
		double EnergyMax[12][6]; 
		double EnergyBuy[12][6]; 
		double EnergyAdj[12][6]; 
		double EnergySell[12][6]; 
//		char EnergyWeekdaySchedule[289];
//		char EnergyWeekendSchedule[289];
		double EnergyWeekdaySchedule[12][24];
		double EnergyWeekendSchedule[12][24];

	
	};

	bool QueryUtilityCompanies(wxArrayString &names, wxString *err=NULL);
	bool QueryUtilityRates(const wxString &name, std::vector<RateInfo> &rates, wxString *err=NULL);
	int UtilityCompanyRateCount(const wxString &name);
	bool RetrieveUtilityRateData(const wxString &guid, RateData &rate, wxString *json_url=NULL, wxString *err=NULL);

};

#endif

#ifndef __urdbopeneiapi_h
#define __urdbopeneiapi_h

#include <vector>

#include <wx/wx.h>
#include <wx/string.h>

#include <wex/jsonreader.h>

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
		wxString BasicInformationComments;
		wxString EnergyComments;
		wxString DemandComments;
		wxString RateURL;
		wxString JSONURL;
		int Version;
	};

	struct ApplicabilityInfo
	{
		double peakkwcapacitymin;
		double peakkwcapacitymax;
		double peakkwcapacityhistory;
		double peakkwhusagemin;
		double peakkwhusagemax;
		double peakkwhusagehistory;
		double voltageminimum;
		double voltagemaximum;
		wxString voltagecategory;
		wxString phasewiring;
	};

	struct RateData
	{
		RateData();
		void Reset();

		RateInfo Header;
		ApplicabilityInfo Applicability;

		wxString StartDate;
		wxString EndDate;

		bool NetMetering;
		double MinAnnualCharge;
		double MinMonthlyCharge;
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
//		wxString EnergyRateUnit; // kWh
		// TODO - handle different max usage units
		wxString EnergyMaxUnit[12][6];
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
	bool RetrieveDiurnalData(wxJSONValue &month_ary, double sched[12][24]);
};


class wxHyperlinkCtrl;
class wxExtTextCtrl;

class OpenEIUtilityRateDialog : public wxDialog
{
public:
	OpenEIUtilityRateDialog(wxWindow *parent, const wxString &title, const wxString &market);


/*user.class.start*/

	void StartHttp();
	void QueryUtilities();
	void QueryRates(const wxString &utility_name);
	void OnEvent(wxCommandEvent &evt);
	void OnTimer(wxTimerEvent &evt);
	void UpdateRateList();
	void UpdateUtilityList();
	void UpdateRateData();

	void OnCommand( wxCommandEvent & );
	void OnClose( wxCloseEvent & );
	bool IsBusy();

	virtual int ShowModal();

	OpenEI::RateData GetCurrentRateData();
private:

	wxButton *btnApply;
	wxStaticText *lblStatus;
	wxHyperlinkCtrl *hypOpenEILink;
	wxHyperlinkCtrl *hypJSONLink;
	wxTextCtrl *txtRateDescription;
//	wxExtTextCtrl *txtRateEndDate;
//	wxExtTextCtrl *txtRateStartDate;
	wxExtTextCtrl *txtRateName;
	wxListBox *lstRates;
	wxListBox *lstUtilities;
	wxButton *btnClose;
	wxExtTextCtrl *txtUtilitySearch;
	wxButton *btnQueryAgain;
	wxComboBox *cboResCom;

	wxArrayString mUtilityCompanies;
	std::vector<OpenEI::RateInfo> mUtilityRates;
	OpenEI::RateData mRateData;
	wxArrayString mGUIDList;
	wxTimer mTimer;
	bool mBusy;
	OpenEI api;
	
/*user.class.end*/
	DECLARE_EVENT_TABLE()
};

#endif

#ifndef __urdbopeneiapi_h
#define __urdbopeneiapi_h

#include <vector>

#include <wx/wx.h>
#include <wx/string.h>

#include "object.h"
#include <wex/jsonreader.h>

class AFSearchListBox;

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
		wxString StartDate;
		wxString EndDate;
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

		bool NetMetering;
		double MinAnnualCharge;
		double MinMonthlyCharge;
		double FixedMonthlyCharge;


		bool HasDemandCharge;
		wxString DemandRateUnit; // kW, kVA or hp
		double DemandReactivePower;

		// month based
		int FlatDemandMonth[12];
		matrix_t<double> DemandFlatStructure;
		
		// diurnal based
		matrix_t<double> DemandTOUStructure;
		double DemandWeekdaySchedule[12][24];
		double DemandWeekendSchedule[12][24];

		bool HasEnergyCharge;	
		matrix_t<double> EnergyStructure;
		double EnergyWeekdaySchedule[12][24];
		double EnergyWeekendSchedule[12][24];

	
	};

	bool QueryUtilityCompanies(wxArrayString &names, wxString *err=NULL);
	// search by zip code per email from Jay Huggins 1/9/15
	bool QueryUtilityCompaniesbyZipcode(const wxString &zipcode, wxArrayString &names, wxString *err=NULL);
	// resolve aliases in database per email from Jay Huggins 1/9/15
	bool ResolveUtilityName(const wxString &name, wxString *urdb_name, wxString *err=NULL);
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
	void QueryUtilitiesByZipCode();
	void QueryRates(const wxString &utility_name);
	void OnEvent(wxCommandEvent &evt);
	void OnTimer(wxTimerEvent &evt);
	void UpdateRateList();
	void UpdateRateData();

	void OnCommand( wxCommandEvent & );
	void OnClose( wxCloseEvent & );
	bool IsBusy();

	virtual int ShowModal();

	OpenEI::RateData GetCurrentRateData();
private:

	wxButton *btnApply;
	wxStaticText *lblStatus;
	wxStaticText *lblUtilityCount;
	wxHyperlinkCtrl *hypOpenEILink;
	wxHyperlinkCtrl *hypJSONLink;
	wxTextCtrl *txtRateDescription;
	wxExtTextCtrl *txtRateName;
	wxExtTextCtrl *txtRateStartDate;
	wxExtTextCtrl *txtRateEndDate;
	wxExtTextCtrl *txtRateGUID;
	AFSearchListBox *lstRates;
	AFSearchListBox *lstUtilities;
	wxButton *btnClose;
	wxButton *btnQueryAgain;
	wxChoice *cboResCom;
	wxCheckBox *chkActiveOnly;

	wxButton *btnQueryZipCode;
	wxTextCtrl *txtZipCode;

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

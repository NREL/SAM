/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

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

	// unused items to add later from https://openei.org/services/doc/rest/util_rates/?version=7
	// read these items to give feedback to user
	struct UnusedItems
	{
		bool HasUnusedItems;
		double FuelAdjustmentsMonthly[12]; // array: Array of 12 decimal numbers, one per month, each is $/kWh
		wxString ServiceType; // enumeration:  "Bundled", "Energy", "Delivery", or "Delivery with Standard Offer"
		double DemandWindow; // decimal: Demand Window (minutes)
		bool HasCoincidentRate;
		matrix_t<double> CoincidentRateStructure; // array: [[{"max":(Decimal),"rate":(Decimal),"adj":(Decimal),"sell":(Decimal)},...],...] 
		double CoincidentSchedule[12][24]; // array: Value is an array of arrays. The 12 top-level arrays correspond to a month of the year. Each month array contains one integer per hour of the day from 12am to 11pm, and the integer corresponds to the index of a period in coincidentratestructure.
		wxString CoincidentRateUnit; // enumeration: kW, hp, kVA, kW daily, hp daily,kVA daily
		double DemandRatchetPercentage[12]; // array: Array of 12 decimal numbers, one Demand Ratchet Percentage per month
		double DemandReactivePowerCharge; // decimal: Demand Reactive Power Charge ($/kVAR)
		wxString EnergyAttrs; // array (as string for information): Other Energy Attributes in a key/value format
		wxString DemandAttrs; // array (as string for information): Other Demand Attributes in a key/value format
		wxString FixedAttrs; // array (as string for information): Other Demand Attributes in a key/value format
		bool IsDefault;
	};

	struct RateData
	{
		RateData();
		void Reset();

		RateInfo Header;
		ApplicabilityInfo Applicability;
		UnusedItems Unused;

		wxString DgRules;
		double MinCharge;
		wxString MinChargeUnits;

		double FixedChargeFirstMeter;
		double FixedChargeAddlMeter;
		wxString FixedChargeUnits;

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
	bool QueryUtilityRates(const wxString &name, std::vector<RateInfo> &rates, wxString *err=NULL);
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
	wxStaticText *lblRateStatus;
	wxStaticText *lblUtilityCount;
	wxHyperlinkCtrl *hypOpenEILink;
	wxHyperlinkCtrl *hypJSONLink;
	wxTextCtrl *txtRateDescription;
    wxExtTextCtrl* txtRateUtility;
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

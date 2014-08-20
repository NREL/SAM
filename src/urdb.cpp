#include <wx/tokenzr.h>
#include <wx/log.h>

#include <wex/utils.h>

#include "urdb.h"
#include "simplecurl.h"



static wxString MyGet(const wxString &url)
{
	wxSimpleCurl curl;
	curl.AddHttpHeader( "Content-type: text/plain" );
	curl.AddHttpHeader( "Cache-Control: no-cache" );
	curl.Start( url, true );
	return curl.GetDataAsString();
}

OpenEI::RateData::RateData()
{
	Reset();
}

void OpenEI::RateData::Reset()
{
	int i,j;

	Header.GUID.Empty();
	Header.Name.Empty();
	Header.Description.Empty();
	Header.Sector.Empty();
	Header.Utility.Empty();
	Header.BasicInformationComments.Empty();
	Header.EnergyComments.Empty();
	Header.DemandComments.Empty();

	Applicability.peakkwcapacityhistory = 0;
	Applicability.peakkwcapacitymax = 0;
	Applicability.peakkwcapacitymin = 0;
	Applicability.peakkwhusagehistory = 0;
	Applicability.peakkwhusagemax = 0;
	Applicability.peakkwhusagemin = 0;
	Applicability.voltagemaximum = 0;
	Applicability.voltageminimum = 0;
	Applicability.voltagecategory.Empty();
	Applicability.phasewiring.Empty();

	NetMetering=false;
	MinMonthlyCharge=0.0;
	MinAnnualCharge=0.0;
	FixedMonthlyCharge=0.0;


	HasEnergyCharge=false;	
//	EnergyRateUnit = "kWh";

	for (i = 0; i < 12; i++)
	{
		for (j = 0; j < 6; j++)
		{
			EnergyBuy[i][j] = EnergyAdj[i][j] = EnergySell[i][j] = 0.0;
			EnergyMax[i][j] = 1e99;
			EnergyMaxUnit[i][j] = "kWh Daily"; // TODO implement max unit
		}
		for (int k = 0; k < 24; k++)
		{
			EnergyWeekdaySchedule[i][k] = 1;
			EnergyWeekendSchedule[i][k] = 1;
		}
	}

	// TODO - coincident demand charges

	HasDemandCharge = false;
	DemandRateUnit = "kW"; // TODO update to handle different values
	DemandReactivePower = 1.0;
	
	for (i=0;i<12;i++)
		FlatDemandMonth[i]=0;

	for (i = 0; i < 12; i++)
	{
		for (j = 0; j < 6; j++)
		{
			FlatDemandCharge[i][j] = FlatDemandAdj[i][j] = 0.0;
			FlatDemandMax[i][j] = 1e99;
			DemandCharge[i][j] = DemandAdj[i][j] = 0.0;
			DemandMax[i][j] = 1e99;
		}
		for (int k = 0; k < 24; k++)
		{
			DemandWeekdaySchedule[i][k] = 1;
			DemandWeekendSchedule[i][k] = 1;
		}
	}
	
}

bool OpenEI::QueryUtilityCompanies(wxArrayString &names, wxString *err)
{

	//  based on emails from Paul and Jay Huggins 3/24/14
//	wxString url = "http://en.openei.org/services/rest/utility_companies?version=2&format=json_plain&callback=callback";
	//  based on email from Jay Huggins 7/8/14 - use latest format - still at version 2
	wxString url = "http://en.openei.org/services/rest/utility_companies?version=latest&format=json_plain&callback=callback";

//	wxString json_data = wxWebHttpGet(url);
	wxString json_data = MyGet(url);
	if (json_data.IsEmpty())
	{
		if (err) *err = "Could not retrieve JSON data for utility rate companies.";
		return false;
	}

	wxJSONReader reader;
	wxJSONValue root;
	if (reader.Parse( json_data, &root )!=0)
	{
		if (err) *err = "Could not process returned JSON data for utility rate companies.";
		return false;
	}

	names.Clear();
	wxJSONValue item_list = root.Item("items");
	int count = item_list.Size();
	for (int i=0;i<count;i++)
	{
		wxString buf = item_list[i].Item("label").AsString();
		buf.Replace("&amp;", "&");
		// version 3 not handling aliases 7/9/14 - EXTREMELY SLOW!!
		//if (UtilityCompanyRateCount(buf) > 0)
			names.Add( buf );
	}

	if (err) *err = wxEmptyString;

	return true;

}

bool OpenEI::QueryUtilityRates(const wxString &name, std::vector<RateInfo> &rates, wxString *err)
{
	wxString utlnm = name;
	utlnm.Replace("&", "%26");
	// production http://dev.openei.org/services/doc/rest/util_rates?version=3
	wxString url = "http://en.openei.org/services/rest/utility_rates?version=3&detail=minimal&format=json_plain&ratesforutility=" + utlnm;
	
	wxString json_data = MyGet(url);
	if (json_data.IsEmpty())
	{
		if (err) *err = "Could not retrieve rate information for " + name;
		return false;
	}

	json_data.Replace("(\"", "(");
	json_data.Replace("\")", ")");

	wxJSONReader reader;
	wxJSONValue root;
	if (reader.Parse( json_data, &root )!=0)
	{
		if (err) *err = "Could not process returned JSON data for utility rates for " + name;
		return false;
	}

	rates.clear();
	wxJSONValue item_list = root.Item("items");
	int count = item_list.Size();
	for (int i=0;i<count;i++)
	{

		RateInfo x;
//		x.GUID = json_string(item_list[i].Item("label")).Mid(5);
		x.GUID = json_string(item_list[i].Item("label"));
		x.Name = json_string(item_list[i].Item("name"));
		x.Utility = json_string(item_list[i].Item("utility"));
		x.Sector = json_string(item_list[i].Item("sector"));
		x.Description = json_string(item_list[i].Item("description"));
		x.Source = json_string(item_list[i].Item("source"));
		x.Version = json_integer(item_list[i].Item("version"));
		x.uri = json_string(item_list[i].Item("uri"));
		rates.push_back(x);
	}

	if (err) *err = wxEmptyString;

	return true;
}

int OpenEI::UtilityCompanyRateCount(const wxString &name)
{
	// production
	wxString url = "http://en.openei.org/services/rest/utility_rates?version=3&limit=500&detail=minimal&format=json_plain&ratesforutility=" + name;
	wxString json_data = MyGet(url);
	if (json_data.IsEmpty())
		return 0;

	wxJSONReader reader;
	wxJSONValue root;
	if (reader.Parse( json_data, &root )!=0)
		return 0;
	
	return root.Item("items").Size();
}

bool OpenEI::RetrieveUtilityRateData(const wxString &guid, RateData &rate, wxString *json_url, wxString *err)
{
	// production
	// version 2
//	wxString url = "http://en.openei.org/services/rest/utility_rates?version=2&format=json_plain&detail=full&getpage=Data:" + guid;
	// version 3
	wxString url = "http://en.openei.org/services/rest/utility_rates?version=3&format=json_plain&detail=full&getpage=" + guid;

	if (json_url) *json_url = url;

	wxString json_data = MyGet(url);
	if (json_data.IsEmpty())
	{
		if (err) *err="Could not retrieve utility rate JSON data for " + guid;
		return false;
	}

	wxJSONReader reader;
	wxJSONValue root;
	if (reader.Parse( json_data, &root )!=0)
	{
		if (err) *err = "Could not process returned JSON data for utility rate: " + guid;
		return false;
	}

	wxJSONValue val = root.Item("items").ItemAt(0);
	if (val.IsNull())
	{
		if (err) *err = "Root JSON structure error - cannot read rate data information.";
		return false;
	}

	rate.Reset();
	
	rate.Header.GUID = guid;
	rate.Header.Name = json_string( val.Item("name"));
	rate.Header.Utility = json_string( val.Item("utility"));
	rate.Header.Sector = json_string( val.Item("sector"));
	rate.Header.Description = json_string( val.Item("description"));
	rate.Header.Source = json_string( val.Item("source"));
	rate.Header.Version = json_integer( val.Item("version"));
	rate.Header.EnergyComments = json_string(val.Item("energycomments"));
	rate.Header.DemandComments = json_string(val.Item("demandcomments"));
	rate.Header.BasicInformationComments = json_string(val.Item("demandcomments"));
	rate.Header.JSONURL = url;
	rate.Header.RateURL = "http://en.openei.org/apps/USURDB/rate/view/" + guid; 

	rate.StartDate = json_string(val.Item("startdate"));
	rate.EndDate = json_string(val.Item("enddate"));

	rate.NetMetering = val.Item("usenetmetering").AsBool();

	// Applicability
	rate.Applicability.peakkwcapacityhistory = json_double(val.Item("peakkwcapacityhistory"));
	rate.Applicability.peakkwcapacitymax = json_double(val.Item("peakkwcapacitymax"));
	rate.Applicability.peakkwcapacitymin = json_double(val.Item("peakkwcapacitymin"));
	rate.Applicability.peakkwhusagehistory = json_double(val.Item("peakkwhusagehistory"));
	rate.Applicability.peakkwhusagemax = json_double(val.Item("peakkwhusagemax"));
	rate.Applicability.peakkwhusagemin = json_double(val.Item("peakkwhusagemin"));
	rate.Applicability.voltagemaximum = json_double(val.Item("voltagemaximum"));
	rate.Applicability.voltageminimum = json_double(val.Item("voltageminimum"));
	rate.Applicability.voltagecategory = json_string(val.Item("voltagecategory"));
	rate.Applicability.phasewiring = json_string(val.Item("phasewiring"));


	wxJSONValue v;
	
	rate.MinAnnualCharge = json_double(val.Item("annualmincharge"));
	rate.MinMonthlyCharge = json_double(val.Item("minmonthlycharge"));
	rate.FixedMonthlyCharge = json_double(val.Item("fixedmonthlycharge"));

	/// Energy Charge

//	rate.EnergyRateUnit = json_string( val.Item("energyrateunit") );

	rate.HasEnergyCharge = true;

	wxJSONValue ers_periods = val.Item("energyratestructure");
	if (ers_periods.IsArray())
	{
		if (ers_periods.Size() > 12) return false;
		for (int period = 0; period < ers_periods.Size(); period++)
		{	
			wxJSONValue ers_tier = ers_periods[period];
			if (ers_tier.IsArray())
			{
				if (ers_tier.Size() > 6) return false;
				for (int tier = 0; tier < ers_tier.Size(); tier++)
				{
					rate.EnergyMax[period][tier] = json_double(ers_tier[tier].Item("max"), 1e99, &rate.HasEnergyCharge);
					rate.EnergyBuy[period][tier] = json_double(ers_tier[tier].Item("rate"), 0.0, &rate.HasEnergyCharge);
					rate.EnergySell[period][tier] = json_double(ers_tier[tier].Item("sell"), 0.0, &rate.HasEnergyCharge);
					rate.EnergyAdj[period][tier] = json_double(ers_tier[tier].Item("adj"), 0.0, &rate.HasEnergyCharge);
					rate.EnergyMaxUnit[period][tier] = json_string(ers_tier[tier].Item("unit"));

				}
			}
		}
	}

	/*
	for (int period=0;period<12;period++)
		for (int tier=0; tier<6; tier++)
		{
			wxString period_string = wxString::Format("energyratestructure/period%d/tier%d", period+1, tier+1);

			rate.EnergyMax[period][tier] = json_double( val.Item(period_string + "max"), 1e99, &rate.HasEnergyCharge );

			rate.EnergyBuy[period][tier] = json_double( val.Item(period_string + "rate"), 0.0, &rate.HasEnergyCharge );

			rate.EnergySell[period][tier] = json_double( val.Item(period_string + "sell"), 0.0, &rate.HasEnergyCharge );
			rate.EnergyAdj[period][tier] = json_double( val.Item(period_string + "adjustment"), 0.0, &rate.HasEnergyCharge );
		}
*/

	if (!RetrieveDiurnalData(val.Item("energyweekdayschedule"), rate.EnergyWeekdaySchedule)) return false;
	if (!RetrieveDiurnalData(val.Item("energyweekendschedule"), rate.EnergyWeekendSchedule)) return false;

	/// DEMAND CHARGES
	rate.HasDemandCharge = true;

	rate.DemandRateUnit = json_string( val.Item("demandrateunit") );

	rate.DemandReactivePower = json_double( val.Item("demandreactivepowercharge") );


	wxJSONValue fdm_periods = val.Item("flatdemandmonths");
	if (fdm_periods.IsArray())
	{
		if (fdm_periods.Size() > 12) return false;
		for (int month = 0; month<12; month++)
			rate.FlatDemandMonth[month] = fdm_periods[month].AsInt();
	}

	wxJSONValue fds_periods = val.Item("flatdemandstructure");
	if (fds_periods.IsArray())
	{
		if (fds_periods.Size() > 12) return false;
		for (int period = 0; period < fds_periods.Size(); period++)
		{
			wxJSONValue fds_tier = fds_periods[period];
			if (fds_tier.IsArray())
			{
				if (fds_tier.Size() > 6) return false;
				for (int tier = 0; tier < fds_tier.Size(); tier++)
				{
					rate.FlatDemandMax[period][tier] = json_double(fds_tier[tier].Item("max"), 1e99, &rate.HasDemandCharge);
					rate.FlatDemandCharge[period][tier] = json_double(fds_tier[tier].Item("rate"), 0.0, &rate.HasDemandCharge);
					rate.FlatDemandAdj[period][tier] = json_double(fds_tier[tier].Item("adj"), 0.0, &rate.HasDemandCharge);
				}
			}
		}
	}



	wxJSONValue drs_periods = val.Item("demandratestructure");
	if (drs_periods.IsArray())
	{
		if (drs_periods.Size() > 12) return false;
		for (int period = 0; period < drs_periods.Size(); period++)
		{
			wxJSONValue drs_tier = drs_periods[period];
			if (drs_tier.IsArray())
			{
				if (drs_tier.Size() > 6) return false;
				for (int tier = 0; tier < drs_tier.Size(); tier++)
				{
					rate.DemandMax[period][tier] = json_double(drs_tier[tier].Item("max"), 1e99, &rate.HasDemandCharge);
					rate.DemandCharge[period][tier] = json_double(drs_tier[tier].Item("rate"), 0.0, &rate.HasDemandCharge);
					rate.DemandAdj[period][tier] = json_double(drs_tier[tier].Item("adj"), 0.0, &rate.HasDemandCharge);
				}
			}
		}
	}



	/*
	for (int month=0; month<12; month++)
	{
		wxString flatmonth_string = wxString::Format("flatdemandmonth%d", month+1);
		rate.FlatDemandMonth[month] = json_integer( val.Item( flatmonth_string ) );
	}

	for (int period=0;period<12;period++)
		for (int tier=0; tier<6; tier++)
		{
			wxString period_string = wxString::Format("flatdemandstructure/period%d/tier%d", period+1, tier+1);

			rate.FlatDemandMax[period][tier] = json_double( val.Item(period_string + "max"), 1e99, &rate.HasDemandCharge );
			rate.FlatDemandCharge[period][tier] = json_double( val.Item(period_string + "rate"), 0.0, &rate.HasDemandCharge );
			rate.FlatDemandAdj[period][tier] = json_double( val.Item(period_string + "adjustment"), 0.0, &rate.HasDemandCharge );
		}


	for (int period=0;period<12;period++)
		for (int tier=0; tier<6; tier++)
		{
			wxString period_string = wxString::Format("demandratestructure/period%d/tier%d", period+1, tier+1);

			rate.DemandMax[period][tier] = json_double( val.Item(period_string + "max"), 1e99, &rate.HasDemandCharge );
			rate.DemandCharge[period][tier] = json_double( val.Item(period_string + "rate"), 0.0, &rate.HasDemandCharge );
			rate.DemandAdj[period][tier] = json_double( val.Item(period_string + "adjustment"), 0.0, &rate.HasDemandCharge );
		}
		*/
	
	if (!RetrieveDiurnalData(val.Item("demandweekdayschedule"), rate.DemandWeekdaySchedule)) return false;
	if (!RetrieveDiurnalData(val.Item("demandweekendschedule"), rate.DemandWeekendSchedule)) return false;


	return true;
}



bool OpenEI::RetrieveDiurnalData(wxJSONValue &month_ary, double sched[12][24])
{
	wxJSONValue hour_ary;

	if (month_ary.IsArray())
	{
		if (month_ary.Size() != 12) return false;
		for (int m = 0; m < 12; m++)
		{
			hour_ary = month_ary[m];
			if (hour_ary.Size() != 24) return false;
			for (int h = 0; h < 24; h++)
			{
				sched[m][h] = hour_ary[h].AsInt() + 1;
			}
		}
	}
	return true;
}


#include <wx/busyinfo.h>
#include <wx/grid.h>
#include <wx/hyperlink.h>

#include <wex/metro.h>
#include <wex/exttext.h>



enum {
  ID_btnApply,
  ID_lblStatus,
  ID_hypOpenEILink,
  ID_txtRateDescription,
  ID_txtRateEndDate,
  ID_txtRateStartDate,
  ID_txtRateName,
  ID_lstRates,
  ID_lstUtilities,
  ID_btnClose,
  ID_txtUtilitySearch,
  ID_btnQueryAgain,
  ID_cboResCom };

BEGIN_EVENT_TABLE( OpenEIUtilityRateDialog, wxDialog )
	EVT_TIMER( wxID_ANY, OpenEIUtilityRateDialog::OnTimer )
	EVT_BUTTON( ID_btnQueryAgain, OpenEIUtilityRateDialog::OnEvent )
	EVT_COMBOBOX( ID_cboResCom, OpenEIUtilityRateDialog::OnEvent )
	EVT_LISTBOX( ID_lstUtilities, OpenEIUtilityRateDialog::OnEvent )
	EVT_LISTBOX( ID_lstRates, OpenEIUtilityRateDialog::OnEvent )
	EVT_TEXT( ID_txtUtilitySearch, OpenEIUtilityRateDialog::OnEvent )
	EVT_BUTTON( ID_btnApply, OpenEIUtilityRateDialog::OnCommand)
	EVT_BUTTON( ID_btnClose, OpenEIUtilityRateDialog::OnCommand)
	EVT_CLOSE( OpenEIUtilityRateDialog::OnClose )
END_EVENT_TABLE()

OpenEIUtilityRateDialog::OpenEIUtilityRateDialog(wxWindow *parent, const wxString &title, const wxString &market)
	 : wxDialog( parent, wxID_ANY, title, wxDefaultPosition, wxSize(800,600), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER)
{
	wxArrayString _data_cboResCom;
	_data_cboResCom.Add("All Schedules");
	_data_cboResCom.Add("Residential Only");
	_data_cboResCom.Add("Commercial Only");
	_data_cboResCom.Add("Lighting Only");
	cboResCom = new wxComboBox(this, ID_cboResCom, "All Schedules", wxPoint(513,6), wxSize(127,21), _data_cboResCom, wxCB_READONLY);

	int cbo_ndx=0;
	for (int i=0; i<(int)_data_cboResCom.Count(); i++)
		if (_data_cboResCom[i].First(market) != wxNOT_FOUND)
		{
			cbo_ndx = i;
			break;
		}
	cboResCom->SetSelection(cbo_ndx);


	btnQueryAgain = new wxButton(this, ID_btnQueryAgain, "Refresh", wxPoint(210,6), wxSize(65,21));

	txtUtilitySearch = new wxExtTextCtrl(this, ID_txtUtilitySearch, "", wxPoint(120,6), wxSize(88,21));
	txtUtilitySearch->SetForegroundColour( wxColour(0, 0, 0) );
	txtUtilitySearch->SetBackgroundColour( wxColour(255, 255, 255) );


	wxArrayString _data_lstUtilities;
	lstUtilities = new wxListBox(this, ID_lstUtilities, wxPoint(9,30), wxSize(266,450), _data_lstUtilities, wxLB_SINGLE|wxLB_HSCROLL);

	wxArrayString _data_lstRates;
	lstRates = new wxListBox(this, ID_lstRates, wxPoint(288,30), wxSize(353,144), _data_lstRates, wxLB_SINGLE|wxLB_HSCROLL);

	txtRateName = new wxExtTextCtrl(this, ID_txtRateName,"", wxPoint(375,201), wxSize(253,21));
	txtRateName->SetEditable( false );
	txtRateName->SetForegroundColour( wxColour(0, 0, 0) );
	txtRateName->SetBackgroundColour( wxColour(255, 255, 255) );

	txtRateStartDate = new wxExtTextCtrl(this, ID_txtRateStartDate, "", wxPoint(375,399), wxSize(253,21));
	txtRateStartDate->SetEditable( false );
	txtRateStartDate->SetForegroundColour( wxColour(0, 128, 192) );
	txtRateStartDate->SetBackgroundColour( wxColour(255, 255, 255) );

	txtRateEndDate = new wxExtTextCtrl(this, ID_txtRateEndDate, "", wxPoint(375,423), wxSize(253,21));
	txtRateEndDate->SetEditable( false );
	txtRateEndDate->SetForegroundColour( wxColour(0, 128, 192) );
	txtRateEndDate->SetBackgroundColour( wxColour(255, 255, 255) );

//	txtRateDescription = new wxTextCtrl(this, ID_txtRateDescription, "", wxPoint(375, 225), wxSize(252, 168), wxTE_MULTILINE | wxTE_DONTWRAP | wxTE_PROCESS_TAB);
	txtRateDescription = new wxTextCtrl(this, ID_txtRateDescription, "", wxPoint(375, 225), wxSize(252, 215), wxTE_MULTILINE | wxTE_DONTWRAP | wxTE_PROCESS_TAB);
	txtRateDescription->SetFont(wxFont(10, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false, "courier"));
	txtRateDescription->ChangeValue("");
	txtRateDescription->SetEditable( false );

	hypOpenEILink = new wxHyperlinkCtrl(this, ID_hypOpenEILink, "Go to rate page on OpenEI.org...", "http://en.openei.org/wiki/Gateway:Utilities", wxPoint(294, 450), wxSize(281, 21));
	hypJSONLink = new wxHyperlinkCtrl(this, ID_hypOpenEILink, "Rate JSON data page...", "http://en.openei.org/wiki/Gateway:Utilities", wxPoint(594, 450), wxSize(281, 21));

	lblStatus = new wxStaticText(this, ID_lblStatus, "", wxPoint(9,486), wxSize(302,21));
	
	btnApply = new wxButton(this, ID_btnApply, "Download and apply utility rate", wxPoint(318,486), wxSize(236,21));
	btnClose = new wxButton(this, ID_btnClose, "Close", wxPoint(558,486), wxSize(80,21));
	
	wxBoxSizer *sz_left_top = new wxBoxSizer( wxHORIZONTAL );
	sz_left_top->Add( new wxStaticText( this, wxID_ANY, "  Search:"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 4 );
	sz_left_top->Add( txtUtilitySearch, 1, wxALL|wxEXPAND, 4 );
	sz_left_top->Add( btnQueryAgain, 0, wxALL|wxEXPAND, 4 );

	wxBoxSizer *sz_left = new wxBoxSizer( wxVERTICAL );
	sz_left->Add( sz_left_top, 0, wxALL|wxEXPAND, 0 );
	sz_left->Add( lstUtilities, 1, wxALL|wxEXPAND, 0 );


	wxBoxSizer *sz_right_top = new wxBoxSizer( wxHORIZONTAL );
	sz_right_top->Add( new wxStaticText(this, wxID_ANY, "Available rate schedules"), 1, wxALL|wxALIGN_CENTER_VERTICAL, 3 );
	sz_right_top->Add( cboResCom, 0, wxALL|wxEXPAND, 3 );

	wxFlexGridSizer *sz_right_grid = new wxFlexGridSizer(2);
	sz_right_grid->AddGrowableCol(1);
	sz_right_grid->Add( new wxStaticText(this, wxID_ANY, "Name"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	sz_right_grid->Add( txtRateName, 1, wxALL|wxEXPAND, 2 );	
//	sz_right_grid->Add(new wxStaticText(this, wxID_ANY, "Description"), 0, wxALL | wxALIGN_CENTER_VERTICAL, 2);
	sz_right_grid->Add(new wxStaticText(this, wxID_ANY, "Applicability"), 0, wxALL | wxALIGN_CENTER_VERTICAL, 2);
	sz_right_grid->Add(txtRateDescription, 1, wxALL | wxEXPAND, 2);
	sz_right_grid->Add( new wxStaticText(this, wxID_ANY, "Start"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	sz_right_grid->Add( txtRateStartDate, 1, wxALL|wxEXPAND, 2 );	
	sz_right_grid->Add( new wxStaticText(this, wxID_ANY, "End"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	sz_right_grid->Add( txtRateEndDate, 1, wxALL|wxEXPAND, 2 );
	
	wxBoxSizer *sz_right = new wxBoxSizer(wxVERTICAL);
	sz_right->Add( sz_right_top, 0, wxALL|wxEXPAND );
	sz_right->Add( lstRates, 1, wxALL|wxEXPAND );
	sz_right->Add( sz_right_grid, 2, wxALL|wxEXPAND );
	sz_right->Add(hypOpenEILink, 0, wxALL | wxEXPAND);
	sz_right->Add(hypJSONLink, 0, wxALL | wxEXPAND);


	wxBoxSizer *sz_main = new wxBoxSizer(wxHORIZONTAL ); 
	sz_main->Add( sz_left, 2, wxALL|wxEXPAND, 4 );
	sz_main->Add( sz_right, 3, wxALL|wxEXPAND, 4 );
	
	wxBoxSizer *sz_bottom = new wxBoxSizer(wxHORIZONTAL );
	sz_bottom->Add( lblStatus, 1, wxALL|wxALIGN_CENTER_VERTICAL, 3 );
	sz_bottom->Add( btnApply, 0, wxALL|wxEXPAND, 4 );
	sz_bottom->Add( btnClose, 0, wxALL|wxEXPAND, 4 );

	wxBoxSizer *sz_top = new wxBoxSizer(wxVERTICAL);
	sz_top->Add( sz_main, 1, wxALL|wxEXPAND, 4 );
	sz_top->Add( sz_bottom, 0, wxALL|wxEXPAND, 4 );

	SetSizer( sz_top );
	
	//lblStatus->Hide();
	mTimer.SetOwner( this );
	mBusy = false;
}

void OpenEIUtilityRateDialog::StartHttp()
{
	lblStatus->SetLabel("Connecting to OpenEI...");
	lblStatus->Show();
	mTimer.Start( 300, true );
}

void OpenEIUtilityRateDialog::QueryUtilities()
{
	lblStatus->SetLabel("Loading utility companies...");
	wxString err;
	wxBusyInfo busy("Communicating with OpenEI.org... please wait", this);
	if (!api.QueryUtilityCompanies(mUtilityCompanies, &err))
	{
		wxMessageBox("Error:\n\n" + err);
		return;
	}

	txtUtilitySearch->SetValue(wxEmptyString);
	UpdateUtilityList();
	lblStatus->SetLabel("Ready.");
	txtUtilitySearch->SetFocus();
}

int OpenEIUtilityRateDialog::ShowModal()
{
	StartHttp();
	return wxDialog::ShowModal();
}

void OpenEIUtilityRateDialog::QueryRates(const wxString &utility_name)
{
	lblStatus->SetLabel("Loading rates for " + utility_name + "...");
	wxString err;
	//wxBusyInfo busy("Communicating with OpenEI.org... please wait", this);
	if (!api.QueryUtilityRates( utility_name, mUtilityRates, &err ))
	{
		wxMessageBox("Error:\n\n" + err);
		return;
	}

	if (mUtilityRates.size() == 0)
		lblStatus->SetLabel("No rates for " + utility_name);
	else
		lblStatus->SetLabel("Ready.");

	UpdateRateList();
}

void OpenEIUtilityRateDialog::UpdateUtilityList()
{
	
	lstUtilities->Freeze();
	lstUtilities->Clear();

	wxString filter = txtUtilitySearch->GetValue().Lower();
	if (filter.IsEmpty())
	{
		lstUtilities->Append( mUtilityCompanies );
	}
	else if (filter.Len() <= 2)
	{
		for (int i=0;i<(int)mUtilityCompanies.Count();i++)
		{
			if (mUtilityCompanies[i].Left( filter.Len() ).Lower() == filter)
				lstUtilities->Append( mUtilityCompanies[i] );
		}
	}
	else
	{
		for (int i = 0; i<(int)mUtilityCompanies.Count(); i++)
		{
			if (mUtilityCompanies[i].Lower().Find(filter) >= 0)
				lstUtilities->Append( mUtilityCompanies[i] );
		}
	}

	lstUtilities->Thaw();
}

void OpenEIUtilityRateDialog::UpdateRateList()
{
	lstRates->Freeze();
	lstRates->Clear();

	mGUIDList.Clear();
	for (int i = 0; i<(int)mUtilityRates.size(); i++)
	{
		if (cboResCom->GetSelection() == 1 && mUtilityRates[i].Sector.Lower() != "residential")
			continue;
	
		if (cboResCom->GetSelection() == 2 && mUtilityRates[i].Sector.Lower() != "commercial")
			continue;

		if (cboResCom->GetSelection() == 3 && mUtilityRates[i].Sector.Lower() != "lighting")
			continue;

		wxString rate = mUtilityRates[i].Sector + "-" + mUtilityRates[i].Name;
		lstRates->Append( rate );
		mGUIDList.Add( mUtilityRates[i].GUID );
	}

	lstRates->Thaw();
	UpdateRateData();
}

OpenEI::RateData OpenEIUtilityRateDialog::GetCurrentRateData()
{
	return mRateData;
}

void OpenEIUtilityRateDialog::UpdateRateData()
{
	int idx = lstRates->GetSelection();
	wxString guid;
	if (idx >= 0 && idx < (int)mGUIDList.Count())
		guid = mGUIDList[idx];

	wxString ssel = lstRates->GetStringSelection();

	if (guid.IsEmpty())
	{
		txtRateName->SetValue(wxEmptyString);
		txtRateDescription->SetValue(wxEmptyString);
		txtRateStartDate->SetValue(wxEmptyString);
		txtRateEndDate->SetValue(wxEmptyString);
//		hypOpenEILink->SetURL("http://en.openei.org/wiki/Gateway:Utilities");
		hypOpenEILink->SetURL("http://en.openei.org/wiki/Utility_Rate_Database");
	}
	else
	{
		mRateData.Reset();
	
		lblStatus->SetLabel("Retrieving rate data for " + ssel + "...");
		wxString json_url;
		wxBusyInfo busy("Communicating with OpenEI.org... please wait", this);
		if (api.RetrieveUtilityRateData(guid, mRateData, &json_url))
		{
			
			txtRateName->SetValue( mRateData.Header.Utility + ": " + mRateData.Header.Name );
			txtRateStartDate->SetValue( mRateData.StartDate );
			txtRateEndDate->SetValue( mRateData.EndDate );
			
			/*
			wxString desc = mRateData.Header.Description + "\n\n";

			desc += wxString::Format("Has Energy Charges? %s\n", mRateData.HasEnergyCharge?"yes":"no");
			desc += wxString::Format("Has Demand Charges? %s\n", mRateData.HasDemandCharge?"yes":"no");
			desc += wxString::Format("\nGUID: '%s'\n", mRateData.Header.GUID.c_str() );
			desc += wxString::Format("\nEnergy comments: '%s'\n", mRateData.Header.EnergyComments.c_str());
			desc += wxString::Format("\nDeamand comments: '%s'\n", mRateData.Header.DemandComments.c_str());
			*/
			wxString desc = "";
			desc += "Deamnd\n";
			desc += wxString::Format("\tMinimum %lg kW\n", mRateData.Applicability.peakkwcapacitymin);
			desc += wxString::Format("\tMaximum %lg kW\n", mRateData.Applicability.peakkwcapacitymax);
			desc += wxString::Format("\tHistory %lg months\n", mRateData.Applicability.peakkwcapacityhistory);
			desc += "Energy\n";
			desc += wxString::Format("\tMinimum %lg kWh\n", mRateData.Applicability.peakkwhusagemin);
			desc += wxString::Format("\tMaximum %lg kWh\n", mRateData.Applicability.peakkwhusagemax);
			desc += wxString::Format("\tHistory %lg months\n", mRateData.Applicability.peakkwhusagehistory);
			desc += "Service Voltage\n";
			desc += wxString::Format("\tMinimum %lg V\n", mRateData.Applicability.voltageminimum);
			desc += wxString::Format("\tMaximum %lg V\n", mRateData.Applicability.voltagemaximum);
			desc += "Character of Service\n";
			desc += wxString::Format("\tVoltage Category %s\n", mRateData.Applicability.voltagecategory.c_str());
			desc += wxString::Format("\tPhase Wiring %s\n", mRateData.Applicability.phasewiring.c_str());

			txtRateDescription->SetValue( desc );
			
			wxString rate_url = "http://en.openei.org/apps/USURDB/rate/view/" + guid;

			hypOpenEILink->SetURL(rate_url);
			hypJSONLink->SetURL(json_url);


			lblStatus->SetLabel("Ready.");
		}
		else
			lblStatus->SetLabel("Could not get rate data for " + ssel );

	}
}

void OpenEIUtilityRateDialog::OnTimer(wxTimerEvent &evt)
{
	mBusy = true;
	QueryUtilities();
	mBusy = false;
}

void OpenEIUtilityRateDialog::OnEvent(wxCommandEvent &evt)
{
	switch (evt.GetId())
	{
	case ID_cboResCom:
		UpdateRateList();
		break;
	case ID_lstUtilities:
		QueryRates( lstUtilities->GetStringSelection() );
		break;
	case ID_lstRates:
		UpdateRateData();
		break;
	case ID_btnQueryAgain:
		QueryUtilities();
		break;
	case ID_txtUtilitySearch:
		UpdateUtilityList();
		break;
	}
}

bool OpenEIUtilityRateDialog::IsBusy()
{
	return mBusy;
}

void OpenEIUtilityRateDialog::OnCommand( wxCommandEvent &evt )
{
	if (evt.GetId() == ID_btnClose)
	{
		if (IsBusy())
		{
			wxMessageBox("Busy processing information, please wait...");
			return;
		}

		EndModal(wxID_CANCEL);
	}
	else
	{
		OpenEI::RateData dat = GetCurrentRateData();
		if (dat.Header.GUID.IsEmpty())
		{
			wxMessageBox("No rate data selected.");
			return;
		}

		EndModal(wxID_OK);
	}
}

void OpenEIUtilityRateDialog::OnClose(wxCloseEvent &evt)
{
	if (IsBusy())
	{
		wxMessageBox("Busy processing information, please wait...");
		return;
	}

	EndModal(wxID_CANCEL);
}


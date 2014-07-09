#include <wx/tokenzr.h>

#include <wex/utils.h>
#include <wex/jsonreader.h>

#include "openeiapi.h"


static wxString MyGet(const wxString &url)
{
	return wxWebHttpGet(url, "Cache-Control", "no-cache");
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

	NetMetering=false;
	HasFlatRate=false;
	FlatRateBuy=0.0;
	FlatRateSell=0.0;
	FlatRateAdj=0.0;
	FixedMonthlyCharge=0.0;


	HasEnergyCharge=false;	
	EnergyRateUnit = "kWh";

	for (i=0;i<12;i++)
		for (j=0;j<6;j++)
		{
			EnergyBuy[i][j]=EnergyBuy[i][j]=EnergySell[i][j]=0.0;
			EnergyMax[i][j]=1e99;
		}

	for (i=0;i<288;i++)
		EnergyWeekdaySchedule[i]=EnergyWeekendSchedule[i]='1';

	EnergyWeekendSchedule[288]=EnergyWeekendSchedule[288]=0;

	HasDemandCharge = false;
	DemandRateUnit = "kW";
	DemandReactivePower = 1.0;
	
	for (i=0;i<12;i++)
		FlatDemandMonth[i]=0;

	for (i=0;i<12;i++)
		for (j=0;j<6;j++)
		{
			FlatDemandCharge[i][j]=FlatDemandAdj[i][j]=0.0;
			FlatDemandMax[i][j]=1e99;
			DemandCharge[i][j]=DemandAdj[i][j]=0.0;
			DemandMax[i][j]=1e99;
		}

	for (i=0;i<288;i++)
		DemandWeekdaySchedule[i]=DemandWeekendSchedule[i]='1';

	DemandWeekdaySchedule[288]=DemandWeekendSchedule[288]=0;

	
}

bool OpenEI::QueryUtilityCompanies(wxArrayString &names, wxString *err)
{

	//  based on emails from Paul and Jay Huggins 3/24/14
	wxString url = "http://en.openei.org/services/rest/utility_companies?version=2&format=json_plain&callback=callback";
	
	wxString json_data = wxWebHttpGet( url );
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
		names.Add( buf );
	}

	if (err) *err = wxEmptyString;

	return true;

}

bool OpenEI::QueryUtilityRates(const wxString &name, std::vector<RateInfo> &rates, wxString *err)
{
	wxString utlnm = name;
	utlnm.Replace("&", "%26");
	// production
	wxString url = "http://en.openei.org/services/rest/utility_rates?version=2&detail=basic&format=json_plain&ratesforutility=" + utlnm;
	
	wxString json_data = MyGet(url);
	if (json_data.IsEmpty())
	{
		if (err) *err = "Could not retrieve rate information for " + name;
		return false;
	}

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
		x.GUID = json_string(item_list[i].Item("label")).Mid(5);
		x.Name = json_string(item_list[i].Item("name"));
		x.Utility = json_string(item_list[i].Item("utility"));
		x.Sector = json_string(item_list[i].Item("sector"));
		x.Description = json_string(item_list[i].Item("description"));
		x.Source = json_string(item_list[i].Item("source"));
		x.Version = json_integer(item_list[i].Item("version"));
		rates.push_back(x);
	}

	if (err) *err = wxEmptyString;

	return true;
}

int OpenEI::UtilityCompanyRateCount(const wxString &name)
{
	// production
	wxString url = "http://en.openei.org/services/rest/utility_rates?version=2&limit=500&detail=minimal&format=json_plain&ratesforutility=" + name;
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
	wxString url = "http://en.openei.org/services/rest/utility_rates?version=2&format=json_plain&detail=full&getpage=Data:" + guid;

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

	rate.StartDate = json_string(val.Item("startdate"));
	rate.EndDate = json_string(val.Item("enddate"));

	rate.NetMetering = (json_string(val.Item("usenetmetering")).Lower() == "true");

	wxJSONValue v;
	
	rate.HasFlatRate = true; // if any of these items are missing, will change to 'false'
	rate.FlatRateBuy = json_double( val.Item("flatratebuy"), 0.0, &rate.HasFlatRate );
	rate.FlatRateSell = json_double( val.Item("flatratesell"), 0.0, &rate.HasFlatRate );
	rate.FlatRateAdj = json_double( val.Item("flatratefueladj"), 0.0, &rate.HasFlatRate );

	rate.FixedMonthlyCharge = json_double( val.Item("fixedmonthlycharge") );

	/// Energy Charge

	rate.EnergyRateUnit = json_string( val.Item("energyrateunit") );

	rate.HasEnergyCharge = true;

	for (int period=0;period<12;period++)
		for (int tier=0; tier<6; tier++)
		{
			wxString period_string = wxString::Format("energyratestructure/period%d/tier%d", period+1, tier+1);

			rate.EnergyMax[period][tier] = json_double( val.Item(period_string + "max"), 1e99, &rate.HasEnergyCharge );

			rate.EnergyBuy[period][tier] = json_double( val.Item(period_string + "rate"), 0.0, &rate.HasEnergyCharge );

			rate.EnergySell[period][tier] = json_double( val.Item(period_string + "sell"), 0.0, &rate.HasEnergyCharge );
			rate.EnergyAdj[period][tier] = json_double( val.Item(period_string + "adjustment"), 0.0, &rate.HasEnergyCharge );
		}

	wxString buf;

	buf = json_string(val.Item("energyweekdayschedule"), wxEmptyString, &rate.HasEnergyCharge);
	if (buf.Len() == 288) strcpy(rate.EnergyWeekdaySchedule, buf.c_str());

	buf = json_string(val.Item("energyweekendschedule"), wxEmptyString, &rate.HasEnergyCharge);
	if (buf.Len() == 288) strcpy(rate.EnergyWeekendSchedule, buf.c_str());

	/// DEMAND CHARGES
	rate.HasDemandCharge = true;

	rate.DemandRateUnit = json_string( val.Item("demandrateunit") );

	rate.DemandReactivePower = json_double( val.Item("demandreactivepowercharge") );

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
	
	buf = json_string(val.Item("demandweekdayschedule"), wxEmptyString, &rate.HasDemandCharge );
	if (buf.Len() == 288) strcpy(rate.DemandWeekdaySchedule, buf.c_str());

	buf = json_string(val.Item("demandweekendschedule"), wxEmptyString, &rate.HasDemandCharge );
	if (buf.Len() == 288) strcpy(rate.DemandWeekendSchedule, buf.c_str());



	return true;
}

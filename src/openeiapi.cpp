#include <wx/tokenzr.h>
#include <wx/log.h>

#include <wex/utils.h>

#include "openeiapi.h"
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
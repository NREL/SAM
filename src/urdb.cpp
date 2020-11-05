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

#include <wx/tokenzr.h>
#include <wx/log.h>

// for handling unix time in IURDB
#include <wx/datetime.h>
#include <time.h> 

#include <wex/easycurl.h>
#include <wex/utils.h>

#include "urdb.h"
#include "widgets.h"
#include "main.h"

static wxString MyGet(const wxString &url)
{
	wxEasyCurl curl;
	curl.AddHttpHeader( "Content-type: text/plain" );
	curl.AddHttpHeader( "Cache-Control: no-cache" );
	curl.Get( url );
	return curl.GetDataAsString();
}


wxString GetDate(const int &_time_t)
{
	if (_time_t <= 0)
		return "N/A";
	else
	{
		return wxDateTime((time_t)_time_t).FormatISODate();
	}
}

OpenEI::RateData::RateData()
{
	Reset();
}

void OpenEI::RateData::Reset()
{
	int i;

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

	DgRules="";
	MinCharge = 0.0;
	MinChargeUnits = "";

	FixedChargeFirstMeter = 0.0;
	FixedChargeAddlMeter = 0.0;
	FixedChargeUnits = "";

	DemandFlatStructure.resize_fill(12, 4, 0);

	for (i = 0; i < 12; i++)
	{
		FlatDemandMonth[i] = 0;
		DemandFlatStructure.at(i, 0) = i;
		DemandFlatStructure.at(i, 1) = 1;
		DemandFlatStructure.at(i, 2) = 1e+38;
		DemandFlatStructure.at(i, 3) = 0;
	}

	HasEnergyCharge=false;	

	EnergyStructure.resize_fill(1, 6, 0);
	// single default value
	EnergyStructure.at(0, 0) = 1;
	EnergyStructure.at(0, 1) = 1;
	EnergyStructure.at(0, 2) = 1e+38;
	EnergyStructure.at(0, 3) = 0;
	EnergyStructure.at(0, 4) = 0;
	EnergyStructure.at(0, 5) = 0;
	
	for (i = 0; i < 12; i++)
	{
		for (int k = 0; k < 24; k++)
		{
			EnergyWeekdaySchedule[i][k] = 1;
			EnergyWeekendSchedule[i][k] = 1;
		}
	}

	HasDemandCharge = false;
	DemandRateUnit = "kW"; // TODO update to handle different values
	DemandReactivePower = 1.0;
	
	DemandFlatStructure.resize_fill(12, 4, 0);

	for (i = 0; i < 12; i++)
	{
		FlatDemandMonth[i] = 0;
		DemandFlatStructure.at(i, 0) = i;
		DemandFlatStructure.at(i, 1) = 1;
		DemandFlatStructure.at(i, 2) = 1e+38;
		DemandFlatStructure.at(i, 3) = 0;
	}

	// data matrix does not allow for zero rows - example for user
	DemandTOUStructure.resize_fill(1, 4, 0);
	DemandTOUStructure.at(0, 0) = 1;
	DemandTOUStructure.at(0, 1) = 1;
	DemandTOUStructure.at(0, 2) = 1e+38;
	DemandTOUStructure.at(0, 3) = 0;
	for (i = 0; i < 12; i++)
	{
		for (int k = 0; k < 24; k++)
		{
			DemandWeekdaySchedule[i][k] = 1;
			DemandWeekendSchedule[i][k] = 1;
		}
	}

	// unused items

	Unused.IsDefault = false;

	for (i = 0; i < 12; i++)
	{
		Unused.FuelAdjustmentsMonthly[i] = 0.0;
		Unused.DemandRatchetPercentage[i] = 0.0;
	}

	Unused.ServiceType = "";
	Unused.DemandWindow = 0;

	Unused.CoincidentRateStructure.resize_fill(1, 4, 0);
	Unused.CoincidentRateStructure.at(0, 0) = 1;
	Unused.CoincidentRateStructure.at(0, 1) = 1;
	Unused.CoincidentRateStructure.at(0, 2) = 1e+38;
	Unused.CoincidentRateStructure.at(0, 3) = 0;

	for (i = 0; i < 12; i++)
	{
		for (int k = 0; k < 24; k++)
		{
			Unused.CoincidentSchedule[i][k] = 1;
		}
	}

	Unused.CoincidentRateUnit.Empty();
	
	Unused.DemandReactivePowerCharge = 0;

}

bool OpenEI::QueryUtilityCompanies(wxArrayString &names, wxString *err)
{

	// OpenEI service returns list of utility companies and their aliases https://openei.org/services/doc/rest/util_cos/?version=3
	wxString url = SamApp::WebApi("urdb_companies_all");
	url.Replace("<SCOPE>", "international");

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
//	wxJSONValue query = root.Item("query");
//	wxJSONValue item_list = query.Item("categorymembers");
	int count = item_list.Size();
	for (int i=0;i<count;i++)
	{
		wxString buf = item_list[i].Item("label").AsString();
//		wxString buf = item_list[i].Item("title").AsString();
		buf.Replace("&amp;", "&");
			names.Add( buf );
	}

	if (err) *err = wxEmptyString;
	return true;

}

bool OpenEI::QueryUtilityCompaniesbyZipcode(const wxString &zipcode, wxArrayString &names, wxString *err)
{

	// NREL Developer API to list utility companies by zip code https://developer.nrel.gov/docs/electricity/utility-rates-v3/
	wxString url = SamApp::WebApi("urdb_companies_by_zip");
	url.Replace("<ADDRESS>", zipcode);

	wxString json_data = MyGet(url);
	if (json_data.IsEmpty())
	{
		if (err) *err = "Could not retrieve JSON data for zip=" + zipcode + ".";
		return false;
	}

	wxJSONReader reader;
	wxJSONValue root;
	if (reader.Parse(json_data, &root) != 0)
	{
		if (err) *err = "Could not process returned JSON data for zip=" + zipcode + ".";
		return false;
	}

	wxJSONValue item_list = root.Item("outputs");
	// does not resolve to OpenEI names only EIA names
	//wxString buf = item_list.Item("utility_name").AsString();
	wxString company_id = item_list.Item("company_id").AsString();
	if (company_id.IsEmpty())
	{
		if (err) *err = "Could not process returned JSON companies for zip=" + zipcode + ".";
		return false;
	}

	// OpenEI ask query to return company name given EIAID https://openei.org/services/doc/ask/
	company_id.Replace("|", "%7C%7C"); // urlencode
	url = SamApp::WebApi("urdb_name");
	url.Replace("<EIAID>", company_id);

	json_data = MyGet(url);
	if (json_data.IsEmpty())
	{
		if (err) *err = "Could not retrieve JSON EIA name for zip=" + zipcode + ".";
		return false;
	}

	if (reader.Parse(json_data, &root) != 0)
	{
		if (err) *err = "Could not process returned JSON EIA name for zip=" + zipcode + ".";
		return false;
	}

	names.Clear();
	item_list = root["results"];
	wxArrayString list_name = item_list.GetMemberNames();
	// list_name[0] should be resolved name
	if (list_name.Count() > 0)
	{
		wxString urdbname = list_name[0];
		if (item_list[list_name[0] ].HasMember("fulltext"))
			urdbname = item_list[list_name[0] ]["fulltext"].AsString();
		urdbname.Replace("&amp;", "&");
		names.Add(urdbname);
	}

	if (err) *err = wxEmptyString;

	return true;

}

bool OpenEI::ResolveUtilityName(const wxString &name, wxString *urdb_name, wxString *err)
{

	wxString utlnm = name;
	utlnm.Replace("&", "%26");

	wxString url = SamApp::WebApi("urdb_name_from_eiaid");
	url.Replace("<EIAID>", utlnm);

	wxString json_data = MyGet(url);
	if (json_data.IsEmpty())
	{
		if (err) *err = "Could not retrieve rate information for " + name;
		return false;
	}

	wxJSONReader reader;
	//	reader.SetSkipStringDoubleQuotes(true);
	wxJSONValue root;
	if (reader.Parse(json_data, &root) != 0)
	{
		if (err) *err = "Could not process returned JSON data for utility rates for " + name;
		return false;
	}

	wxString urdbname = "";
	wxJSONValue item_list = root["results"];
	wxArrayString list_name = item_list.GetMemberNames();
	// list_name[0] should be resolved name
	if (list_name.Count() > 0)
	{
		urdbname = list_name[0];
		if (item_list[list_name[0]].HasMember("fulltext"))
			urdbname = item_list[list_name[0]]["fulltext"].AsString();
	}
	else
		urdbname = name;

	if (urdb_name) *urdb_name = urdbname;
	if (err) *err = wxEmptyString;

	return true;
}


bool OpenEI::QueryUtilityRates(const wxString &name, std::vector<RateInfo> &rates, wxString *err)
{
	wxString utlnm = name;
	utlnm.Replace("&", "%26");
	
	int max_limit = 500;
	int offset;
	int old_offset;
	int count;
	wxString url;
	wxString json_data;
	wxJSONReader reader;
	wxJSONValue root;
	wxJSONValue item_list;
	wxJSONValue items;

	// initial query to get first 500 rates
	// OpenEI International Utility Rate Database https://openei.org/services/doc/rest/util_rates/?version=7
	offset = 0;
	url = SamApp::WebApi("urdb_rates");
	url.Replace("<LIMIT>", wxString::Format("%d", max_limit));
	url.Replace("<DETAIL>", "minimal"); // don't need rate details for this call
	url.Replace("&offset=<OFFSET>", wxString::Format("&offset=%d", offset));
	url.Replace("<UTILITYNAME>", utlnm);
	url.Replace("<GUID>", "");
	url.Replace("<APIKEY>", wxString(sam_api_key));

	json_data = MyGet(url);
	if (json_data.IsEmpty())
	{
		if (err) *err = "Could not retrieve rate information for " + name + " " + url;
		return false;
	}

//	reader.SetSkipStringDoubleQuotes(true);

	if (reader.Parse( json_data, &root )!=0)
	{
		if (err) *err = "Could not process returned JSON data for utility rates for " + name;
		return false;
	}

	item_list = root.Item("items");

	rates.clear();
	// download additional rates, up to 500 at a time
	old_offset = offset;
	offset = max_limit + 1;
	count = item_list.Size();
	while (count != 0)
	{
		url.Replace(wxString::Format("&offset=%d",old_offset), wxString::Format("&offset=%d", offset));
		json_data = MyGet(url);
		reader.Parse(json_data, &root);
		items = root.Item("items");
		count = items.Size();
		for (int i = 0; i < count; i++)
		{
			item_list[i+offset ] = items[i];
		}
		old_offset = offset;
		offset += max_limit;
	}

	count = item_list.Size();
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
		x.StartDate = GetDate(json_integer(item_list[i].Item("startdate")));
		x.EndDate = GetDate(json_integer(item_list[i].Item("enddate")));
//		wxLogStatus("urdb startdate=" + x.StartDate);
//		wxLogStatus("urdb enddate=" + x.EndDate);
		rates.push_back(x);
	}

	if (err) *err = wxEmptyString;

	return true;
}
/*
int OpenEI::UtilityCompanyRateCount(const wxString &name)
{
	// production
	// rest service going away - update to api.openei.org per
	// https://en.openei.org/services/doc/rest/util_rates?version=3
	//	wxString url = "https://en.openei.org/services/rest/utility_rates?version=3&limit=500&detail=minimal&format=json_plain&ratesforutility=" + name;
	wxString url = "https://api.openei.org/utility_rates?version=3&limit=500&detail=minimal&format=json_plain&ratesforutility=" + name + "&api_key=" + wxString(sam_api_key);
	wxString json_data = MyGet(url);
	if (json_data.IsEmpty())
		return 0;

	wxJSONReader reader;
	wxJSONValue root;
	if (reader.Parse( json_data, &root )!=0)
		return 0;
	
	return root.Item("items").Size();
}
*/

bool OpenEI::RetrieveUtilityRateData(const wxString &guid, RateData &rate, wxString *json_url, wxString *err)
{
	// international rates
	//wxString url = "https://dev-api.openei.org/utility_rates?version=4&format=json&detail=full&getpage=" + guid + "&api_key=" + wxString(sam_api_key);

	// pushed to production update from Jay 10/2/15
	//wxString url = "https://api.openei.org/utility_rates?version=4&format=json&detail=full&getpage=" + guid + "&api_key=" + wxString(sam_api_key);

	wxString url = SamApp::WebApi("urdb_rates");
	url.Replace("<LIMIT>", "1");
	url.Replace("<DETAIL>", "full"); // don't need rate details for this call
	url.Replace("<OFFSET>", "");
	url.Replace("<UTILITYNAME>", "");
	url.Replace("<GUID>", guid);
	url.Replace("<APIKEY>", wxString(sam_api_key));

	if (json_url) *json_url = url;

	wxString json_data = MyGet(url);
	if (json_data.IsEmpty())
	{
		if (err) *err="Could not retrieve utility rate JSON data for " + guid;
		return false;
	}

	wxJSONReader reader;
//	reader.SetSkipStringDoubleQuotes(true);
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
	rate.Header.RateURL = SamApp::WebApi("urdb_view_rate") + "/rate/view/" + guid;

	rate.Header.StartDate = GetDate(json_integer(val.Item("startdate")));
	rate.Header.EndDate = GetDate(json_integer(val.Item("enddate")));

	// Metering Option
	rate.DgRules = json_string(val.Item("dgrules"));

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

	// Unused Items

	rate.Unused.HasUnusedItems = false;

	wxJSONValue isd = val.Item("is_default");
	if (isd.Size() > 0)
		rate.Unused.IsDefault = isd.AsBool();

	wxJSONValue st = val.Item("servicetype");
	if (st.Size() > 0)
		rate.Unused.ServiceType = st.AsString();

	wxJSONValue fam = val.Item("fueladjustmentsmonthly");
	if (fam.Size() > 0)
	{
		rate.Unused.HasUnusedItems = true;
		for (int i = 0; i < 12; i++)
			rate.Unused.FuelAdjustmentsMonthly[i] = fam[i].AsDouble();
	}

	wxJSONValue dw = val.Item("demandwindow");
	if (dw.Size() > 0)
	{
		rate.Unused.HasUnusedItems = true;
		rate.Unused.DemandWindow = dw.AsDouble();
	}

	wxJSONValue dmp = val.Item("demandratchetpercentage");
	if (dmp.Size() > 0)
	{
		rate.Unused.HasUnusedItems = true;
		for (int i = 0; i < 12; i++)
			rate.Unused.DemandRatchetPercentage[i] = dmp[i].AsDouble();
	}

	// energy, fixed, and demand attributes

	wxArrayString key;

	wxJSONValue ea = val.Item("energyattrs");
	wxString ea_str = "";
	if (ea.IsArray())
	{
		rate.Unused.HasUnusedItems = true;
		for (int i = 0; i < ea.Size(); i++)
		{
			key = ea[i].GetMemberNames();
			ea_str.Append(wxString::Format("%s, %s\n",key[0],ea[i].Item(key[0]).AsString()));
		}
		rate.Unused.EnergyAttrs = ea_str;
	}

	wxJSONValue fa = val.Item("fixedattrs");
	wxString fa_str = "";
	if (fa.IsArray())
	{
		rate.Unused.HasUnusedItems = true;
		for (int i = 0; i < fa.Size(); i++)
		{
			key = fa[i].GetMemberNames();
			ea_str.Append(wxString::Format("%s, %s\n", key[0], fa[i].Item(key[0]).AsString()));
		}
		rate.Unused.FixedAttrs = fa_str;
	}

	wxJSONValue da = val.Item("demandattrs");
	wxString da_str = "";
	if (da.IsArray())
	{
		rate.Unused.HasUnusedItems = true;
		for (int i = 0; i < da.Size(); i++)
		{
			key = da[i].GetMemberNames();
			da_str.Append(wxString::Format("%s, %s\n", key[0], da[i].Item(key[0]).AsString()));
		}
		rate.Unused.DemandAttrs = da_str;
	}

	rate.Unused.CoincidentRateUnit = json_string(val.Item("coincidentrateunit"));

	int num_cr_rows = 0;
	wxJSONValue cr_periods = val.Item("coincidentratestructure");
	if (cr_periods.IsArray())
	{
		for (int period = 0; period < cr_periods.Size(); period++)
		{
			wxJSONValue cr_tier = cr_periods[period];
			if (cr_tier.IsArray())
			{
				for (int tier = 0; tier < cr_tier.Size(); tier++)
				{
					num_cr_rows++;
				}
			}
		}
	}
	if (num_cr_rows > 0)
	{
		rate.Unused.HasCoincidentRate = true;
		rate.Unused.HasUnusedItems = true;
		rate.Unused.CoincidentRateStructure.resize_fill(num_cr_rows, 4, 0.0);
		int cr_row = 0;
		for (int period = 0; period < cr_periods.Size(); period++)
		{
			wxJSONValue cr_tier = cr_periods[period];
			for (int tier = 0; tier < cr_tier.Size(); tier++)
			{
				double max = json_double(cr_tier[tier].Item("max"), 1e38, &rate.Unused.HasCoincidentRate);
				double charge = json_double(cr_tier[tier].Item("rate"), 0.0, &rate.Unused.HasCoincidentRate);
				double adj = json_double(cr_tier[tier].Item("adj"), 0.0, &rate.Unused.HasCoincidentRate);
				rate.Unused.CoincidentRateStructure.at(cr_row, 0) = period + 1;
				rate.Unused.CoincidentRateStructure.at(cr_row, 1) = tier + 1;
				rate.Unused.CoincidentRateStructure.at(cr_row, 2) = max;
				rate.Unused.CoincidentRateStructure.at(cr_row, 3) = charge + adj;
				cr_row++;
			}
		}
	}

	if (!RetrieveDiurnalData(val.Item("coincidentschedule"), rate.Unused.CoincidentSchedule)) return false;

	// Minimum Charge
	rate.MinCharge = json_double(val.Item("mincharge"));
	rate.MinChargeUnits = json_string(val.Item("minchargeunits"));

	// Fixed Charge
	rate.FixedChargeFirstMeter = json_double(val.Item("fixedchargefirstmeter"));
	rate.FixedChargeAddlMeter =json_double(val.Item("fixedchargeeaaddl"));
	rate.FixedChargeUnits = json_string(val.Item("fixedchargeunits"));

	// Energy Charge
	rate.HasEnergyCharge = true;

	// first check for energy rate structure and resize matrix if present
	int num_ec_rows = 0; // default to one for each month
	wxJSONValue ers_periods = val.Item("energyratestructure");
	if (ers_periods.IsArray())
	{
		for (int period = 0; period < ers_periods.Size(); period++)
		{
			wxJSONValue ers_tier = ers_periods[period];
			if (ers_tier.IsArray())
			{
				for (int tier = 0; tier < ers_tier.Size(); tier++)
				{
					num_ec_rows++;
				}
			}
		}
	}

	if (num_ec_rows > 0)
	{
		rate.EnergyStructure.resize_fill(num_ec_rows, 6, 0.0);

	// next, assign rate values
		int es_row = 0;
		for (int period = 0; period < ers_periods.Size(); period++)
		{
			wxJSONValue ers_tier = ers_periods[period];
			for (int tier = 0; tier < ers_tier.Size(); tier++)
			{
				double max = json_double(ers_tier[tier].Item("max"), 1e38, &rate.HasEnergyCharge);
				double buy = json_double(ers_tier[tier].Item("rate"), 0.0, &rate.HasEnergyCharge);
				double sell = json_double(ers_tier[tier].Item("sell"), 0.0, &rate.HasEnergyCharge);
				double adj = json_double(ers_tier[tier].Item("adj"), 0.0, &rate.HasEnergyCharge);
				wxString units = json_string(ers_tier[tier].Item("unit"));
				int iunits = -1; // unsupported
				if (units.Lower() == "kwh")
					iunits = 0;
				else if (units.Lower() == "kwh/kw")
					iunits = 1;
				else if (units.Lower() == "kwh daily")
					iunits = 2;
				else if (units.Lower() == "kwh/kw daily")
					iunits = 3;
				rate.EnergyStructure.at(es_row, 0) = period + 1;
				rate.EnergyStructure.at(es_row, 1) = tier + 1;
				rate.EnergyStructure.at(es_row, 2) = max;
				rate.EnergyStructure.at(es_row, 3) = iunits;
				rate.EnergyStructure.at(es_row, 4) = buy+adj;
				rate.EnergyStructure.at(es_row, 5) = sell;
				es_row++;
			}
		}
	}

	if (!RetrieveDiurnalData(val.Item("energyweekdayschedule"), rate.EnergyWeekdaySchedule)) return false;
	if (!RetrieveDiurnalData(val.Item("energyweekendschedule"), rate.EnergyWeekendSchedule)) return false;
	
	// Demand Charge
	rate.HasDemandCharge = true;
	rate.DemandRateUnit = json_string( val.Item("demandrateunit") );
	rate.DemandReactivePower = json_double( val.Item("demandreactivepowercharge") );

	int num_months = 0;
	wxJSONValue fdm_periods = val.Item("flatdemandmonths");
	if (fdm_periods.IsArray())
	{
		// addresses issue from Pieter 6/26/15 for Upper Cumberland EMC GS3 rate with incorrect json - not an entry for every month.
		num_months = fdm_periods.Size();
		if (num_months != 12) return false;
		for (int month = 0; month <num_months; month++)
		{
			rate.FlatDemandMonth[month] = fdm_periods[month].AsInt();
		}
	}
	if (num_months == 12)
	{
		wxJSONValue fds_periods = val.Item("flatdemandstructure");
		int fds_row = 0;
		if (fds_periods.IsArray())
		{
			for (int period = 0; period < fds_periods.Size(); period++)
			{
				wxJSONValue fds_tier = fds_periods[period];
				if (fds_tier.IsArray())
				{
					for (int tier = 0; tier < fds_tier.Size(); tier++)
					{
						fds_row++;
					}
				}
			}
		}
		fds_row *= num_months; //estimate - may resize as below.
		if (fds_row > 0)
		{
			rate.DemandFlatStructure.resize_fill(fds_row, 4, 0.0);

			int fd_row = 0;
			for (int m = 0; m < num_months; m++)
			{
				int period = rate.FlatDemandMonth[m];
				if ( period >= 0 && period < fds_periods.Size())
				{
					wxJSONValue fds_tier = fds_periods[period];
					for (int tier = 0; tier < fds_tier.Size(); tier++)
					{
						double max = json_double(fds_tier[tier].Item("max"), 1e38, &rate.HasDemandCharge);
						double charge = json_double(fds_tier[tier].Item("rate"), 0.0, &rate.HasDemandCharge);
						double adj = json_double(fds_tier[tier].Item("adj"), 0.0, &rate.HasDemandCharge);

						rate.DemandFlatStructure.at(fd_row, 0) = m;
						rate.DemandFlatStructure.at(fd_row, 1) = tier + 1;
						rate.DemandFlatStructure.at(fd_row, 2) = max;
						rate.DemandFlatStructure.at(fd_row, 3) = charge + adj;
						fd_row++;
					}
				}
			}
			rate.DemandFlatStructure.resize_preserve(fd_row, 4, 0.0);
		}
	}


	// first check for energy rate structure and resize matrix if present
	int num_dc_rows = 0; // default to one for each month
	wxJSONValue drs_periods = val.Item("demandratestructure");
	if (drs_periods.IsArray())
	{
		for (int period = 0; period < drs_periods.Size(); period++)
		{
			wxJSONValue drs_tier = drs_periods[period];
			if (drs_tier.IsArray())
			{
				for (int tier = 0; tier < drs_tier.Size(); tier++)
				{
					num_dc_rows++;
				}
			}
		}
	}
	if (num_dc_rows > 0)
	{
		rate.DemandTOUStructure.resize_fill(num_dc_rows, 4, 0.0);

		// next, assign rate values
		int ds_row = 0;
		for (int period = 0; period < drs_periods.Size(); period++)
		{
			wxJSONValue drs_tier = drs_periods[period];
			for (int tier = 0; tier < drs_tier.Size(); tier++)
			{
				double max = json_double(drs_tier[tier].Item("max"), 1e38, &rate.HasDemandCharge);

				double charge = json_double(drs_tier[tier].Item("rate"), 0.0, &rate.HasDemandCharge);
				double adj = json_double(drs_tier[tier].Item("adj"), 0.0, &rate.HasDemandCharge);

				rate.DemandTOUStructure.at(ds_row, 0) = period + 1;
				rate.DemandTOUStructure.at(ds_row, 1) = tier + 1;
				rate.DemandTOUStructure.at(ds_row, 2) = max;
				rate.DemandTOUStructure.at(ds_row, 3) = charge + adj;
				ds_row++;
			}
		}
	}
	
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
  ID_lblActiveRateCount,
  ID_lblRateStatus,
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
  ID_cboResCom, 
  ID_chkActiveOnly,
  ID_btnQueryZipCode
};

BEGIN_EVENT_TABLE( OpenEIUtilityRateDialog, wxDialog )
	EVT_TIMER( wxID_ANY, OpenEIUtilityRateDialog::OnTimer )
	EVT_BUTTON(ID_btnQueryAgain, OpenEIUtilityRateDialog::OnEvent)
	EVT_BUTTON(ID_btnQueryZipCode, OpenEIUtilityRateDialog::OnEvent)
	EVT_CHOICE(ID_cboResCom, OpenEIUtilityRateDialog::OnEvent)
	EVT_LISTBOX( ID_lstUtilities, OpenEIUtilityRateDialog::OnEvent )
	EVT_LISTBOX(ID_lstRates, OpenEIUtilityRateDialog::OnEvent)
	EVT_CHECKBOX(ID_chkActiveOnly, OpenEIUtilityRateDialog::OnEvent)
	EVT_TEXT(ID_txtUtilitySearch, OpenEIUtilityRateDialog::OnEvent)
	EVT_BUTTON( ID_btnApply, OpenEIUtilityRateDialog::OnCommand)
	EVT_BUTTON( ID_btnClose, OpenEIUtilityRateDialog::OnCommand)
	EVT_CLOSE( OpenEIUtilityRateDialog::OnClose )
END_EVENT_TABLE()

OpenEIUtilityRateDialog::OpenEIUtilityRateDialog(wxWindow *parent, const wxString &title, const wxString &market)
	 : wxDialog( parent, wxID_ANY, title, wxDefaultPosition, wxScaleSize(800,600), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER)
{

	cboResCom = new wxChoice(this, ID_cboResCom);
	cboResCom->Append("All");
	cboResCom->Append("Residential");
	cboResCom->Append("Commercial");
	cboResCom->Append("Lighting");

	int cbo_ndx=0;
	for (int i=0; i<(int)cboResCom->GetCount(); i++)
	{
		if (cboResCom->GetString(i).First(market) != wxNOT_FOUND)
		{
			cbo_ndx = i;
			break;
		}
	}
	cboResCom->SetSelection(cbo_ndx);

	chkActiveOnly = new wxCheckBox(this, ID_chkActiveOnly, "Show active");
	chkActiveOnly->SetValue(true);

	btnQueryAgain = new wxButton(this, ID_btnQueryAgain, "Show all");
	lblUtilityCount = new wxStaticText(this, ID_lblStatus, "");

	lstUtilities = new AFSearchListBox(this, ID_lstUtilities);

	lstRates = new AFSearchListBox(this, ID_lstRates);
	lblRateStatus = new wxStaticText(this, ID_lblRateStatus, "Select a utility to show available rates.");

	txtRateName = new wxExtTextCtrl(this, ID_txtRateName);
	txtRateName->SetEditable( false );
	txtRateName->SetForegroundColour( wxColour(0, 0, 0) );
	txtRateName->SetBackgroundColour( wxColour(255, 255, 255) );

	txtRateStartDate = new wxExtTextCtrl(this, ID_txtRateStartDate);
	txtRateStartDate->SetEditable(false);
	txtRateStartDate->SetForegroundColour(wxColour(0, 0, 0));
	txtRateStartDate->SetBackgroundColour(wxColour(255, 255, 255));

	txtRateEndDate = new wxExtTextCtrl(this, ID_txtRateEndDate);
	txtRateEndDate->SetEditable(false);
	txtRateEndDate->SetForegroundColour(wxColour(0, 0, 0));
	txtRateEndDate->SetBackgroundColour(wxColour(255, 255, 255));

	txtRateGUID = new wxExtTextCtrl(this, ID_txtRateEndDate);
	txtRateGUID->SetEditable(false);
	txtRateGUID->SetForegroundColour(wxColour(0, 0, 0));
	txtRateGUID->SetBackgroundColour(wxColour(255, 255, 255));

	txtRateDescription = new wxTextCtrl(this, ID_txtRateDescription, "", wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE | wxTE_WORDWRAP | wxTE_PROCESS_TAB | wxTE_READONLY );

	hypOpenEILink = new wxHyperlinkCtrl(this, ID_hypOpenEILink, "View rate on OpenEI website...", "https://en.openei.org/wiki/Utility_Rate_Database");
	hypJSONLink = new wxHyperlinkCtrl(this, ID_hypOpenEILink, "Get rate as JSON...", "https://en.openei.org/wiki/Utility_Rate_Database");

	lblStatus = new wxStaticText(this, ID_lblStatus, "");
	
	btnApply = new wxButton(this, ID_btnApply, "Download and apply utility rate");
	btnClose = new wxButton(this, ID_btnClose, "Cancel");

	txtZipCode = new wxExtTextCtrl(this);
	btnQueryZipCode = new wxButton(this, ID_btnQueryZipCode, "Search by zip code");

	wxBoxSizer *sz_zipcode = new wxBoxSizer(wxHORIZONTAL);
	sz_zipcode->Add(new wxStaticText(this, wxID_ANY, "Zip code:"), 0, wxALL | wxALIGN_CENTER_VERTICAL, 2);
	sz_zipcode->Add(txtZipCode, 0, wxALL | wxALIGN_CENTER_VERTICAL, 2);
	sz_zipcode->AddStretchSpacer();
	sz_zipcode->Add(btnQueryZipCode, 0, wxALL, 4);

	wxBoxSizer *sz_utilitites = new wxBoxSizer(wxHORIZONTAL);
	sz_utilitites->Add(btnQueryAgain, 0, wxALL, 4);
	sz_utilitites->Add(lblUtilityCount, 1, wxALL | wxALIGN_CENTER_VERTICAL, 2);

	wxBoxSizer *sz_left = new wxBoxSizer( wxVERTICAL );
	sz_left->Add(sz_zipcode);
	sz_left->Add( lstUtilities, 1, wxALL|wxEXPAND, 0 );
	sz_left->Add( sz_utilitites);

	wxFlexGridSizer *sz_right_grid = new wxFlexGridSizer(2);
	sz_right_grid->AddGrowableCol(1);
	sz_right_grid->Add( new wxStaticText(this, wxID_ANY, "Name"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	sz_right_grid->Add( txtRateName, 1, wxALL|wxEXPAND, 2 );	
	sz_right_grid->Add(new wxStaticText(this, wxID_ANY, "Description"), 0, wxALL | wxALIGN_CENTER_VERTICAL, 2);
	sz_right_grid->Add(txtRateDescription, 1, wxALL | wxEXPAND, 2);
	sz_right_grid->Add( new wxStaticText(this, wxID_ANY, "Start"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	sz_right_grid->Add( txtRateStartDate, 1, wxALL|wxEXPAND, 2 );	
	sz_right_grid->Add(new wxStaticText(this, wxID_ANY, "End"), 0, wxALL | wxALIGN_CENTER_VERTICAL, 2);
	sz_right_grid->Add(txtRateEndDate, 1, wxALL | wxEXPAND, 2);
	sz_right_grid->Add(new wxStaticText(this, wxID_ANY, "GUID"), 0, wxALL | wxALIGN_CENTER_VERTICAL, 2);
	sz_right_grid->Add(txtRateGUID, 1, wxALL | wxEXPAND, 2);

	wxBoxSizer *sz_select_rates = new wxBoxSizer(wxHORIZONTAL);
	sz_select_rates->Add(cboResCom, 0, wxRIGHT, 10 );
	sz_select_rates->Add(chkActiveOnly, 2, wxEXPAND, 0);

	wxBoxSizer *sz_right = new wxBoxSizer(wxVERTICAL);
	sz_right->Add(lblRateStatus, 0, wxALL | wxEXPAND);
	sz_right->Add(sz_select_rates, 0, wxALL|wxEXPAND, 3);
	sz_right->Add(lstRates, wxEXPAND, wxALL | wxEXPAND);
	sz_right->Add(sz_right_grid, 2, wxALL|wxEXPAND, 3 );
	sz_right->Add(hypOpenEILink, 0, wxALL | wxEXPAND);
	sz_right->Add(hypJSONLink, 0, wxALL | wxEXPAND);

	wxBoxSizer *sz_main = new wxBoxSizer(wxHORIZONTAL ); 
	sz_main->Add( sz_left, 2, wxALL|wxEXPAND, 4 );
	sz_main->Add( sz_right, 3, wxALL|wxEXPAND, 4 );
	
	wxBoxSizer *sz_bottom = new wxBoxSizer(wxHORIZONTAL);
	sz_bottom->Add( lblStatus, 3, wxEXPAND, 0 );
	sz_bottom->Add( btnApply, 0, wxALL, 4 );
	sz_bottom->Add( btnClose, 0, wxALL, 4 );

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
	//lblStatus->Show();
	mTimer.Start( 300, true );
}

void OpenEIUtilityRateDialog::QueryUtilities()
{
	lblStatus->SetLabel("Loading utility companies...");
	wxString err;
	wxBusyInfo busy("Getting list of utility companies...", this);
	if (!api.QueryUtilityCompanies(mUtilityCompanies, &err))
	{
		busy.~wxBusyInfo();
		lstUtilities->Clear();
		wxMessageBox("Error:\n\n" + err);
		return;
	}

	lstUtilities->Freeze();
	lstUtilities->Clear();
	lstUtilities->Append(mUtilityCompanies);
	lstUtilities->Thaw();

	lblStatus->SetLabel("Ready.");
	lblUtilityCount->SetLabel(wxString::Format("%d utilities", (int)lstUtilities->Count()));
	lstUtilities->SetFocus();
}

void OpenEIUtilityRateDialog::QueryUtilitiesByZipCode()
{
	lblStatus->SetLabel("Loading utility companies...");
	wxString err;
	wxBusyInfo busy("Getting list of utility companies...", this);
	wxString zip_code = txtZipCode->GetValue();
	if (!api.QueryUtilityCompaniesbyZipcode(zip_code, mUtilityCompanies, &err))
	{
		busy.~wxBusyInfo();
		lstUtilities->Clear();
		wxMessageBox("Error:\n\n" + err);
		return;
	}

	lstUtilities->Freeze();
	lstUtilities->Clear();
	lstUtilities->Append(mUtilityCompanies);
	lstUtilities->Thaw();

	lblStatus->SetLabel("Ready.");
	lblUtilityCount->SetLabel(wxString::Format("%d utilities", (int)lstUtilities->Count()));
	lstUtilities->SetFocus();
}

int OpenEIUtilityRateDialog::ShowModal()
{
	StartHttp();
	return wxDialog::ShowModal();
}

void OpenEIUtilityRateDialog::QueryRates(const wxString &utility_name)
{
	wxBusyInfo busy("Getting available rates...", this);
	lblStatus->SetLabel(wxString::Format("Loading rates for %s...", utility_name));
	wxString err;

	wxString urdb_utility_name = "";
	// first resolve aliases
	if (!api.ResolveUtilityName(utility_name, &urdb_utility_name, &err))
	{
		urdb_utility_name = utility_name;
	}

	if (urdb_utility_name == "")
		urdb_utility_name = utility_name;

	// get any rates
	if (!api.QueryUtilityRates(urdb_utility_name, mUtilityRates, &err))
	{
		busy.~wxBusyInfo();
		wxMessageBox("Error:\n\n" + err);
		return;
	}

	lblRateStatus->SetLabel(wxString::Format("%d rates available for %s", (int)mUtilityRates.size(), urdb_utility_name));

	if (mUtilityRates.size() == 0)
		lblStatus->SetLabel("No rates for " + utility_name);
	else
		lblStatus->SetLabel("Ready.");

	UpdateRateList();
}

void OpenEIUtilityRateDialog::UpdateRateList()
{
	lstRates->Freeze();
	lstRates->Clear();

	mGUIDList.Clear();
	for (int i = 0; i < (int)mUtilityRates.size(); i++)
	{
		if (chkActiveOnly->GetValue() && mUtilityRates[i].EndDate != "N/A")
			continue;

		if (cboResCom->GetSelection() == 1 && mUtilityRates[i].Sector.Lower() != "residential")
			continue;

		if (cboResCom->GetSelection() == 2 && mUtilityRates[i].Sector.Lower() != "commercial")
			continue;

		if (cboResCom->GetSelection() == 3 && mUtilityRates[i].Sector.Lower() != "lighting")
			continue;

		wxString rate = mUtilityRates[i].Name + "  (" + mUtilityRates[i].GUID + ")";
		lstRates->Append(rate);
		mGUIDList.Add(mUtilityRates[i].GUID);
	}

	if ( chkActiveOnly->GetValue() )
		chkActiveOnly->SetLabel(wxString::Format("Show active (%d active rates)", (int)lstRates->Count()));
	else
		chkActiveOnly->SetLabel(wxString::Format("Show active"));

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
	wxLogStatus(wxString::Format("selection=%d", idx));
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
		txtRateGUID->SetValue(wxEmptyString);
		hypOpenEILink->SetURL(SamApp::WebApi("urdb_view_rate"));
	}
	else
	{
		mRateData.Reset();
	
		lblStatus->SetLabel("Retrieving rate data for " + ssel + "...");
		wxString json_url;
		wxBusyInfo busy("Getting rate data...", this);
		if (api.RetrieveUtilityRateData(guid, mRateData, &json_url))
		{
			
			txtRateName->SetValue(mRateData.Header.Name);
			txtRateStartDate->SetValue( mRateData.Header.StartDate );
			txtRateEndDate->SetValue(mRateData.Header.EndDate);
			txtRateGUID->SetValue(mRateData.Header.GUID);

			wxString desc = mRateData.Header.Description;
			
			txtRateDescription->SetValue( desc );
			wxString rate_url = SamApp::WebApi("urdb_view_rate") + "/rate/view/" + guid;

			hypOpenEILink->SetURL(rate_url);
			hypJSONLink->SetURL(json_url);

			lblStatus->SetLabel("Ready.");
		}
		else
			lblStatus->SetLabel("Could not get rate data for " + ssel );

	}
}

void OpenEIUtilityRateDialog::OnTimer(wxTimerEvent &)
{
	mBusy = true;
	QueryUtilities();
	mBusy = false;
}

void OpenEIUtilityRateDialog::OnEvent(wxCommandEvent &evt)
{
	switch (evt.GetId())
	{
	case ID_chkActiveOnly:
	case ID_cboResCom:
		UpdateRateList();
		break;
	case ID_lstUtilities:
		QueryRates( lstUtilities->GetStringSelection() );
		lblUtilityCount->SetLabel(wxString::Format("%d utilities", (int)lstUtilities->Count()));
		break;
	case ID_lstRates:
		UpdateRateData();
		break;
	case ID_btnQueryAgain:
		txtZipCode->Clear();
		QueryUtilities();
		break;
	case ID_btnQueryZipCode:
		QueryUtilitiesByZipCode();
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

void OpenEIUtilityRateDialog::OnClose(wxCloseEvent &)
{
	if (IsBusy())
	{
		wxMessageBox("Busy processing information, please wait...");
		return;
	}

	EndModal(wxID_CANCEL);
}


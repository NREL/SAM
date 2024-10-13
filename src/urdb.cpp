/*
BSD 3-Clause License

Copyright (c) Alliance for Sustainable Energy, LLC. See also https://github.com/NREL/SAM/blob/develop/LICENSE
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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
    curl.Get(url);
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
	EnergyStructure.at(0, 0) = 1; // period
	EnergyStructure.at(0, 1) = 1; // tier
	EnergyStructure.at(0, 2) = 1e+38; // max usage
	EnergyStructure.at(0, 3) = 0; // max usage units
	EnergyStructure.at(0, 4) = 0; // buy rate (skip adjustments)
	EnergyStructure.at(0, 5) = 0; // sell rate
	
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

    LookbackPercent = 0.0;
    LookbackRange = 0;
    for (i = 0; i < 12; i++) {
        LookbackMonths[i] = true;
    }
   

	// unused items

	Unused.IsDefault = false;

	for (i = 0; i < 12; i++)
	{
		Unused.FuelAdjustmentsMonthly[i] = 0.0;
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
	
	Unused.DemandReactivePowerCharge = 0.0;

}

bool OpenEI::QueryUtilityCompanies(wxArrayString &names, wxString *err)
{

	// OpenEI service returns list of utility companies and their aliases https://openei.org/services/doc/rest/util_cos/?version=3
	wxString url = SamApp::WebApi("urdb_companies_all");
	url.Replace("<SCOPE>", "international");

	wxString json_data = MyGet(url);
	if (json_data.IsEmpty() )
	{
		if (err) *err = "Could not retrieve JSON data for utility rate companies.";
		return false;
	}

	rapidjson::Document reader;
	reader.Parse(json_data.c_str());

	if (reader.HasParseError())
	{
		if (err) *err = wxString::Format("Could not process returned JSON data for utility rate companies.\nRapidJSON ParseErrorCode = %d", reader.GetParseError());
		return false;
	}

    if ( reader.HasMember("error") )
    {
		if (reader["error"].HasMember("message")) {
			wxString str = reader["error"]["message"].GetString();
			if (err) *err = "URDB API error: " + str;
		}
        return false;
    }

	names.Clear();

	if (reader.HasMember("items")) {

		if (reader["items"].IsArray()) {

			auto item_list = reader["items"].GetArray();

			int count = item_list.Size();
			for (int i = 0; i < count; i++)	{
				if (item_list[i].HasMember("label")) {
					wxString buf = item_list[i]["label"].GetString();
					buf.Replace("&amp;", "&");
					names.Add(buf);
				}
			}

			if (err) *err = wxEmptyString;
			return true;
		}
		else
			return false;
	}
	else
		return false;

}

bool OpenEI::QueryUtilityCompaniesbyZipcode(const wxString &zipcode, wxArrayString &names, wxString *err)
{

	// NREL Developer API to list utility companies by zip code https://developer.nrel.gov/docs/electricity/utility-rates-v3/
	wxString url = SamApp::WebApi("urdb_companies_by_zip");
	url.Replace("<ADDRESS>", zipcode);

	wxString json_data = MyGet(url);
	if (json_data.IsEmpty())
	{
		if (err) *err = "Web API call to urdb_companies_by_zip returned empty JSON for zip code = " + zipcode + ".";
		return false;
	}

//	wxJSONReader reader;
//	wxJSONValue root;

	rapidjson::Document reader;
	reader.Parse(json_data.c_str());

	if (reader.HasParseError())
	{
		if (err) *err = wxString::Format("Could not parse JSON for zipcode = %s.\nRapidJSON ParseErrorCode = %d", zipcode, reader.GetParseError());;
		return false;
	}

	wxString company_id = wxEmptyString;

	if (reader.HasMember("outputs")) {
		//wxJSONValue item_list = root.Item("outputs");
		// does not resolve to OpenEI names only EIA names
		//wxString buf = item_list.Item("utility_name").AsString();
		// EIAID number for utility company
		// some zip codes return company_id with multiple EIAIDs separated by single pipes
		if (reader["outputs"].HasMember("company_id"))
			company_id = reader["outputs"]["company_id"].GetString();
	}
	if (company_id.IsEmpty())
	{
		if (err) *err = "JSON returned by web API call to urdb_companies_by_zip returned empty company_id for zip code =" + zipcode + ".";
		return false;
	}

	// OpenEI ask query to return company name given EIAID https://openei.org/services/doc/ask/
    url = SamApp::WebApi("urdb_ask");
    url.Replace("<QUESTION>", "[[Category:Utility+Companies]][[EiaUtilityId::"+company_id+"]]");
    url.Replace("<PROPERTIES>", "?EiaUtilityId");

    url.Replace("|", "||"); // double pipe to separate multiple EIAIDs in ask query
    
    json_data = MyGet(url);
	if (json_data.IsEmpty())
	{
		if (err) *err = "URDB ask for query by zip returned empty JSON for EIAID = " + company_id + ".";
		return false;
	}

	reader.Parse(json_data.c_str());
	if (reader.HasParseError())
	{
		if (err) *err = wxString::Format("URDB ask for query by zip: Failed to parse JSON for EIAID = %s\nRapidJSON ParseErrorCode = %d", company_id, reader.GetParseError());
		return false;
	}

	names.Clear();

	if (reader.HasMember("results")) {
		if (reader["results"].IsObject()) {
			const auto& item_list = reader["results"].GetObject();
			//wxArrayString list_name = item_list.GetMemberNames();
			if (item_list.MemberCount() > 0)	{
				for (size_t i = 0; i < item_list.MemberCount(); i++) {
					wxString urdbname = (item_list.begin() + i)->name.GetString(); 
					if ((item_list.begin() + i)->value.HasMember("fulltext"))
						urdbname = (item_list.begin() + i)->value["fulltext"].GetString();
					urdbname.Replace("&amp;", "&");
					names.Add(urdbname);
				}
			}

			if (err) *err = wxEmptyString;

			return true;
		}
		else
			return false;
	}
	else
		return false;

}


bool OpenEI::QueryUtilityRates(const wxString &name, std::vector<RateInfo> &rates, wxString *err)
{
	rates.clear(); // reset rates list
	
	wxString utlnm = name;
	utlnm.Replace("&", "%26");
	
	size_t max_limit = 500;
	size_t offset;
	size_t old_offset;
	size_t count;
	wxString url;
	wxString json_data;


	// initial query to get first 500 rates
	// OpenEI International Utility Rate Database https://openei.org/services/doc/rest/util_rates/?version=7
	offset = 0;
	url = SamApp::WebApi("urdb_rates");
	url.Replace("<LIMIT>", wxString::Format("%d", (int)max_limit));
	url.Replace("<DETAIL>", "minimal"); // don't need rate details for this call
	url.Replace("&offset=<OFFSET>", wxString::Format("&offset=%d", (int)offset));
	url.Replace("<UTILITYNAME>", utlnm);
	url.Replace("<GUID>", "");
	url.Replace("<APIKEY>", wxString(sam_api_key));



	// change from UTF8 to UTF16 encoding to address unicode characters per SAM GitHub issue 1848
	rapidjson::GenericDocument < rapidjson::UTF16<> > reader;
	bool retrieve_rates = true;
	bool ret = true;
	// up to maxlimit rates added to rates vector

	while (retrieve_rates && ret) {

		json_data = MyGet(url);
		if (json_data.IsEmpty()) {
			if (err) *err = "Web API call to urdb_rates returned empty JSON for utility company name = " + name; // do not report url because it has private API key
			ret = false;
		}
		else {
			reader.Parse(json_data.c_str());

			if (reader.HasParseError())	{
				if (err) *err = wxString::Format("Could not parse JSON for utility company name = %s.\nRapidJSON ParseErrorCode = %d", name, reader.GetParseError());
				ret = false;
			} 
			else if (!reader.HasMember(L"items")) {
				if (err) *err = "No rates found for utility company name = " + name;
				ret = false;
			}
			else if (!reader[L"items"].IsArray()) {
				if (err) *err = "No rates found for utility company name = " + name;
				ret = false;
			}
			else {
				auto item_list = reader[L"items"].GetArray();

				count = item_list.Size();

				if (count == 0) {
					if (err) *err = "No rates found for utility company name = " + name;
					ret = false;
				}
				else {
					// add to rates vector
					for (int i = 0; i < count; i++) {
						RateInfo x;
						x.GUID = item_list[i][L"label"].GetString();
						x.Name = item_list[i][L"name"].GetString();
						x.Utility = item_list[i][L"utility"].GetString();
						// optional
						if (item_list[i].HasMember(L"sector"))
							x.Sector = item_list[i][L"sector"].GetString();
						// optional
						if (item_list[i].HasMember(L"description"))
							x.Description = item_list[i][L"description"].GetString();
						// optional
						if (item_list[i].HasMember(L"source"))
							x.Source = item_list[i][L"source"].GetString();
						// optional
						if (item_list[i].HasMember(L"version"))
							x.Version = item_list[i][L"version"].GetInt();
						x.uri = item_list[i][L"uri"].GetString();
						// optional
						if (item_list[i].HasMember(L"startdate"))
							x.StartDate = GetDate(item_list[i][L"startdate"].GetInt());
						// optional
						if (item_list[i].HasMember(L"enddate"))
							x.EndDate = GetDate(item_list[i][L"enddate"].GetInt());
						else
							x.EndDate = "N/A";
						rates.push_back(x);
					}
					// update offset and url
					old_offset = offset;
					offset += max_limit;

					url.Replace(wxString::Format("&offset=%d", (int)old_offset), wxString::Format("&offset=%d", (int)offset));
					retrieve_rates = (count == max_limit); // more rates to be returned
				}
			}
		}
	}
	return ret;
}

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
		if (err) *err="Web API call to urdb_rates returned empty JSON for GUID = " + guid;
		return false;
	}


	rapidjson::Document reader;
	reader.Parse(json_data.c_str());

	if (reader.HasParseError())
	{
		if (err) *err = wxString::Format("Could not parse JSON for GUID = %s.\nRapidJSON ParseErrorCode = %d", guid, reader.GetParseError());
		return false;
	}

//	wxJSONValue val = root.Item("items").ItemAt(0);
	auto& val = reader["items"][0];
	if (val.IsNull())
	{
		if (err) *err = "Root JSON structure error: Items is null for GUID = " + guid;
		return false;
	}

	rate.Reset();
	
	/*
					// optional
				if (item_list[i].HasMember("description"))
					x.Description = item_list[i]["description"].GetString();
				// optional
				if (item_list[i].HasMember("source"))
					x.Source = item_list[i]["source"].GetString();
				// optional
				if (item_list[i].HasMember("version"))
					x.Version = item_list[i]["version"].GetInt();
				x.uri = item_list[i]["uri"].GetString();
				// optional
				if (item_list[i].HasMember("startdate"))
					x.StartDate = GetDate(item_list[i]["startdate"].GetInt());
				// optional
				if (item_list[i].HasMember("enddate"))
					x.EndDate = GetDate(item_list[i]["enddate"].GetInt());
				else
					x.EndDate = "N/A";

	
	*/



	rate.Header.GUID = guid;
	rate.Header.Name =  val["name"].GetString();
	rate.Header.Utility =  val["utility"].GetString();
	rate.Header.Sector =  val["sector"].GetString();
	// optional
	if (val.HasMember("description"))
		rate.Header.Description = val["description"].GetString();
	// optional
	if (val.HasMember("source"))
		rate.Header.Source = val["source"].GetString();
	// optional
	if (val.HasMember("version"))
		rate.Header.Version = val["version"].GetInt();
	// optional
	if (val.HasMember("energycomments"))
		rate.Header.EnergyComments = val["energycomments"].GetString();
	// optional
	if (val.HasMember("demandcomments"))
		rate.Header.DemandComments = val["demandcomments"].GetString();
	// optional
	if (val.HasMember("basicinformationcomments"))
		rate.Header.BasicInformationComments = val["basicinformationcomments"].GetString();
	rate.Header.JSONURL = url;
	rate.Header.RateURL = SamApp::WebApi("urdb_view_rate") + "/rate/view/" + guid;

	// optional
	if (val.HasMember("startdate"))
		rate.Header.StartDate = GetDate(val["startdate"].GetInt());
	// optional
	if (val.HasMember("enddate"))
		rate.Header.EndDate = GetDate(val["enddate"].GetInt());
	else
		rate.Header.EndDate = "N/A";
	// Metering Option
	// optional
	if (val.HasMember("dgrules"))
		rate.DgRules = val["dgrules"].GetString();

	// Applicability
	// optional
	if (val.HasMember("peakkwcapacityhistory"))
		rate.Applicability.peakkwcapacityhistory = val["peakkwcapacityhistory"].GetDouble();
	// optional
	if (val.HasMember("peakkwcapacitymax"))
		rate.Applicability.peakkwcapacitymax = val["peakkwcapacitymax"].GetDouble();
	// optional
	if (val.HasMember("peakkwcapacitymin"))
		rate.Applicability.peakkwcapacitymin = val["peakkwcapacitymin"].GetDouble();
	// optional
	if (val.HasMember("peakkwhusagehistory"))
		rate.Applicability.peakkwhusagehistory = val["peakkwhusagehistory"].GetDouble();
	// optional
	if (val.HasMember("peakkwhusagemax"))
		rate.Applicability.peakkwhusagemax = val["peakkwhusagemax"].GetDouble();
	// optional
	if (val.HasMember("peakkwhusagemin"))
		rate.Applicability.peakkwhusagemin = val["peakkwhusagemin"].GetDouble();
	// optional
	if (val.HasMember("voltagemaximum"))
		rate.Applicability.voltagemaximum = val["voltagemaximum"].GetDouble();
	// optional
	if (val.HasMember("voltageminimum"))
		rate.Applicability.voltageminimum = val["voltageminimum"].GetDouble();
	// optional
	if (val.HasMember("voltagecategory"))
		rate.Applicability.voltagecategory = val["voltagecategory"].GetString();
	// optional
	if (val.HasMember("phasewiring"))
		rate.Applicability.phasewiring = val["phasewiring"].GetString();

	// Unused Items, set HasUnusedItems to true for items that would affect bill calculation

	rate.Unused.HasUnusedItems = false;


//	wxJSONValue isd = val.Item("is_default");
//    if ( isd.IsBool() )
	if (val.HasMember("is_default"))
		if (val["is_default"].IsBool())
			rate.Unused.IsDefault = val["is_default"].GetBool();

//	wxJSONValue st = val.Item("servicetype");
//    if ( st.IsString())
	if (val.HasMember("servicetype"))
		if (val["servicetype"].IsString())
		    rate.Unused.ServiceType = val["servicetype"].GetString();

//	wxJSONValue fam = val.Item("fueladjustmentsmonthly");
//	if (fam.Size() > 0)
//	{
	if (val.HasMember("fueladjustmentsmonthly")) {
		if (val["fueladjustmentsmonthly"].IsArray()) {
			auto fam = val["fueladjustmentsmonthly"].GetArray();
			rate.Unused.HasUnusedItems = true;
			for (int i = 0; i < 12; i++)
				rate.Unused.FuelAdjustmentsMonthly[i] = fam[i].GetDouble();
		}
	}

//	wxJSONValue dw = val.Item("demandwindow");
//	if (dw.IsDouble())
	if (val.HasMember("demandwindow")) {
		if (val["demandwindow"].IsNumber()) {
			rate.Unused.HasUnusedItems = true;
			rate.Unused.DemandWindow = val["demandwindow"].GetDouble();
		}
	}

//   wxJSONValue drpc = val.Item("demandreactivepowercharge");
//   if (drpc.IsDouble() )
	if (val.HasMember("demandreactivepowercharge")) {
		if (val["demandreactivepowercharge"].IsNumber()) {
			rate.Unused.HasUnusedItems = true;
			rate.Unused.DemandReactivePowerCharge = val["demandreactivepowercharge"].GetDouble();
		}
	}

	// energy, fixed, and demand attributes

//	wxArrayString key;

	if (val.HasMember("energyattrs")) {
		auto& ea = val["energyattrs"];
		wxString ea_str = "";
		if (ea.IsArray()){
			rate.Unused.HasUnusedItems = true;
			for (size_t i = 0; i < ea.Size(); i++)	{
//				key = ea[i].GetMemberNames();
//				ea_str.Append(wxString::Format("%s, %s\n", key[0], ea[i].Item(key[0]).AsString()));
				if (ea[i].IsObject()) {
					auto obj = ea[i].GetObject();
					wxString str1 = obj.begin()->name.GetString();
					wxString str2 = obj.begin()->value.GetString();
					ea_str.Append(wxString::Format("%s, %s\n", str1.c_str(), str2.c_str()));
				}
			}
			rate.Unused.EnergyAttrs = ea_str;
		}
	}
//	wxJSONValue fa = val.Item("fixedattrs");
	if (val.HasMember("fixedattrs")) {
		auto& fa = val["fixedattrs"];
		wxString fa_str = "";
		if (fa.IsArray())		{
			rate.Unused.HasUnusedItems = true;
			for (size_t i = 0; i < fa.Size(); i++)	{
//				key = fa[i].GetMemberNames();
//				fa_str.Append(wxString::Format("%s, %s\n", key[0], fa[i].Item(key[0]).AsString()));
				if (fa[i].IsObject()) {
					auto obj = fa[i].GetObject();
					wxString str1 = obj.begin()->name.GetString();
					wxString str2 = obj.begin()->value.GetString();
					fa_str.Append(wxString::Format("%s, %s\n", str1.c_str(), str2.c_str()));
				}
			}
			rate.Unused.FixedAttrs = fa_str;
		}
	}
	/*
	failing for demandattr
	{"items":[{"label":"539f74d4ec4f024411ed0cf3","uri":"https:\/\/apps.openei.org\/IURDB\/rate\/view\/539f74d4ec4f024411ed0cf3","sector":"Commercial","energyweekendschedule":[[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]],"flatdemandunit":"kW","eiaid":97,"demandattrs":[{"Interuptible Demand Charge per kW":"$1.50\/kw"},{"Transformation Charge":"$0.50\/kVA"}],"flatdemandmonths":[0,0,0,0,0,0,0,0,0,0,0,0],"revisions":[1349714258,1349714484,1354885044,1403771140,1405948276,1427405193],"energyratestructure":[[{"rate":0.075,"unit":"kWh"}]],"flatdemandstructure":[[{"rate":11}]],"startdate":1301641200,"demandunits":"kW","demandcomments":"$0.50 kVA charge can be voided if customer owns their own transformer","fixedchargeunits":"$\/month","utility":"Adams Electric Coop","description":"Available to cooperative members requiring three phase electric service and a monthly average demand greater than two hundred kilowatts.\r\n\r\nTransformation charge is $0.50\/KVA installed transformer capacity.","name":"Rate Schedule LLFI (Low Load Factor Interruptible)","source":"Rate Binder #7 (Illinois State University)","approved":true,"country":"USA","fixedchargefirstmeter":90,"energyweekdayschedule":[[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]}]}
	
	*/


//	wxJSONValue da = val.Item("demandattrs");
	if (val.HasMember("demandattrs")) {
		auto& da = val["demandattrs"];
		wxString da_str = "";
		if (da.IsArray())		{
			rate.Unused.HasUnusedItems = true;
			for (size_t i = 0; i < da.Size(); i++)			{
//				key = da[i].GetMemberNames();
//				da_str.Append(wxString::Format("%s, %s\n", key[0], da[i].Item(key[0]).AsString()));
				if (da[i].IsObject()) {
					auto obj = da[i].GetObject();
					wxString str1 = obj.begin()->name.GetString();
					wxString str2 = obj.begin()->value.GetString();
					da_str.Append(wxString::Format("%s, %s\n", str1.c_str(), str2.c_str()));
				}
			}
			rate.Unused.DemandAttrs = da_str;
		}
	}

	if (val.HasMember("coincidentrateunit"))
		rate.Unused.CoincidentRateUnit = val["coincidentrateunit"].GetString();


	if (val.HasMember("coincidentratestructure")) {
		int num_cr_rows = 0;
		auto& cr_periods = val["coincidentratestructure"];
		if (cr_periods.IsArray()){
			for (size_t period = 0; period < cr_periods.Size(); period++)	{
				auto& cr_tier = cr_periods[period];
				if (cr_tier.IsArray())	{
					for (size_t tier = 0; tier < cr_tier.Size(); tier++){
						num_cr_rows++;
					}
				}
			}
		}
		if (num_cr_rows > 0){
			rate.Unused.HasCoincidentRate = true;
			rate.Unused.HasUnusedItems = true;
			rate.Unused.CoincidentRateStructure.resize_fill(num_cr_rows, 4, 0.0);
			int cr_row = 0;
			for (size_t period = 0; period < cr_periods.Size(); period++)	{
				auto& cr_tier = cr_periods[period];
				for (size_t tier = 0; tier < cr_tier.Size(); tier++)	{
//					double max = json_double(cr_tier[tier].Item("max"), 1e38, &rate.Unused.HasCoincidentRate);
//					double charge = json_double(cr_tier[tier].Item("rate"), 0.0, &rate.Unused.HasCoincidentRate);
//					double adj = json_double(cr_tier[tier].Item("adj"), 0.0, &rate.Unused.HasCoincidentRate);
					double max = 1e38;
					if (cr_tier[tier].HasMember("max"))
						if (cr_tier[tier]["max"].IsNumber())
							max = cr_tier[tier]["max"].GetDouble();
					double charge = 0.0;
					if (cr_tier[tier].HasMember("rate"))
						if (cr_tier[tier]["rate"].IsNumber())
							charge = cr_tier[tier]["rate"].GetDouble();
					double adj =  0.0;
					if (cr_tier[tier].HasMember("adj"))
						if (cr_tier[tier]["adj"].IsNumber())
							adj = cr_tier[tier]["adj"].GetDouble();
					rate.Unused.CoincidentRateStructure.at(cr_row, 0) = period + 1;
					rate.Unused.CoincidentRateStructure.at(cr_row, 1) = tier + 1;
					rate.Unused.CoincidentRateStructure.at(cr_row, 2) = max;
					rate.Unused.CoincidentRateStructure.at(cr_row, 3) = charge + adj;
					cr_row++;
				}
			}
		}
	}

	if (val.HasMember("coincidentschedule"))
		if (!RetrieveDiurnalData(val["coincidentschedule"], rate.Unused.CoincidentSchedule)) return false;

	// Minimum Charge
	if (val.HasMember("mincharge"))
		rate.MinCharge = val["mincharge"].GetDouble();
	if (val.HasMember("minchargeunits"))
		rate.MinChargeUnits = val["minchargeunits"].GetString();
    if (rate.MinChargeUnits == "$/day")
        rate.Unused.HasUnusedItems = true;

	// Fixed Charge
	if (val.HasMember("fixedchargefirstmeter"))
		rate.FixedChargeFirstMeter = val["fixedchargefirstmeter"].GetDouble();
	if (val.HasMember("fixedchargeeaaddl"))
		rate.FixedChargeAddlMeter = val["fixedchargeeaaddl"].GetDouble();
	if (val.HasMember("fixedchargeunits"))
		rate.FixedChargeUnits = val["fixedchargeunits"].GetString();
    if (rate.FixedChargeUnits != "$/month")
        rate.Unused.HasUnusedItems = true;

	// Energy Charge
	rate.HasEnergyCharge = true;

	// first check for energy rate structure and resize matrix if present
	if (val.HasMember("energyratestructure")) {
		int num_ec_rows = 0; // default to one for each month
		//	wxJSONValue ers_periods = val.Item("energyratestructure");
		auto& ers_periods = val["energyratestructure"];
		if (ers_periods.IsArray())	{
			for (size_t period = 0; period < ers_periods.Size(); period++)	{
				auto& ers_tier = ers_periods[period];
				if (ers_tier.IsArray())	{
					for (size_t tier = 0; tier < ers_tier.Size(); tier++)	{
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
			for (size_t period = 0; period < ers_periods.Size(); period++)	{
				auto& ers_tier = ers_periods[period];
				for (size_t tier = 0; tier < ers_tier.Size(); tier++) {
//					double max = json_double(ers_tier[tier].Item("max"), 1e38, &rate.HasEnergyCharge);
					double max = 1e38;
					if (ers_tier[tier].HasMember("max"))
						if (ers_tier[tier]["max"].IsNumber()) // some max values are stored as integers
							max = ers_tier[tier]["max"].GetDouble();
					double buy = 0.0;
					if (ers_tier[tier].HasMember("rate"))
						if (ers_tier[tier]["rate"].IsNumber())
							buy = ers_tier[tier]["rate"].GetDouble();
					double sell = 0.0;
					if (ers_tier[tier].HasMember("sell"))
						if (ers_tier[tier]["sell"].IsNumber())
							sell = ers_tier[tier]["sell"].GetDouble();
					double adj = 0.0;
					if (ers_tier[tier].HasMember("adj"))
						if (ers_tier[tier]["adj"].IsNumber())
							adj = ers_tier[tier]["adj"].GetDouble();
					wxString units;
					if (ers_tier[tier].HasMember("unit"))
						units = ers_tier[tier]["unit"].GetString();
					rate.EnergyUnits = "";
					int iunits = 0; // assume kWh if units not provided
					if (units.Lower() == "kwh")
						iunits = 0;
					else if (units.Lower() == "kwh/kw")
						iunits = 1;
					else if (units.Lower() == "kwh daily")
						iunits = 2;
					else if (units.Lower() == "kwh/kw daily")
						iunits = 3;
					else {
						rate.Unused.HasUnusedItems = true;
						rate.EnergyUnits = units;
					}
					rate.EnergyStructure.at(es_row, 0) = period + 1;
					rate.EnergyStructure.at(es_row, 1) = tier + 1;
					rate.EnergyStructure.at(es_row, 2) = max;
					rate.EnergyStructure.at(es_row, 3) = iunits;
					rate.EnergyStructure.at(es_row, 4) = buy + adj;
					rate.EnergyStructure.at(es_row, 5) = sell;
					es_row++;
				}
			}
		}

		if (!RetrieveDiurnalData(val["energyweekdayschedule"], rate.EnergyWeekdaySchedule)) return false;
		if (!RetrieveDiurnalData(val["energyweekendschedule"], rate.EnergyWeekendSchedule)) return false;
	}

	// Demand Charge
	rate.HasDemandCharge = true;
	if (val.HasMember("demandrateunit"))
		rate.DemandRateUnit = val["demandrateunit"].GetString();


	if (val.HasMember("lookbackpercent")) {
		auto& lp = val["lookbackpercent"];
		if (lp.IsNumber()) {
			rate.LookbackPercent = lp.GetDouble() * 100.0;
		}
		else if (lp.IsInt()) {
			rate.LookbackPercent = lp.GetInt() * 100.0;
		}
	}

	if (val.HasMember("lookbackrange")) {
		auto& lr = val["lookbackrange"];
		if (lr.IsInt())
			rate.LookbackRange = lr.GetInt();
	}

	if (val.HasMember("lookbackmonths")) {
		auto& lm = val["lookbackmonths"];
		if (lm.IsArray()) {
			for (size_t i = 0; i < lm.Size(); i++) {
				rate.LookbackMonths[i] = lm[i].GetBool();
			}
		}
	}

	int num_months = 0;
	if (val.HasMember("flatdemandmonths")) {
		auto& fdm_periods = val["flatdemandmonths"];
		if (fdm_periods.IsArray()) {
			// addresses issue from Pieter 6/26/15 for Upper Cumberland EMC GS3 rate with incorrect json - not an entry for every month.
			num_months = fdm_periods.Size();
			if (num_months != 12) return false;
			for (int month = 0; month < num_months; month++) {
				rate.FlatDemandMonth[month] = fdm_periods[month].GetInt();
			}
		}
	}
	if (num_months == 12) {
		if (val.HasMember("flatdemandmonths")) {
			auto& fds_periods = val["flatdemandstructure"];
			int fds_row = 0;
			if (fds_periods.IsArray()) {
				for (size_t period = 0; period < fds_periods.Size(); period++) {
					auto& fds_tier = fds_periods[period];
					if (fds_tier.IsArray()) {
						for (size_t tier = 0; tier < fds_tier.Size(); tier++) {
							fds_row++;
						}
					}
				}
			}
			fds_row *= num_months; //estimate - may resize as below.
			if (fds_row > 0) {
				rate.DemandFlatStructure.resize_fill(fds_row, 4, 0.0);

				int fd_row = 0;
				for (size_t m = 0; m < num_months; m++) {
					int period = rate.FlatDemandMonth[m];
					if (period >= 0 && period < (int)fds_periods.Size())
					{
						auto& fds_tier = fds_periods[period];
						for (size_t tier = 0; tier < fds_tier.Size(); tier++)
						{
							//						double max = json_double(fds_tier[tier].Item("max"), 1e38, &rate.HasDemandCharge);
							//						double charge = json_double(fds_tier[tier].Item("rate"), 0.0, &rate.HasDemandCharge);
							//						double adj = json_double(fds_tier[tier].Item("adj"), 0.0, &rate.HasDemandCharge);
							double max = 1e38;
							if (fds_tier[tier].HasMember("max"))
								if (fds_tier[tier]["max"].IsNumber())
									max = fds_tier[tier]["max"].GetDouble();
							double charge = 0.0;
							if (fds_tier[tier].HasMember("rate"))
								if (fds_tier[tier]["rate"].IsNumber())
									charge = fds_tier[tier]["rate"].GetDouble();
							double adj = 0.0;
							if (fds_tier[tier].HasMember("adj"))
								if (fds_tier[tier]["adj"].IsNumber())
									adj = fds_tier[tier]["adj"].GetDouble();
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
	}


	// first check for energy rate structure and resize matrix if present
	int num_dc_rows = 0; // default to one for each month
	if (val.HasMember("demandratestructure")) {
		auto& drs_periods = val["demandratestructure"];
		if (drs_periods.IsArray()) {
			for (size_t period = 0; period < drs_periods.Size(); period++) {
				auto& drs_tier = drs_periods[period];
				if (drs_tier.IsArray()) {
					for (size_t tier = 0; tier < drs_tier.Size(); tier++) {
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
			for (size_t period = 0; period < drs_periods.Size(); period++) {
				auto& drs_tier = drs_periods[period];
				for (size_t tier = 0; tier < drs_tier.Size(); tier++) {
					//				double max = json_double(drs_tier[tier].Item("max"), 1e38, &rate.HasDemandCharge);
					//				double charge = json_double(drs_tier[tier].Item("rate"), 0.0, &rate.HasDemandCharge);
					//				double adj = json_double(drs_tier[tier].Item("adj"), 0.0, &rate.HasDemandCharge);
					double max = 1e38;
					if (drs_tier[tier].HasMember("max"))
						if (drs_tier[tier]["max"].IsNumber())
							max = drs_tier[tier]["max"].GetDouble();
					double charge = 0.0;
					if (drs_tier[tier].HasMember("rate"))
						if (drs_tier[tier]["rate"].IsNumber())
							charge = drs_tier[tier]["rate"].GetDouble();
					double adj = 0.0;
					if (drs_tier[tier].HasMember("adj"))
						if (drs_tier[tier]["adj"].IsNumber())
							adj = drs_tier[tier]["adj"].GetDouble();

					rate.DemandTOUStructure.at(ds_row, 0) = period + 1;
					rate.DemandTOUStructure.at(ds_row, 1) = tier + 1;
					rate.DemandTOUStructure.at(ds_row, 2) = max;
					rate.DemandTOUStructure.at(ds_row, 3) = charge + adj;
					ds_row++;
				}
			}
		}

		if (!RetrieveDiurnalData(val["demandweekdayschedule"], rate.DemandWeekdaySchedule)) return false;
		if (!RetrieveDiurnalData(val["demandweekendschedule"], rate.DemandWeekendSchedule)) return false;
	}
	return true;
}



bool OpenEI::RetrieveDiurnalData(rapidjson::Value &month_ary, double sched[12][24])
{
	rapidjson::Value hour_ary;

	if (month_ary.IsArray())
	{
		if (month_ary.Size() != 12) return false;
		for (int m = 0; m < 12; m++)
		{
			hour_ary = month_ary[m];
			if (hour_ary.Size() != 24) return false;
			for (int h = 0; h < 24; h++)
			{
				sched[m][h] = hour_ary[h].GetInt() + 1;
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
  ID_txtRateUtility,
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

    txtZipCode = new wxExtTextCtrl(this);
    btnQueryZipCode = new wxButton(this, ID_btnQueryZipCode, "Search by zip code");

    lstUtilities = new AFSearchListBox(this, ID_lstUtilities);

    btnQueryAgain = new wxButton(this, ID_btnQueryAgain, "Show all");
    lblUtilityCount = new wxStaticText(this, ID_lblStatus, "");
    lblStatus = new wxStaticText(this, ID_lblStatus, "");

    lblRateStatus = new wxStaticText(this, ID_lblRateStatus, "Select a utility to show available rates.");

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

	lstRates = new AFSearchListBox(this, ID_lstRates);

    txtRateUtility = new wxExtTextCtrl(this, ID_txtRateUtility);
    txtRateUtility->SetEditable(false);
    txtRateUtility->SetForegroundColour(wxColour(0, 0, 0));
    txtRateUtility->SetBackgroundColour(wxColour(255, 255, 255));

	txtRateName = new wxExtTextCtrl(this, ID_txtRateName);
	txtRateName->SetEditable( false );
	txtRateName->SetForegroundColour( wxColour(0, 0, 0) );
	txtRateName->SetBackgroundColour( wxColour(255, 255, 255) );
    txtRateName->SetEditable(false);

    txtRateDescription = new wxTextCtrl(this, ID_txtRateDescription, "", wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE | wxTE_WORDWRAP | wxTE_PROCESS_TAB | wxTE_READONLY);

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

	hypOpenEILink = new wxHyperlinkCtrl(this, ID_hypOpenEILink, "View rate on OpenEI website...", SamApp::WebApi("urdb_wiki"));
	hypJSONLink = new wxHyperlinkCtrl(this, ID_hypOpenEILink, "Get rate as JSON...", SamApp::WebApi("urdb_wiki"));

	btnApply = new wxButton(this, ID_btnApply, "Download and apply utility rate");
	btnClose = new wxButton(this, ID_btnClose, "Cancel");

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
    sz_right_grid->Add(new wxStaticText(this, wxID_ANY, "Utility"), 0, wxALL | wxALIGN_CENTER_VERTICAL, 2);
    sz_right_grid->Add(txtRateUtility, 1, wxALL | wxEXPAND, 2);
    sz_right_grid->Add( new wxStaticText(this, wxID_ANY, "Name"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	sz_right_grid->Add( txtRateName, 1, wxALL|wxEXPAND, 2 );	
	sz_right_grid->Add(new wxStaticText(this, wxID_ANY, "Description"), 0, wxALL | wxALIGN_CENTER_VERTICAL, 2);
	sz_right_grid->Add(txtRateDescription, 1, wxALL | wxEXPAND, 2);
	sz_right_grid->Add( new wxStaticText(this, wxID_ANY, "Start"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	sz_right_grid->Add(txtRateStartDate, 1, wxALL|wxEXPAND, 2 );	
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

    this->Layout();

	//lblStatus->Hide();
	mTimer.SetOwner( this );
	mBusy = false;
}

void OpenEIUtilityRateDialog::StartHttp()
{
	lblStatus->SetLabel("Connecting to OpenEI...");
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
		wxMessageBox("Error Getting List of Utility Companies\n\n" + err, "URDB Download Message");
		return;
	}

	lstUtilities->Freeze();
	lstUtilities->Clear();
	lstUtilities->Append(mUtilityCompanies);
	lstUtilities->Thaw();

	lblStatus->SetLabel("Ready.");
	lblUtilityCount->SetLabel(wxString::Format("%d utilities", (int)lstUtilities->Count()));

}

void OpenEIUtilityRateDialog::QueryUtilitiesByZipCode()
{
	lblStatus->SetLabel("Loading companies...");
	wxString err;
	wxBusyInfo busy("Getting list of companies...", this);
	wxString zip_code = txtZipCode->GetValue();
	if (!api.QueryUtilityCompaniesbyZipcode(zip_code, mUtilityCompanies, &err))
	{
		busy.~wxBusyInfo();
		lstUtilities->Clear();
		wxMessageBox("Query by Zipcode Error\n\n" + err, "URDB Download Message");
		return;
	}

	lstUtilities->Freeze();
	lstUtilities->Clear();
	lstUtilities->Append(mUtilityCompanies);
	lstUtilities->Thaw();

	lblStatus->SetLabel("Ready.");
	lblUtilityCount->SetLabel(wxString::Format("%d companies", (int)lstUtilities->Count()));
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
	lblStatus->SetLabel("Loading rates...");

	mUtilityRates.clear();
	wxString err;

    // get rates
    if (!api.QueryUtilityRates(utility_name, mUtilityRates, &err))
    {
		busy.~wxBusyInfo();
		wxMessageBox("Utility Rate Query Error\n\n" + err, "URDB Download Message");
		return;
	}

    lblRateStatus->SetLabel(wxString::Format("%d rates available for %s", (int)mUtilityRates.size(), utility_name));

	if (mUtilityRates.size() == 0)
		lblStatus->SetLabel(wxString::Format("No rates for %s.", utility_name));
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
        txtRateUtility->SetValue(wxEmptyString);
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
	
		lblStatus->SetLabel("Retrieving rate data...");
		wxString json_url;
		wxBusyInfo busy("Getting rate data...", this);
		if (api.RetrieveUtilityRateData(guid, mRateData, &json_url))
		{

            txtRateUtility->SetValue(mRateData.Header.Utility);
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
			lblStatus->SetLabel(wxString::Format("Could not get rate data for %s.",ssel));

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


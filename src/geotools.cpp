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

#include <wx/wx.h>

#include <wex/easycurl.h>

#include <rapidjson/reader.h>

#include "main.h"
#include "geotools.h"

static wxString MyGet(const wxString& url)
{
    wxEasyCurl curl;
    curl.AddHttpHeader("Content-type: text/plain");
    curl.AddHttpHeader("Cache-Control: no-cache");
    curl.Get(url);
    return curl.GetDataAsString();
}

// Geocode using Google API for non-NREL builds of SAM
bool GeoTools::GeocodeGoogle(const wxString& address, double* lat, double* lon, double* tz, bool showprogress) {
    wxBusyCursor _curs;

    bool success = false;

    wxString plusaddr = address;
    plusaddr.Replace(", ", "+");
    plusaddr.Replace(",", "+");
    plusaddr.Replace("   ", " ");
    plusaddr.Replace("  ", " ");
    plusaddr.Replace(" ", "+");
   
    // Get lat/lon from Google geocoding API
    wxString url = SamApp::WebApi("google_geocode_api");
    url = url + "&address=";
    url = url + plusaddr;
    url.Replace("<GOOGLEAPIKEY>", wxString(google_api_key));

    wxEasyCurl curl;
    wxBusyCursor curs;
    if (showprogress) {
        if (!curl.Get(url, "Geocoding address '" + address + "'..."))
            return false;
    }
    else {
        if (!curl.Get(url))
            return false;
    }

    rapidjson::Document reader;
    auto str = curl.GetDataAsString();
    reader.Parse(str.c_str());

    if (!reader.HasParseError()) {
        if (reader.HasMember("results")) {
            if (reader["results"].IsArray()) {
                if (reader["results"][0].HasMember("geometry")) {
                    if (reader["results"][0]["geometry"].IsArray()) {
                        if (reader["results"][0]["geometry"][0].HasMember("location")) {
                            if (reader["results"][0]["geometry"][0]["location"].HasMember("lat")) {
                                if (reader["results"][0]["geometry"][0]["location"]["lat"].IsNumber()) {
                                    *lat = reader["results"][0]["geometry"][0]["location"]["lat"].GetDouble();
                                    success = true;
                                }
                            }
                            if (reader["results"][0]["geometry"][0]["location"].HasMember("lng")) {
                                if (reader["results"][0]["geometry"][0]["location"]["lng"].IsNumber()) {
                                    *lon = reader["results"][0]["geometry"][0]["location"]["lng"].GetDouble();
                                    success &= true;
                                }
                            }
                        }
                    }
                }
            }
        }
        // check status code
        success = false;//overrides success of retrieving data

        success = false;//overrides success of retrieving data

        if (reader.HasMember("status")) {
            if (reader["status"].IsString()) {
                str = reader["status"].GetString();
                success = str.Lower() == "ok";
            }
        }
    }

    if (!success)
        return false;


    if (tz != 0) {
        success = false;

        // get timezone from Goolge timezone API
        url = SamApp::WebApi("google_timezone_api") + wxString::Format("&location=%.14lf,%.14lf", *lat, *lon);
        url.Replace("<GOOGLEAPIKEY>", wxString(google_api_key));

        bool ok;
        if (showprogress)
            ok = curl.Get(url, "Geocoding address...");
        else
            ok = curl.Get(url);


        str = curl.GetDataAsString();
        reader.Parse(str.c_str());

        if (!reader.HasParseError()) {
            if (reader.HasMember("rawOffset")) {
                if (reader["rawOffset"].IsNumber()) {
                    *tz = reader["rawOffset"].GetDouble() / 3600.0;
                    success = true;
                }
                else if (reader["rawOffset"].IsInt()) {
                    *tz = reader["rawOffset"].GetInt() / 3600.0;
                    success = true;
                }
            }
        }
    
        // check status code
        success = false;//overrides success of retrieving data

        if (reader.HasMember("status")) {
            if (reader["status"].IsString()) {
                str = reader["status"].GetString();
                success = str.Lower() == "ok";
            }
        }
    }
    
    return success;

}

// Geocode using NREL Developer API (MapQuest) for NREL builds of SAM
bool GeoTools::GeocodeDeveloper(const wxString& address, double* lat, double* lon, double* tz, bool showprogress) {
    wxBusyCursor _curs;

    bool success = false;

    wxString plusaddr = address;
    plusaddr.Replace(", ", "+");
    plusaddr.Replace(",", "+");
    plusaddr.Replace("   ", " ");
    plusaddr.Replace("  ", " ");
    plusaddr.Replace(" ", "+");

    wxString url = SamApp::WebApi("nrel_geocode_api") + "&location=" + plusaddr;
    url.Replace("<GEOCODEAPIKEY>", wxString(geocode_api_key));

    wxEasyCurl curl;
    wxBusyCursor curs;
    if (showprogress) {
        if (!curl.Get(url, "Geocoding address '" + address + "'..."))
            return false;
    }
    else {
        if (!curl.Get(url))
            return false;
    }
 
    rapidjson::Document reader;
    auto str = curl.GetDataAsString();
    reader.Parse(str.c_str());


    /* example for "denver, co"
{"info":
    {"statuscode":0,"copyright":
        {"text":"© 2024 MapQuest, Inc.","imageUrl":"http://api.mqcdn.com/res/mqlogo.gif","imageAltText":"© 2024 MapQuest, Inc."}
    ,"messages":[]}
    ,"options":{"maxResults":-1,"ignoreLatLngInput":false}
    ,"results":
    [{"providedLocation":{"location":"denver co"},"locations":
        [{"street":"","adminArea6":"","adminArea6Type":"Neighborhood","adminArea5":"Denver","adminArea5Type":"City","adminArea4":"Denver","adminArea4Type":"County","adminArea3":"CO","adminArea3Type":"State","adminArea1":"US","adminArea1Type":"Country","postalCode":"","geocodeQualityCode":"A5XAX","geocodeQuality":"CITY","dragPoint":false,"sideOfStreet":"N","linkId":"0","unknownInput":"","type":"s","latLng":{"lat":39.74001,"lng":-104.99202},"displayLatLng":{"lat":39.74001,"lng":-104.99202},"mapUrl":""}]
    }
]}
        */

    if (!reader.HasParseError()) {
        if (reader.HasMember("results")) {
            if (reader["results"].IsArray()) {
                if (reader["results"][0].HasMember("locations")) {
                    if (reader["results"][0]["locations"].IsArray()) {
                        if (reader["results"][0]["locations"][0].HasMember("latLng")) {
                            if (reader["results"][0]["locations"][0]["latLng"].HasMember("lat")) {
                                if (reader["results"][0]["locations"][0]["latLng"]["lat"].IsNumber()) {
                                    *lat = reader["results"][0]["locations"][0]["latLng"]["lat"].GetDouble();
                                    success = true;
                                }
                            }
                            if (reader["results"][0]["locations"][0]["latLng"].HasMember("lng")) {
                                if (reader["results"][0]["locations"][0]["latLng"]["lng"].IsNumber()) {
                                    *lon = reader["results"][0]["locations"][0]["latLng"]["lng"].GetDouble();
                                    success &= true;
                                }
                            }
                        }
                    }
                }
            }
        }
        // check status code
        success = false;//overrides success of retrieving data

        if (reader.HasMember("info")) {
            if (reader["info"].HasMember("statuscode")) {
                if (reader["info"]["statuscode"].IsInt()) {
                    success = reader["info"]["statuscode"].GetInt() == 0;
                }
            }
        }
    }

    if (!success)
        return false;


    if (tz != 0) 
    {
        success = false;

        url = SamApp::WebApi("bing_maps_timezone_api");
        url.Replace("<POINT>", wxString::Format("%.14lf,%.14lf", *lat, *lon));
        url.Replace("<BINGAPIKEY>", wxString(bing_api_key));

        if (showprogress) 
        {
            if (!curl.Get(url, "Geocoding address '" + address + "'..."))
                return false;
        }
        else {
            if (!curl.Get(url))
                return false;
        }

        str = curl.GetDataAsString();
        reader.Parse(str.c_str());

        if (!reader.HasParseError()) {
            if (reader.HasMember("resourceSets")) {
                if (reader["resourceSets"].IsArray()) {
                    if (reader["resourceSets"][0].HasMember("resources")) {
                        if (reader["resourceSets"][0]["resources"].IsArray()) {
                            if (reader["resourceSets"][0]["resources"][0].HasMember("timeZone")) {
                                if (reader["resourceSets"][0]["resources"][0]["timeZone"].HasMember("utcOffset")) {
                                    if (reader["resourceSets"][0]["resources"][0]["timeZone"]["utcOffset"].IsString()) {
                                        wxString stz = reader["resourceSets"][0]["resources"][0]["timeZone"]["utcOffset"].GetString();
                                        wxArrayString as = wxSplit(stz, ':');
                                        if (as.Count() != 2) return false;
                                        if (!as[0].ToDouble(tz)) return false;
                                        double offset = 0;
                                        if (as[1] == "30") offset = 0.5;
                                        if (*tz < 0)
                                            *tz = *tz - offset;
                                        else
                                            *tz = *tz + offset;
                                        success = true;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            // check status code
            success = false;//overrides success of retrieving data

            if (reader.HasMember("statusDescription")) {
                if (reader["statusDescription"].IsString()) {
                    str = reader["statusDescription"].GetString();
                    success = str.Lower() == "ok";
                }
            }
        }

    }
    return success;
}


wxBitmap GeoTools::StaticMap(double lat, double lon, int zoom, MapProvider service) {
    if (zoom > 21) zoom = 21;
    if (zoom < 1) zoom = 1;
    wxString zoomStr = wxString::Format("%d", zoom);

    wxString url;
    if (service == GOOGLE_MAPS) {
        url = SamApp::WebApi("google_static_map_api");
        url.Replace("<POINT>", wxString::Format("%.14lf,%.14lf", lat, lon));
        url.Replace("<ZOOM>", zoomStr);
        url.Replace("<GOOGLEAPIKEY>", wxString(google_api_key));

    }
    else {
        url = SamApp::WebApi("bing_maps_imagery_api");
        url.Replace("<POINT>", wxString::Format("%.14lf,%.14lf", lat, lon));
        url.Replace("<ZOOMLEVEL>", zoomStr);
        url.Replace("<BINGAPIKEY>", wxString(bing_api_key));
    }

    wxEasyCurl curl;
    bool ok = curl.Get(url, "Obtaining aerial imagery...");
    return ok ? wxBitmap(curl.GetDataAsImage(wxBITMAP_TYPE_JPEG)) : wxNullBitmap;
}

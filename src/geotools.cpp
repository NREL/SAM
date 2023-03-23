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
#include <wex/jsonval.h>
#include <wex/jsonreader.h>

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

bool GeoTools::GeocodeGoogle(const wxString& address, double* lat, double* lon, double* tz, bool showprogress) {
    wxBusyCursor _curs;

    wxString plusaddr = address;
    plusaddr.Replace("   ", " ");
    plusaddr.Replace("  ", " ");
    plusaddr.Replace(" ", "+");
   
    // Get lat/lon from Google geocoding API
    wxString url = SamApp::WebApi("google_api") + "&address=" + plusaddr;
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

    wxJSONReader reader;
    wxJSONValue root;
    if (reader.Parse(curl.GetDataAsString(), &root) == 0) {
        wxJSONValue loc = root.Item("results").Item(0).Item("geometry").Item("location");
        if (!loc.IsValid()) return false;
        *lat = loc.Item("lat").AsDouble();
        *lon = loc.Item("lng").AsDouble();

        if (root.Item("status").AsString() != "OK")
            return false;
    }
    else
        return false;

    if (tz != 0) {
        // get timezone from Goolge timezone API
        wxString url = SamApp::WebApi("google_timezone_api") + wxString::Format("&location=%.14lf,%.14lf", *lat, *lon);
        url.Replace("<GOOGLEAPIKEY>", wxString(google_api_key));

        bool ok;
        if (showprogress)
            ok = curl.Get(url, "Geocoding address...");
        else
            ok = curl.Get(url);

        if (ok && reader.Parse(curl.GetDataAsString(), &root) == 0) {
            wxJSONValue val = root.Item("rawOffset");
            if (val.IsDouble()) *tz = val.AsDouble() / 3600.0;
            else *tz = val.AsInt() / 3600.0;

            return root.Item("status").AsString() == "OK";
        }
        else
            return false;
    } // if no tz argument given then return true
    else return true;
}

bool GeoTools::GeocodeDeveloper(const wxString& address, double* lat, double* lon, double* tz, bool showprogress) {
    wxBusyCursor _curs;

    wxString plusaddr = address;
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

    wxJSONReader reader;
    wxJSONValue root;
    if (reader.Parse(curl.GetDataAsString(), &root) == 0)
    {
        wxJSONValue loc = root.Item("results").Item(0).Item("locations").Item(0).Item("displayLatLng");
        if (!loc.IsValid()) return false;
        *lat = loc.Item("lat").AsDouble();
        *lon = loc.Item("lng").AsDouble();

        if (root.Item("info").Item("statuscode").AsInt() != 0)
            return false;
    }
    else
        return false;

    if (tz != 0) 
    {
        wxString url = SamApp::WebApi("bing_maps_timezone_api");
        url.Replace("<POINT>", wxString::Format("%.14lf,%.14lf", *lat, *lon));
        url.Replace("<BINGAPIKEY>", wxString(bing_api_key));

        wxEasyCurl curl;
        wxBusyCursor curs;
        if (showprogress) 
        {
            if (!curl.Get(url, "Geocoding address '" + address + "'..."))
                return false;
        }
        else {
            if (!curl.Get(url))
                return false;
        }

        wxJSONReader reader;
        wxJSONValue root;
        if (reader.Parse(curl.GetDataAsString(), &root) == 0) {
            wxJSONValue loc = root.Item("resourceSets").Item(0).Item("resources").Item(0).Item("timeZone");
            if (!loc.IsValid()) return false;
            wxString stz = loc.Item("utcOffset").AsString();
            wxArrayString as = wxSplit(stz, ':');
            if (as.Count() != 2) return false;
            if (!as[0].ToDouble(tz)) return false;
            double offset = 0;
            if (as[1] == "30") offset = 0.5;
            if (*tz < 0)
                *tz = *tz - offset;
            else
                *tz = *tz + offset;

            if (root.Item("statusDescription").AsString() != "OK")
                return false;
        }
        else
            return false;
    }
    return true;
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

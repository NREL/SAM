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

// geocoding function using google APIs.
// call is synchronous.  Optionally determine time
// zone from lat/lon using second service call


class GeoTools
{
public:
    // Use Google Geocoding API to return lat/lon from address
    // Requires a Google Cloud Project account with billing enabled
    // This is an alternative to GeocodeDeveloper for non-NREL builds of SAM
    // Google API key is defined in private.h
    static bool GeocodeGoogle(const wxString& address,
        double* lat, double* lon, double* tz = 0, bool showprogress = true);

    // Use private NREL Developer API (MapQuest wrapper) to return lat/lon from address
    // Requires a special private API key from NREL defined in private.h
    static bool GeocodeDeveloper(const wxString& address,
        double* lat, double* lon, double* tz = 0, bool showprogress = true);

    enum MapProvider {
        GOOGLE_MAPS, BING_MAPS
    };

    // Return a map for a given lat/lon and zoom level as a bitmap image
    static wxBitmap StaticMap(double lat, double lon, int zoom, MapProvider service);
};
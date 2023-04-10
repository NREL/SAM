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


// can be used to indicate specialized releases for particular testers, i.e. 'iscc-ge'
// by default, should be NULL
static const char *version_label = 0; //"iscc-ge";

// NREL Developer API:
// For calls to NREL Developer APIs for weather file downloads, REopt calls, etc.
// Get an API key at https://developer.nrel.gov
const char *sam_api_key = "";

// Email address used to register for NREL Developer API
// For calls to NSRDB and other NREL Developer APIs that require an email address
const char* user_email = "";

// Google APIs:
// For non-NREL builds of SAM, use this instead of NREL geocoding API
// Get a Google Cloud Platform account at https://code.google.com/apis/console
const char *google_api_key = "";

// Bing Map APIs:
// Used for static map underlay in 3D shade calculator (Google map can be used as an option instead)
// Get a Bing Maps developer key at https://www.bingmapsportal.com/
const char *bing_api_key = "";

// Private NREL Developer geocoding API for NREL versions of SAM
// Use Google API for geocoding for non-NREL versions of SAM
const char *geocode_api_key = "";

// specific to SAM and SAM-private - can be separate include file
// main window title
static const char* MAIN_WINDOW_TITLE = "SAM (Open Source) ";

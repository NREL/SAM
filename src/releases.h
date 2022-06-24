class HelpWin;
static HelpWin* gs_helpWin = 0;

enum { ID_BACK = wxID_HIGHEST + 439, ID_BROWSER, ID_HOME, ID_EMAIL_SUPPORT, ID_WEBSITE, ID_FORUM, ID_RELEASE_NOTES, ID_SCRIPT_REFERENCE };


static SamApp::ver releases[] = {
	//intermediate version numbers are required in this list in order for the version upgrade script (versions.lk) to work correctly
	//please clarify the reason for the new version in a comment. Examples: public release, variable changes, internal release, public beta release, etc.
	//the top version should always be the current working version
			{ 2021, 12, 02 }, // 2021.12.02 ssc 267 public release
			{ 2021, 11, 30 }, // 2021.11.30 ssc 265 release candidate beta expires 11/30/2022
			{ 2021, 11, 27 }, // 2021.11.27 ssc 264 release candidate beta expires 11/27/2022
			{ 2021, 11, 22 }, // 2021.11.22 ssc 263 release candidate beta expires 11/22/2022
			{ 2021, 10, 27 }, // 2021.10.27 ssc 262 Cambium beta expires 10/27/2022
			{ 2021, 9, 10 }, // 2021.9.10 ssc 261 Community Solar Beta expires 9/10/2022
			{ 2021, 9, 7 }, // 2021.9.7 ssc 260 Community Solar Beta expires 9/7/2022
			{ 2021, 7, 27 }, // 2021.7.27 ssc 259 EPRI Beta expires 7/27/2022
			{ 2021, 7, 12 }, // 2021.7.12 ssc 258 EPRI Beta expires 7/12/2022
			{ 2021, 6, 2 }, // 2021.6.2 ssc 257 EPRI Beta expires 6/2/2022
			{ 2021, 4, 19 }, // 2021.4.19 ssc 255 EPRI Beta expires 4/19/2022
			{ 2021, 3, 25 }, // 2021.3.25 ssc 254 EPRI Beta expires 3/25/2022
			{ 2021, 3, 8 }, // 2021.3.8 ssc 253 EPRI Beta expires 3/8/2022
			{ 2021, 2, 24 }, // 2021.2.24 ssc 251 release
			{ 2020, 11, 29 }, // 2020.11.29 ssc 250 release r1 ssc 252 2/25/2021 and r2 ssc 256 on 5/12/2021
			{ 2020, 11, 17 }, // 2020.11.17 ssc 247 beta  - expires 11/17/2021
			{ 2020, 11, 12 }, // 2020.11.12 ssc 246 beta  - expires 11/12/2021
			{ 2020, 11, 5 }, // 2020.11.5 ssc 245 beta for Ty - expires 11/5/2021
			{ 2020, 11, 3 }, // 2020.11.3 ssc 244 beta for the 2021 release - expires 11/3/2021
			{ 2020, 2, 29 }, //2020.2.29 release
			{ 2020, 02, 24 }, //2020.2.24 beta
			{ 2020, 02, 17 }, //VS2019 beta release
			{ 2020, 02, 14 }, //CSP beta release
			{ 2020, 1, 17 }, //Updated Beta for release testing - expires 1/17/2021 ssc version 232
			{ 2020, 1, 14 }, //Updated Beta for release testing - expires 1/14/2021 ssc version 231
			{ 2020, 1, 6 }, //Updated Beta for release testing - expires 1/6/2021 ssc version 230
			{ 2020, 1, 3 }, //Updated Beta for release testing - expires 1/3/2021 ssc version 229
			{ 2019, 12, 31 }, //Updated Beta for release testing - expires 12/31/2020.
			{ 2019, 12, 26 }, //Updated Beta for internal release testing - no expiration.
			{ 2019, 12, 19 }, //Updated Beta for internal release testing - no expiration.
			{ 2019, 12, 16 }, //Updated Beta for internal release testing - no expiration.
			{ 2019, 12, 9 }, //Updated Beta for internal release testing - no expiration.
			{ 2019, 12, 2 }, //Updated Beta for ME and Fuel Cells expires 12/2/2020
			{ 2019, 11, 27 }, //Beta for ME and Fuel Cells expires 11/27/2020
			{ 2019, 11, 11 }, //Beta for ME 11/11/2020
			{ 2019, 10, 14 }, //Beta for MHK and Wind_PRUF ssc 220 expires 10/14/2020
			{ 2019, 10, 7 }, //Beta for MHK ssc 218 expires 10/7/2020
			{ 2019, 10, 4 }, //Beta for MHK ssc 217 expires 10/4/2020
			{ 2019, 9, 26 }, //Beta for MHK ssc 215 expires 9/26/2020
			{ 2019, 7, 15 }, //Beta for Wind PRUF project expires 7/15/2020
			{ 2019, 7, 11 }, //Beta for MHK ssc 211 expires 7/11/2020
			{ 2019, 4, 3 }, //Beta for fuel cells and batteries 4/3/2020
			{ 2019, 3, 4 }, //Beta for fuel cells 3/4/2020
			{ 2019, 1, 21 }, //Beta for fuel cells 1/21/2020
			{ 2018, 12, 20 }, //Beta for fuel cells 12/20/2019
			{ 2018, 11, 29 }, //Beta for fuel cells 11/29/2019
		{ 2018, 11, 11 }, // public Veteran's Day release !
			{ 2018, 11, 8 }, //Release candidate for testing expires 11/8/2019
			{ 2018, 11, 5 }, //Beta version for testing expires 11/5/2019
			{ 2018, 10, 29 }, //Beta version for testing expires 10/29/2019
			{ 2018, 10, 17 }, //Beta version for defaults expires 10/17/2019
			{ 2018, 9, 20 }, //new version number for MPPT upgrades
			{ 2018, 9, 13 }, // Beta for Webinar - expires 9/13/2019.
			{ 2018, 9, 10 }, // Beta for Webinar - expires 9/10/2019.
			{ 2018, 8, 29 }, // Beta for Bifacial - expires 8/29/2019.
			{ 2018, 8, 20 }, // Beta for testing - internal with no expiration.
			{ 2018, 8, 13 }, // Beta for Bifacial - expires 8/13/2019
			{ 2018, 7, 17 }, // Beta for Bifacial - expires 7/17/2019
			{ 2018, 7, 11 }, // Beta for Bifacial - expires 7/11/2019
			{ 2018, 4, 3 }, // Beta for MHK - expires 5/31/2018
			{ 2018, 4, 2 }, // Beta for Southern company - expires 4/2/2019
			{ 2018, 1, 29 }, // Beta release for OEA/OEI
			{ 2018, 1, 3}, // Beta release for Host Developer
		{ 2017, 9, 5 }, // public Labor Day release !
			{ 2017, 8, 28 }, // Beta release candidate - expires 12/30/17
			{ 2017, 8, 18 }, // Beta release - expires 12/30/17
			{ 2017, 8, 11 }, // Beta release - expires 12/30/17
			{ 2017, 7, 28 }, // Beta release - expires 12/30/17
			{ 2017, 5, 15 }, // Beta release - expires 7/31/17
			{ 2017, 5, 11 }, // Beta release - no expiration
			{ 2017, 4, 11 }, // Beta release
			{ 2017, 2, 28 }, // Beta release
			{ 2017, 2, 14 }, // Beta release
		{ 2017, 1, 17 }, // public 'ones and sevens' release !
			{ 2016, 12, 29 }, // Beta release - expires 2/28/17
			{ 2016, 10, 25 }, // Beta release
			{ 2016, 7, 21 }, // Beta release - expires 12/31/16
			{ 2016, 5, 4 }, //dc adjustment factor added, internal release
		{ 2016, 3, 14 }, // public pi-day release!
			{ 2016, 3, 2 }, // Beta release - expires 4/15/16
			{ 2016, 2, 29 }, // internal release
			{ 2016, 2, 26 }, // utility rate changes
			{ 2016, 2, 22 }, // self-shading update
			{ 2016, 2, 19 }, // PV variable changes
			{ 2016, 2, 16 }, // new versioning scheme
			{ 2016, 1, 21 }, // internal release
			{ 2015, 11, 16 }, // utility rate variable changes
			{ 2015, 10, 29 }, // battery model variable changes
			{ 2015, 10, 16 }, // internal release
			{ 2015, 9, 30 }, // internal release
			{ 2015, 9, 9 }, // CSP and net metering changes
			{ 2015, 8, 17 }, // CSP variable changes
		{ 2015, 6, 30 }, // public release
			{ 2015, 5, 27 }, // CSP variable changes
			{ 2015, 4, 10 }, // CSP variable changes
		{ 2015, 1, 30 }, // public release
		{ 2014, 11, 24 }, // public release
		{    0,  0,  0 } };
#pragma once

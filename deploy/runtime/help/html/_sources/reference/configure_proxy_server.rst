Web Access and Proxies
======================

SAM uses your computer’s default internet access settings to communicate with websites using either the HTTP or HTTPS communications protocols. SAM communicates with websites to download weather files, electricity rate data, to display information from or hypertext links from the SAM website, and to :doc:`collect software usage data <registration>`.

Website Addresses 
~~~~~~~~~~~~~~~~~~

The following is a list of all of the URLs that SAM may use. You can find this list for the version of SAM you are using in the runtime/webapis.conf file in your SAM :ref:`installation folder <installationfolder>`.

    https://sam.nlr.gov/support
    https://sam.nlr.gov/11/115
    https://dev.virtualearth.net/REST/v1/Imagery/Map/Aerial/<POINT>/<ZOOMLEVEL>?mapSize=800,800&format=jpeg&key=<BINGAPIKEY>
    https://atlas.microsoft.com/map/static?subscription-key=<AZUREAPIKEY>&zoom=<ZOOMLEVEL>&tilesetId=microsoft.imagery&api-version=2024-04-01&language=en-us&center=<LONLAT>&height=800&width=800
    https://dev.virtualearth.net/REST/v1/TimeZone/<POINT>?key=<BINGAPIKEY>
    https://atlas.microsoft.com/timezone/byCoordinates/json?subscription-key=<AZUREAPIKEY>&api-version=1.0&query=<LATLON>
    https://scenarioviewer.nlr.gov/api/project/detail/?uuid=0f92fe57-3365-428a-8fe8-0afc326b3b43
    https://maps.googleapis.com/maps/api/geocode/json?&sensor=false&key=<GOOGLEAPIKEY>
    https://maps.googleapis.com/maps/api/timezone/json?timestamp=1&sensor=false&key=<GOOGLEAPIKEY>
    https://maps.googleapis.com/maps/api/staticmap?<CENTER>&zoom=<ZOOM>&size=800x800&maptype=hybrid&sensor=false&format=jpg-baseline&key=<GOOGLEAPIKEY>
    https://sam.nlr.gov/11/116
    https://www.nohrsc.noaa.gov/interactive/html/graph.html?station=<STATION>&w=600&h=400&o=a&uc=0&by=<BEGINYEAR>&bm=<BEGINMONTH>&bd=1&bh=<BEGINHOUR>&ey=<ENDYEAR>&em=<ENDMONTH>&ed=1&eh=<ENDHOUR>&data=<DATA>&units=1&region=us
    https://developer.nlr.gov/api/mapquest/geocoding/v1/address?_app_id=sam&outFormat=json&api_key=<GEOCODEAPIKEY>
    https://developer.nlr.gov/api/nsrdb/v2/solar/puerto-rico-download.csv?api_key=<SAMAPIKEY>&email=<USEREMAIL>
    https://developer.nlr.gov/api/nsrdb/v2/solar/puerto-rico-download.json?api_key=<SAMAPIKEY>&email=<USEREMAIL>
    https://developer.nlr.gov/api/nsrdb/v2/solar/nsrdb-data-query.json?&api_key=<SAMAPIKEY>&email=<USEREMAIL>&wkt=POINT(<LON>%20<LAT>)
    https://developer.nlr.gov/api/sam/v1/tracker
    https://sam.nlr.gov/sites/default/files/content/updates/messages.html
    https://samrepo.nlr.gov/updates/welcome.lk
    https://nrel.github.io/SAM/doc/releasenotes.html
    https://developer.nlr.gov/api/reopt/stable/job/<RUN_UUID>/results/?api_key=<SAMAPIKEY>
    https://developer.nlr.gov/api/reopt/stable/job?format=json&api_key=<SAMAPIKEY>
    mailto:sam.support@nlr.gov
    https://en.openei.org/w/index.php?title=Special:Ask&q=<QUESTION>&po=<PROPERTIES>&eq=yes&p[format]=json
    https://api.openei.org/utility_companies?version=3&format=json&api_key=<SAMAPIKEY>&scope=<SCOPE>
    https://developer.nlr.gov/api/utility_rates/v3.json?api_key=<SAMAPIKEY>&lat=<LAT>&lon=<LON>
    https://api.openei.org/utility_rates?version=8&format=json&limit=<LIMIT>&detail=<DETAIL>&offset=<OFFSET>&ratesforutility=<UTILITYNAME>&getpage=<GUID>&api_key=<APIKEY>
    https://apps.openei.org/IURDB
    https://en.openei.org/wiki/Utility_Rate_Database
    https://developer.nlr.gov/api/wave/v2/wave/us-west-coast-hindcast-download.csv?api_key=<SAMAPIKEY>&wkt=POINT(<LON>%20<LAT>)&interval=180&email=<USEREMAIL>&attributes=significant_wave_height,energy_period&names=<YEAR>
    https://developer.nlr.gov/api/wave/v2/wave/us-atlantic-hindcast-download.csv?api_key=<SAMAPIKEY>&wkt=POINT(<LON>%20<LAT>)&interval=180&email=<USEREMAIL>&attributes=significant_wave_height,energy_period&names=<YEAR>
    https://developer.nlr.gov/api/wave/v2/wave/hawaii-hindcast-download.csv?api_key=<SAMAPIKEY>&wkt=POINT(<LON>%20<LAT>)&interval=180&email=<USEREMAIL>&attributes=significant_wave_height,energy_period&names=<YEAR>
    https://developer.nlr.gov/api/wave/v2/wave/alaska-hindcast-download.csv?api_key=<SAMAPIKEY>&wkt=POINT(<LON>%20<LAT>)&interval=180&email=<USEREMAIL>&attributes=significant_wave_height,energy_period&names=<YEAR>
    https://developer.nlr.gov/api/wave/v2/wave/us-wave-v1-0-0-gom-and-pr-download.csv?api_key=<SAMAPIKEY>&wkt=POINT(<LON>%20<LAT>)&interval=180&email=<USEREMAIL>&attributes=significant_wave_height,energy_period&names=<YEAR>
    https://sam.nlr.gov
    https://developer.nlr.gov/api/wind-toolkit/v2/wind/wtk-download.csv?&api_key=<SAMAPIKEY>&email=<USEREMAIL>&full_name=SAM%20User&affiliation=none&reason=none&wkt=POINT(<LON>%20<LAT>)&utc=false&leap_day=false&names=<YEAR>&interval=<INTERVAL>&attributes=<ATTRS>

Proxy Server Configuration
~~~~~~~~~~~~~~~~~~~~~~~~~~

If your organization uses a web proxy server to connect to the internet, and you are having trouble registering SAM or accessing online databases from SAM, you may need to create a proxy configuration file. The file is a one-line text file with one proxy server address and an optional custom port. 

To create a proxy configuration file:

#. Start SAM.

#. When you see the registration window, click Proxies.

#. Type the your organization's web proxy server address. For example if the proxy server address is "proxy-server.myorganization.org", type:

```proxy-server.myorganization.org```

To specify a custom port, add it to the name with a colon separator (no spaces). For example, if the custom port number is 9142:

 ```proxy-server.myorganization.org:9142```

The next time you start SAM, it will use the proxy server address to access the internet. You can ch

SAM stores this proxy address in a text file named proxy.txt in the :ref:`SAM installation folder <installationfolder>`. It contains a single line with the proxy server address and optional custom port.

If you use your computer with different internet connections, and not all connections use a proxy server, or they use different proxy servers, you can change the proxy configuration by following the steps above. Delete the proxy server address to connect directly with no proxy server.
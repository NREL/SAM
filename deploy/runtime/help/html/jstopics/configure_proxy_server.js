hmLoadTopic({
hmKeywords:"",
hmTitle:"Web Access and Proxies",
hmDescription:"SAM uses your computer’s default internet access settings to communicate with websites using either the HTTP or HTTPS communications protocols. SAM communicates with websites...",
hmPrevLink:"registration.html",
hmNextLink:"function_keys.html",
hmParentLink:"index.html",
hmBreadCrumbs:"Reference",
hmTitlePath:"Reference > Web Access and Proxies",
hmHeader:"<h1 class=\"p_Heading1\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_Heading1\">Web Access and Proxies<\/span><\/h1>\n\r",
hmBody:"<p class=\"p_Text\"><span class=\"f_Text\">SAM uses your computer’s default internet access settings to communicate with websites using either the HTTP or HTTPS communications protocols. SAM communicates with websites to download weather files, electricity rate data, to display information from or hypertext links from the SAM website, and to <a href=\"registration.html\" class=\"topiclink\">collect software usage data<\/a>.<\/span><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><span class=\"f_Heading3_atocs_\">Website Addresses <\/span><\/h3>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The following is a list of all of the URLs that SAM may use. You can find this list for the version of SAM you are using in the <\/span><span class=\"f_CHFileName\">runtime\/webapis.conf<\/span><span class=\"f_Text\"> file in your SAM <a href=\"installation.html#installationfolder\" class=\"topiclink\">installation folder<\/a>.<\/span><\/p>\n\r<p class=\"p_List\"><span class=\"f_List\">https:\/\/sam.nrel.gov\/sites\/default\/files\/content\/mobile\/android\/readme.html<\/span><\/p>\n\r<p class=\"p_List\"><span class=\"f_List\">https:\/\/cambium.nrel.gov\/api\/project\/detail\/?uuid=579698fe-5a38-4d7c-8611-d0c5969b2e54<\/span><\/p>\n\r<p class=\"p_List\"><span class=\"f_List\">https:\/\/cambium.nrel.gov\/api\/load_data\/<\/span><\/p>\n\r<p class=\"p_List\"><span class=\"f_List\">https:\/\/sam.nrel.gov\/sites\/default\/files\/content\/mobile\/ios\/readme.html<\/span><\/p>\n\r<p class=\"p_List\"><span class=\"f_List\">https:\/\/sam.nrel.gov\/support<\/span><\/p>\n\r<p class=\"p_List\"><span class=\"f_List\">https:\/\/developer.nrel.gov\/api\/nsrdb\/v2\/solar\/nsrdb-data-query.json?&amp;api_key=&lt;SAMAPIKEY&gt;&amp;wkt=POINT(&lt;LON&gt;%20&lt;LAT&gt;)<\/span><\/p>\n\r<p class=\"p_List\"><span class=\"f_List\">https:\/\/nrel.github.io\/SAM\/doc\/releasenotes.html<\/span><\/p>\n\r<p class=\"p_List\"><span class=\"f_List\">https:\/\/developer.nrel.gov\/api\/reopt\/v1\/job\/&lt;RUN_UUID&gt;\/results\/?api_key=&lt;SAMAPIKEY&gt;<\/span><\/p>\n\r<p class=\"p_List\"><span class=\"f_List\">https:\/\/developer.nrel.gov\/api\/reopt\/v1\/job?format=json&amp;api_key=&lt;SAMAPIKEY&gt;<\/span><\/p>\n\r<p class=\"p_List\"><span class=\"f_List\">mailto:sam.support@nrel.gov<\/span><\/p>\n\r<p class=\"p_List\"><span class=\"f_List\">https:\/\/en.openei.org\/w\/index.php?title=Special:Ask&amp;q=&lt;QUESTION&gt;&amp;po=&lt;PROPERTIES&gt;&amp;eq=yes&amp;p[format]=json<\/span><\/p>\n\r<p class=\"p_List\"><span class=\"f_List\">https:\/\/api.openei.org\/utility_companies?version=3&amp;format=json&amp;api_key=&lt;SAMAPIKEY&gt;&amp;scope=&lt;SCOPE&gt;<\/span><\/p>\n\r<p class=\"p_List\"><span class=\"f_List\">https:\/\/developer.nrel.gov\/api\/utility_rates\/v3.json?api_key=&lt;SAMAPIKEY&gt;&amp;address=&lt;ADDRESS&gt;<\/span><\/p>\n\r<p class=\"p_List\"><span class=\"f_List\">https:\/\/api.openei.org\/utility_rates?version=8&amp;format=json&amp;limit=&lt;LIMIT&gt;&amp;detail=&lt;DETAIL&gt;&amp;offset=&lt;OFFSET&gt;&amp;ratesforutility=&lt;UTILITYNAME&gt;&amp;getpage=&lt;GUID&gt;&amp;api_key=&lt;APIKEY&gt;<\/span><\/p>\n\r<p class=\"p_List\"><span class=\"f_List\">https:\/\/en.openei.org\/apps\/IURDB<\/span><\/p>\n\r<p class=\"p_List\"><span class=\"f_List\">https:\/\/developer.nrel.gov\/api\/wave\/v2\/wave\/us-west-coast-hindcast-download.csv?api_key=&lt;SAMAPIKEY&gt;&amp;wkt=POINT(&lt;LON&gt;%20&lt;LAT&gt;)&amp;interval=180&amp;email=&lt;USEREMAIL&gt;&amp;attributes=significant_wave_height,energy_period&amp;names=&lt;YEAR&gt;<\/span><\/p>\n\r<p class=\"p_List\"><span class=\"f_List\">https:\/\/developer.nrel.gov\/api\/wave\/v2\/wave\/us-atlantic-hindcast-download.csv?api_key=&lt;SAMAPIKEY&gt;&amp;wkt=POINT(&lt;LON&gt;%20&lt;LAT&gt;)&amp;interval=180&amp;email=&lt;USEREMAIL&gt;&amp;attributes=significant_wave_height,energy_period&amp;names=&lt;YEAR&gt;<\/span><\/p>\n\r<p class=\"p_List\"><span class=\"f_List\">https:\/\/developer.nrel.gov\/api\/wave\/v2\/wave\/hawaii-hindcast-download.csv?api_key=&lt;SAMAPIKEY&gt;&amp;wkt=POINT(&lt;LON&gt;%20&lt;LAT&gt;)&amp;interval=180&amp;email=&lt;USEREMAIL&gt;&amp;attributes=significant_wave_height,energy_period&amp;names=&lt;YEAR&gt;<\/span><\/p>\n\r<p class=\"p_List\"><span class=\"f_List\">https:\/\/sam.nrel.gov<\/span><\/p>\n\r<p class=\"p_List\"><span class=\"f_List\">https:\/\/developer.nrel.gov\/api\/wind-toolkit\/wind\/wtk_srw_download?year=&lt;YEAR&gt;&amp;lat=&lt;LAT&gt;&amp;lon=&lt;LON&gt;&amp;hubheight=&lt;HUBHEIGHT&gt;&amp;api_key=&lt;SAMAPIKEY&gt;&amp;email=&lt;USEREMAIL&gt;<\/span><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><span class=\"f_Heading3_atocs_\">Proxy Server Configuration<\/span><\/h3>\n\r<p class=\"p_Text\"><span class=\"f_Text\">If your organization uses a web proxy server to connect to the internet, and you are having trouble registering SAM or accessing online databases from SAM, you may need to create a proxy configuration file. The file is a one-line text file with one proxy server address and an optional custom port. <\/span><\/p>\n\r<p class=\"p_TitleProcedure\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_TitleProcedure\">To create a proxy configuration file:<\/span><\/p>\n\r<p class=\"p_ProcedureStep\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_ProcedureStep\" style=\"display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">1.<\/span><span class=\"f_ProcedureStep\">Start SAM.<\/span><\/p>\n\r<p class=\"p_ProcedureStep\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_ProcedureStep\" style=\"display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">2.<\/span><span class=\"f_ProcedureStep\">When you see the registration window, click Proxies.<\/span><\/p>\n\r<p class=\"p_ProcedureStep\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_ProcedureStep\" style=\"display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">3.<\/span><span class=\"f_ProcedureStep\">Type the your organization\'s web proxy server address. For example if the proxy server address is &quot;proxy-server.myorganization.org&quot;, type:<\/span><\/p>\n\r<p class=\"p_ProcedureStepNoNumber\"><span class=\"f_CHCode\">proxy-server.myorganization.org<\/span><\/p>\n\r<p class=\"p_ProcedureStepNoNumber\"><span class=\"f_ProcedureStepNoNumber\">To specify a custom port, add it to the name with a colon separator (no spaces). For example, if the custom port number is 9142:<\/span><\/p>\n\r<p class=\"p_ProcedureStepNoNumber\"><span class=\"f_ProcedureStepNoNumber\"> <\/span><span class=\"f_CHCode\">proxy-server.myorganization.org:9142<\/span><\/p>\n\r<p class=\"p_ProcedureStepNoNumber\"><span class=\"f_ProcedureStepNoNumber\">The next time you start SAM, it will use the proxy server address to access the internet. You can ch<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">SAM stores this proxy address in a text file named <\/span><span class=\"f_CHFileName\">proxy.txt<\/span><span class=\"f_Text\"> in the <a href=\"installation.html#installationfolder\" class=\"topiclink\">SAM installation folder<\/a>. It contains a single line with the proxy server address and optional custom port.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">If you use your computer with different internet connections, and not all connections use a proxy server, or they use different proxy servers, you can change the proxy configuration by following the steps above. Delete the proxy server address to connect directly with no proxy server.<\/span><\/p>\n\r"
})

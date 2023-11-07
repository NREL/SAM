hmLoadTopic({
hmKeywords:"",
hmTitle:"Detailed Photovoltaic Model",
hmDescription:"The following overview is to help you get started modeling  a photovoltaic system with the detailed photovoltaic model. For a description of the model, see Performance Models.",
hmPrevLink:"weather_format_csv_marine_energy.html",
hmNextLink:"pv_location_and_resource.html",
hmParentLink:"index.html",
hmBreadCrumbs:"",
hmTitlePath:"Detailed Photovoltaic Model",
hmHeader:"<h1 class=\"p_Heading1\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_Heading1\">Detailed Photovoltaic Model<\/span><\/h1>\n\r",
hmBody:"<p class=\"p_Text\"><span class=\"f_Text\">The following overview is to help you get started modeling &nbsp;a photovoltaic system with the detailed photovoltaic model. For a description of the model, see <a href=\"technology_options.html\" class=\"topiclink\">Performance Models<\/a>.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">For a complete technical description of SAM\'s photovoltaic model, see Gilman, P. (2015). SAM Photovoltaic Model Technical Reference. National Renewable Energy Laboratory. 59 pp.; NREL\/TP-6A20-64102. (<a href=\"http:\/\/www.nrel.gov\/docs\/fy15osti\/64102.pdf\" target=\"_blank\" class=\"weblink\">PDF 840 KB<\/a>)<\/span><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><span class=\"f_Heading3_atocs_\">Basic Steps<\/span><\/h3>\n\r<h4 class=\"p_Heading4\"><span class=\"f_Heading4\">1. Choose a weather file<\/span><\/h4>\n\r<p class=\"p_ProcedureStepLast\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_ProcedureStepLast\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_ProcedureStepLast\">On the <a href=\"pv_location_and_resource.html\" class=\"topiclink\">Location and Resource<\/a> page, choose a weather file to represent the solar resource at the project location.<\/span><\/p>\n\r<h4 class=\"p_Heading4\"><span class=\"f_Heading4\">2. Specify the system\'s characteristics<\/span><\/h4>\n\r<p class=\"p_ProcedureStep\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_ProcedureStep\" style=\"display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">1.<\/span><span class=\"f_ProcedureStep\">On the <a href=\"pv_module.html\" class=\"topiclink\">Module<\/a> page, choose a model option and module.<\/span><\/p>\n\r<p class=\"p_ProcedureStepNoNumber\"><span class=\"f_ProcedureStepNoNumber\">For most applications, use the CEC Performance Model with Module Database model unless your module is not in the list, in which case you can use the CEC Performance Model with User Specifications.<\/span><\/p>\n\r<p class=\"p_ProcedureStep\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_ProcedureStep\" style=\"display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">2.<\/span><span class=\"f_ProcedureStep\">On the <a href=\"pv_inverter.html\" class=\"topiclink\">Inverter<\/a> page, choose a model option and inverter. <\/span><\/p>\n\r<p class=\"p_ProcedureStepNoNumber\"><span class=\"f_ProcedureStepNoNumber\">Use the Inverter CEC Database option. For an inverter that is not in the list, if you have the manufacturer\'s data sheet, choose the Inverter Datasheet model.<\/span><\/p>\n\r<p class=\"p_ProcedureStep\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_ProcedureStep\" style=\"display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">3.<\/span><span class=\"f_ProcedureStep\">On the <a href=\"pv_system_design.html\" class=\"topiclink\">System Design<\/a> page, specify the system\'s size and array tracking options.<\/span><\/p>\n\r<p class=\"p_ProcedureStepNoNumber\"><span class=\"f_ProcedureStepNoNumber\">Use the specify desired array size option for preliminary analysis, and then use specify modules and inverters to refine the system design.<\/span><\/p>\n\r<p class=\"p_ProcedureStepLast\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_ProcedureStep\" style=\"display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">4.<\/span><span class=\"f_ProcedureStepLast\">If you want to account for shading losses, use the <a href=\"pv_shading.html\" class=\"topiclink\">Shading and Layout<\/a> page for external shading by nearby objects, array self-shading, or to account for losses due to the array being covered by snow when your weather data includes snow depth data.<\/span><\/p>\n\r<p class=\"p_ProcedureStepLast\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_ProcedureStep\" style=\"display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">5.<\/span><span class=\"f_ProcedureStepLast\">Account for soiling losses, DC power losses, AC power losses, and losses due to system availability requirements on the <a href=\"pv_losses.html\" class=\"topiclink\">Losses<\/a> page. You can use the default values if you do not have detailed information about those losses for your system.<\/span><\/p>\n\r<h4 class=\"p_Heading4\"><span class=\"f_Heading4\">5. Specify the project costs<\/span><\/h4>\n\r<p class=\"p_ProcedureStepLast\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_ProcedureStepLast\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_ProcedureStepLast\">On the <a href=\"cc_pv.html\" class=\"topiclink\">Installation Costs<\/a> and <a href=\"oc_operating.html\" class=\"topiclink\">Operating Costs<\/a> pages, specify values for capital cost and operation and maintenance costs.<\/span><\/p>\n\r<h4 class=\"p_Heading4\"><span class=\"f_Heading4\">6. Run a simulation and review results<\/span><\/h4>\n\r<p class=\"p_ProcedureStepLast\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_ProcedureStep\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_ProcedureStep\">Click Simulate, and review <a href=\"pv_results.html\" class=\"topiclink\">results<\/a> on the <a href=\"results_page.html\" class=\"topiclink\">Results Page<\/a>.<\/span><\/p>\n\r<p class=\"p_ProcedureStepLast\"><img alt=\"SS_MainWindow-RunAllSimulationsButton\" style=\"margin:0;width:16.5000rem;height:7.3125rem;border:none\" src=\".\/images\/ss_mainwindow-runallsimulationsbutton.png\"\/><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><a id=\"postsimulationwarnings\" class=\"hmanchor\"><\/a><span class=\"f_Heading3_atocs_\">Post-simulation Notices<\/span><\/h3>\n\r<p class=\"p_Text\"><span class=\"f_Text\">After completing a simulation, SAM checks to see whether the inverter appears to be over- or under-sized based on the actual DC output of the array. If it finds any problems, it displays <a href=\"notices.html\" class=\"topiclink\">notices<\/a> after simulations are complete.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Note\">Post-simulation checks include:<\/span><\/p>\n\r<p class=\"p_List\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_Emphasis\">Inverter undersized:<\/span><span class=\"f_List\"> The array output is greater than inverter rated capacity for one or more of the 8,760 hours in one year. SAM reports the number of hours that the array\'s simulated DC output is greater than the inverter\'s AC rated capacity.<\/span><\/p>\n\r<p class=\"p_ListNoNumber\"><span class=\"f_ListNoNumber\">If the number of hours is small compared to the 8,760 hours in a year, you may choose to ignore the message. Otherwise, you may want to try increasing the inverter capacity.<\/span><\/p>\n\r<p class=\"p_ListNoNumber\"><span class=\"f_ListNoNumber\">For example, for a system with 400 kWdc array capacity and 150 kWac total inverter capacity, SAM displays the following warning message: &quot;pvsamv1: Inverter undersized: The array output exceeded the inverter rating 157.62 kWdc for 2128 hours.&quot;<\/span><\/p>\n\r<p class=\"p_ListNoNumber\"><span class=\"f_ListNoNumber\">The following <a href=\"timeseries.html\" class=\"topiclink\">time series graphs<\/a> show the array\'s DC output in red, and the system\'s AC output in blue, indicating that the inverter capacity is limiting the system\'s AC output:<\/span><\/p>\n\r<p class=\"p_ListNoNumber\"><img alt=\"IMG_PVError-time-series-inverter-undersized\" style=\"margin:0;width:25.0000rem;height:18.7500rem;border:none\" src=\".\/images\/img_pverror-time-series-inverter-undersized.png\"\/><span class=\"f_ListNoNumber\"> <img alt=\"IMG_PVError-days-inverter-undersized\" style=\"margin:0;width:25.0000rem;height:18.7500rem;border:none\" src=\".\/images\/img_pverror-days-inverter-undersized.png\"\/><\/span><\/p>\n\r<p class=\"p_ListLast\" style=\"text-indent: 0; page-break-inside: avoid; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_Emphasis\">Inverter output less than 75 percent of inverter rated capacity<\/span><span class=\"f_ListLast\">: SAM compares the inverter\'s maximum AC output to the total inverter AC capacity and displays a simulation warning if the inverter\'s maximum AC output is less than 75% of the total inverter rated AC capacity.<\/span><\/p>\n\r<p class=\"p_ListNoNumber\"><span class=\"f_ListNoNumber\">For example, for a system with 400 kWdc array capacity and 750 kWac inverter capacity, SAM displays the following warning message: &quot;pvsamv1: Inverter oversized: The maximum inverter output was 43.13% of the rated value 750 kWac.&quot;<\/span><\/p>\n\r<p class=\"p_ListNoNumber\"><span class=\"f_ListNoNumber\">In this case, the time series graph of gross AC output shows that the inverter output never reaches the 750 kWac capacity.<\/span><\/p>\n\r<p class=\"p_ListNoNumber\"><img alt=\"IMG_PVError-time-series-inverter-oversized\" style=\"margin:0;width:25.0000rem;height:18.7500rem;border:none\" src=\".\/images\/img_pverror-time-series-inverter-oversized.png\"\/><\/p>\n\r"
})

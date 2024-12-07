hmLoadTopic({
hmKeywords:"",
hmTitle:"P50\/P90 Simulations",
hmDescription:"The P50\/P90 simulation option is available for performance models other than Detailed PV and PVWatts, which use the Uncertainty option instead.",
hmPrevLink:"stochastic.html",
hmNextLink:"pvuncertainty.html",
hmParentLink:"simulation_options.html",
hmBreadCrumbs:"<a href=\"simulation_options.html\">Simulation<\/a>",
hmTitlePath:"Simulation > P50\/P90 Simulations",
hmHeader:"<h1 class=\"p_Heading1\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_Heading1\">P50\/P90 Simulations<\/span><\/h1>\n\r",
hmBody:"<p class=\"p_Text\"><span class=\"f_Text\">The P50\/P90 simulation option is available for performance models other than Detailed PV and PVWatts, which use the <a href=\"pvuncertainty.html\" class=\"topiclink\">Uncertainty<\/a> option instead.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">A P90 value is a value that is expected to be met or exceeded 90% of the time. SAM can generate P50 and P90 values for a system\'s annual electricity to grid output and other metrics by running hourly simulations over a multi-year period.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">For a video demonstrating the P50\/P90 capability, <\/span><span class=\"f_List\">see <a href=\"https:\/\/sam.nrel.gov\/simulation-options.html\" target=\"_blank\" class=\"weblink\" title=\"https:\/\/sam.nrel.gov\/simulation-options.html\">Parametric and Statistical Analysis in SAM<\/a><\/span><span class=\"f_Text\">.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">SAM\'s P50\/P90 simulations require at least 10 weather files of consecutive single-year data. The files must be in one of the <a href=\"weather_format.html\" class=\"topiclink\">weather file formats<\/a> that SAM can read and use the naming convention described below.<\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Notes.<\/span><span class=\"f_Note\"> <\/span><br \/>\n\r<span class=\"f_Note\">&nbsp;<\/span><br \/>\n\r<span class=\"f_Note\">P50\/P90 simulations and results are separate from the case simulation and results. It is not possible to display hourly results from P50\/P90 simulations.<\/span><br \/>\n\r<span class=\"f_Note\">&nbsp;<\/span><br \/>\n\r<span class=\"f_Note\">The <\/span><span class=\"f_CHInterfaceElement\">degradation rate<\/span><span class=\"f_Note\"> on the <a href=\"degradation.html\" class=\"topiclink\">Degradation<\/a> page does not affect the annual energy values that SAM reports in the P50\/P90 analysis results. However it does affect the LCOE value because SAM calculates the LCOE over the analysis period for each weather file year and applies the degradation rate to calculate the annual system output over the analysis period.<\/span><br \/>\n\r<span class=\"f_Note\">&nbsp;<\/span><br \/>\n\r<span class=\"f_Note\">For a description of SAM\'s P50\/P90 methodology, see Dobos, A. P.; Gilman, P.; Kasberg, M. (2012). P50\/P90 Analysis for Solar Energy Systems Using the System Advisor Model: Preprint. 8 pp.; NREL Report No. CP-6A20-54488. (<a href=\"http:\/\/www.nrel.gov\/docs\/fy12osti\/54488.pdf\" target=\"_blank\" class=\"weblink\" title=\"http:\/\/www.nrel.gov\/docs\/fy12osti\/54488.pdf\">PDF 372 KB)<\/a><\/span><\/p>\n\r<p class=\"p_TitleProcedure\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_TitleProcedure\">Before running P50\/P90 simulations:<\/span><\/p>\n\r<p class=\"p_ProcedureStepLast\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_ProcedureStep\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_ProcedureStepLast\">If you have your own set of weather files to use for the P50\/P90 simulations, put them all in a single folder with no other files, and make sure that each file name ends with an underscore followed by the year as follows:<\/span><span class=\"f_ProcedureStep\"> <\/span><span class=\"f_Emphasis\">&lt;file name&gt;_&lt;year&gt;.&lt;extension&gt;<\/span><span class=\"f_ProcedureStep\">. For example, <\/span><span class=\"f_Emphasis\">portland_psm_1998.csv<\/span><span class=\"f_ProcedureStep\">, <\/span><span class=\"f_Emphasis\">portland_psm_1999.csv<\/span><span class=\"f_ProcedureStep\">, etc.<\/span><\/p>\n\r<p class=\"p_ListLast\" style=\"text-indent: 0; page-break-inside: avoid; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_ProcedureStep\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_ProcedureStepLast\">If you are downloading files from the National Solar Radiation Database (NSRDB), use the <\/span><span class=\"f_CHInterfaceElement\">Download files for all years (P50\/P90)<\/span><span class=\"f_ProcedureStepLast\"> option to download multiple single-year files for P50\/P90 simulations. SAM will create a folder based on the location name in your weather file download folder, and place the files in the folder with names that follow the <\/span><span class=\"f_Emphasis\">&lt;file name&gt;_&lt;year&gt;.&lt;extension&gt;<\/span><span class=\"f_ListLast\"> convention.<\/span><\/p>\n\r<p class=\"p_TextCenter\"><img alt=\"SS_P50P90-solar-resource-download\" style=\"margin:0 auto 0 auto;width:37.5000rem;height:7.1875rem;border:none\" src=\".\/images\/ss_p50p90-solar-resource-download.png\"\/><\/p>\n\r<p class=\"p_TitleProcedure\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_TitleProcedure\">To run a P50\/P90 simulation:<\/span><\/p>\n\r<p class=\"p_ProcedureStep\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_ProcedureStep\" style=\"display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">1.<\/span><span class=\"f_ProcedureStep\">On the main window, click Configure Simulations to view the Configure Simulations page.<\/span><\/p>\n\r<p class=\"p_ProcedureStepNoNumber\"><img alt=\"SS_MainWIndow_P50P90Button\" style=\"margin:0;width:13.6250rem;height:6.9375rem;border:none\" src=\".\/images\/ss_mainwindow_p50p90button.png\"\/><\/p>\n\r<p class=\"p_ProcedureStep\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_ProcedureStep\" style=\"display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">2.<\/span><span class=\"f_ProcedureStep\">On the P50\/P90 Simulations page, for <\/span><span class=\"f_CHInterfaceElement\">Select weather file folder<\/span><span class=\"f_ProcedureStep\">, click <\/span><span class=\"f_CHInterfaceElement\">...<\/span><span class=\"f_ProcedureStep\"> and navigate to the folder containing the series of single-year weather files.<\/span><\/p>\n\r<p class=\"p_ProcedureStep\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_ProcedureStep\" style=\"display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">3.<\/span><span class=\"f_ProcedureStep\">Click <\/span><span class=\"f_CHInterfaceElement\">Run P50\/P90 analysis<\/span><span class=\"f_ProcedureStep\">.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">SAM displays a table of P50\/90 metrics for the results variables available for the case.<\/span><\/p>\n\r"
})

hmLoadTopic({
hmKeywords:"",
hmTitle:"Degradation",
hmDescription:"The degradation inputs allow you to model a decline in the system\'s output over time due, for example, to aging of equipment.",
hmPrevLink:"ptes_system_control.html",
hmNextLink:"degradation_ac.html",
hmParentLink:"index.html",
hmBreadCrumbs:"",
hmTitlePath:"Degradation",
hmHeader:"<h1 class=\"p_Heading1\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_Heading1\">Degradation<\/span><\/h1>\n\r",
hmBody:"<p class=\"p_Text\"><span class=\"f_Text\">The degradation inputs allow you to model a decline in the system\'s output over time due, for example, to aging of equipment.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">SAM models annual degradation differently for the different performance models:<\/span><\/p>\n\r<p class=\"p_List\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_List\"><a href=\"degradation_ac.html\" class=\"topiclink\">AC degradation<\/a> with single year simulation for all models except detailed photovoltaic, PV battery, custom generation battery, fuel cell, and geothermal models.<\/span><\/p>\n\r<p class=\"p_List\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_List\"><a href=\"degradation_dc.html\" class=\"topiclink\">DC degradation<\/a> with simulation over analysis period for the detailed photovoltaic, PV battery, and fuel cell model, where degradation applies to the DC output of the photovoltaic array. Lifetime simulations allow for modeling the effect of PV module degradation on inverter power limiting losses over time, and for battery replacements where applicable.<\/span><\/p>\n\r<p class=\"p_List\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_List\">For the Detailed Photovoltaic and PVWatts models, when you specify a single degradation rate rather than a table of annual degradation rates, SAM applies the degradation rate linearly. For the other performance models, it applies the degradation rate to the previous year\'s output, so it is effectively compounded.<\/span><\/p>\n\r<p class=\"p_List\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_List\">AC degradation with simulation over analysis period for the custom generation battery model, where the system generates AC power. Lifetime simulations allow for modeling battery replacements.<\/span><\/p>\n\r<p class=\"p_ListLast\" style=\"text-indent: 0; page-break-inside: avoid; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_ListLast\">The geothermal model calculates the electricity generated by the system in each month over its lifetime rather than hourly or subhourly over a single year. The degradation rate is not available for geothermal systems because the model calculates the system\'s electrical output from year to year.<\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Notes.<\/span><br \/>\n\r<span class=\"f_CHNote\">&nbsp;<\/span><br \/>\n\r<span class=\"f_Note\">When you specify degradation in each year, SAM ignores the value you specify for Year 1 (Row 1 of the input table) because it assumes degradation applies in Years 2 and later.<\/span><br \/>\n\r<span class=\"f_Note\">&nbsp;<\/span><br \/>\n\r<span class=\"f_Note\">The multi-year simulation uses the same weather file for each year, so it does not accurately represent the effect of changes in the solar resource and weather from year to year over the analysis period.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The Detailed Photovoltaic and PV Battery models have an additional set of inputs for <a href=\"degradation_dc.html#lifetimedaily\" class=\"topiclink\">lifetime daily losses<\/a> that can be used to model losses in the system that vary over the analysis period and are not covered by annual degradation.<\/span><\/p>\n\r"
})

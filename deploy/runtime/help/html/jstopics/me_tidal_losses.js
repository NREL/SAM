hmLoadTopic({
hmKeywords:"",
hmTitle:"Losses",
hmDescription:"The inputs on the Losses page account for potential sources of energy loss that area not accounted for by the marine energy performance model.",
hmPrevLink:"me_tidal_array.html",
hmNextLink:"geo_overview.html",
hmParentLink:"mhk_tidal.html",
hmBreadCrumbs:"<a href=\"mhk_tidal.html\">Marine Energy Tidal<\/a>",
hmTitlePath:"Marine Energy Tidal > Losses",
hmHeader:"<h1 class=\"p_Heading1\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_Heading1\">Losses<\/span><\/h1>\n\r",
hmBody:"<p class=\"p_Text\"><span class=\"f_Text\">The inputs on the Losses page account for potential sources of energy loss that area not accounted for by the marine energy performance model.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The default 2% transmission loss and a 5% array\/device downtime loss are based on the <a href=\"https:\/\/energy.sandia.gov\/energy\/renewable-energy\/water-power\/technology-development\/reference-model-project-rmp\/\" target=\"_blank\" class=\"weblink\" title=\"https:\/\/energy.sandia.gov\/energy\/renewable-energy\/water-power\/technology-development\/reference-model-project-rmp\/\">reference model project<\/a>. <\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Total Losses<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">SAM applies the total loss to the annual energy production of the system. The total loss percentage is the sum of the different loss categories.<\/span><\/p>\n\r<h4 class=\"p_Heading4\"><span class=\"f_Heading4\">System Availability<\/span><\/h4>\n\r<p class=\"p_Text\"><span class=\"f_Text\">System availability losses are reductions in the system\'s output due to operational requirements such as maintenance down time or other situations that prevent the system from operating as designed. <\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Notes.<\/span><span class=\"f_Note\"> <\/span><br \/>\n\r<span class=\"f_Note\">&nbsp;<\/span><br \/>\n\r<span class=\"f_Note\">To model curtailment, or forced outages or reduction in power output required by the grid operator, use the inputs on the <a href=\"grid_limits.html\" class=\"topiclink\">Grid Limits<\/a> page. The Grid Limits page is not available for all performance models.<\/span><br \/>\n\r<span class=\"f_Note\">&nbsp;<\/span><br \/>\n\r<span class=\"f_Note\">For the PV Battery model, battery dispatch is affected by the system availability losses. For the PVWatts Battery, Generic Battery, and Standalone Battery battery dispatch ignores the system availability losses.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">To edit the system availability losses, click <\/span><span class=\"f_CHInterfaceElement\">Edit losses<\/span><span class=\"f_Text\">.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The <a href=\"edit_losses.html\" class=\"topiclink\">Edit Losses<\/a> window allows you to define loss factors as follows:<\/span><\/p>\n\r<p class=\"p_List\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_List\">Constant loss is a single loss factor that applies to the system\'s entire output. You can use this to model an availability factor.<\/span><\/p>\n\r<p class=\"p_ListLast\" style=\"text-indent: 0; page-break-inside: avoid; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_ListLast\">Time series &nbsp;losses apply to specific time steps.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">SAM reduces the system\'s output in each time step by the loss percentage that you specify for that time step. For a given time step, a loss of zero would result in no adjustment. A loss of 5% would reduce the output by 5%, and a loss of -5% would increase the output value by 5%.<\/span><\/p>\n\r"
})

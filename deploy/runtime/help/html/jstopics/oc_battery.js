hmLoadTopic({
hmKeywords:"",
hmTitle:"Battery Operating Costs",
hmDescription:"Operating costs represent annual expenditures on equipment and services that occur after the system is installed. ",
hmPrevLink:"oc_operating.html",
hmNextLink:"oc_pv-battery.html",
hmParentLink:"operating_costs.html",
hmBreadCrumbs:"<a href=\"operating_costs.html\">Operating Costs<\/a>",
hmTitlePath:"Operating Costs > Battery Operating Costs",
hmHeader:"<h1 class=\"p_Heading1\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_Heading1\">Battery Operating Costs<\/span><\/h1>\n\r",
hmBody:"<p class=\"p_Text\"><span class=\"f_Text\">Operating costs represent annual expenditures on equipment and services that occur after the system is installed. <\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The inputs that appear on the Operating Costs page depend on whether you are using the Standalone Battery, &nbsp;Generic-Battery or a hybrid system configuration.<\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Notes.<\/span><span class=\"f_Note\"> <\/span><br \/>\n\r<span class=\"f_Note\">&nbsp;<\/span><br \/>\n\r<span class=\"f_Note\">The operating cost inputs are in Year 1 dollars. SAM applies the inflation rate from the <\/span><span class=\"f_VariableDescription\"><a href=\"financial_parameters.html\" class=\"topiclink\">Financial Parameters<\/a><\/span><span class=\"f_Note\"> page to calculate the operating cost in each year of the cash flow. If you specify a non-zero escalation rate for an operating cost category, it applies both the inflation rate and escalation rate.<\/span><br \/>\n\r<span class=\"f_Note\">&nbsp;<\/span><br \/>\n\r<span class=\"f_Note\">For financial models with reserve accounts on the Financial Parameters page, you can use either the operating costs or the major equipment replacement reserve accounts for the cost of major equipment replacements.<\/span><br \/>\n\r<span class=\"f_Note\">&nbsp;<\/span><br \/>\n\r<span class=\"f_Text\">For expenses such as component replacements that occur in particular years, you can use an <a href=\"oc_battery.html#annualschedule\" class=\"topiclink\">annual schedule<\/a> to assign costs to individual years.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Nameplate capacity<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The nominal storage capacity of the battery in DC kilowatt-hours from the Battery Cell and System page..<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">For the Custom Generation Profile - Battery configuration, the system capacity in AC kilowatts is from the <a href=\"custom_generation_profile.html\" class=\"topiclink\">Generation Profile<\/a> page. <\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Fixed annual cost, \$\/yr<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">A fixed annual cost that applies to each year in the project cash flow.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Fixed cost by capacity<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">A fixed annual cost per unit of nameplate capacity as defined above.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Variable cost by generation<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">A variable annual cost proportional to the annual output of the photovoltaic system (MWac) or power plant (MWac), and annual energy discharged by the battery (MWac). The variable operating cost of the battery is calculated from the total energy discharged to the grid and load (for behind-the-meter batteries).<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Replacement cost, \$\/kWhdc of battery nameplate capacity<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The replacement cost in Year 1 dollars per unit of nameplate battery capacity as defined above.<\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Note.<\/span><span class=\"f_Note\"> To include battery replacements costs in your analysis, be sure to choose a battery replacement option on the <a href=\"battery_life.html\" class=\"topiclink\">Battery Life<\/a> page.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Escalation rate, %\/yr<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">For each operating cost category, you can specify an optional annual <\/span><span class=\"f_CHInterfaceElement\">Escalation Rate<\/span><span class=\"f_VariableDescription\"> to represent an expected annual increase or decrease in operating cost above the annual inflation rate specified on the <a href=\"financial_parameters.html\" class=\"topiclink\">Financial Parameters<\/a> page.<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Specify an escalation rate of zero for an operating cost that increases annually at the rate of inflation, a positive escalation rate for a cost that increases at a higher rate than the inflation rate, or a negative escalation rate for a cost that increases at a lower rate than the inflation rate. (Set the inflation rate to the negative value of the inflation rate for a cost that does not increase with inflation.)<\/span><\/p>\n\r<div style=\"text-align: left; text-indent: 0; border-color: #0072c6; border-style: solid; border-width: thin; background: #ebebeb; padding: 0.1250rem 0.1250rem 0.1250rem 0.1250rem; margin: 0.1875rem 0 0.1875rem 0;\"><table style=\"border:none;border-spacing:0;padding:0;line-height: normal;\"><tr style=\"vertical-align:baseline\"><td style=\"border:none;padding:0;width:1.1875rem\"><img id=\"toggle0186a1_ICON\" class=\"dropdown-toggle-icon\" alt=\"Click to expand or collapse\" title=\"Click to expand or collapse\" style=\"margin:0;width:1.0000rem;height:1.0000rem;border:none\" src=\".\/images\/ico-plus-16x16.png\"\/><\/td><td style=\"border:none;padding:0\"><span class=\"f_Heading2_atoc_\"><a id=\"toggle0186a1_LINK\" class=\"dropdown-toggle\" style=\"font-style: normal; font-weight: bold; color: #0072c6; background-color: transparent; text-decoration: none;\" title=\"Click to expand or collapse\" href=\"javascript:void(0)\" data-type=\"dropdown\" data-state=\"0\" data-icon=\"toggle0186a1_ICON\" data-src0=\".\/images\/ico-plus-16x16.png\" data-src1=\".\/images\/ico-minus-16x16.png\">Land Lease Costs<\/a><\/span><\/td><\/tr><\/table><\/div>\n\r<div id=\"toggle0186a1\" class=\"dropdown-toggle-body\" style=\"text-align: left; text-indent: 0; padding: 0 0 0 0; margin: 0 0 0.3750rem 0;display:none\"><table class=\"ToggleContentTable\" >\n\r<tr class=\"ToggleContentTable\">\n\r<td class=\"ToggleContentTable\"><p class=\"p_Text\"><span class=\"f_Text\">For front-of-meter projects and performance models that estimate a land area, you can model land lease payments that are treated as an operating cost in the project cash flow.<\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Note.<\/span><span class=\"f_Note\"> Land lease costs are not available for the distributed behind-the-meter financial models: Residential, Commercial, or Third Party Ownership models.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Land area estimate, acres and ha<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The calculated estimate of land area required for the project from the System Design page for the photovoltaic models, or from the Solar Field or Heliostat Field page for concentrating solar power (CSP) models.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Annual land lease cost, \$\/acre or \$\/ha<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The annual lease cost. Choose the units to specify the cost in either \$\/acre or \$\/hectare. You can specify the cost in Year 1 dollars with an optional escalation rate, or click <\/span><img alt=\"SS_AnnSched-valschedbutton\" style=\"margin:0;width:1.4375rem;height:1.5000rem;border:none\" src=\".\/images\/ss_annsched-valschedbutton.png\"\/> to specify the cost by year.<\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Land lease escalation rate<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">If you specify the land lease cost as a single value, you can specify an escalation rate to model land lease payments that increase annually. SAM applies the escalation rate in addition to the inflation rate on the Financial Parameters page.<\/span><\/p>\n\r<\/td>\n\r<\/tr>\n\r<\/table>\n\r<\/div>\n\r<div style=\"text-align: left; text-indent: 0; border-color: #0072c6; border-style: solid; border-width: thin; background: #ebebeb; padding: 0.1250rem 0.1250rem 0.1250rem 0.1250rem; margin: 0.1875rem 0 0.1875rem 0;\"><table style=\"border:none;border-spacing:0;padding:0;line-height: normal;\"><tr style=\"vertical-align:baseline\"><td style=\"border:none;padding:0;width:1.1875rem\"><img id=\"toggle0186a2_ICON\" class=\"dropdown-toggle-icon\" alt=\"Click to expand or collapse\" title=\"Click to expand or collapse\" style=\"margin:0;width:1.0000rem;height:1.0000rem;border:none\" src=\".\/images\/ico-plus-16x16.png\"\/><a id=\"annualschedule\" class=\"hmanchor\"><\/a><\/td><td style=\"border:none;padding:0\"><span class=\"f_Heading2_atoc_\"><a id=\"toggle0186a2_LINK\" class=\"dropdown-toggle\" style=\"font-style: normal; font-weight: bold; color: #0072c6; background-color: transparent; text-decoration: none;\" title=\"Click to expand or collapse\" href=\"javascript:void(0)\" data-type=\"dropdown\" data-state=\"0\" data-icon=\"toggle0186a2_ICON\" data-src0=\".\/images\/ico-plus-16x16.png\" data-src1=\".\/images\/ico-minus-16x16.png\">Using Annual Schedules to Specify Operating Costs in Specific Years<\/a><\/span><\/td><\/tr><\/table><\/div>\n\r<div id=\"toggle0186a2\" class=\"dropdown-toggle-body\" style=\"text-align: left; text-indent: 0; padding: 0 0 0 0; margin: 0 0 0.3750rem 0;display:none\"><table class=\"ToggleContentTable\" >\n\r<tr class=\"ToggleContentTable\">\n\r<td class=\"ToggleContentTable\"><p class=\"p_Text\"><span class=\"f_Text\">SAM allows you to operating costs either as a single annual cost, or as an annual schedule or table of costs. An annual schedule makes it possible to assign costs to particular years in the analysis period. Annual schedules can be used to account for component replacement costs and other periodic costs that do not recur on a regular annual basis.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">You choose whether to specify an operating cost as a single annual value or an annual schedule with the grey and blue button next to each variable. SAM uses the option indicated by the blue highlight on the button: A blue highlighted “Value” indicates a single, regularly occurring annual value. A blue highlighted “Sched” indicates that the value is specified as an annual schedule.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">For example, to account for component replacement costs, you can specify the fixed annual cost category as an annual schedule, and assign the cost of replacing or rebuilding the component to particular years. For a 30-year project using a component with a seven-year life, you would assign a replacement cost to years seven, 14, and 21. Or, to account for expected improvements in the component\'s reliability in the future, you could assign component replacement costs in years seven, 17, and 27. After running a simulation, you can see the replacement costs in the project <a href=\"cashflow.html\" class=\"topiclink\">cash flow<\/a> in the appropriate column under Operating Expenses. SAM accounts for the operating costs in the other economic metrics including the levelized cost of energy and net present value (NPV).<\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHInterfaceElement\">Notes.<\/span><span class=\"f_Note\"> <\/span><br \/>\n\r<span class=\"f_Note\">&nbsp;<\/span><br \/>\n\r<span class=\"f_Note\">If you use the annual schedule option to specify equipment replacement costs, SAM does not calculate any residual or salvage value of system components based on the annual schedule. SAM calculates salvage value separately, using the salvage value you specify on the <a href=\"generic_solar_system.html\" class=\"topiclink\">Financial Parameters<\/a> page.<\/span><br \/>\n\r<span class=\"f_Note\">&nbsp;<\/span><br \/>\n\r<span class=\"f_Text\">Dollar values in the annual schedule are in nominal or current dollars. SAM does not apply inflation and escalation rates to values in annual schedules.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The following procedure describes how to define the fixed annual cost category as an annual schedule. You can use the same procedure for any of the other operation and maintenance cost categories.<\/span><\/p>\n\r<p class=\"p_TitleProcedure\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_TitleProcedure\">To assign component replacement costs to particular years:<\/span><\/p>\n\r<p class=\"p_ProcedureStep\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_ProcedureStep\" style=\"display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">1.<\/span><span class=\"f_ProcedureStep\">In the Fixed Annual Cost category, note that the &quot;Value&quot; label of the grey and blue button is blue indicating that the single value mode is active for the variable.<\/span><\/p>\n\r<p class=\"p_ProcedureStepNoNumber\"><img alt=\"SS_AnnSched-Value-OM\" style=\"margin:0;width:19.2500rem;height:1.6875rem;border:none\" src=\".\/images\/ss_annsched-value-om.png\"\/><\/p>\n\r<p class=\"p_ProcedureStepNoNumber\"><span class=\"f_ProcedureStepNoNumber\">In this case, SAM would assign an annual cost of \$284 to each year in the project cash flow.<\/span><\/p>\n\r<p class=\"p_ProcedureStep\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_ProcedureStep\" style=\"display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">2.<\/span><span class=\"f_ProcedureStep\">Click the button so that &quot;Sched&quot; label is highlighted in blue. SAM replaces the variable\'s value with an Edit button.<\/span><\/p>\n\r<p class=\"p_ProcedureStepNoNumber\"><img alt=\"SS_AnnSched-SchedEdit\" style=\"margin:0;width:5.5625rem;height:1.4375rem;border:none\" src=\".\/images\/ss_annsched-schededit.png\"\/><\/p>\n\r<p class=\"p_ProcedureStep\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_ProcedureStep\" style=\"display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">3.<\/span><span class=\"f_ProcedureStep\">Click <\/span><span class=\"f_CHInterfaceElement\">Edit<\/span><span class=\"f_ProcedureStepLast\"> to open the <a href=\"edit_data_table_column.html\" class=\"topiclink\">Edit Schedule<\/a> window.<\/span><\/p>\n\r<p class=\"p_ProcedureStep\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_ProcedureStep\" style=\"display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">4.<\/span><span class=\"f_ProcedureStep\">In the Edit Schedule window, use the vertical scroll bar to find the year of the first replacement, and type the replacement cost in current or constant dollars for that year.<\/span><\/p>\n\r<p class=\"p_ProcedureStepNoNumber\"><span class=\"f_ProcedureStepNoNumber\">To delete a value, select it and press the Delete key on your keyboard.<\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Note.<\/span><span class=\"f_Note\"> You must type a value for each year. If you delete a value, SAM will clear the cell, and you must type a number in the cell or SAM will consider the schedule to be invalid. Type a zero for years with no annual costs.<\/span><\/p>\n\r<p class=\"p_ProcedureStepLast\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_ProcedureStep\" style=\"display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">5.<\/span><span class=\"f_ProcedureStepLast\">When you have finished editing the schedule, click <\/span><span class=\"f_CHInterfaceElement\">Accept<\/span><span class=\"f_ProcedureStepLast\">.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">Because you must specify an operating cost category as either an annual cost or annual schedule, to assign both a recurring annual fixed cost and periodic replacement cost, you must type the recurring cost in each year of the annual schedule, and for years with replacement costs, type the sum of the recurring and replacement costs.<\/span><\/p>\n\r<\/td>\n\r<\/tr>\n\r<\/table>\n\r<\/div>\n\r"
})

hmLoadTopic({
hmKeywords:"",
hmTitle:"Trough Installation Costs",
hmDescription:"Installation Costs are costs associated with installing the system, and include equipment, labor, engineering, permitting, and any other costs that apply in Year 0 of the...",
hmPrevLink:"cc_custom_generation.html",
hmNextLink:"cc_tower.html",
hmParentLink:"installation_costs.html",
hmBreadCrumbs:"<a href=\"installation_costs.html\">Installation Costs<\/a>",
hmTitlePath:"Installation Costs > Trough Installation Costs",
hmHeader:"<h1 class=\"p_Heading1\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_Heading1\">Trough Installation Costs<\/span><\/h1>\n\r",
hmBody:"<p class=\"p_Text\"><span class=\"f_Text\">Installation Costs are costs associated with installing the system, and include equipment, labor, engineering, permitting, and any other costs that apply in Year 0 of the project cash flow. Some costs, such as debt-related and sales tax costs are specified on the <a href=\"financial_parameters.html\" class=\"topiclink\">Financial Parameters<\/a> page.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">SAM uses the variables on the Installation Costs page to calculate the project investment cost.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">Recurring costs that apply in Years 1 and later of the project cash flow are on the <a href=\"operating_costs.html\" class=\"topiclink\">Operating Costs<\/a> page.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">Variable values in boxes with white backgrounds are values that you can edit. Boxes with blue backgrounds contain calculated values or values from other pages that SAM displays for your information.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">SAM provides the categories under <\/span><span class=\"f_CHInterfaceElement\">Direct Capital Costs<\/span><span class=\"f_Text\"> and <\/span><span class=\"f_CHInterfaceElement\">Indirect Capital Costs<\/span><span class=\"f_Text\"> for your convenience to help keep track of project installation costs. Only the <\/span><span class=\"f_CHInterfaceElement\">Total Installed Cost<\/span><span class=\"f_Text\"> value affects the cash flow calculations, so you can assign capital costs to the different cost categories in whatever way makes sense for your analysis. For example, you could assign the cost of designing the array to the module cost category or to the engineering category with equivalent results. After you assign costs to the categories, you should verify that the total installed cost value is what you expect.<\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">CSP Installation Costs Notes.<\/span><span class=\"f_Note\"> <\/span><br \/>\n\r<span class=\"f_Note\">&nbsp;<\/span><br \/>\n\r<span class=\"f_Note\">The default cost values that appear when you create a file or case are intended to illustrate SAM\'s use. The cost data are meant to be realistic, but not to represent actual costs for a specific project. For more information and a list of online resources for cost data, see the <a href=\"https:\/\/sam.nrel.gov\/concentrating-solar-power\/csp-cost-data\" target=\"_blank\" class=\"weblink\" title=\"https:\/\/sam.nrel.gov\/concentrating-solar-power\/csp-cost-data\">SAM website<\/a>.<\/span><br \/>\n\r<span class=\"f_Note\">&nbsp;<\/span><br \/>\n\r<span class=\"f_Note\">The direct capital costs in \$\/kWe are in kilowatts of gross power block capacity rather than nameplate capacity because the size and cost of the power block is determined by the gross capacity, not the net capacity. The total installed cost in \$\/kWe (actually overnight installed cost because it does not include financing during construction costs, which are accounted for on the <a href=\"financial_parameters.html\" class=\"topiclink\">Financial Parameters<\/a> page) is in kilowatts of nameplate capacity, because that is what is delivered to the grid and is consistent with how costs are reported for utility generation technologies. The indirect costs in \$\/Wac are in Watts of nameplate power block capacity because those costs that use the entire plant as the basis, not just the power block.<\/span><br \/>\n\r<span class=\"f_Note\">&nbsp;<\/span><br \/>\n\r<span class=\"f_Note\">The Installation Costs page is only available for cases with a cash-flow-based financial mode. It is not available with the No Financial Model option or the LCOE Calculator financial model.<\/span><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><span class=\"f_Heading3_atocs_\">Direct Capital Costs<\/span><\/h3>\n\r<p class=\"p_Text\"><span class=\"f_Text\">A direct capital cost represents an expense for a specific piece of equipment or installation service that applies in year zero of the cash flow.<\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Note:<\/span><span class=\"f_Note\"> Because SAM uses only the total installed cost value in cash flow calculations, how you distribute costs among the different direct capital cost categories does not affect the final results.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Site Improvements (\$\/m<\/span><span class=\"f_VariableName\" style=\"font-size: 0.70rem; vertical-align: super;\">2<\/span><span class=\"f_VariableName\">)<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">A cost per square meter of solar field area to account for expenses related to site preparation and other equipment not included in the solar field cost category.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Solar Field (\$\/m<\/span><span class=\"f_VariableName\" style=\"font-size: 0.70rem; vertical-align: super;\">2<\/span><span class=\"f_VariableName\">)<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">A cost per square meter of solar field area to account for expenses related to installation of the solar field, including labor and equipment.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">HTF System (\$\/m<\/span><span class=\"f_VariableName\" style=\"font-size: 0.70rem; vertical-align: super;\">2<\/span><span class=\"f_VariableName\">)<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">A cost per square meter of solar field area to account for expenses related to installation of the heat transfer fluid pumps and piping, including labor and equipment.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Storage (\$\/kWht)<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Cost per thermal megawatt-hour of storage capacity to account for expenses related to installation of the thermal storage system, including equipment and labor.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Fossil Backup (\$\/kWe)<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Cost per electric megawatt of power block gross capacity to account for the installation of a fossil backup system, including equipment and labor.<\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Note.<\/span><span class=\"f_Text\"> <\/span><span class=\"f_Note\">In versions of SAM released after February 2020, fossil backup is not available for the Physical Trough model because it was not incorporated into the new dispatch controller logic at the time of the software release, so the Fossil Backup cost should be zero. If you want to use fossil backup, use version SAM 2018.11.11, available on the SAM website <a href=\"https:\/\/sam.nrel.gov\/download\" target=\"_blank\" class=\"weblink\" title=\"https:\/\/sam.nrel.gov\/download\">Download page<\/a>.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Power Plant (\$\/kWe)<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Cost per electric megawatt of power block gross capacity to account for the installation of the power block, including equipment and labor.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Balance of Plant (\$\/kWe)<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Cost per electric megawatt of power block gross capacity to account for additional costs.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Contingency (%)<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">A percentage of the sum of the site improvements, solar field, HTF system, storage, fossil backup, and power plant costs to account for expected uncertainties in direct cost estimates.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Total Direct Cost (\$)<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The sum of improvements, solar field, HTF system, storage, fossil backup, power plant costs, and contingency costs.<\/span><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><span class=\"f_Heading3_atocs_\">Indirect Capital Costs<\/span><\/h3>\n\r<p class=\"p_Text\"><span class=\"f_Text\">An indirect cost is typically one that cannot be identified with a specific piece of equipment or installation service.<\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Note:<\/span><span class=\"f_Note\"> Because SAM uses only the total installed cost value in cash flow calculations, how you distribute costs among the different indirect capital cost categories does not affect the final results.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Total Land Area<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The total land area required for the project, from the Solar Field or Heliostat Field page.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Nameplate<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The system\'s nameplate capacity from the Power Block or Power Cycle page.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">EPC and Owner Cost<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">EPC (engineer-procure-construct) and owner costs are associated with the design and construction of the project. SAM calculates the total cost as the sum of the Non-fixed Cost and Fixed Cost.<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Typical costs that may be appropriate to include in the EPC and Owner category are: Permitting, royalty payments, consulting, management or legal fees, geotechnical and environmental surveys, interconnection costs, spare parts inventories, commissioning costs, and the owner\'s engineering and project development activities.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Total Land Costs<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Costs associated with land purchases, which SAM calculates as the sum of a non-fixed cost and a fixed cost. Use the Land category described below for land costs, and inputs on the <a href=\"financial_parameters.html\" class=\"topiclink\">Financial Parameters<\/a> page for financing costs.<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">SAM calculates the total EPC and Owner Costs and Total Land Costs by adding the four costs that you can specify using different units.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">\$\/acre<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">A cost in dollars per total land area in acres.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">% of direct cost<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">A cost as a percentage of <\/span><span class=\"f_CHInterfaceElement\">Total Direct Cost<\/span><span class=\"f_VariableDescription\"> under <\/span><span class=\"f_CHInterfaceElement\">Direct Capital Cost<\/span><span class=\"f_VariableDescription\">.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">\$\/Wac<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">A cost in dollars per AC Watt of nameplate capacity.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">\$<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">A fixed dollar amount<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Total Indirect Cost (\$)<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The sum of engineer-procure-construct costs, project-land-miscellaneous costs.<\/span><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><span class=\"f_Heading3_atocs_\">Sales Tax<\/span><\/h3>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Sales tax basis, %<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The percentage of total direct cost used to the calculate sales tax amount.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">SAM calculates the total sales tax amount by multiplying the sales tax rate from the <a href=\"financial_parameters.html\" class=\"topiclink\">Financial Parameters<\/a> page by the sales tax basis on the Installation Costs page: <\/span><\/p>\n\r<p class=\"p_EquationExcel\"><span class=\"f_EquationExcel\">Total Sales Tax (\$) = Sales Tax Rate (%) × Sales Tax Basis (%) × Total Direct Cost (\$)<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">For an explanation of the effect of sales tax on income tax, see <\/span><span class=\"f_CHInterfaceElement\">Sales Tax<\/span><span class=\"f_Text\"> on the <a href=\"financial_parameters.html\" class=\"topiclink\">Financial Parameters<\/a> topic for the financial model you are using (Residential, Commercial, Single Owner, etc.)<\/span><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><span class=\"f_Heading3_atocs_\">Total Installed Cost<\/span><\/h3>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The total installed cost is the sum of all of the direct and indirect capital costs and sales tax. SAM uses this value to calculate the project\'s <a href=\"mtf_costs.html\" class=\"topiclink\">net capital cost<\/a>, which is the total installed cost less any cash incentives on the <a href=\"incentives.html\" class=\"topiclink\">Incentives<\/a> page and plus any additional financing costs from <a href=\"financial_parameters.html\" class=\"topiclink\">Financial Parameters<\/a> page.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Total installed cost, \$<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The sum of total direct cost and total indirect cost.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Total installed cost per capacity<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Total installed cost divided by the total system rated or nameplate capacity. This value is provided for reference only. SAM does not use it in cash flow calculations.<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The units depend on the type of system: For photovoltaic systems: \$\/Wdc, for standalone battery systems: \$\/Wac, for other systems: \$\/kWac.<\/span><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><span class=\"f_Heading3_atocs_\">About the CSP Default Cost Assumptions<\/span><\/h3>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The default values the Installation Costs and Operating Costs pages for the CSP models reflect the National Renewable Energy Laboratory\'s best estimate of representative costs for CSP systems in the United States at the time of the release of the current version of SAM. The values are based on cost studies undertaken by NREL, review of published literature, and conversations with developers and industry insiders. Costs are reviewed prior to each new SAM release.<\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Note.<\/span><span class=\"f_Note\"> Always review all of the inputs for your SAM project to determine whether they are appropriate for your analysis.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The parabolic trough and power tower default cost values were largely developed from the following studies:<\/span><\/p>\n\r<p class=\"p_List\"><span class=\"f_List\">Kurup, P.; Turchi, C. S. &nbsp;(2015). “Parabolic Trough Collector Cost Update for the System Advisor Model (SAM),” NREL\/TP-6A20-65228, 2015. (<a href=\"http:\/\/www.nrel.gov\/docs\/fy16osti\/65228.pdf\" target=\"_blank\" class=\"weblink\" title=\"http:\/\/www.nrel.gov\/docs\/fy16osti\/65228.pdf\">PDF 2.1 MB<\/a>)<\/span><\/p>\n\r<p class=\"p_List\"><span class=\"f_List\">Turchi, C. S.; &nbsp;Heath, G. A. (2013). “Molten Salt Power Tower Cost Model for the System Advisor Model (SAM),” NREL\/TP-5500-57625, 2013. (<a href=\"http:\/\/www.nrel.gov\/docs\/fy13osti\/57625.pdf\" target=\"_blank\" class=\"weblink\" title=\"http:\/\/www.nrel.gov\/docs\/fy13osti\/57625.pdf\">PDF 2.5 MB<\/a>)<\/span><\/p>\n\r<p class=\"p_List\"><span class=\"f_List\">Kolb, G. J.; Ho, C. K. (2011), T. R. Mancini, and J. A. Gary, &quot;Power Tower Technology Roadmap and Cost Reduction Plan,&quot; SAND2011-2419. (<a href=\"http:\/\/prod.sandia.gov\/techlib\/access-control.cgi\/2011\/112419.pdf\" target=\"_blank\" class=\"weblink\" title=\"http:\/\/prod.sandia.gov\/techlib\/access-control.cgi\/2011\/112419.pdf\">PDF 503 KB<\/a>)<\/span><\/p>\n\r<p class=\"p_ListLast\" style=\"page-break-inside: avoid;\"><span class=\"f_ListLast\">Turchi, C. S. (2010). &quot;Parabolic Trough Reference Plant for Cost Modeling with the Solar Advisor Model (SAM),&quot; NREL\/TP-550-47605. (<a href=\"http:\/\/www.nrel.gov\/docs\/fy10osti\/47605.pdf\" target=\"_blank\" class=\"weblink\" title=\"http:\/\/www.nrel.gov\/docs\/fy10osti\/47605.pdf\">PDF 7.2 MB<\/a>)<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">These studies differed in some important assumptions. The differences in the location and cooling method assumptions for the default cases of the CSP technologies are outlined in the following table. The choice of location affects solar resource and the assumed labor costs associated with the case. Construction labor rates in Arizona tend to be lower than in California, which reduces installed costs. For the Winter 2016 version of SAM, all CSP systems are assumed to use dry cooling. SAM can model systems with dry cooling, wet cooling, or a hybrid of the two. If you change the type of cooling system on the Power Cycle input page, you should also change other parameters as appropriate because SAM does not make those changes automatically. For example, different types of cooling systems would require different power cycle costs and design-point performance parameters. Perhaps less obvious, the site preparation costs are lower for dry-cooled (air-cooled) systems than for wet-cooled (evaporative) systems because of the elimination of the large blowdown evaporation ponds required for wet systems.<\/span><\/p>\n\r<div style=\"text-align: center; text-indent: 0; page-break-inside: avoid; padding: 0 0 0 0; margin: 0 0 0 0;\"><table style=\"width:100%; border:none; border-spacing:0;\">\n\r<tr>\n\r<td style=\"vertical-align:top; padding:0.2500rem; border:none\"><p class=\"p_TableHeadColumn\" style=\"page-break-inside: avoid;\"><span class=\"f_TableHeadColumn\">Technology<\/span><\/p>\n\r<\/td>\n\r<td style=\"vertical-align:top; padding:0.2500rem; border:none\"><p class=\"p_TableHeadColumn\" style=\"page-break-inside: avoid;\"><span class=\"f_TableHeadColumn\">Default Location<\/span><\/p>\n\r<\/td>\n\r<td style=\"vertical-align:top; padding:0.2500rem; border:none\"><p class=\"p_TableHeadColumn\" style=\"page-break-inside: avoid;\"><span class=\"f_TableHeadColumn\">Default Cooling method<\/span><\/p>\n\r<\/td>\n\r<\/tr>\n\r<tr>\n\r<td style=\"vertical-align:top; background-color:#C0C0C0; padding:0.2500rem; border:none\"><p class=\"p_TableTextLeft\" style=\"page-break-inside: avoid;\"><span class=\"f_TableTextLeft\">Physical Trough<\/span><\/p>\n\r<\/td>\n\r<td style=\"vertical-align:top; background-color:#C0C0C0; padding:0.2500rem; border:none\"><p class=\"p_TableTextLeft\" style=\"page-break-inside: avoid;\"><span class=\"f_TableTextLeft\">SW Arizona (Tucson weather file)<\/span><\/p>\n\r<\/td>\n\r<td style=\"vertical-align:top; background-color:#C0C0C0; padding:0.2500rem; border:none\"><p class=\"p_TableTextLeft\" style=\"page-break-inside: avoid;\"><span class=\"f_TableTextLeft\">Dry<\/span><\/p>\n\r<\/td>\n\r<\/tr>\n\r<tr>\n\r<td style=\"vertical-align:top; padding:0.2500rem; border:none\"><p class=\"p_TableTextLeft\" style=\"page-break-inside: avoid;\"><span class=\"f_TableTextLeft\">Empirical Trough<\/span><\/p>\n\r<\/td>\n\r<td style=\"vertical-align:top; padding:0.2500rem; border:none\"><p class=\"p_TableTextLeft\" style=\"page-break-inside: avoid;\"><span class=\"f_TableTextLeft\">SW Arizona (Tucson weather file)<\/span><\/p>\n\r<\/td>\n\r<td style=\"vertical-align:top; padding:0.2500rem; border:none\"><p class=\"p_TableTextLeft\" style=\"page-break-inside: avoid;\"><span class=\"f_TableTextLeft\">Dry<\/span><\/p>\n\r<\/td>\n\r<\/tr>\n\r<tr>\n\r<td style=\"vertical-align:top; background-color:#C0C0C0; padding:0.2500rem; border:none\"><p class=\"p_TableTextLeft\" style=\"page-break-inside: avoid;\"><span class=\"f_TableTextLeft\">Molten Salt Power Tower<\/span><\/p>\n\r<\/td>\n\r<td style=\"vertical-align:top; background-color:#C0C0C0; padding:0.2500rem; border:none\"><p class=\"p_TableTextLeft\" style=\"page-break-inside: avoid;\"><span class=\"f_TableTextLeft\">Southern California (Daggett weather file)<\/span><\/p>\n\r<\/td>\n\r<td style=\"vertical-align:top; background-color:#C0C0C0; padding:0.2500rem; border:none\"><p class=\"p_TableTextLeft\" style=\"page-break-inside: avoid;\"><span class=\"f_TableTextLeft\">Dry<\/span><\/p>\n\r<\/td>\n\r<\/tr>\n\r<tr>\n\r<td style=\"vertical-align:top; padding:0.2500rem; border:none\"><p class=\"p_TableTextLeft\" style=\"page-break-inside: avoid;\"><span class=\"f_TableTextLeft\">Linear Fresnel<\/span><\/p>\n\r<\/td>\n\r<td style=\"vertical-align:top; padding:0.2500rem; border:none\"><p class=\"p_TableTextLeft\" style=\"page-break-inside: avoid;\"><span class=\"f_TableTextLeft\">SW Arizona (Tucson weather file)<\/span><\/p>\n\r<\/td>\n\r<td style=\"vertical-align:top; padding:0.2500rem; border:none\"><p class=\"p_TableTextLeft\" style=\"page-break-inside: avoid;\"><span class=\"f_TableTextLeft\">Dry<\/span><\/p>\n\r<\/td>\n\r<\/tr>\n\r<tr>\n\r<td style=\"vertical-align:top; background-color:#C0C0C0; padding:0.2500rem; border:none\"><p class=\"p_TableTextLeft\" style=\"page-break-inside: avoid;\"><span class=\"f_TableTextLeft\">Generic Solar<\/span><\/p>\n\r<\/td>\n\r<td style=\"vertical-align:top; background-color:#C0C0C0; padding:0.2500rem; border:none\"><p class=\"p_TableTextLeft\" style=\"page-break-inside: avoid;\"><span class=\"f_TableTextLeft\">SW Arizona (Tucson weather file)<\/span><\/p>\n\r<\/td>\n\r<td style=\"vertical-align:top; background-color:#C0C0C0; padding:0.2500rem; border:none\"><p class=\"p_TableTextLeft\" style=\"page-break-inside: avoid;\"><span class=\"f_TableTextLeft\">Dry<\/span><\/p>\n\r<\/td>\n\r<\/tr>\n\r<\/table>\n\r<\/div>\n\r<p class=\"p_Text\"><span class=\"f_Text\">&nbsp;<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The trough, tower, and linear Fresnel models assume the “balance of plant” cost category is composed of the steam generation system (Kolb, 2011). This choice is made to allow users to highlight the effect of a direct steam generation (DSG) design. In a DSG design, the balance of plant cost category is zero because steam generation occurs in the solar receiver.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The lesser commercial activity in linear Fresnel systems makes cost values for those technologies more uncertain than for troughs and towers. Linear Fresnel costs are estimated based on discussions with developers and researchers.<\/span><\/p>\n\r"
})

hmLoadTopic({
hmKeywords:"",
hmTitle:"Wind Installation Costs",
hmDescription:"The Wind Installation Costs page allows you to specify the costs of a wind power project. For information about sources of wind cost data, see https:\/\/sam.nrel.gov\/wind.html.",
hmPrevLink:"cc_standalone_battery.html",
hmNextLink:"cc_geothermal.html",
hmParentLink:"installation_costs.html",
hmBreadCrumbs:"<a href=\"installation_costs.html\">Installation Costs<\/a>",
hmTitlePath:"Installation Costs > Wind Installation Costs",
hmHeader:"<h1 class=\"p_Heading1\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_Heading1\">Wind Installation Costs<\/span><\/h1>\n\r",
hmBody:"<p class=\"p_Text\"><span class=\"f_Text\">The Wind Installation Costs page allows you to specify the costs of a wind power project. For information about sources of wind cost data, see <a href=\"https:\/\/sam.nrel.gov\/wind.html\" target=\"_blank\" class=\"weblink\" title=\"https:\/\/sam.nrel.gov\/wind.html\">https:\/\/sam.nrel.gov\/wind.html<\/a>.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">Installation Costs are costs associated with installing the system, and include equipment, labor, engineering, permitting, and any other costs that apply in Year 0 of the project cash flow. Some costs, such as debt-related and sales tax costs are specified on the <a href=\"financial_parameters.html\" class=\"topiclink\">Financial Parameters<\/a> page.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">SAM uses the variables on the Installation Costs page to calculate the project investment cost and annual operating costs reported in the project <a href=\"cashflow.html\" class=\"topiclink\">cash flow<\/a> and used to calculate cost metrics.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">Recurring costs that apply in Years 1 and later of the project cash flow are on the <a href=\"operating_costs.html\" class=\"topiclink\">Operating Costs<\/a> page.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">Variable values in boxes with white backgrounds are values that you can edit. Boxes with blue backgrounds contain calculated values or values from other pages that SAM displays for your information.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">SAM provides the categories under <\/span><span class=\"f_CHInterfaceElement\">Direct Capital Costs<\/span><span class=\"f_Text\"> and <\/span><span class=\"f_CHInterfaceElement\">Indirect Capital Costs<\/span><span class=\"f_Text\"> for your convenience to help keep track of project installation costs. Only the <\/span><span class=\"f_CHInterfaceElement\">Total Installed Cost<\/span><span class=\"f_Text\"> value affects the cash flow calculations, so you can assign capital costs to the different cost categories in whatever way makes sense for your analysis. For example, you could assign the cost of designing the array to the module cost category or to the engineering category with equivalent results. After you assign costs to the categories, you should verify that the total installed cost value is what you expect.<\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Notes.<\/span><br \/>\n\r<span class=\"f_CHNote\">&nbsp;<\/span><br \/>\n\r<span class=\"f_Note\">The default cost values that appear when you create a file or case are intended to illustrate SAM\'s use. The cost data are meant to be realistic, but not to represent actual costs for a specific project. For more information and a list of online resources for cost data, see the technology pages on the <a href=\"https:\/\/sam.nrel.gov\" target=\"_blank\" class=\"weblink\" title=\"https:\/\/sam.nrel.gov\">SAM website<\/a>.<\/span><br \/>\n\r<span class=\"f_Note\">&nbsp;<\/span><br \/>\n\r<span class=\"f_Note\">The Installation Costs page only available for cases with a cash-flow-based financial model. It is not available with the No Financial Model option or the LCOE Calculator financial model.<\/span><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><span class=\"f_Heading3_atocs_\">Capital Cost Models<\/span><\/h3>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The capital cost models are implementations of NREL cost models that you can use to estimate turbine and balance-of-system (BOS) costs.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Land-based installation<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Choose this option to apply capital costs from NREL cost models for land-based wind farms.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Offshore installation<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Choose this option to use default values for offshore wind farms.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Estimate turbine costs now<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Replace the <\/span><span class=\"f_CHInterfaceElement\">Turbine cost<\/span><span class=\"f_VariableDescription\"> &quot;cost per kW&quot; value with a value from the NREL cost models, and set the &quot;cost per turbine&quot; and &quot;fixed cost&quot; values to zero. The cost estimates include sales tax, so this also sets the sales tax basis to zero.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Go to balance-of-system (BOS) cost model inputs<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Click this button to show the inputs for either the land-based or offshore BOS cost models. To apply a BOS cost from one of the BOS, first populate the inputs for either the land-based or offshore BOS model, and then click <\/span><span class=\"f_CHInterfaceElement\">Apply BOS Estimate<\/span><span class=\"f_VariableDescription\"> to apply the value. <\/span><\/p>\n\r<p class=\"p_VariableDescription\" style=\"text-indent: 0; padding-left: 0.8125rem; margin-left: 1.5000rem;\"><span class=\"f_VariableDescription\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:0.8125rem;margin-left:-0.8125rem\">&#8226;<\/span><span class=\"f_VariableDescription\"><a href=\"cc_wind.html#landbosmodel\" class=\"topiclink\">Land Based Balance of System Cost model<\/a><\/span><\/p>\n\r<p class=\"p_VariableDescription\" style=\"text-indent: 0; padding-left: 0.8125rem; margin-left: 1.5000rem;\"><span class=\"f_VariableDescription\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:0.8125rem;margin-left:-0.8125rem\">&#8226;<\/span><span class=\"f_VariableDescription\"><a href=\"cc_wind.html#offshorebos\" class=\"topiclink\">Offshore Balace of System Cost model<\/a><\/span><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><span class=\"f_Heading3_atocs_\">Capital Costs<\/span><\/h3>\n\r<p class=\"p_Text\"><span class=\"f_Text\">A capital cost represents an expense for a specific piece of equipment or installation service that applies in year zero of the cash flow.<\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Note:<\/span><span class=\"f_Note\"> Because SAM uses only the total installed cost value in cash flow calculations, how you distribute costs among the different direct capital cost categories does not affect the final results.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">For each direct cost category, you can specify the cost in \$\/kW of wind farm capacity, a fixed cost in \$, or a cost per turbine in \$\/turbine. If you specify more than one cost, for example a foundation cost in both \$\/kW and \$\/turbine, SAM adds the values together to calculate the total category cost.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Turbine Cost<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The cost of a single turbine. You can type values in \$\/kW, \$\/turbine, fixed amount, or a combination of the three. The total turbine cost is the sum of three values.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Balance of System cost<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Material, labor, and other costs associated with building turbine foundations for the entire wind farm. You can type values in \$\/kW, \$\/turbine, fixed amount, or a combination of the three. The total turbine cost is the sum of three values.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Wind farm capacity<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The wind farm\'s nameplate capacity from the <a href=\"wind_farm.html\" class=\"topiclink\">Wind Farm<\/a> page.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Number of turbines<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The number of turbines in the project from the <a href=\"wind_farm.html\" class=\"topiclink\">Wind Farm<\/a> page.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Sales tax basis, %<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The percentage of total direct cost used to the calculate sales tax amount.<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">SAM calculates the total sales tax amount by multiplying the sales tax rate from the <a href=\"financial_parameters.html\" class=\"topiclink\">Financial Parameters<\/a> page by the sales tax basis on the Installation Costs page: <\/span><\/p>\n\r<p class=\"p_EquationExcel\"><span class=\"f_EquationExcel\">Total Sales Tax (\$) = Sales Tax Rate (%) × Sales Tax Basis (%) × Total Direct Cost (\$)<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">For an explanation of the effect of sales tax on income tax, see <\/span><span class=\"f_CHInterfaceElement\">Sales Tax<\/span><span class=\"f_VariableDescription\"> on the <a href=\"financial_parameters.html\" class=\"topiclink\">Financial Parameters<\/a> topic for the financial model you are using (Residential, Commercial, Single Owner, etc.).<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Total installed cost<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The sum of the total turbine cost and total balance-of-system cost.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Total installed cost per kW<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The total installed cost divided by the wind farm nameplate capacity.<\/span><\/p>\n\r<div style=\"text-align: left; text-indent: 0; border-color: #0072c6; border-style: solid; border-width: thin; background: #ebebeb; padding: 0.1250rem 0.1250rem 0.1250rem 0.1250rem; margin: 0.1875rem 0 0.1875rem 0;\"><table style=\"border:none;border-spacing:0;padding:0;line-height: normal;\"><tr style=\"vertical-align:baseline\"><td style=\"border:none;padding:0;width:1.1875rem\"><a id=\"landbosmodel\" class=\"hmanchor\"><\/a><img id=\"toggle0186a1_ICON\" class=\"dropdown-toggle-icon\" alt=\"Click to expand or collapse\" title=\"Click to expand or collapse\" style=\"margin:0;width:1.0000rem;height:1.0000rem;border:none\" src=\".\/images\/ico-plus-16x16.png\"\/><\/td><td style=\"border:none;padding:0\"><span class=\"f_Heading2_atoc_\"><a id=\"toggle0186a1_LINK\" class=\"dropdown-toggle\" style=\"font-style: normal; font-weight: bold; color: #0072c6; background-color: transparent; text-decoration: none;\" title=\"Click to expand or collapse\" href=\"javascript:void(0)\" data-type=\"dropdown\" data-state=\"0\" data-icon=\"toggle0186a1_ICON\" data-src0=\".\/images\/ico-plus-16x16.png\" data-src1=\".\/images\/ico-minus-16x16.png\">Land-Based Balance of System Cost LandBOSSE Model<\/a><\/span><\/td><\/tr><\/table><\/div>\n\r<div id=\"toggle0186a1\" class=\"dropdown-toggle-body\" style=\"text-align: left; text-indent: 0; padding: 0 0 0 0; margin: 0 0 0.3750rem 0;display:none\"><table class=\"ToggleContentTable\" >\n\r<tr class=\"ToggleContentTable\">\n\r<td class=\"ToggleContentTable\"><p class=\"p_Text\"><span class=\"f_Text\">The Land-Based Balance of System cost model is an implementation of NREL\'s LandBOSSE model Costs inputs apply only when you choose the <\/span><span class=\"f_CHInterfaceElement\">Land-based installation<\/span><span class=\"f_Text\"> option under <\/span><span class=\"f_CHInterfaceElement\">Capital Cost Models<\/span><span class=\"f_Text\">. SAM applies the results of the BOS cost model to the <\/span><span class=\"f_CHInterfaceElement\">Balance of System Cost<\/span><span class=\"f_Text\"> input when you click <\/span><span class=\"f_CHInterfaceElement\">Apply BOS Estimate<\/span><span class=\"f_Text\">.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">For information and references about the NREL LandBOSSE model in SAM, see the &quot;Wind Cost Data&quot; section of the <a href=\"https:\/\/sam.nrel.gov\/wind.html\" target=\"_blank\" class=\"weblink\" title=\"https:\/\/sam.nrel.gov\/wind.html\">Wind page on the SAM website<\/a>. Those materials include:<\/span><\/p>\n\r<p class=\"p_List\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_List\">Eberle, A.; Roberts, O.; Key, A.; Bhaskar, P.; Dykes, K. (2019) NREL\'s Balance-of-System Cost Model for Land-Based Wind. National Renewable Energy Laboratory. 62 pp. NREL\/TP-6A20-72201. (<a href=\"https:\/\/www.nrel.gov\/docs\/fy19osti\/72201.pdf\" target=\"_blank\" class=\"weblink\">PDF 3.0 MB<\/a>)<\/span><\/p>\n\r<p class=\"p_List\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_List\">LandBOSSE in SAM (Tutorial\/Documentation). Description of the LandBOSSE implementation in SAM. (<a href=\"https:\/\/sam.nrel.gov\/images\/web_page_files\/sam-wind-landbosse-tutorial-nov-2020.pdf\" target=\"_blank\" class=\"weblink\">PDF 603 KB<\/a>)<\/span><\/p>\n\r<p class=\"p_ListLast\" style=\"text-indent: 0; page-break-inside: avoid; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_ListLast\"><a href=\"https:\/\/github.com\/wisdem\/landbosse\" target=\"_blank\" class=\"weblink\">LandBOSSE source code on GitHub<\/a>.<\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Note.<\/span><span class=\"f_Note\"> The LandBOSSE cost model runs separately from the SAM simulation. The cost model calculates a cost estimate that you can use for SAM\'s balance-of-system cost input in the Capital Costs section at the top of the Installation Costs page. The SAM simulation uses the value of that input in the financial model\'s cash flow calculations.<\/span><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><span class=\"f_Heading3_atocs_\">Enable LandBOSSE Model<\/span><\/h3>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The first time you check Enable Land-Based Balance of System Cost Model, SAM installs the NREL LandBOSSE BOS cost model. Once the cost model is installed, when you check the box, SAM enables the BOS model inputs.<\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Note.<\/span><span class=\"f_Note\"> It may take a few minutes to install the LandBOSSE model. After the model is installed, it takes a few moments to run, so be prepared to wait after clicking <\/span><span class=\"f_CHInterfaceElement\">Enable Land-Based Balance of System Cost Model<\/span><span class=\"f_Note\">, <\/span><span class=\"f_CHInterfaceElement\">Calculate BOS<\/span><span class=\"f_Note\">, or <\/span><span class=\"f_CHInterfaceElement\">Apply BOS Estimate<\/span><span class=\"f_Note\">.<\/span><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><span class=\"f_Heading3_atocs_\">Balance of System Model Inputs<\/span><\/h3>\n\r<p class=\"p_Text\"><span class=\"f_Text\">To enable these inputs, click <\/span><span class=\"f_CHInterfaceElement\">Enable Land-Based Balance of System Cost Model<\/span><span class=\"f_Text\">.<\/span><\/p>\n\r<p class=\"p_ListLast\" style=\"text-indent: 0; page-break-inside: avoid; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_ListLast\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_ListLast\">For a detailed description of these inputs and instructions for using the LandBOSSE model, see &quot;LandBOSSE in SAM (Tutorial\/Documentation)&quot;, a description of the LandBOSSE model implementation in SAM. (<a href=\"https:\/\/sam.nrel.gov\/images\/web_page_files\/sam-wind-landbosse-tutorial-nov-2020.pdf\" target=\"_blank\" class=\"weblink\">PDF 603 KB<\/a>)<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Interconnect voltage, kV<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The voltage at the interconnection point between the project substation and the interconnecting utility substation.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Distance to interconnect, miles<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The distance between the substation and point of interconnection with the grid.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Turbine foundation depth, m<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The depth of the turbine foundation. The LandBOSSE model uses costs for a shallow spread-foot foundation design.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Turbine rated thrust, N<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The maximum force experienced by the wind turbine under extreme conditions. This affects the foundation cost.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Labor cost multiplier<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Multiplier for all labor costs in the model.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">50-year gust velocity, m\/s<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Wind velocity for the extreme 50-year wind gust at the project site.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Calculate BOS<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">After you have entered values for the model inputs, click <\/span><span class=\"f_CHInterfaceElement\">Calculate BOS<\/span><span class=\"f_VariableDescription\"> to run the model and calculate a balance-of-system cost estimate. It may take a few moments for the model to run. When the model finishes, it populates values under <\/span><span class=\"f_CHInterfaceElement\">Balance of System Model Results<\/span><span class=\"f_VariableDescription\"> and <\/span><span class=\"f_CHInterfaceElement\">Balance of System Model Detailed Results<\/span><span class=\"f_VariableDescription\">.<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">If you want to use the calculated cost estimate for your SAM simulation, click <\/span><span class=\"f_CHInterfaceElement\">Apply BOS estimate<\/span><span class=\"f_VariableDescription\">.<\/span><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><span class=\"f_Heading3_atocs_\">Balance of System Model Results<\/span><\/h3>\n\r<p class=\"p_Text\"><span class=\"f_Text\">After you click <\/span><span class=\"f_CHInterfaceElement\">Calculate BOS<\/span><span class=\"f_Text\">, when the LandBOSSE model finishes running, it displays the total BOS cost estimate along with cost details from the model.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Total BOS Cost Estimate, \$<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The total balance-of-system cost estimate calculated by the LandBOSSE model.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">BOS Cost Estimate per kW<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The cost estimate in dollars per kW of nameplate capacity. When you click <\/span><span class=\"f_CHInterfaceElement\">Apply BOS Estimate<\/span><span class=\"f_VariableDescription\">, SAM copies this value to the balance-of-system cost input under <\/span><span class=\"f_Term\">Capital Costs<\/span><span class=\"f_VariableDescription\">.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Apply BOS Estimate<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Click to replace the balance-of-system &quot;cost per kW&quot; SAM cost input under <\/span><span class=\"f_CHInterfaceElement\">Capital Costs<\/span><span class=\"f_VariableDescription\"> with the BOS cost estimate calculated by the LandBOSSE model. This sets the balance-of-system &quot;cost per turbine&quot; and &quot;fixed cost&quot; to zero.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Rock Trenching Required<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The percentage of total gravel\/rock thermal backfill required to optimize soil thermal resistivity for collector cabling.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Contingency<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">A percentage of the BOS total costs set aside for unexpected costs that occur during the construction period.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Warranty Management<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The cost of personnel to manage contracts during the warranty period and make any required claims as a percentage of the total balance-of-system costs.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Sales and Use Tax<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Taxes on projects vary dramatically by counties and states, with some having Renewable Energy Zones without taxes. &nbsp;For more information on sales and use taxes in the United States, see the <a href=\"http:\/\/taxfoundation.org\/article\/state-and-local-sales-tax-rates-2013\" target=\"_blank\" class=\"weblink\" title=\"http:\/\/taxfoundation.org\/article\/state-and-local-sales-tax-rates-2013\">Tax Foundation<\/a> and <a href=\"http:\/\/www.dsireusa.org\/summarytables\/finre.cfm\" target=\"_blank\" class=\"weblink\" title=\"http:\/\/www.dsireusa.org\/summarytables\/finre.cfm\">DSIRE USA<\/a> websites.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Overhead<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Percentage of the project budget for overhead costs such as administration, trailer rental, utilities, etc.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Profit Margin<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Percentage of the BOS project that is being considered as the project profit margin for the BOS activities.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Development Fee<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">This is the accumulation of different costs of developing a project such as real estate, wind resource study, interconnection costs, environmental and permitting, etc. &nbsp;This could also include project profits created by the sale of the project from one developer to another developer.<\/span><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><span class=\"f_Heading3_atocs_\">Balance of System Detailed Results<\/span><\/h3>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The detailed results shows the cost breakdown of results from the LandBOSSE model. For details<\/span><span class=\"f_ListLast\">, see &quot;LandBOSSE in SAM (Tutorial\/Documentation)&quot;, a description of the LandBOSSE model implementation in SAM. (<a href=\"https:\/\/sam.nrel.gov\/images\/web_page_files\/sam-wind-landbosse-tutorial-nov-2020.pdf\" target=\"_blank\" class=\"weblink\">PDF 603 KB<\/a>).<\/span><\/p>\n\r<\/td>\n\r<\/tr>\n\r<\/table>\n\r<\/div>\n\r<div style=\"text-align: left; text-indent: 0; border-color: #0072c6; border-style: solid; border-width: thin; background: #ebebeb; padding: 0.1250rem 0.1250rem 0.1250rem 0.1250rem; margin: 0.1875rem 0 0.1875rem 0;\"><table style=\"border:none;border-spacing:0;padding:0;line-height: normal;\"><tr style=\"vertical-align:baseline\"><td style=\"border:none;padding:0;width:1.1875rem\"><a id=\"offshorebos\" class=\"hmanchor\"><\/a><img id=\"toggle0186a2_ICON\" class=\"dropdown-toggle-icon\" alt=\"Click to expand or collapse\" title=\"Click to expand or collapse\" style=\"margin:0;width:1.0000rem;height:1.0000rem;border:none\" src=\".\/images\/ico-plus-16x16.png\"\/><\/td><td style=\"border:none;padding:0\"><span class=\"f_Heading2_atoc_\"><a id=\"toggle0186a2_LINK\" class=\"dropdown-toggle\" style=\"font-style: normal; font-weight: bold; color: #0072c6; background-color: transparent; text-decoration: none;\" title=\"Click to expand or collapse\" href=\"javascript:void(0)\" data-type=\"dropdown\" data-state=\"0\" data-icon=\"toggle0186a2_ICON\" data-src0=\".\/images\/ico-plus-16x16.png\" data-src1=\".\/images\/ico-minus-16x16.png\">Offshore Balance of System Cost Model<\/a><\/span><\/td><\/tr><\/table><\/div>\n\r<div id=\"toggle0186a2\" class=\"dropdown-toggle-body\" style=\"text-align: left; text-indent: 0; padding: 0 0 0 0; margin: 0 0 0.3750rem 0;display:none\"><table class=\"ToggleContentTable\" >\n\r<tr class=\"ToggleContentTable\">\n\r<td class=\"ToggleContentTable\"><p class=\"p_Text\"><span class=\"f_Text\">The offshore balance-of-system cost model is for a wind farm installed offshore, in a body of water such as coastal ocean waters or lake. The model calculates a value for the <\/span><span class=\"f_CHInterfaceElement\">Balance of System<\/span><span class=\"f_Text\"> cost input in \$\/kW based on the data you provide for the detailed costs.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">For a description of the model and its inputs, see <\/span><span class=\"f_Text\" style=\"font-family: Arial,Helvetica,sans-serif; background-color: #ffffff;\">Maness, M.; Maples, B.; Smith, A.; NREL Offshore Balance-of-System Model. National Renewable Energy Laboratory, NREL\/TP-6A20-66874. (<a href=\"http:\/\/www.nrel.gov\/docs\/fy17osti\/66874.pdf\" target=\"_blank\" class=\"weblink\">PDF 4.7 MB<\/a>).<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\" style=\"font-family: Arial,Helvetica,sans-serif; background-color: #ffffff;\">After you specify values for the detailed inputs, click&nbsp;<\/span><span class=\"f_CHInterfaceElement\">Apply BOS Estimate<\/span><span class=\"f_Text\" style=\"font-family: Arial,Helvetica,sans-serif; background-color: #ffffff;\">&nbsp;to calculate the BOS cost and automatically apply it to the&nbsp;<\/span><span class=\"f_CHInterfaceElement\">Balance of System Cost<\/span><span class=\"f_Text\" style=\"font-family: Arial,Helvetica,sans-serif; background-color: #ffffff;\">.<\/span><\/p>\n\r<\/td>\n\r<\/tr>\n\r<\/table>\n\r<\/div>\n\r"
})

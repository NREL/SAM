hmLoadTopic({
hmKeywords:"",
hmTitle:"PPA Price",
hmDescription:"The PPA price is the power purchase price for the PPA financial models. SAM assumes that these projects sell electricity at a price negotiated through a power purchase...",
hmPrevLink:"mtf_payback.html",
hmNextLink:"mtf_ppa_escalation.html",
hmParentLink:"financial_metrics.html",
hmBreadCrumbs:"<a href=\"financial_metrics.html\">Financial Metrics<\/a>",
hmTitlePath:"Financial Metrics > PPA Price",
hmHeader:"<h1 class=\"p_Heading1\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_Heading1\">PPA Price<\/span><\/h1>\n\r",
hmBody:"<p class=\"p_Text\"><span class=\"f_Text\">The PPA price is the power purchase price for the PPA financial models. SAM assumes that these projects sell electricity at a price negotiated through a power purchase agreement (PPA).<\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Notes.<\/span><br \/>\n\r<span class=\"f_CHNote\">&nbsp;<\/span><br \/>\n\r<span class=\"f_Note\">When you specify a <a href=\"mtf_ppa_escalation.html\" class=\"topiclink\">PPA price escalation<\/a> rate on the <a href=\"financial_parameters.html\" class=\"topiclink\">Financial Parameters<\/a> page, the PPA price increases from year to year. The PPA price does not increase with inflation.<\/span><br \/>\n\r<span class=\"f_Note\">&nbsp;<\/span><br \/>\n\r<span class=\"f_Note\">If your project results in an unrealistically high PPA price or IRR, you may need to adjust the inputs. See <a href=\"mtf_npv.html#interpreting\" class=\"topiclink\">Interpreting the NPV<\/a> for tips.<\/span><br \/>\n\r<span class=\"f_Note\">&nbsp;<\/span><br \/>\n\r<span class=\"f_Note\">You can see the annual PPA price on the <a href=\"data.html\" class=\"topiclink\">Data Tables<\/a> tab of the results page by displaying the PPA price variable under <\/span><span class=\"f_CHInterfaceElement\">Annual Data<\/span><span class=\"f_Note\">.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The PPA price in the Metrics table is either the value that you specify as an input on the Financial Parameters page, or a value that SAM calculates based on the target internal rate of return (IRR) that you specify. <\/span><\/p>\n\r<p class=\"p_List\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_List\">When the solution mode on the <a href=\"financial_parameters.html\" class=\"topiclink\">Financial Parameters<\/a> page is <\/span><span class=\"f_CHInterfaceElement\">Specify PPA Price<\/span><span class=\"f_List\">, the Year one PPA price in the Metrics table is the value that you specify.<\/span><\/p>\n\r<p class=\"p_List\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_List\">When the solution mode is <\/span><span class=\"f_CHInterfaceElement\">Specify IRR Target<\/span><span class=\"f_List\">, SAM determines the PPA price required for project revenues to meet the internal rate of return target that you specify on the Financial Parameters page.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The Year one PPA price that SAM reports in the Metrics table is equivalent to the bid price in a power purchase agreement.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The PPA price is just one measure that is useful for determining whether a project is financially feasible. For example, a project that meets your target IRR with a reasonable PPA price may not be feasible if the net present value (NPV) is negative, or if the size of debt is not within the range that the project can realistically expect to receive a loan.<\/span><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><a id=\"tod\" class=\"hmanchor\"><\/a><span class=\"f_Heading3_atocs_\">PPA Price and TOD Factors<\/span><\/h3>\n\r<p class=\"p_Text\"><span class=\"f_Text\"><a href=\"revenue_ppa.html#tod\" class=\"topiclink\">Time-of-delivery (TOD) factors<\/a> allow the power price to vary with time within the structure of a power purchase agreement with a single bid price. The agreement consists of a PPA price and a set of PPA price multipliers and the periods to which they apply.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">For analyses involving PPA price time-of-delivery (TOD) factors, for each hour of the year, SAM multiplies the PPA bid price by the TOD factor to calculate the power price in that hour.<\/span><\/p>\n\r"
})

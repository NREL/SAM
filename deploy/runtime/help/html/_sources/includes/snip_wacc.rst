
**WACC**
  SAM displays the WACC for projects with debt when you specify the debt size as a debt percent. It is shown for reference and not used in any calculations.

  This is a calculated value that you cannot directly edit. To change its value, change one of the parameters in the following equations:

*WACC = [ Nominal Discount Rate ÷ 100 × (1 - Debt Percent ÷ 100)*
 *+ Debt Percent ÷ 100 × Loan Rate ÷ 100 ×  (1 - Effective Tax Rate ÷ 100 ) ] × 100*

  See the **Nominal discount rate** variable description for its equation. The effective tax rate is a single number that includes both the federal income tax rate and state income tax rate. SAM uses the effective tax rate for several calculations requiring a total income tax value:

*Effective Tax Rate = [ Federal Tax Rate ÷ 100 × ( 1 - State Tax Rate ÷ 100 ) + State Tax Rate ÷ 100 ] × 100*
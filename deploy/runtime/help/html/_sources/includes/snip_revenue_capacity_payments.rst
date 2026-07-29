
Capacity payments are is an annual payment to the project for available capacity. SAM reports the annual capacity payment revenue in the project :doc:`cash flow <../results/cashflow>`.

The capacity payment is either the fixed amount you specify, or the product of the capacity payement ($/MW), system nameplate capacity (MW), and capacity credit (%).

.. note:: If your project does not involve capacity payments, set **Capacity payment amount** to zero.

**Capacity basis**
  Specify the annual capacity payment as a dollar per megawatt value with the capacity credit specified as a percentage of the system nameplate capacity. See the system nameplate description below.

**Fixed amount**
  Specify the annual capacity payment as a dollar amount.

**Capacity payment amount, $/MW or $/year**
  When you choose the capacity basis option, enter a $/MW value in Year 1 dollars for the annual capacity payment.

.. note:: SAM assumes the capacity payment rate in $/MW is an annual rate. If you are given a daily or monthly rate, you should convert it to an annual rate. For example, a $100/MW daily rate would be equivalent to $36,500/MW annually. Similarly, a $100/MW monthly rate would be $1,200/MW annually.

  When you choose the fixed amount, enter a $ value in Year 1 dollars for the annual capacity payment.

  You can also click the blue and grey **Value/Sched** button and then **Edit** to use the :doc:`Edit Schedule <../window-reference/win_edit_data_table_column>`   window to enter a different payment amount for each year of the analysis period instead of a single value.

**Capacity payment escalation**
  The annual escalation rate applies to the annual capacity payment in Years 2 and later. If you expect the capacity payment amount to increase from year to year, enter the escalation rate as an annual percentage.

  The capacity payment escalation rate is disabled when you choose the Schedule option to enter annual values for the capacity payment amount.

**Capacity credit (eligible % of nameplate), %**
  The percentage of the system's nameplate capacity that is eligible for capacity payments when you choose the **Capacity basis** option. If this value is zero, no capacity payments will be made.

  For systems with battery storage that earn a capacity payment based on the battery's capacity in kW (maximum discharge power), you can use the capacity credit percentage to represent the battery capacity. For example, for a PV-battery system with a 100 MW array and 60 MWac battery, you could set the capacity credit percentage to 60.

**System nameplate, MW**
  The system's nameplate capacity. Note that the nameplate capacity is defined differently for different kinds of systems. For example for photovoltaic systems, the nameplate capacity is the total DC array capacity, and for wind and concentrating solar power systems, it is an AC capacity.

**Battery maximum discharge power, MWac**
  For systems with battery storage, the battery's maximum discharge power from the Battery Cell and System page. 

  For PV Battery and Custom Generation Profile - Battery   configurations, SAM shows this battery capacity value for reference, but does not use it to calculate the capacity payment amount.

  For Standalone Battery configurations, SAM uses the battery maximum discharge power to calculate the capacity payment amount.
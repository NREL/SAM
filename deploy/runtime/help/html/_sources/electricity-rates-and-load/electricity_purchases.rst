Electricity Purchases
=====================

The options on the Electricity Purchases page determine how front-of-meter projects pay for electricity required by the system when it is not generating electricity, such as to charge batteries, meet photovoltaic inverter night-time loads, or meet concentrating solar power or industrial process heat parasitic loads for freeze protection and other equipment.

The cost of purchasing electricity is treated as a tax-deductible operating expense in the project :doc:`cash flow <../results/cashflow>`. You can explore the cost details in the :doc:`electricity bill results <electricity_bill_results>`.

Electricity Purchases
~~~~~~~~~~~~~~~~~~~~~

SAM provides two options for defining the retail rate structure for electricity purchases.

**Use PPA or market prices**
  The **Use PPA Price** option is for projects that purchase power at the same rate as the PPA price, adjusted as appropriate by time-of-delivery (TOD) multipliers and power price escalation rates.

.. note:: The Use PPA Price or Market Prices option is not available with **Specify IRR target** on the Revenue page (Financial Parameters page for Partnership Flip and Sale Leaseback financial models) because SAM needs to know the PPA price at the start of the simulation to calculate the cost of electricity purchases.

   If you want to use the Specify IRR Target option with the PPA price as the price for electricity purchases, choose **Use retail electricity rate(s)**, define an energy rate table with one row, and set the buy rate to the PPA price. You may need to start with an initial guess for the PPA price and run iterative simulations to determine the PPA price. To use time series price data, check **Use hourly (subhourly) buy rates instead of TOU rates** and import the price data.

**Use retail electricity rate(s)**
  The Use Retail Electricity Rate(s) option is for projects that purchase power at retail rates. You can define the retail buy rate as a fixed rate, a set of time-of-use rates with optional tiers, or a table of hourly or subhourly time-series buy rates. The retail rate structure may also include fixed and minimum charges and demand charges.

  You can specify the rate structure by hand or download rate data from the OpenEI Utility Rate Database.
 
.. note:: Only the **Buy all / sell all** metering option with no sell rate is available for electricity purchases.

   SAM uses the same inputs to define retail rates for electricity purchases as it does for behind-the-meter projects that may involve net metering and/or sell rates that are described in the retail rates documentation below. For the front-of-meter system, these inputs are disabled, so you can ignore the descriptions.

Rate Structure Definitions
~~~~~~~~~~~~~~~~~~~~~~~~~~

SAM's monthly electricity bill calculator includes the features listed below. The total monthly bill is the sum of fixed, energy, and demand charges for each month.

* **Fixed charge**: A fixed amount in dollars that the project pays each month. This amount is added to any other charges to calculate the monthly bill.

* **Minimum charge**: When the total monthly or annual bill amount falls below the minimum monthly charge or minimum annual charge, the minimum charge applies instead of the smaller amount.

* **Flat, time-of-use, and tiered energy rates**: Energy rates in dollars per kilowatt-hour ($/kWh) that may vary with hour of day and month of year, cumulative consumption, or both.

* **Flat, time-of-use, and tiered demand rates**: Monthly fees in dollars per kilowatt ($/kW) paid by the project for the billing demand. Demand charges may be flat (constant), vary with hour of day and month of year, or vary with monthly cumulative consumption.

* **Annual escalation rate**: Applies to the total annual electricity bill in Years 2 and later of the project cash flow in addition to the inflation rate from the :doc:`Financial Parameters <../financial-parameters/fin_overview>` page. Note that all rates and charges on the Electricity Rates page are in Year 1 dollars.

OpenEI Utility Rate Database
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

NLR's Open Energy Information (OpenEI) `Utility Rate Database (URDB) <https://openei.org/wiki/Utility_Rate_Database>`__ hosts a database of retail electricity rate structures for electric service providers in the United States and some other countries. SAM allows you to search the database and import rate structure data from the database to the input variables on the Electricity Rates page. SAM accesses `Version 8 of the URDB API <https://openei.org/services/doc/rest/util_rates/?version=8>`__.

**Search for Rates**
  Click to search the OpenEI database for a rate structure and import the structure into SAM. This feature requires a :doc:`web connection <../reference/configure_proxy_server>`  :

  #. In the OpenEI Utility Rate Database window, once the list of electric service providers appears, type either a zip code in **Zip code** or a few letters of the service provider's name in **Filter** to show providers who meet the criteria you typed.

     Some service providers have shortcut or alternative names that SAM may recognize, for example, "PG&E" for Pacific Gas and Electric or Wisconsin Public Service Co for Alliant Energy. The OpenEI database uses a lookup table for many of these shortcut and alternative names, but you may need to use the provider's full name to find it in the list.

  #. Click the name of the service provider to display a list of rate schedules available for that provider.

  #. Click the rate structure name to choose it and display summary information.

     By default, SAM only shows active rate structures for the sector suggested by the financial model of the case (commercial and residential), but you can change the option by clicking **Residential Only**, **Commercial Only**, or **All Schedules** in the upper right corner of the window. (**Lighting** shows rate schedules for street lighting that are not typically applicable to SAM.) "Active" rates are current rates that do not have an end date. Clear the **Show active** check box to show a list of all rates, including old rates with end dates.

  #. Click **Download and apply utility rate** to download the data and populate the inputs on the Electricity Rates page.

.. note:: SAM does not use location information from the weather file to determine the utility service provider for your analysis. You can type a zip code in the OpenEI Utility Rate Database window to list service providers for a particular address.

   In some cases, the data in the OpenEI database may be incorrect. This is especially true for rates structures with **ratcheting demand rates**. Be sure to compare the data you import to the information on the utility service provider's rate sheet.

   Some rate structures have elements or use units that SAM cannot model, such as a fixed charge in $/day units. In this case, SAM displays a red message "This rate from the URDB contains items that SAM's electricity bill calculator does not consider. See Unused Items below. If you see that message, expand the Unused Items panel at the bottom of the page to see the unused information, and if necessary, modify the rate inputs to approximate the item. For example, you could multiply the $/day fixed charge by 365 days/yr ÷ 12 months/yr to estimate a value to add to SAM's fixed monthly charge input.

Save / Load Rate Data
~~~~~~~~~~~~~~~~~~~~~

You can save and load utility rate structure data from SAM to a text file of comma-separated values (CSV). This is useful for sharing electricity rate data between SAM project files or with other people. The utility rate data file format is a comma-separated list of input variables and their values.

.. note:: To see what the file format looks like, save the current rate to a file and open it with a spreadsheet program or text editor.

   You can import and export each individual time-of-use rate table separately using the **Import** and **Export** buttons next to each table.

**Save rate to file**
  Save the values of all input variables on the Electricity Rates to a text file. Use the .csv   file extension when you save the file.

**Load rate from file**
  Import data from a properly-formatted text file to the inputs on the Electricity Rates page. To create a file to use as a template, click **Save rate to file**, and then replace the data in the template with your own rate data.

Metering and Billing
~~~~~~~~~~~~~~~~~~~~

For front-of-meter projects, SAM uses the buy all / sell all option and disables the other options.

**Buy all /sell all**
  For the buy all / sell all option, specify the buy rate(s) for electricity purchases in the Energy Charges table.

  Alternatively, you can provide time series buy and sell rates to use instead of the rates in the Energy Charges table. Note that you can combine time series buy rates with TOU sell rates and vice versa:

  Check **Use hourly (subhourly) buy rates instead of TOU** rates and click **Edit array** to enter or import time series buy rates $/kWh. This disables the buy rate column in the Energy Charges table.

Fixed Charge
~~~~~~~~~~~~
A fixed monthly charge is a fee that the project pays to the electric service provider and does not depend on the quantity of electricity consumed or generated by the project.

**Fixed monthly charge, $**
  A fixed dollar amount that applies to all months

Minimum Charges
~~~~~~~~~~~~~~~
Minimum charges apply when either the monthly or annual electricity bill falls below a minimum value.

**Monthly minimum charge, $**
  If the monthly electricity bill with system for a given month is less than the monthly minimum, the bill for that month is the monthly minimum charge.

**Annual minimum charge, $**
  The minimum annual charge for Year 1. If the annual electricity bill with system for Year 1 is less than the minimum value, SAM sets the Year 1 annual bill to the minimum value. For the purposes of the annual minimum charge, the year starts January 1 and ends December 31. The annual minimum charge does not affect the monthly electricity bill.

.. note:: Like all of the dollar values on the Electricity Rates page, the minimum charge amounts are Year 1 values. If you apply an annual minimum charge, the monthly electricity bill with system data shown in the :doc:`Results <electricity_bill_results>` does not reflect the annual minimum charge.

Annual Escalation
~~~~~~~~~~~~~~~~~

The escalation rate is an annual percentage increase that applies to the monthly electricity bill in Years 2 and later. Escalation is in addition to the inflation rate.

.. note:: Three factors affect the annual electricity bill value from year to year: The electricity bill escalation rate, inflation rate from the :doc:`Financial Parameters <../financial-parameters/fin_overview>` page, and degradation rate from the :doc:`Degradation <../degradation/degradation>` page.

**Electricity bill escalation rate, $/yr**
  The escalation rate is a single value that applies in addition to the inflation rate. For example, for an annual consumption of 1,000 kWh, flat buy rate of $0.10/kWh, escalation rate of 1%/yr, and an inflation rate of 2.5%/yr, the Year 1 annual electricity bill without system would be $100, Year 2 = $104, Year 3 = $107, etc.

  In some cases, it may be appropriate to use an annual schedule to define a different escalation rate for each year. When you specify the escalation rate using an annual schedule, SAM applies only the escalation rate and excludes inflation from the calculation of out-year values.

**To specify annual escalation rates (optional):**

.. include:: ../includes/snip_annual_values.rst

.. _electricity-purchases-description:

Description and Applicability
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

These variables help you identify the rate structure but do not affect simulation results. If you download rate structure data from the Utility Rate Database, SAM populates the variables automatically.

**Name**
  The name or title of the rate structure.

**Schedule**
  The schedule name, often an alphanumeric designation used by the electric service provider.

**Source**
  A description of the data source.

  If you downloaded the rate structure from the `OpenEI database <http://en.openei.org/wiki/Gateway:Utilities>`__, the source is a web link to the rate's entry page on the OpenEI website where you can find a link to the original source document.  You can use your mouse to select and copy the link and paste it into your web browser.

**Description**
  Text describing the rate structure.

**Applicability**
  The applicability values are from the U.S. Utility Rate Database, and are for your reference. These values do not affect SAM simulations.

.. _electricity-purchases-energycharge:

Energy Charges
~~~~~~~~~~~~~~

SAM uses the information in the energy rate table and weekday and weekend schedules to calculate the energy charge portion of each month's electricity bill based on the quantity of electricity delivered to the battery from the grid over the month.

Energy Rate Table
~~~~~~~~~~~~~~~~~

Each row in the table defines the rates and tier limit for one period and tier. You can define up to 12 time-of-use periods and 12 tiers.

.. note:: It may be easier to create and modify a complex rate table in a text editor or spreadsheet program than in the table. You can use the **Import** and **Export** buttons to work with the data as comma-separated text outside of SAM. To see what the data format looks like, create a table in SAM and export it to a text file.

**Number of entries**
  The total number of energy rates in the structure, equal to the product of the number of time-of-use periods and tiers. The number of rows in the table is equal to the number of entries. When you change the number of entries, SAM changes the number of rows in the table.

.. note:: If you change the number of entries to a smaller number, you will lose data in the rows when SAM resizes the table.

**Period (1-12)**
  Each period number represents a time-of-use period for energy charges. For example, for a simple rate structure with one summer period and one period, you could assign Period 1 to the summer months and Period 2 to the winter months. The time-of-use period must be a number between 1 and 12 and each period must be associated with a time defined by the :doc:`weekday and weekend schedules <../reference/weekday_schedule>`  .

**Tier**
  For each time-of-use period, you can define one or more tiers. Each tier is defined by a maximum usage, buy rate, and optional sell rate.

  SAM accumulates tiers on a monthly basis, so any period that occurs in a given month must have the same number of tiers. For example, in the weekday schedule if the row for March contains Periods 2, 3, and 6, then those three periods must have the same number of tiers.

**Max. Usage**
  For rate structures with tiered rates, the maximum usage is the maximum cumulative consumption over the time-of-use period in the month for which the tier's rates apply.

  For example, for a three-tier structure with one buy rate for cumulative consumption up to 500 kWh/month, a second rate for cumulative consumption in excess of 500 kWh/month and up to 800 kWh/month, and a third rate for cumulative consumption in excess of 800 kWh/month, you would specify **Max Usage** for Tier 1 at 500 kWh, Tier 2 at 800 kWh, and Tier 3 at 1e+038 kWh.

**Max. Usage Units**
  The units for each tier's maximum consumption. To change the units, click the cell in the table for the units you want to change, and choose the units from the list that appears.

  .. image:: ../images/SS_UtilityRate-max-usage-units.png
     :align: center
     :alt: SS_UtilityRate-max-usage-units.png

  For maximum usage units of kWh/kW or kWh/kW daily, SAM calculates the energy charge for a given month from both the total consumption in kWh and billing demand in kW for each time-of-use period and tier in that month. See :ref:`Billing Demand <electricity-purchases-billing-demand>`   for details.

.. note:: For rate structures that include tiers with kWh/kW or kWh/kW daily maximum usage units, the maximum usage units for Tier 1 must be kWh/kW.

**Buy ($/kWh)**
  The price paid by the project in dollars per kilowatt-hour for electricity delivered by the grid for each period and tier.

Weekday and Weekend Schedules
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Weekday and Weekend schedules determine the time of day and time of year for each time-of-use period you define in the energy rates table. For each time-of-use period in the energy rate table, select a rectangle in the weekday or weekend schedule that represents the period and type the period number. For example, if Period 1 represents a summer rate, and the summer months are June through August, select all of the squares in the **Jun**, **Jul**, and **Aug** rows type the number 1 on your keyboard. See :doc:`Weekday Weekend Schedules <../reference/weekday_schedule>` for detailed instructions.

**Weekday**
  The time-of-day and month-of-year matrix that assigns a period representing set of time-of-use rates to the five working days of the week. For electricity bill calculations, SAM assumes that weekdays are Monday through Friday and that the year begins on Monday, January 1, in the hour ending at 1:00 am.

**Weekend**
  The time-of-day and month-of-year matrix that assigns time-of-use periods to the two weekend days of the week: Saturday and Sunday.

.. _electricity-purchases-billing-demand:

Billing Demand
~~~~~~~~~~~~~~

Billing demand is the peak kilowatt (kW) value used to calculate demand charges and for some special types of energy charges that involve tiered energy rates with maximum usage units of either kWh/kW or kWh/kW daily. 

By default, SAM calculates billing demand for each month based on the consumption in that month.

For rate structures that calculate billing demand based on consumption in previous months, you should enable billing demand lookback to define how billing demand is calculated.

.. note:: For tiered energy rates with kWh/kW or kWh/kW daily units, if billing demand lookback is enabled, the energy charge for each time-of-use period and tier is calculated from the cumulative consumption in kWh in that period and tier *and* the billing demand over some number of months previous to the current billing month in kW. If billing demand lookback is not enabled, the charge is calculated from the cumulative consumption kWh and billing demand in kW for the current month.

**Enable billing demand lookback**
  Check the box to calculate billing demand for the current month's electricity from consumption in past months.

  Clear the box to calculate billing demand based for the current month's bill from consumption in the current month.

**Lookback period, months**
  The period preceding each month over which to determine the billing demand. The lookback period is typically the 11 months preceding the current month, but you can set it to any value between 1 and 11.

**Minimum billing demand, kW**
  The lower limit of the billing demand. SAM uses the greater of the minimum billing demand and the calculated billing demand for the electricity bill calculation.

**Monthly peak demand in Year 0, kW**
  The monthly peak demand for the year before the renewable energy system is installed. SAM starts the cash flow calculation on January 1 of Year 1, which is the first year that the system generates power, so it does not have information about the load in the previous year.

  To specify Year 0 monthly peak demand values, clear the Use Monthly Peaks from Year 1 option.

Billing Demand by Month Table
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Billing Demand by Month table defines how the billing demand is calculated by month over the lookback period.

**Billing Demand Percentage, %**
  For each month in the lookback period, enter a percentage of the actual peak consumption. SAM calculates the billing demand for each month by multiplying the the actual peak consumption that month by the billing demand percentage.

**Consider Actual Demand, 0/1**
  Enter a zero in the table for each month that the billing demand is calculated as a percentage of the actual peak consumption.

  Enter a one for each month that the billing demand is calculated as the greater of the actual peak consumption and percentage of the actual peak.

Billing Demand by Time-of use Period
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Billing Demand by Time-of-use Period table applies when the rate structure includes :ref:`demand rates <electricity-purchases-demand>` by time-of-use period and tiers. It defines which time-of-use periods for demand charges should be considered in the billing demand calculation.

.. note:: The table must include one row for each time-of-use period in the   Demand Rates by Time-of-use Period and/or Tiers table under **Demand Charges**. For example, if the demand rate structure has two time of use periods and three tiers, the Billing Demand by time-of-use Period table should have two rows, one for Period 1 and one for Period 2.

**Period**
  The time-of-use period number from the Demand Rates by Time-of-use Period and/or Tiers table under **Demand Charges**.

**Included in Billing Demand (0/1)**
  For each time-of-use period, assign a 1 to include consumption in that period in the billing demand calculation. Assign a zero to exclude consumption in that period from the billing demand calculation.

.. note:: You must assign a 1 to at least one time-of-use period for the demand charge calculations to work correctly, even if the **Charge ($/kW)** column in the   Demand Rates by Time-of-use Period and/or Tiers table is set to zero.. note:: for all periods.

.. _electricity-purchases-demand:

Demand Charges
~~~~~~~~~~~~~~

The demand rate tables and weekday and weekend schedules provide information that SAM uses to calculate the demand charge portion of each month's electricity bill. A demand charge is a fee that the project pays to the electric service provider for the billing demand in each month.

The demand charge can be calculated from a monthly flat or tiered demand rate structure, a demand rate structure with time-of-use periods and/or tiers, or a combination of both.

.. note:: By default, SAM defines the billing demand for each month based on consumption in that month. For demand rates based on consumption in past months, such as ratcheting demand rates, enable billing demand lookback under :ref:`Billing Demand <electricity-purchases-billing-demand>`.

**Enable demand charges**
  Check the box to include demand charges in the monthly electricity bill. You can disable demand charges without losing the rate and schedule data by clearing the check box.

Demand Rates by Month with Optional Tiers Rate Table
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Use the Demand Rates by Month with Optional Tiers rate table for fixed monthly demand rates, or monthly demand rates with tiers.

For a basic demand rate with no tiers, set the number of entries to 12 (one for each month) and assign a tier value of 1 to each month. The peak demand for Tier 1 should be 1e+038, a very large number that represents no limit. Specify the demand rate in $/kW for each month.

.. note:: It may be easier to create and modify a complex rate table in a text editor or spreadsheet program than in the table. You can use the **Import** and **Export** buttons to work with the data as comma-separated text outside of SAM. To see what the data format looks like, create a table in SAM and export it to a text file.

**Number of entries**
  The number of demand charges in the rate structure. For a rate structure with a demand charge for each month and no tiers, the number of entries should be 12.

  For a demand rate structure with tiers, the number of entries should be the total number of tiers. The number of rows in the table is equal to the number of entries. When you change the number of entries, SAM changes the number of rows in the table.

.. note:: If you change the number of entries to a smaller number, you will lose data in the rows when SAM resizes the table.

**Month**
  The month for the demand rate. For a flat demand rate structure with no tiers, the table should have one row for each month of the year.

  For a demand rate structure with monthly tiers, choose a month for each tier by clicking a cell in the Month column, and choosing a month from the list.

  .. image:: ../images/SS_UtilityRate-demand-month.png
     :align: center
     :alt: SS_UtilityRate-demand-month.png

**Tier**
  The tier number(s) that apply to each month. Each tier is defined by a peak demand in kW and demand rate in $/kW. 

  For a demand rate structure with no tiers, set the tier value to 1 for all months.

  For a demand rate structure with tiers, set the number of entries to the total number of tiers so that the table has a row for each tier. For each tier, choose a month, and set the tier value, peak demand in kW and demand rate in $/kW.

  For example, if SAM calculates the billing demand for January as 700 kW and January has two tiers, Tier 1 = 500 kW at $10/kW and Tier 2 = 1000 kW at $20/kW, then the demand charge for January would be 500 kW × $10/kW + 200 kW × $20/kW = $9,000.

**Peak (kW)**
  For demand rates with tiers, the maximum demand for each tier.

  For a demand rate with no tiers, type a very large number for the peak demand (1e+38 is the largest number SAM will accept).

**Charge ($/kW)**
  The demand rate for each month (and tier, if applicable). SAM multiplies the demand rate in $/kW by the applicable billing demand to calculate the monthly demand charge portion of the monthly electricity bill.

Demand Rates by Time-of-use and/or Tiers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Use the Demand Rates by Time-of-use and/or Tiers rate table for a demand rate structure that includes time-of-use periods or a combination of time-of-use periods and tiers.

For a demand rate structure that varies with time of day and year with no tiers, use Tier 1 to specify the peak demand in kW and demand charge in $/kW for each period.   Assign a period number to the Weekday or Weekend schedules by using your mouse to select a block in the matrix and typing the period number on your keyboard. Type the letters a, b, or c for the period numbers 10, 11, and 12, respectively. The demand charge for Tiers 2-6 should zero.

For a demand rate structure that varies with time of day and year with tiers, specify a peak demand in kW and demand charge in $/kW for each tier that applies for each period. The peak demand for highest tier should be 1e+038. Then assign a period number to the Weekday and Weekend matrices for each period.

.. note:: It may be easier to create and modify a complex rate table in a text editor or spreadsheet program than in the table. You can use the **Import** and **Export** buttons to work with the data as comma-separated text outside of SAM. To see what the data format looks like, create a table in SAM and export it to a text file.

**Number of entries**
  The total number of time-of-use periods and tiers the demand rate structure. SAM sizes the table based on the number of entries.  The number of rows in the table is equal to the number of entries. When you change the number of entries, SAM changes the number of rows in the table.

.. note:: If you change the number of entries to a smaller number, you will lose data in the rows when SAM resizes the table.

**Period (1-12)**
  The time-of-use period must be a number between 1 and 12, and must also be defined in the either the :doc:`weekday or weekend schedule <../reference/weekday_schedule>`  . If tiers apply to a time-of-use period, assign the same period number to each tier in the period.

**Tier**
  The tier number(s) that apply to each time-of-use period. Each tier is defined by a maximum demand in kW, and demand rate in $/kW

  For a demand rate with time-of-use periods and no tiers, assign the tier value 1 to all periods.

**Peak (kW)**
  The peak demand for the tier and period.

**Charge ($/kW)**
  The demand rate that applies to the tier and period.
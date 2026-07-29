
Time-of-delivery (TOD) factors are a set of multipliers that apply to the PPA price so you can model a situation where the PPA price changes on an hourly, subhourly, and/or seasonal basis.

There are two ways to define TOD factors:

**TOD factors by schedule**
Use this option to define the factors using up to nine TOD periods with hour-by-month weekday and weekend schedule matrices, or to choose TOD schedules from a library of data.

**TOD factors by time step**
Use this option to assign a TOD factor to each simulation time step. For example, if you are running hourly simulations, you can assign a different TOD factor to each hour of the year.

SAM calculates the PPA price for each hour by multiplying the Year 1 :doc:`PPA price <../financial-metrics/mtf_ppa_price>` shown in the results Summary page by the TOD factors you define for each of up to nine periods.

 


.. note:: If your analysis is for a project with a fixed PPA price that does not vary with time of day or month of year, use the **TOD Factor by Schedule** option, select **Uniform Dispatch** in the library and click **Apply values from library** to ensure that all of the PPA multiplier values are set to one.


.. note:: When you click **Apply values from library**, SAM replaces the TOD data with data from the library. If you are using your own TOD data, you can use the **Save data to file** button at the bottom of the Time of Delivery Factors page to save your data.

**Download forecast price data from Cambium**
  If you do not have time series price data for your analysis, you can download hourly marginal cost data for modeled futures of the U.S. electricity sector from the `NLR Scenario Viewer <https://scenarioviewer.nlr.gov/>`__  . SAM runs the Cambium Time Series Prices :doc:`macro <../reference/macros>`, which prompts you to choose from the available price data, and then automatically sets the following inputs:

* Chooses the **Specify PPA price** option.

* Sets **PPA price** to $1/kWh

* Chooses the **TOD factors by schedule** option

* Converts the price data from $/MWh to $/kWh and set the **TOD factors by time step** to the downloaded price data.

  For a discussion of how to use Cambium modeled price data in your analysis, see https://energyanalysis.lbl.gov/publications/integrating-cambium-marginal-costs.

To populate values from the library:

#. Choose **TOD factors by schedule**.

#. Click a row in the library.

#. Click **Apply values from library**.

To specify your own TOD factors by schedule:

#. Choose **TOD factors by schedule**.

#. Type a PPA price multiplier value for up to nine periods.

#. Use the schedule matrices to define the hour and month for each period as described below.

To specify your own TOD factors by time step:

#. Choose TOD factors by time step.

#. Click **Edit array** and enter your TOD factors in the table. If you want to enter time series prices in the table as $/kWh or $/MWh values, set **PPA price** to $1/kWh or $0.001/kWh respectively.

To download price data from the Cambium database:

#. Click **Download forecast price data from Cambium** and follow the prompts.

About TOD Factors by Schedule
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**TOD factors**
  For each of the nine periods in the list, to calculate the PPA price in a given period, SAM multiplies the Year 1 PPA price by the multiplier you specify. For example, if Period 1 is the summer peak, defined as Noon to 7 p.m. in June through September with a multiplier of 2.38, and the Year PPA price is 6 cents/kWh, the price that the project would receive for each unit of energy it sells during those summer peak hours would be 6 cents/kWh × 2.38 = 14.28 cents/kWh.

**Weekday Schedule, Weekend Schedule**
  The weekday and weekend matrices allow you to associate each of the nine periods with a time of day and month of year. To use the matrices, for each period, draw a rectangle on the matrix with your mouse, and type the period number with your keyboard. See :doc:`Weekday Weekend Schedules <../reference/weekday_schedule>`   for details.

About the TOD Schedules and Factors Library
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The TOD Schedules and Factors library stores a selection of TOD data that you can use to explore different scenarios if you do not have data from another source. You can also use one of the library schedules as a starting point and modify it to meet your requirements. Remember to click **Apply values from library** to use data after you click a row in the library list.

The library is stored as a CSV file in the libraries folder of your SAM installation. For details about how to access the file, see :doc:`Libraries <../reference/libraries>`.

**Uniform Dispatch**
  This option removes TOD factors and schedules. Use this when the PPA price does not vary with time of day or month of year.

**Generic Summer Peak**
  This is a fictitious schedule with a June - September summer peak period and six TOD periods representing baseline, shoulder, and peak daily prices with with peak prices in summer afternoons.

**Generic Duck Curve**
  This is a fictitious schedule representing a basic "duck curve" shape with three TOD periods for a baseline night-time prices, peak evening prices, and low daytime prices.

**Generic Low Carbon Duck Curve**
  This schedule is based on data from an NLR California low carbon grid study for 2030 assuming 50% of the state's electricity generation is from renewable energy sources. See Brinkman, G.; Jorgenson, J. (2016) Low carbon grid study: Analysis of a 50% emission reduction in California. National Renewable Energy Laboratory. NREL/TP-6A20-64884. (`PDF 2.2 MB <https://www.nlr.gov/docs/fy16osti/64884.pdf>`__  )

**California PUC Advice Letter Data**
  For the 2015 and 2016 data in the library, the factors are from the following California Public Utilities Commission `Renewable Auction Mechanism Program <https://www.cpuc.ca.gov/industries-and-topics/electrical-energy/electric-power-procurement/rps/rps-procurement-programs/rps-renewable-auction-mechanism>`__ advice letters:

  * PG&E 2016: Advice Letter 4780-E (January 22, 2016)

  * SCE 2015: Advice Letter 3244-E (August 6, 2015)

  * SDG&E 2015: Advice Letter 2717-E Attachment A (July 13, 2015)

  PG&E and SDG&E offer different TOD options for projects that meet "energy only" or "full capacity deliverability" requirements, and SDG&E has options for "local" and "system" projects. Use your interconnection application and other documents to determine which TOD factors are appropriate for your project so you can choose the correct option in SAM.

  For the 2009 and 2011 data, the factors are California Energy Commission Market Price Referent (MPR) values adopted by the California Public Utilities Commission (CPUC) in 2011. The SAM library includes values from Appendix B of Resolution E-4442 (2011) and Resolution E-4298 (2009). See the `CPUC Resolution Search Form <https://docs.cpuc.ca.gov/resolutionsearchform.aspx>`__ to find these documents.

Time of Delivery Factors in Results
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SAM reports hourly, monthly, and annual TOD-related results. You can view these results on the :doc:`Data <../results/data>` and  :doc:`Graphs <../results/graphs>` pages.

Single Values
-------------

The single value results are available .

**First year energy from the system in TOD period*****n***
  The total quantity of electricity delivered to the grid (and sold) by the system in Year one for each of the nine TOD periods, in kWh per year.

**First year energy price for TOD period*****n***
  The power price for each of the nine TOD periods, equal to the product of the TOD factor for each period and the PPA price shown in the :doc:`Metrics table <../results/summary>`  .

**First year revenue from the system in TOD period*****n***
  The dollar value of electricity sold by the project in Year one for each of the nine TOD periods, in dollars per year.

Monthly Data
------------

The monthly variables are available on the Results page in :doc:`Graphs <../results/graphs>` and :doc:`Tables <../results/data>`.

**First year energy from the system by month for TOD period*****n***
  The total quantity of electricity delivered to the grid (and sold) by the system in Year one for the given TOD period in each month, in kWh.

**First year revenue from the system by month for TOD period*****n***
  Total dollar value of electricity sold by the project in Year one for each month.

Annual Data (All Years)
-----------------------

.. note:: The first row in the annual data table is equivalent to Year zero in the project :doc:`cash flow <../results/cashflow>`, before the system starts generating electricity.

**Energy produced by the system in TOD period*****n***
  The total amount of electricity delivered to the grid (and sold) by the system in each year of the analysis period for each of the nine TOD periods.

**Revenue from the system in TOD period*****n***
  The total dollar value of electricity sold by project in each year of the analysis period for each of the nine TOD periods.
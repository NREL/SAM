Generation Profile
==================

The Custom Generation Profile model allows you to represent a power plant using a simple model based on capacity factor and nameplate capacity, or to import hourly or subhourly electric generation data from another simulation model or measured from an operating system.

You can use the Custom Generation profile model for the following applications:

* Model a thermal power plant as a baseline case for comparison with renewable alternatives.

* Use power generation profiles for any type of power system from other software with SAM's financial models.

* Use measured data from an installed plant with SAM's financial models.

The Custom Generation Profile - Battery model combines the Custom Generation Profile model with the battery storage model. You can use the Custom Generation Profile - Battery model for a battery system that responds to an hourly or subhourly generation profile from another model or from data measured from a real system.

Combine Cases
~~~~~~~~~~~~~

Versions of SAM before SAM 2021.12.02 came with the "Combine Cases" macro that you could use to automatically calculate the generation profile by adding up generation data from other cases in the SAM file. This macro has been replaced by the **Generate production profiles and nameplate capacity from open cases** option described below.

After running a simulation, you can see the hourly energy values in the :doc:`Tables <../results/data>` or :doc:`time series graphs <../results/timeseries>` on the :doc:`Results <../getting-started/results_page>` page.

Custom Generation Profile
~~~~~~~~~~~~~~~~~~~~~~~~~

The custom generation profile model provides two overall options for describing the system's performance:

* Specify a nameplate capacity and capacity factor to model a constant generation profile.

* Import hourly or subhourly generation data from a file.

**Constant generation profile from nameplate capacity and capacity factor**
  Model a generation profile that remains constant throughout the day and year. For this option, you provide the nameplate capacity, capacity factor, and loss values as inputs, and SAM calculates a constant generation profile to match.

**Import single-year hourly or subhourly generation profile from file**
  Import hourly or subhourly generation data over one year into SAM. SAM will use the same generation profile for each year in the analysis period.

  .. note:: The analysis period is specified on the Financial Parameters page.

**Import lifetime hourly or subhourly generation profile from file**
  Import hourly or subhourly generation data over the analysis period. 

**Calculate generation profiles and nameplate capacity from open cases**
  Combine the generation profiles from other cases in the file to use as input for this case. This option only works for cases that run hourly simulations. For cases that run a simulation over the analysis period (lifetime simulation), SAM only uses the Year 1 data to generate the combined generation profile.

  .. note:: As of SAM 2023, hybrid system configurations are available that combine the output of PV, wind, custom generation profile and fuel cell models as an alternative to this Calculate Generation Profiles option.

**Nameplate capacity, kWac**
  The system's nameplate electrical capacity in electric kilowatts.

  If your analysis involves a financial model, SAM uses the nameplate capacity to calculate any capacity-based costs that you specify in $/W on the :doc:`Installation costs <../installation-costs/cc_custom_generation>`   and :doc:`Operating costs <../operating-costs/oc_operating>` pages and any capacity-based incentives on the :doc:`Incentives <../incentives-and-depreciation/cash_incentives>`   page. 

  It also uses the nameplate capacity to calculate the capacity factor when you import power generation data as input.

  This input is disabled for **Calculate generation profiles and nameplate capacity from open cases** because SAM automatically calculates the nameplate capacity from the values in the cases being combined.

**Nominal capacity factor, %**
  The nominal capacity factor is the plant capacity factor, not including plant losses or reduction in output due to system availability.

  The nominal capacity factor is only available with the **Constant generation profile from nameplate capacity and capacity factor** option. SAM uses the value to calculate the total annual generation value as described below.

  When you import or generate power generation data, this input is disabled because for these options, SAM does not use the capacity factor to calculate the generation profile.

  .. note:: SAM reports a separate :doc:`capacity factor <../performance-metrics/mtp_capacity_factor>` in the results after you run a simulation that reflects plant losses and any reduction in output due to system availability.

**Combined nameplate capacity, kWac**
  When you choose **Calculate generation profiles and nameplate capacity from open cases**, SAM automatically calculates the nameplate capacity as the sum of nameplate capacities of each case. You can edit this value if you want to use a different value than the calculated value.

**To import generation profile data:**

#. If you have generation data for a single year, choose **Import single-year hourly or subhourly generation profile from file**. If you have generation data over the analysis period, choose **Import lifetime hourly or subhourly generation profile from file**.

#. Click **Edit data** or single-year data, or **Edit lifetime data** for lifetime data to open a window for importing data.

#. If you are importing subhourly data for a single year, click **Change time step** and enter the time step in minutes. For hourly data, the time step should be 60 minutes. If you are importing lifetime data, choose the appropriate **Mode** (for the **Subhourly** mode,  choose **Time step in minutes**).

#. Click **Import** to import the data from a text file. 

   The data must be in a single column with one row for each time step. Each row should contain a value in kW of electricity generated per time step. For example, an hourly data set for a single year should contain 8,760 rows of kW values. A 15-minute data set for one year would contain 35,040 rows of kW values. Lifetime data sets should contain data over the same number of years as the analysis period on the Financial Parameters page.

   The first row is reserved for a header, so do not include any electricity generation data in the first row. SAM checks the number of data rows in the file to ensure it is consistent with the time step you specify. For example, for a 60-minute time step over a single year, the text file should contain 8761 rows: One row at the top of the file for the header followed by 8760 data rows. A 15-minute data set would contain a total of 35,041 rows.

   To paste the data from a text editor, spreadsheet, or other software, copy the data to your clipboard, and then click **Paste**.

#. Click **OK** to return to the Power Plant page.

#. Type a value for **Nameplate capacity**. This number might be the same as the peak annual generation, or a different value.

**To generate generation profiles from other cases:**

#. Click **Add** in the menu bar at the top of the window to create a case for each system's output that you want to include in the generation profile.

#. Choose **Calculate generation profiles and nameplate capacity from open cases**.

#. Click **Select cases** to choose the cases you want to use to calculate the production profile.

#. Click **OK** to start the process: SAM automatically runs a simulation for each case you selected and generates the combined generation profile.

#. Verify that the value in **Nameplate capacity** is the value you want to use for your analysis and change it if necessary.

#. Click **Edit array** to verify the time series production data.

Losses
------

The power plant loss accounts for   output reductions caused by inefficiencies in the system, such as from wiring losses or other factors.

**Power plant loss**
  A percentage applied to the system's output in each time step for both the constant generation and imported data options. For example, a value of 5% would reduce the system output in each time step by 5%. Set the loss percentage to zero if all losses are accounted for in the nameplate capacity, capacity factor, or imported generation data.

Calculated Values Based on Input Assumptions
--------------------------------------------

The calculated values are equivalent to the metrics shown on the Results page after a simulation. Use these values to verify that the nameplate capacity and capacity factor are correct, or that data you imported from a file and nameplate capacity are correct.

**Total annual generation**
  The system's total annual output. If you are using the custom generation profile model with a financial model with a project cash flow, this value is equivalent to the system's output in its first year of operation.

  For the constant generation option: *Total Annual Generation (kWh/year) = Nameplate Capacity (kW) × Capacity Factor (%) ÷ 100 % × ( 1 - Loss (%)  ÷ 100% ) × 8,760 hours/year*

  For the import generation from file option: *Total Annual Generation (kWh/year) = Sum of kW Values in Profile (kW) × ( 1 - Loss (%)  ÷ 100% ) × 8,760 hours/year ÷ Number of time steps in profile*

**Peak annual generation**
  The system's maximum output in kW over one year. This value is useful for choosing a nameplate capacity when you import generation data from a file. In some cases, it may be appropriate to set the nameplate capacity to the peak generation value, or to one slightly higher or lower depending on the purpose of your analysis.

  The peak annual generation is what you predict the system's actual peak output will be, and the nameplate value is a nominal value that SAM uses for $/kW cost calculations in the financial model.

**Capacity factor after plant loss**
  The capacity factor, accounting for the reduction in output when you specify a non-zero plant loss percentage. Use this capacity value value when you import generation data to verify that the nameplate capacity you enter is reasonable. For example, you might expect a capacity factor greater than 90% for a thermal power plant, about 20% for a photovoltaic system, 40% for a concentrating solar power system, or about 60% for a wind system.

  .. note:: The capacity factor after plant loss shown on the Power Plant page accounts for the plant loss, but not for :ref:`system availability <availcurtail>`. If you include system availability losses in your analysis, the :doc:`capacity factor <../performance-metrics/mtp_capacity_factor>` SAM reports in the results after you run a simulation reflects those losses.

  For the constant generation option, the calculated capacity factor depends on the value that you entered for **Capacity Factor**: *Capacity Factor After Plant Loss (%) = Capacity Factor (%) × ( 1 - Loss (%) ÷ 100% )*

  For the import generation from file option: *Capacity Factor After Plant Loss (%) = Total Annual Generation (kWh) ÷ Peak Annual Generation (kW) ÷ 8760 hours/year × 100%*

Heat Rate for Fuel Cost Calculation
------------------------------------

You only need to specify a heat rate if you are using the custom generation profile model for a thermal power plant that consumes a fuel.

**Heat rate**
  SAM uses the heat rate value to calculate the cost of fuel consumed by the system using a simple conversion of the kWh values you specify for the energy output to equivalent MMBtu of fuel: *Fuel Cost ($/yr) = Fossil Fuel Cost ($) × Heat Rate (MMBtu/MWhe) × Energy (kWh/yr) ÷ 1000 (kWh/MWh)*

  Where Fossil Fuel Cost is an annual cost in $/MMBtu that you specify on the :doc:`Operating costs <../operating-costs/oc_operating>`   page, under Operation and Maintenance costs, and Fuel Cost and Energy are reported on the :doc:`Results <../getting-started/results_page>`   page.

  If you are using the generic plant to represent a system that does not consume fuel, you should change the heat rate value to zero.

**Nominal thermal-to-electric conversion efficiency**
  SAM calculates and displays the system's thermal to electric conversion efficiency for reference to help you verify that the heat rate you specified is reasonable. SAM does not use this value during the simulation: *Thermal-to-electric conversion efficiency = 100% ÷ Heat Rate (MMBtu/MWhe) ÷ 0.2931 (Wh/Btu)*

.. _availcurtail:

System Availability
~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_system_availability.rst


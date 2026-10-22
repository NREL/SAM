Data Tables
===========

The **Data Tables** tab on the Results page allows you to create a table of time series data from the performance and financial models.

To view time series data in graphical form, use the :doc:`Time Series <timeseries>` tab.

For descriptions of some key output metrics, see :doc:`Financial Metrics <../financial-metrics/mtf_overview>` and :doc:`Performance Metrics <../performance-metrics/mtp_overview>`.

To create a table of data:

#. On the Results page, click the **Data tables** tab. You should see a list of variable categories: Single Values, Monthly Data, etc. If you do not see the list, :doc:`run a simulation <../getting-started/run_simulations>` to generate results.

.. image:: ../images/SS_Results-ButtonsDataBrowser.png
   :align: center
   :alt: SS_Results-ButtonsDataBrowser.png

#. Expand the list to display variables in each category, and choose variables to display in the table. The categories depend on the number of values for each variable.

Type a few letters of the variable name to filter the list of variables.

.. image:: ../images/SS_Results-DataChoose.png
   :align: center
   :alt: SS_Results-DataChoose.png

Data Categories
~~~~~~~~~~~~~~~

The data categories are based on the number of values that a variable represents.

.. image:: ../images/SS_Results-DataTableCategories.png
   :align: center
   :alt: SS_Results-DataTableCategories.png

**Single Values**
  Results that have a single value, including:

*   Some input variable values from the :doc:`Installation costs <../installation-costs/index>`, :doc:`Operating costs <../operating-costs/operating_costs>`, :doc:`Financial Parameters <../financial-parameters/fin_overview>`, and :doc:`Incentive <../incentives-and-depreciation/cash_incentives>`   pages.

*   Annual totals or averages of hourly performance model results, or time-dependent pricing data.

**Monthly Data**
  Results that have of twelve values, including monthly averages and totals of hourly values

**Annual Data**
  Results that have a value for each year in the analysis period, including:

* :doc:`Cash flow <cashflow>`   data

*   Annual output data

.. note:: Row 1 in the annual data table is equivalent to Year zero in the project :doc:`cash flow <cashflow>`.

**Hourly Data**
  Results with 8,760 hourly values from the performance model results.

If you do not see the **Hourly Data** list, :doc:`run a simulation <../getting-started/run_simulations>` to generate the list. SAM deletes stored hourly data when you close the file to save storage space.

For subhourly simulations, SAM displays an additional category for the smallest time step, e.g., **1 Minute Data**, **15 Minute Data**, etc.

**Utility Rate Data by Tier/Period**
  Results from the Residential and Commercial financial models showing monthly dollar and kWh totals by utility rate structure time-of-use period and tier.

**Utility Rate Data by Year**
  Results from the Residential and Commercial financial models showing monthlydollar and kWh totals totals by year.

Exporting Data from Tables
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. image:: ../images/SS_Results-DataTableExport.png
   :align: center
   :alt: SS_Results-DataTableExport.png

SAM offers three options for exporting data:

* **Copy to clipboard** copies the table to your clipboard. You can paste the entire table into a word processing document, spreadsheet, presentation or other software.

* **Save as CSV** saves the table in a comma-delimited text file that you can open in a spreadsheet program or text editor.

* **Send to Excel** saves the table in an Excel file (Windows only).
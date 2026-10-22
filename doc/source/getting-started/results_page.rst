Results
=======

After you run a simulation, SAM displays results from both the performance and financial models. You can :doc:`export data <../reference/export_data>` from any graph or table displayed on the results pages to Excel or text files.

To display results:

* Click **Simulate** to run a simulation and display results.

.. image:: ../images/SS_MainWindow-RunAllSimulationsButton.png
   :align: center
   :alt: SS_MainWindow-RunAllSimulationsButton.png

Or, click Switch to Results |SS_Button-Results| to show results without running a simulation.

.. image:: ../images/SS_Button-ExportViewData.png
   :align: center
   :alt: SS_Button-ExportViewData.png

Use the tabs along the top of the Results page to display tables and graphs of data:

.. image:: ../images/SS_Results-Tabs.png
   :align: center
   :alt: SS_Results-Tabs.png

If you do not see all of the tabs because of the width of your computer's screen, click the ellipses to show tabs that do not fit in the horizontal bar:

.. image:: ../images/SS_Results-Ellipses.png
   :align: center
   :alt: SS_Results-Ellipses.png

Performance Model Results
-------------------------

* :doc:`Summary <../results/summary>` displays the :doc:`Metrics table <../results/summary>` with key metrics and graphs that summarize the performance model results, such as total annual electrical output, capacity factor, etc.

* :doc:`Losses <../results/losses>` shows an energy loss diagram (not available for all performance models).

* :doc:`Graphs <../results/graphs>` allows you to create graphs of variables with hourly, monthly, annual, and single values.

* :doc:`Data <../results/data>` allows you to build  tables of hourly, monthly, and annual results.

* :doc:`Time series <../results/timeseries>` graphs display time series data and statistical graphs of hourly or subhourly data.

* :doc:`Profiles <../results/profiles>` displays time series data as daily profiles by month.

* :doc:`Statistics <../results/statistics>` displays the mean, maximum, minimum, sum, standard deviation, and average daily minimum and maximum values of time series data.

* :doc:`Heat map <../results/heatmap>` displays one year's worth of time series data set in a single two-dimensional graphical representation using colors to indicate magnitude.

Financial Model Results
-----------------------

SAM's financial model uses the sum of the performance model's 8,760 hourly output values in kWh as an input representing the system's total annual electrical output in kWh. The financial model then calculates the project's cash flow based on the inputs you specify on the Installation Costs, Operating Costs, and Financial Parameters pages. SAM displays financial model results in the following places:

* :doc:`Summary <../results/summary>` displays the :doc:`Metrics table <../results/summary>` with key metrics such as the LCOE, PPA price, IRR, and payback period and graphs of the project's after-tax cash flow.

* :doc:`Cash Flow <../results/cashflow>` shows details of the project's cash flow.

* :doc:`Data <../results/data>` allows you to build tables of cost and cash flow data along, metrics, and time-dependent electricity sales and price data.

* :doc:`Time series data <../results/timeseries>` includes time-dependent electricity sales and price data.

.. |SS_Button-Results| image:: ../images/SS_Button-Results.png

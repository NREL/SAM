Edit Data Table by Column
=========================

For some variables, you can specify either a single value, or one or two columns of values over a range. The range might be a range of temperatures for parabolic trough absorber emittances, or a range of values over the analysis period for annual operation and maintenance costs.

Variables with an option to enter a table of values have a small **Value** / **Sched** or **Value / Table** button next to the variable label.

When the word "Value" is highlighted in blue or green, the variable is in Value mode and has a single value that you define by typing its value in the input box: 

|SS_AnnSched-Value| 

.. image:: ../images/SS_AnnSched-Value-Green.png
   :align: center
   :alt: SS_AnnSched-Value-Green.png

When the word "Sched" or "Table" is highlighted in blue or green, the variable is in Schedule or Table mode and has multiple values that you define using a table (the values do not have to follow a pattern):

.. image:: ../images/SS_AnnSched-SchedEdit.png
   :align: center
   :alt: SS_AnnSched-SchedEdit.png

.. image:: ../images/SS_AnnSched-Green.png
   :align: center
   :alt: SS_AnnSched-Green.png

.. image:: ../images/SS_AnnSched-Table.png
   :align: center
   :alt: SS_AnnSched-Table.png


 
.. note:: In Schedule or Table mode, be sure to use values with the correct units.


.. note:: In some cases SAM automatically sets the number of rows in the table to the analysis period and does not show **Number of values** in the window.


.. note:: When you specify currency values in Schedule mode, use the nominal or current dollar values. SAM does not apply the inflation rate to values you specify in an annual schedule.


.. note:: When you specify annual rates in Schedule mode, SAM applies the rate in each year to the first value in the table. For example, on the :doc:`Degradation <../degradation/degradation>` page, a degradation rate 0.5% for Year 5 when the Year 1 net annual output is 10,000 kWh results in an annual output value of (1 - 0.005) × 10,000 kWh = 9,950 kWh in year 5. Similarly, on the :doc:`Electricity Rates <../electricity-rates-and-load/electricity_rates>` page, for a Year 1 buy rate of $0.10/kWh and an escalation rate of 0.25% results in a Year 3 buy rate of (1 + 0.0025) × $0.10/kWh = $0.10025/kWh.

To enter a schedule of values for an input:

#. Click **Sched** or **Table** to change the variable's mode from a single value to schedule mode. SAM displays the Edit button.

|SS_AnnSched-SchedEdit| 

.. image:: ../images/SS_AnnSched-Green.png
   :align: center
   :alt: SS_AnnSched-Green.png

#. Click **Edit** to open the Edit Data Table window.

.. image:: ../images/SS_AnnSched-Table.png
   :align: center
   :alt: SS_AnnSched-Table.png

#. If **Number of values** is available, type the number of rows you need in the table. For an annual schedule, this number should be equal to or less than the number of years in the analysis period defined on the :doc:`Financial Parameters <../financial-parameters/fin_overview>` page.

If **Number of values** is not available, SAM automatically sets the number of rows equal to the analysis period.

.. note:: If you are assigning annual values to a variable and you specify a number of values greater than the number of years in the analysis period, SAM ignores any values in the table for years after the end of the analysis period, which does not affect analysis results. However, if you specify a number less than the analysis period, SAM assigns a zero to each year after the number of years you specify, which may cause unexpected results.

#. For each row in the table, type a valid value for the input.

You can also copy a row of values from Excel, or a line of comma-separated values from a text file to your computer's clipboard, and click **Paste** to paste them into the table.

#. Click **OK** to save the values and return to the SAM input page.

.. |SS_AnnSched-Value| image:: ../images/SS_AnnSched-Value.png
.. |SS_AnnSched-SchedEdit| image:: ../images/SS_AnnSched-SchedEdit.png

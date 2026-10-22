
SAM allows you to operating costs either as a single annual cost, or as an annual schedule or table of costs. An annual schedule makes it possible to assign costs to particular years in the analysis period. Annual schedules can be used to account for component replacement costs and other periodic costs that do not recur on a regular annual basis.

You choose whether to specify an operating cost as a single annual value or an annual schedule with the grey and blue button next to each variable. SAM uses the option indicated by the blue highlight on the button: A blue highlighted “Value” indicates a single, regularly occurring annual value. A blue highlighted “Sched” indicates that the value is specified as an annual schedule.

For example, to account for component replacement costs, you can specify the fixed annual cost category as an annual schedule, and assign the cost of replacing or rebuilding the component to particular years. For a 30-year project using a component with a seven-year life, you would assign a replacement cost to years seven, 14, and 21. Or, to account for expected improvements in the component's reliability in the future, you could assign component replacement costs in years seven, 17, and 27. After running a simulation, you can see the replacement costs in the project :doc:`cash flow <../results/cashflow>` in the appropriate column under Operating Expenses. SAM accounts for the operating costs in the other economic metrics including the levelized cost of energy and net present value (NPV).

.. note:: If you use the annual schedule option to specify equipment replacement costs, SAM does not calculate any residual or salvage value of system components based on the annual schedule. SAM calculates salvage value separately, using the salvage value you specify on the Financial Parameters page.

Dollar values in the annual schedule are in nominal or current dollars. SAM does not apply inflation and escalation rates to values in annual schedules.

The following procedure describes how to define the fixed annual cost category as an annual schedule. You can use the same procedure for any of the other operation and maintenance cost categories.

To assign component replacement costs to particular years:

#. In the Fixed Annual Cost category, note that the "Value" label of the grey and blue button is blue indicating that the single value mode is active for the variable.

.. image:: /images/SS_AnnSched-Value-OM.png
   :align: center
   :alt: SS_AnnSched-Value-OM.png

In this case, SAM would assign an annual cost of $284 to each year in the project cash flow.

#. Click the button so that "Sched" label is highlighted in blue. SAM replaces the variable's value with an Edit button.

.. image:: /images/SS_AnnSched-SchedEdit.png
   :align: center
   :alt: SS_AnnSched-SchedEdit.png

#. Click **Edit** to open the :doc:`Edit Schedule <../window-reference/win_edit_data_table_column>` window.

#. In the Edit Schedule window, use the vertical scroll bar to find the year of the first replacement, and type the replacement cost in current or constant dollars for that year.

To delete a value, select it and press the Delete key on your keyboard.

.. note:: You must type a value for each year. If you delete a value, SAM will clear the cell, and you must type a number in the cell or SAM will consider the schedule to be invalid. Type a zero for years with no annual costs.

#. When you have finished editing the schedule, click **Accept**.

Because you must specify an operating cost category as either an annual cost or annual schedule, to assign both a recurring annual fixed cost and periodic replacement cost, you must type the recurring cost in each year of the annual schedule, and for years with replacement costs, type the sum of the recurring and replacement costs.
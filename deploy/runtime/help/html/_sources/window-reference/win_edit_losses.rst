Edit Losses
===========

The Edit Losses window allows you to define losses in the system.

* Constant loss is a single loss that applies to all simulation time steps.

* Time series losses apply to specific time steps.

SAM applies the loss percentages to each applicable simulation time step as appropriate. For a given time step, a loss of zero would result in no adjustment. A loss of 5% would result in a reduction of 5%, while a loss of -5% would result in an increase of 5%.

.. note:: SAM applies all losses that apply to a given time step. If the constant loss is not zero, and both a time series loss and custom period loss apply to the same time step, all three losses would apply to that time step. For example, if you specify a constant loss of 5%, time series losses in hourly mode of 2% for Hours 745-468, and custom period losses of 25% for the first seven days in February, and the loss is being applied to a value of 455 for February 1 at 11:00 am, the value after losses would be 455 × ( 1 - 0.05 ) × ( 1 - 0.02 ) × ( 1 - 0.25 ) = 317.7.

.. image:: ../images/SS_Hourly_Factors.png
   :align: center
   :alt: SS_Hourly_Factors.png

**Constant loss**
  A loss that applies to all simulation time steps.

**Enable time series losses**
  Check the box to enable losses by time step. Click **Edit time series losses** to open the :doc:`Edit Lifetime Data <win_edit_data_lifetime>`   window, and use the table in the window to specify the losses.

  The check box allows you to enable and disable time series losses without deleting the data in the loss table.

.. note:: If you choose **Hourly**, **Subhourly**, or **Three Hourly** for **Mode**, the mode must be consistent with the simulation time step. For example, the Three Hourly mode is for Marine Energy models and will not work with solar or wind models that run hourly or subhourly  simulations.

**Enable time series losses with custom periods**
  Check the box when you want to apply losses to specific time periods. Choose a date and time for the start time and end time, and specify a loss for that period. Click **Add period** to specify more than one time period. In the example above, a 5% loss applies from April 1 12:15 pm to April 7 11:30 pm, and a 2% loss applies from September 15 8:30 am to September 20 5:00 pm.

  The check box allows you to enable custom period losses without deleting the loss schedule. 

System availability losses are reductions in the system's output due to operational requirements such as maintenance down time or other situations that prevent the system from operating as designed. 

.. note:: To model curtailment, forced outages or reduction in power output required by the grid operator, use the inputs on the :doc:`Grid Limits <../grid/grid_limits>` page. The Grid Limits page is not available for all performance models. For the PV Battery model, battery dispatch is affected by the system availability losses. For the PVWatts Battery, Custom Generation Profile - Battery, and Standalone Battery models, battery dispatch ignores the system availability losses.

To edit the system availability losses, click **Edit losses**.

The :doc:`Edit Losses <../window-reference/win_edit_losses>` window allows you to define loss factors as follows:

* Constant loss is a single loss factor that applies to the system's entire output. You can use this to model an availability factor.

* Time series losses apply to specific time steps.

SAM reduces the system's output in each time step by the loss percentage that you specify for that time step. For a given time step, a loss of zero would result in no adjustment. A loss of 5% would reduce the output by 5%, and a loss of -5% would increase the output value by 5%.
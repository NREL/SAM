System Control
==============

The System Control inputs determine the operating parameters of the system.

Plant Energy Consumption
~~~~~~~~~~~~~~~~~~~~~~~~

**Fraction of gross power consumed at all times, MWe/MWcap**
  A fixed electric load applied to all hours of the simulation, expressed as a fraction of rated gross power at design from the :doc:`System Design <../csp-power-tower-molten-salt/mspt_system_design>`   page.

**Balance of Plant Parasitic, MWe/MWcap**
  Losses as a fraction of the power block nameplate capacity that apply in hours when the power block operates.

**Aux heater boiler parasitic (MWe/MWcap)**
  A parasitic load that is applied as a function of the thermal output of the auxiliary fossil-fired heaters.

The BOP and auxiliary heater parasitic at design are calculated as follows:

*Parasitic Loss (MWe) = P (MWe/MWcap) × F × ( C0 + C1 + C2 ) × Design Turbine Gross Output (MWe)*

System Availability
~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_system_availability.rst

Dispatch Optimization
~~~~~~~~~~~~~~~~~~~~~

When you run a simulation without enabling optimization, the plant controller determines operation based on the value of the PPA multiplier at that time step. If the multiplier is less than one, then TES charges if the TES can accept the energy. If the multiplier is greater than one, then the TES discharges if TES has available energy.

**Enable dispatch optimization**
  Check the box to enable automatic optimization.

**Time horizon for dispatch optimization**
  The time period that SAM uses as a basis for the optimization. Default value is 48 hours.

**Frequency for dispatch reoptimization**
  The time period to determine how often SAM runs the optimization. Default value is 24 hours.

**Cycle startup cost penalty, $/start**
  The penalty in the dispatch optimization algorithm associated with starting up the power cycle. The cost is applied any time the power cycle goes from an “off” state to an “on” or “standby” state in the next time period. This penalty affects the optimal solution, which seeks to maximize revenue. This value *does not*   affect actual operating costs or the calculated SAM financial metrics.

**Heater startup cost penalty, $/start**
  The penalty in the dispatch optimization algorithm associated with starting up the solar field and receiver. The cost is applied any time the solar field goes from an “off” state to an “on” state in the next time period. This penalty affects the optimal solution, which seeks to maximize revenue. This value *does not*   affect actual operating costs or the calculated SAM financial metrics.

**Power generation ramping penalty**
  The penalty imposed for changing power cycle electrical production from one time step to the next. By penalizing changes, the resulting dispatch profile exhibits improved stability and is potentially more realizable in practice. Increasing this penalty may reduce achieved revenue for the project. This penalty affects the optimal solution, which seeks to maximize revenue. This value does not affect actual operating costs or the calculated SAM financial metrics.

**Minimum cycle downtime**
  The minimum duration of time the cycle must remain off after turning off. This parameter, along with the startup penalty, can help eliminate frequent cycle startup and shutdown events.

**Minimum cycle uptime**
  The minimum duration of time the cycle must remain on after turning on. This parameter, along with the startup penalty, can help eliminate frequent cycle startup and shutdown events.

**Time horizon for dispatch optimization**
  The period over which a single call to mixed integer linear program is optimized to maximize revenue.

**Frequency for dispatch reoptimization**
  The period over which the engineering model solves using the dispatch signal before the problem is reoptimized for the next horizon. For example, if the time horizon is 48 hours and the frequency is 24 hours, then the simulation rolling time horizon begins as follows: 1) dispatch is optimized for hours 1-48, 2) engineering model uses dispatch signal to solve for hours 1-24, 3) dispatch is optimized for hours 25-72 using initial conditions from last timestep solved by engineering model, 4) engineering model uses dispatch signal to solve for hours 25-48. 5) repeat over annual simulation.

**Objective function time weighting exponent**
  The relative weight due to time in the dispatch optimization objective function. A weighting factor of 0.99 indicates that the objective function terms are multiplied by *0.99**\ :sup:`t`\*    for each timestep t in the optimization horizon (48 hours, by default). A value of 1.0 indicates no time weighting, a value less than one indicates that – all things equal – generation is preferred sooner than later, and a value greater than one indicates that generation is preferred later than sooner. As the value is displaced from unity, the optimization algorithm is typically able to solve the dispatch problem more quickly, but the resulting revenue may decrease. Note that a value of 0.99 corresponds to an objective discounting in the 24th time period (one day ahead) of *0.99**\ :sup:`24`\**= 0.79*, which is to say that the optimization routine values revenue generated in hour one 21% more than in hour 24, though revenue multipliers and efficiency terms may be identical.

**Maximum branch and bound iterations**
  Limits the number of iterations in the optimization routine. If you are experiencing problems with the optimization, you can increase the number. The default value is 30,000.

**Solution optimality gap tolerance**
  Determines the tolerance for the optimization solution. You can decrease the tolerance if you are experiencing problems with the optimization. Default value is 0.001.

**Optimization solver timeout limit, seconds**
  Limits the amount of time the optimization will attempt to find a solution. You can increase the timeout limit if you are experiencing problems with the optimization. Default value is 5 seconds.
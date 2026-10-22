
The Charge Limits and Priority inputs are constraints on the battery dispatch calculations.

The limits on minimum and maximum state of charge affect both the amount of energy available for battery dispatch calculations, and the depth of charge-discharge cycles for battery life calculations. Choose values appropriate for the type of battery you are modeling. The default values state of charge range of 15% to 95% is reasonable for most Li-ion battery types.

**Minimum state of charge, %**
  A limit on the quantity of energy that can be drained from the battery as a percentage of available capacity. For example, a value of 15% would prevent the battery from discharging below 15% of the battery's available capacity in a given time step.

  .. Note:: The minimum and maximum state of charge are both as a percentage of available battery capacity, which changes over time as the batteries degrade and are replaced. The available capacity in a given time step is likely to be less than the battery's nominal capacity shown on the Battery Cell and System page.

**Maximum state of charge, %**
  A limit on the quantity of energy that can be sent to the battery as a percentage of available capacity. For example a value of 95% would prevent the battery from charging above 95% state of charge.

**Initial state of charge, %**
  The state of charge of the battery at the beginning of the simulation. A value of 100% would be for a fully charged battery, 0% would be for a fully discharged battery.

**Minimum time at charge state, min**
  There may be periods of time where the photovoltaic or other power system output varies above and below the load causing rapid cycling of the battery. Rapid cycling may also result for dispatch options that respond to power prices when prices change rapidly from time step to time step. This kind of cycling, especially if the cycles are deep, may degrade battery performance over time. The minimum time at charge state prevents the battery from changing between charging and discharging within the number of minutes that you specify.

  .. Note:: The minimum time at charge state only applies when the simulation time step is smaller than the minimum time at charge state. For example, the default value of 10 minutes would only apply for 5-minute or 1-minute simulation time steps; It has no effect for hourly simulations.
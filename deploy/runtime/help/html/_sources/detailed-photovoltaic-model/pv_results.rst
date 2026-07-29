Detailed PV Model Results
=========================

You can find the Detailed Photovoltaic model time series results on the :doc:`Results <../getting-started/results_page>` page after running a simulation. SAM displays time series data under time series data under Lifetime and Hourly (or n-minute) headings, depending on the simulation time step. 

* The Lifetime results are outputs of the performance model simulation, which runs over the analysis period from the Financial Parameters page.

* The Hourly (or n-minute) Data results are for weather file records and other inputs that are for a single year. The Detailed Photovoltaic model uses the same weather data for each year of the lifetime simulation.

The time series results are available on the following tabs of the Results page:

* :doc:`Data tables <../results/data>` 

* :doc:`Time series <../results/timeseries>`

* :doc:`Profiles <../results/profiles>`

* :doc:`Statistics <../results/statistics>`

* :doc:`Heat map <../results/heatmap>`

* :doc:`PDF/CDF <../results/pdfcdf>`


SAM also reports monthly and annual totals of some of the time series values under the headings **Single Values**, **Monthly Data**, and **Annual Data**.

Detailed Photovoltaic Time Series Results
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Variable Name
     - Units
     - Description
   * - AC lifetime daily loss
     - kW
     - AC power loss specified on the DC Degradation page in %/day. This input is only available for the Detailed Photovoltaic model cases with a financial model. The loss percentage is applied to the inverter gross AC output power after the AC wiring, transformer, and transmission loss.
   * - AC performance adjustment loss
     - kW
     - The combined constant, time series, and time series with custom period AC loss specified on the Losses page under "System Availability." It is applied to the inverter gross AC output after the AC wiring, transformer, transmission, and AC lifetime daily loss, and shown on the loss diagram as "AC Availability and curtailment."
   * - AC wiring loss
     - 
     - AC loss calculated from the AC wiring loss percentage on the Losses page and applied to calculate the inverter AC output power.
   * - Absolute air mass
     - 
     - The optical length of the atmosphere at sea level, empirically corrected for atmospheric pressure to account for elevation and weather effects.
   * - Albedo
     - 
     - The albedo (ground reflectance) value, either from the weather file, or from the input value on the System Design page, depending on whether Use albedo in weather file if it is specified is checked.
   * - Array POA
     - kW
     - These Array POA variables in kW represent the solar radiation incident on all of the subarrays in the array. They are helpful as estimates of losses between the available sunlight and sunlight that reaches the photovoltaic cells in the subarrays. The Subarray [n] POA variables in W/m² are actual values used in the model's power calculations.


       **beam radiation after shading soiling**

         beam radiation incident on the entire array after soiling, shading, and snow losses are applied for all subarrays


       **front-side beam radiation nominal**

         beam radiation incident on the front of the entire array before shading and soiling factors are applied


       **front-side total radiation**

         total radiation incident on the front side of the entire array


       **front-side total radiation nominal**

         solar energy available before any irradiance losses are applied.


         The sum of these values over Year 1 is the Nominal POA irradiance (kWh) value shown in the Losses diagram on the Results page.


       **front-side total radiation after shading**

         accounts for shading and snow losses from the Shading and Layout page


       **front-side total radiation after shading soiling**

         accounts  for those losses plus soiling losses from the Losses page


       **front-side total radiation after shading soiling reflection (IAM)**

         accounts for shading, soiling and reflection losses from the module cover (also called incident angle modifier, or IAM losses)


       **radiation total shading soiling reflection (IAM)**

         solar energy available to the entire array for conversion to electricity. For mono-facial modules, this represents the energy available on the front side of the module. For bifacial modules, it is the energy available from both the front and rear sides of the module


       **rear-side total radiation after reflection (IAM)**

         total radiation incident on the rear side of the entire array, accounting for all losses.  This is the solar energy available to the photovoltaic cells on the rear of the module for conversion to electricity


       **rear-side radiation blocked by [...]**

         indicate how much of the rear-side radiation irradiance is blocked by structures under the array, and by shading and soiling


       **rear-side radiation reflected from [...]**

         indicate how much of the rear-side radiation is from the ground and from neighboring rows
   * - Array bifacial electrical mismatch loss
     - kW
     - Loss due to voltage differences in rear-side power.
   * - DC lifetime daily loss
     - kW
     - DC power loss specified on the DC Degradation page in %/day. This input is only available for the Detailed Photovoltaic model cases with a financial model. The loss percentage is applied to the nominal DC power after other DC losses from the Losses page.
   * - DC power loss due to snow
     - kW
     - Reduction in DC power due to snow covering the array, reported when **Estimate snow losses** is checked on the Shading and Layout page.
   * - Inverter AC output power
     - kW
     - Power at the inverter output, before AC, transformer, and availability losses.
   * - Inverter DC input power
     - kW
     - Power at the inverter DC input, after DC losses.
   * - Inverter MPPT [n] Nominal DC voltage
     - V
     - The DC voltage at inverter MPPT input *n*.  For inverters with one input, the inverter DC voltage is Inverter MPPT 1 Nominal DC voltage. For inverters with multiple MPPT inputs defined on the Inverter and System Design pages, the voltage for each input *n* is Inverter MPPT [n] Nominal DC voltage.
   * - Inverter clipping loss AC power limit
     - kW
     - Inverter output in excess of the inverter's rated output. During hours with clipping losses, the inverter's AC output in kW should be equal to the inverter's rated capacity in kW.
   * - Inverter clipping loss DC MPPT voltage limits
     - kW
     - Reduction in output caused by the array DC voltage falling outside of the inverter minimum or maximum MPPT voltage ratings.
   * - Inverter efficiency
     - %
     - The inverter DC-to-AC conversion efficiency, calculated as the inverter AC output power divided by the inverter DC input power.
   * - Inverter night time loss
     - kW
     - Inverter's AC electricity consumption at night when the array's output is zero, equal to the value from the Inverter page.
   * - Inverter power consumption loss
     - kW
     - Inverter's electricity consumption during all hours, calculated based on the value from the Inverter page. SAM does not calculate this value with the Inverter Datasheet model with weighted efficiency, or with the Inverter Part Load model because the loss is assumed to be included in the weighted efficiency values.
   * - Inverter thermal derate DC power loss
     - kW
     - The reduction in electrical power caused by the effect of heat on the inverter, determined by the inverter temperature derate curve on the Inverter page.
   * - Inverter total power loss
     - kW
     - Power losses due to power use at night and power use during operation inputs on the Inverter page.
   * - Irradiance DNI, DHI, GHI  from weather file
     - W/m²
     - The diffuse normal irradiance, diffuse horizontal irradiance, and global horizontal irradiance values from the weather file. "NaN" indicates that the weather file does not contain the data. The Detailed Photovoltaic model requires that the weather file contains data for at least two of these irradiance components.
   * - Irradiance DNI, DHI, GHI calculated
     - 
     - The direct normal irradiance, diffuse horizontal irradiance, and global horizontal irradiance values that SAM calculates from the the irradiance data in the weather file when data for that irradiance component is missing from the weather file. The Detailed Photovoltaic model irradiance calculations require DNI and DHI data. When you use a weather file that is missing one of these irradiance components, the model calculates values for the missing component. Choose the appropriate **DNI and GHI**, **GHI and DHI**, and **DNI and DHI** option on the Location and Resource page to tell SAM what irradiance data to use from the weather file.
   * - Irradiance POA from weather file
     - W/m²
     - Plane-of-array total irradiance from the weather file. "NaN" indicates that the weather file does not contain POA irradiance data. SAM uses this data as input instead of the DHI, DNI, and/or GHI data when you choose the **POA from reference cell** or **POA from pyranometer** option on the Location and Resource page.
   * - Irradiance absorbed by the ground within the array
     - kW
     - Solar irradiance available on the ground occupied by the array after ground reflectance. Useful for agrivoltaic applications to estimate energy available for crops.
   * - Irradiance incident on the ground within the array
     - kW
     - Solar irradiance available on the ground occupied by the array before ground reflectance. Useful for agrivoltaic applications to estimate energy available for crops.
   * - Subarray 1..4 Angle of incidence
     - deg
     - Solar angle of incidence on the photovoltaic array either at the midpoint of the hour, or for a weather file with minute data, at the minute indicated for the time step.
   * - Subarray 1..4 Angle of incidence modifier
     - 
     - The ratio of irradiance incident on the front of the module cover to the irradiance incident on the photovoltaic cell, equal to **Subarray [n] POA front total irradiance after shading soiling reflection (IAM)** divided by **Subarray [n] POA fron total irradiance after shading soiling**.
   * - Subarray 1..4 Axis rotation for 1 axis trackers
     - deg
     - For 1-axis tracking, tracker's actual axis rotation angle for this subarray. See ideal axis rotation below.
   * - Subarray 1..4 axis rotation ideal for 1 axis trackers (deg)
     - deg
     - For one-axis tracking, the value of this subarray angle of rotation that minimizes its incidence angle with the sun. When backtracking is enabled for one-axis trackers, the actual axis rotation angle is different from the ideal rotation during morning and afternoon hours to avoid shading.  The difference between the ideal axis rotation and the axis rotation is the rotation adjustment due to backtracking.
   * - Subarray 1..4 Cell temperature
     - °C
     - Temperature of module cells in this subarray. For subhourly simulations, see **Subarray [n] Cell temperature (steady state)**.
   * - Subarray 1..4 Cell temperature (steady state)
     - °C
     - Temperature of module cells in this subarray. For hourly simulations, see **Subarray [n] Cell temperature**.
   * - Subarray 1..4 DC power gross
     - kW
     - Power at the array output before DC losses from the Losses page are applied.
   * - Subarray 1..4 External shading and soiling beam irradiance factor
     - frac
     - The shading factor for this subarray that applies to the current time step. The beam shading factor is converted from the percentage beam shading loss specified in the shading loss tables. A value of 0 means no shading. A value of 1 means the beam irradiance incident on the subarray is completely blocked.
   * - Subarray 1..4 Module efficiency
     - %
     - Module conversion efficiency, calculated as the subarray gross DC output divided by the total irradiance incident on the subarray after shading, soiling, and reflection (IAM) losses.
   * - Subarray 1..4 Open circuit voltage
     - V
     - String open circuit voltage for this subarray. Shown for reference.
   * - Subarray 1..4 Operating DC voltage
     - V
     - String operating voltage for this subarray. Used to ensure array voltage is within inverter input voltage limits.
   * - Subarray 1..4 POA front beam irradiance after shading and soiling
     - W/m²
     - Direct normal radiation incident on the front of this subarray after shading and soiling losses from the Shading and Layout page are applied.
   * - Subarray 1..4 POA front diffuse irradiance after shading and soiling
     - W/m²
     - Diffuse radiation incident on the front of  this subarray after shading and soiling losses from the Shading and Layout page are applied.
   * - Subarray 1..4 POA front total irradiance
     - W/m²
     - Total irradiance on the front of this subarray (sum of POA beam and POA diffuse) starting with irradiance from the sun and ending with irradiance available to the photovoltaic cells on the front of the modules in order of decreasing irradiance.


       **nominal**

         POA irradiance before any irradiance losses are applied


       **after shading**

         accounts for shading and snow losses from the Shading and Layout page 


       **after shading soiling**

         accounts for those losses plus soiling losses from the Losses page


       **after shading soiling reflection (IAM)**

         accounts for shading, soiling and reflection losses from the module cover (also called incident angle modifier, or IAM losses)
   * - Subarray 1..4 POA rear total irradiance after reflection (IAM)
     - W/m²
     - Total irradiance on the rear of this subarray available to photovoltaic cells for conversion to electricity.
   * - Subarray 1..4 POA total irradiance after reflection
     - W/m²
     - Total irradiance available to photovoltaic cells in this subarray. This is the solar energy available for conversion to electricity. For bifacial modules, this the combined front- and rear-side irradiance.
   * - Subarray 1..4 Partial external shading DC factor
     - 
     - DC loss factor applied by the partial shading model when you enable time series shading losses on the Shading and Layout page and enable the partial shading model.
   * - Subarray 1..4 Self-shading linear beam irradiance factor
     - 
     - Loss factor applied to beam irradiance for the linear self-shading model.
   * - Subarray 1..4 Self-shading non-linear DC factor
     - 
     - DC loss factor applied to the subarray’s DC electrical output by the non-linear self-shading model to account for the reduction in incident beam irradiance on the subarray..
   * - Subarray 1..4 Self-shading non-linear ground diffuse irradiance factor
     - 
     - Loss factor applied to the subarray’s plane-of-array diffuse irradiance by the non-linear self-shading model to account for the reduction in incident ground-reflected diffuse irradiance.
   * - Subarray 1..4 Self-shading non-linear sky diffuse irradiance factor
     - 
     - Loss factor applied to the subarray’s plane-of-array diffuse irradiance by the non-linear self-shading model to account for the reduction in incident diffuse irradiance from the sky.
   * - Subarray 1..4 Snow cover DC power loss
     - kW
     - Reduction in the subarray’s DC electrical output caused by snow covering the array, reported with **Estimate snow losses** is checked on the Shading and Layout page.
   * - Subarray 1..4 Soiling beam irradiance factor
     - 
     - Loss factor applied to the incident beam irradiance to account for soiling of modules in the subarray, determined by the soiling losses from the :doc:`pv_electrical_losses` page.. 
   * - Subarray 1..4 String short circuit DC current
     - A
     - The short circuit current for this subarray. This is a theoretical value used to help with string sizing. SAM does not report the operating DC current, but it can be calculated by dividing the subarray DC gross power by the operating DC voltage.
   * - Subarray 1..4 Surface azimuth
     - deg
     - Angle of the array from due north for this subarray.
   * - Subarray 1..4 Surface tilt
     - deg
     - Angle of the array from the horizontal for this subarray.
   * - Subarray 1..4 Beam POA clearsky irradiance
     - W/m²
     - Estimate of the front-side POA beam irradiance under clear sky conditions. For reference only.
   * - Subarray 1..4 Diffuse POA clearsky irradiance
     - W/m²
     - Estimate of the front-side POA diffuse irradiance under clear sky conditions. For reference only.
   * - Subarray 1..4 Ground reflected POA clearsky irradiance
     - W/m²
     - Estimate of the front-side POA ground-reflected irradiance under clear sky conditions. For reference only.
   * - Subarray 1..4 Rear POA clearsky irradiance
     - W/m²
     - Estimate of the rear-side total irradiance under clear sky conditions.  For reference only.
   * - Sun altitude angle
     - deg
     - Altitude angle of the sun either at the midpoint of the hour, or for a weather file with minute data, at the minute indicated for the time step.
   * - Sun azimuth angle
     - deg
     - Azimuth angle of the sun either at the midpoint of the hour, or for a weather file with minute data, at the minute indicated for the time step.
   * - Sun position time
     - hour
     - The hour of day for the sun angles in the current time step.
   * - Sun up over horizon
     - 
     - A flag value indicating the sun's position relative to the horizon: 0=below horizon, 1=above horizon, 2=rising, 3=setting. SAM uses the flag in internal calculations to determine what calculations to make during the simulation.
   * - Sun zenith angle
     - deg
     - Zenith angle of the sun either at the midpoint of the hour, or for a weather file with minute data, at the minute indicated for the time step.
   * - System power before grid curtailment
     - kW
     - AC power at the inverter output before any adjustments based on curtailment inputs on the Grid page. These values do account for the interconnection limit if it is enabled.
   * - System power before grid interconnect
     - kW
     - AC power adjusted before any interconnection limits on the Grid page are applied. These values are only reported if the interconnection limit is enabled, and do not include the effect of curtailment.
   * - System power generated
     - kW
     - AC power at the grid interconnection point. A negative value indicates the system is using grid power for inverter night-time consumption, to charge the battery, or to account for transformer or other AC losses.
   * - Transformer load loss
     - kW
     - Distribution or substation transformer constant AC loss, typically mostly from the magnetizing current in the transformer’s core from the Losses page.
   * - Transformer no load loss
     - kW
     - Distribution or substation transformer variable AC loss, typically mostly from the transformer’s primary and secondary coil from the Losses page.
   * - Transformer total loss
     - kW
     - Total AC loss from the distribution or substation transformer.
   * - Transmission loss
     - kW
     - AC transmission loss from the Losses page.
   * - Weather file ambient temperature
     - °C
     - Ambient temperature from the weather file (dry-bulb temperature).
   * - Weather file snow depth
     - cm
     - Depth of snow from the weather file. Required if **Estimate snow losses** is checked on the Shading and Layout page. 
   * - Weather file wind speed
     - m/s
     - Wind speed from weather file.
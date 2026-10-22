Solar Water Heating Model Hourly Data
=====================================

After running a simulation, SAM displays time series results for the solar water heating model on the Results page :doc:`Tables <../results/data>` and :doc:`Time Series <../results/timeseries>` graphs. It also displays the :doc:`solar fraction <../performance-metrics/mtp_solar_fraction>` on the :doc:`Summary <../results/summary>` tab.

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Variable Name
     - Units
     - Description
   * - System energy
     - kWh
     - The total energy saved by the solar water system over one year, equal to the sum of the time series System power generated values (described below).
   * - Hot water draw
     - kg/hr
     - The hourly usage of hot water specified in the draw profile on the input page.
   * - Irradiance - Beam
     - W/m2
     - Direct normal irradiance value from the weather file.
   * - Irradiance - Diffuse
     - W/m2
     - Diffuse horizontal irradiance value from the weather file.
   * - Irradiance - Incident
     - W/m2
     - The total hourly incident global irradiance incident on the collector.
   * - Irradiance - Transmitted
     - W/m2
     - The total hourly radiation that makes it into the collector. Depends on the optical properties of the collector.
   * - Operation mode
     - 
     - 1 – startup mode, useful energy is collected and tank temperature is somewhat stratified.


       2 – mixed mode, useful energy is collected and tank temperature is fairly uniform.


       3 - stratified mode, no useful energy is collected and tank temperature is very stratified.
   * - P pump
     - W
     - Electric pump power required to drive the collector loop and heat exchanger loop.
   * - System power generated
     - kW
     - Power saved by the system in a single time step, equal to:


       *Gen = Q auxiliary only - Q auxiliary - P pump*


       For any time step  that Gen > Load, SAM sets Gen = Load because the solar water heating system cannot export power.
   * - Q auxiliary
     - W
     - Electric power required by the auxiliary heater to raise the water temperature from the solar storage tank to the set temperature: |EQ_SWH_Qaux|, where |EQ_SWH_Tdelivvar| is the temperature of the water delivered from the solar tank. Because solar heat has been added to the water, |EQ_SWH_TdelivgtTmain|, and less power is needed to bring the water to the desired set temperature than would be required without the solar water heating system.
   * - Q auxiliary only
     - W
     - Electric power that would be required without the solar water heating system: |EQ_SWH_Qauxonly|.
   * - Q delivered
     - W
     - Thermal power delivered by the solar water heating system at the solar storage tank outlet: |EQ_Q-delivered|
   * - Q loss
     - W
     - Envelope loss to room: |EQ_SWH_Qloss2|.
   * - Q transmitted
     - W
     - Solar irradiance transmitted through the collector glass, accounting for collector area: |EQ_SWH_Qtransmitted|, where Itransmitted is the transmitted irradiance and Ac is the total collector area.
   * - Q useful
     - W
     - Power delivered to the water tank, equal to the power received by the collector minus losses from the collector to the surroundings: |EQ_Q-useful|
   * - Shading losses
     - %
     - Percent loss of incident beam irradiance due to shading, determined by the shading factors that you specify on the Solar Water Heating page.
   * - T ambient
     - °C
     - The mid-hour ambient temperature calculated by averaging the end-of-hour temperature from the previous hour with the end-of-hour temperature from the current hour in the weather file.
   * - T cold
     - °C
     - The temperature of the cold portion of the solar storage tank volume in stratified mode. If the tank is not stratified, this value is equal to the previous hour's cold temperature.
   * - T delivered
     - °C
     - The temperature of the water delivered from the storage tank.
   * - T hot
     - °C
     - The temperature of the hot portion of the solar storage tank volume in stratified mode. If not stratified, this value is equal to the previous hour's hot temperature.
   * - T mains
     - °C
     - The temperature of water incoming from the supply source.
   * - T tank
     - °C
     - The mean temperature of the solar storage tank.
   * - V cold
     - m3
     - The estimated volume of the cold portion of the solar storage tank, where "cold" is with respect to the hot portion of the tank. SAM models the hot and cold portions as separate nodes. The cold volume increases as users draw water from the tank and mains water replaces it.
   * - V hot
     - m3
     - The estimated volume of the hot portion of the solar storage tank, where "hot" is with respect to the cold portion of the tank. SAM models the hot and cold portions as separate nodes. The hot volume increases from hour to hour as the useful energy from the collector is added until the hot volume is equal to the tank volume (and cold volume is zero).

.. |EQ_SWH_Qaux| image:: ../images/EQ_SWH_Qaux.png
.. |EQ_Q-useful| image:: ../images/EQ_Q-useful.png
.. |EQ_SWH_Tdelivvar| image:: ../images/EQ_SWH_Tdelivvar.png
.. |EQ_SWH_Qloss2| image:: ../images/EQ_SWH_Qloss2.png
.. |EQ_SWH_TdelivgtTmain| image:: ../images/EQ_SWH_TdelivgtTmain.png
.. |EQ_Q-delivered| image:: ../images/EQ_Q-delivered.png
.. |EQ_SWH_Qauxonly| image:: ../images/EQ_SWH_Qauxonly.png
.. |EQ_SWH_Qtransmitted| image:: ../images/EQ_SWH_Qtransmitted.png

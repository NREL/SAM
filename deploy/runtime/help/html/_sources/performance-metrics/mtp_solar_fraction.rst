Solar Fraction
==============

For :doc:`solar water heating <../solar-water-heating/swh_overview>` systems, the solar fraction is the ratio of solar energy to total energy delivered to the water storage tank. The solar fraction is based on the energy values for year one of the project cash flow.

**Solar Fraction**
*Solar Fraction = Q auxiliary only (kWh/year) ÷ System Energy (kWh/year)*

**System energy** is the sum of the time series **System power generated** output, and represents total electricity saved by the system, and **Q auxiliary only** is the energy that would be delivered to the tank without the solar collector. See the Solar Water Heating :doc:`Results <../solar-water-heating/swh_results>` for details.

.. note:: SAM assumes that the solar water heating system does not export energy. For any time step that "system power generated" is greater than the load defined on the Electric Load page, it sets system power generated equal to the load.
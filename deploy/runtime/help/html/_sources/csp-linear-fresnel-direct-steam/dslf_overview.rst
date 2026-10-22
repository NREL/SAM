CSP Linear Fresnel Direct Steam
===============================

The Direct Steam Linear Fresnel model predicts the performance of a direct-steam generation (DSG) plant that produces superheated steam at temperature and pressure settings that you specify. The model is designed to allow several technology configurations and characterization options that allow flexible and accurate performance analysis.

The solar field can be configured with an adjustable number of modules for the boiler and superheater sections, and the modules can use different geometry and optical performance input data depending on whether they are part of the boiler or superheater sections. The model allows you to specify whether the solar field uses a recirculated boiler or a once-through design. SAM models the steam mass flow rate, temperature, pressure, enthalpy, and quality throughout the field and uses this information to predict thermal losses, pressure drops, and transient effects for each hour of the year.

Several options are available for modeling the performance of the solar field. Collector optical performance can be specified using incidence angle modifier equations in the transversal and longitudinal directions, or an optical efficiency table can provide the optical efficiency as a function of either solar position or collector incidence angles. The Linear Fresnel model allows you to specify thermal loss relationships using either a set of polynomial equations or with a detailed evacuated tube receiver model.

The Linear Fresnel model represents  all major subsystems associated with direct steam systems, including the solar field, optional auxiliary fossil backup system, the steam Rankine power cycle, heat rejection system, feedwater pumps, and plant control system. Output from the model includes financial metrics as well as detailed performance data covering temperature, pressure, mass flow rates, thermal energy flow rats, water use, parasitic power consumption, turbine power output, and many other values.

The linear Fresnel model can also be used for compact linear Fresnel reflector (CLFR) systems by using the appropriate coefficients with the polynomial heat loss model for the receiver geometry and heat loss parameters on the Collector and Receiver page.

The direct steam linear Fresnel model is described in the following publications:

* Wagner, M.; Zhu, G. (2012). A Direct-steam Linear Fresnel Performance Model for NREL's System Advisor Model. NREL Conference Paper CP-5500-55044. (`PDF 647 KB <https://docs.nlr.gov/docs/fy12osti/55044.pdf>`__)

* Wagner, M. (2012). Results and Comparison from the SAM Linear Fresnel Technology Performance Model: Preprint. NREL Conference Paper CP-5500-54758. (`PDF 726 KB <https://docs.nlr.gov/docs/fy12osti/54758.pdf>`__)
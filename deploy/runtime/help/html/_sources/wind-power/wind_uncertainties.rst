Uncertainties
=============

The Uncertainties inputs allow you to specify uncertainty as a percentage of either mean wind speed or mean energy production for a range of different factors that contribute to uncertainty in the the wind resource or power output of a wind farm.

When you enable uncertainties, SAM calculates probability distributions of annual electricity production and displays them on the :doc:`Uncertainties <../results/uncertainties>` tab of the :doc:`Results <../results/results>` page. It also displays a :doc:`p90 energy <../performance-metrics/mtp_p90_energy>` value in the Metrics table on the :doc:`Summary <../results/summary>` tab.

The uncertainty model draws from the `IEC 61400-15 <https://www.iec.ch/dyn/www/f?p=103:38:0::::FSP_ORG_ID,FSP_APEX_PAGE,FSP_PROJECT_ID:1282,23,21031#>`__ uncertainty framework as described in Damiani, R. (2018). Uncertainty and Risk Assessment in the Design Process for Wind. National Renewable Energy Laboratory, NREL/TP-5000-67499 (`PDF 8.9 MB <https://www.nlr.gov/docs/fy18osti/67499.pdf>`__).

.. note:: The uncertainty model requires a good understanding of the IEC framework described in Damiani (2018).

To use the uncertainty model:

#. Choose **Enable Uncertainty Analysis**.

.. image:: ../images/SS_WindUncertainty-enable.png
   :align: center
   :alt: SS_WindUncertainty-enable.png

#. Review the uncertainty inputs for wind resource and energy-related uncertainties and modify as appropriate for your analysis. Note the calculated total project uncertainties at the bottom of the page.

#. Review and modify as appropriate the inputs on the :doc:`Losses <wind_losses>` page.

#. Run a simulation.

#. On the Results page, review the p90 value in the Metrics table on the Summary tab, and the distribution tables and graphs on the Uncertainties tab.
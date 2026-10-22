Performance Ratio
=================

The performance ratio  is a measure of a photovoltaic system's annual electric generation output in AC kWh compared to its nameplate rated capacity in DC kW, taking into account the solar resource at the system's location, and shading and soiling of the array.

.. note:: SAM only calculates the system performance factor for the detailed photovoltaic model. It does not calculate the value for the PVWatts or high concentration photovoltaic (HCPV) models, or for the other technologies.

**Performance ratio**
*Performance Ratio = Annual Energy (kWh) ÷ ( Annual POA Total Radiation (Nominal) (kWh) × Module Efficiency (%) )*

  Where A*nnual energy*   is the system's total net AC output in Year One reported in the :doc:`Metrics <mtp_annual_energy>`   table, A*nnual POA Total Radiation (Nominal)*   is the total solar radiation incident on the array before shading and soiling losses are applied over the year, and *Module Efficiency*   is the nominal efficiency of the modules in the array at standard test conditions STC.

To see the values used in this calculation, after running a simulation, on Results page, click :doc:`Data <../results/data>` and find the variables under **Single Values**. The module efficiency is from the :doc:`Module <../detailed-photovoltaic-model/pv_module>` page.

The method used to calculate the photovoltaic system performance factor is based on the method described in the SMA technical bulletin `Performance Ratio: Quality Factor for the PV Plant <https://files.sma.de/downloads/Perfratio-TI-en-11.pdf>`__, Perfratio-Tl-en-1.1, Version 1.1.
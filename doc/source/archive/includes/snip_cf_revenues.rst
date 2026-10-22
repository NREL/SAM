
**PPA price (cents/kWh)**
  The PPA price in Year One is either the value you specify on the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page, or the value calculated by SAM to meet the IRR target you specify on the Financial Parameters page. The PPA price in Year One is the PPA price:

*PPA Price in Year One = Adjusted PPA Price*

  For analyses that do not involve time-dependent pricing, *Adjusted PPA Price*   is equal to the :doc:`PPA Price <../financial-metrics/mtf_ppa_price>`   reported on the :doc:`Metrics table <../results/summary>`  . 

  For analyses that do involve time-dependent pricing, for each hour of the year, SAM multiplies the :doc:`PPA price <../financial-metrics/mtf_ppa_price>`   shown in the :doc:`Metrics table <../results/summary>`   (equivalent to the bid price) by the TOD factor for that hour to calculate a set of hourly energy prices for Year one. The *Adjusted PPA Price*   is the average of the 8,760 hourly values.


 
When you choose **Specify PPA Price** for **Solution Mode** on the Financial Parameters page, the :doc:`PPA Price <../financial-metrics/mtf_ppa_price>` in the :doc:`Metrics table <../results/summary>` is equal to the price you specify. When you choose **Specify IRR Target**, SAM calculates the PPA price required to meet the IRR target that you specify.
 
.. note:: The TOD factors are on the Thermal Energy Storage page for CSP systems (parabolic trough, power tower, linear Fresnel, generic solar system), and on the :ref:`Revenue <revenue-ppa-tod>` page for photovoltaic and other systems.


.. note:: To remove time-dependent pricing from your analysis, set the TOD factor for each of the 9 periods to one.  Where Adjusted :doc:`PPA Price <../financial-metrics/mtf_ppa_price>`   is either the value you specify on the Financial Parameters page, or a value calculated to cover project costs based on the target IRR you specify on the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page, adjusted by the time-of-delivery (TOD) factors.

  In Years Two and later:

*PPA Price in Year n>1 = PPA Price in Year n-1 × (1 + PPA Escalation Rate)^(n-1)*

  Where *PPA Escalation Rate*   is from the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page.

.. note:: SAM uses the PPA Price to calculate the :doc:`levelized cost of energy <../financial-metrics/mtf_lcoe>`.

Revenue for PPA Single Owner Financial Model
--------------------------------------------

The Single Owner financial model reports three revenue values in the project cash flow.

**PPA revenue gross**
  The gross project revenue earned from electricity sales. For a project with a PPA price that does not vary with time of delivery, this is the annual energy to grid in kWh multiplied by the PPA price in $/kWh.

**PPA revenue lost to self-consumption**
  Revenue lost to grid power purchases to meet power system self-consumption loads such as inverter nighttime power consumption for photovoltaic systems, and freeze protection power and other parasitic loads for concentrating power systems.

  For systems for battery storage, when the option on the :doc:`Electricity Rates <../electricity-rates-and-load/electricity_rates>`   page is **No Electricity Rate**, revenue lost to self-consumption also includes power for battery charging.

**PPA revenue net**
  The net revenue earned from electricity sales, accounting for revenue lost do self-consumption.

Revenue for PPA Partnership and Sale Leaseback Models
-----------------------------------------------------

**PPA revenue to project**
  The project's annual revenue from electricity sales, not accounting for salvage value:

*Total PPA Revenue = Net Energy (kWh) × PPA Price (cents/kWh) × 0.01 ($/cents)*

**Salvage value**
.. include:: /includes/snip_cf_salvage_value.rst

**Total revenue to project**
  The projects annual revenue, accounting for salvage value.

*Total Revenue = Total PPA Revenue + Salvage Value*

  Because Salvage Value is zero for all years except the last year of the analysis period, the Total Revenue and Total PPA Revenue are the same for all years except for the last year.

  For the Single Owner and Leveraged Partnership Flip financial models (both of which include debt), total revenue may also include production-based :doc:`incentive <../incentives-and-depreciation/cash_incentives>`   (PBI) amounts. For each PBI amount that you check on the :doc:`fin_overview <../financial-parameters/fin_overview>`   page under **Debt Service**, **Production Based Incentives (PBI) Available for Debt Service**, SAM displays a row for the PBI above the Total Revenue row, and includes the amount in the Total Revenue amount:

*Total Revenue = Total PPA Revenue + Salvage Value + PBI*
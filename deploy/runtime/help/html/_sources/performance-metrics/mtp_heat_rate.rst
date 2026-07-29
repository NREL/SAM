Heat Rate and Thermal Efficiency
================================

For biomass power systems, SAM reports the heat rate and thermal efficiency values from the system's :doc:`annual energy <mtp_annual_energy>` value calculated by the performance model, and the biomass feed rate and overall feedstock heating values from the :doc:`Feedstock <../biopower/biopower_feedstock>` page.

**Gross Heat Rate, MMBtu/MWh**
*Gross Heat Rate (MMBtu/MWh) =**Dry Biomass Feed Rate (lb/year)**× Average Overall Feedstock HHV (MMBtu/lb) ÷ Net Annual Energy (MWh/year)*

**Net Heat Rate, MMBtu/MWh**
*Net Heat Rate (MMBtu/MWh) =**Dry Biomass Feed Rate (lb/year)**× Average Overall Feedstock LLV (MMBtu/lb) ÷ Net Annual Energy (MWh/year)*

The dry biomass feed rate is the annual feed rate from the :doc:`Feedstock <../biopower/biopower_feedstock>` page (and also shown as Annual Biomass Usage in the Metrics table) converted from ton/year to lb/year (2000 lb = 1 ton), the average overall feedstock heating values are from the Feedstock page, converted from Btu/lb to MMBtu/lb (1,000,000 Btu = 1 MMBtu), Net Annual Energy is from the Metrics table.

**Thermal Efficiency HHV, %**
*Thermal Efficiency HHV (%) = 341.23 (MMBtu/MWh) ÷ Gross Heat Rate (MMBtu/MWh)*

**Thermal Efficiency LHV, %**
*Thermal Efficiency LHV (%) = 341.23 (MMBtu/MWh) ÷ Net Heat Rate (MMBtu/MWh)*

Where 341.23 MMBtu/MWh is the conversion factor between MMBtu and MWh units, and the heat rates are the values described above.
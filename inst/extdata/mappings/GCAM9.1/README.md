# GCAM 9.1 mapping overrides

This directory contains only the mappings that differ from the current
`GCAM8.2` reporting profile. `saveDataFiles_GCAM9.1.R` first creates explicit
GCAM 9.1 copies of the vetted GCAM 8.2 package data, then replaces these files:

- electricity capacity and generation mappings;
- CO2 and non-CO2 technology/sector mappings;
- final-energy and transport-final-energy mappings;
- the complete 572-row electricity capital-cost table, including the 2021
  model period, `SMR`, and `large reactor`.

The remaining GCAM 9.1 additions that were discovered during the Zaratan
workflow (core energy-price markets, water sector/subsector keys, and the
international-aviation policy input) are constructed and documented directly
in `saveDataFiles_GCAM9.1.R`.

These source mappings came from the UMD-CGS `dev_commdef_9p1` environment used
for the GCAM 9.1 cloud runs. The GCAM 8.2 package objects are not overwritten;
the build creates separate `*_v9.1` objects.

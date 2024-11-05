# BLN
Bodemindicatoren voor Landbouwgronden in Nederland. A soil quality assessment framework for application in the Netherlands.

## Version 0.1.0

### Added
- 1 BLN wrapper function `bln_field` to estimate all indicators and ecosystem services (BLN scores) for agricultural fields
- 18 functions starting with `bln_prod` to evaluate soil quality for its contribution to crop production. This includes soil chemical, physical and biological functions.
- 6 functions starting with `bln_gw` to evaluate soil quality for its contribution to groundwater recharge and groundwater quality
- 4 functions starting with `bln_sw` to evaluate soil quality for its contribution to surface water quality
- 3 functions starting with `bln_clim` to evaluate soil quality for its contribution to carbon sequestration
- 7 helper function in `bln helpers` for water retention, input formatting and weighing function
- 4 evaluating functions in `bln_evaluate` to estimate a BLN indicator from a BLN function
- 2 helper functions `bln_add_management` and `bln_calc_psp` as replacement of OBIC functions in view of speed
- 7 package tables added: `bln_parms`, `bln_soiltype`, `bln_thresholds`, `bln_crops`, `bln_lsw`, `bln_farm_hf`, `bln_lsw_farm_hf`

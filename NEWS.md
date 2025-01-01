# BLN 0.5.0 2025-01-01

## Added
- vignette `bln_intro` given a short intro to the package and the main function to assess soil quality
- vignette `bln_column_description` describing all inputs and outputs of the BLN package
- vignette `bln_ess_crop_production` describing the assessment of soil health in view of crop production
- internal package tables for `bln_input_description` and `bln_output_description`
- wrapper function `bln_field_dt` to apply the `bln_field` function on a data.table
- checkmate on argument `output` in `bln_field`
- add ignore to vignettes to avoid synchronize html pages with git

## Fixed
- ensure that `B_LSW_ID` in the wrapper function `bln_field` is always converted to character
- add `B_AER_CBS` as input in internal table in `bln_c_sulfur`

## Updated
- `bln_lsw_farm_hf` table 

# BLN 0.4.0 2024-12-24

## Added
* function `bln_format_gtclass` for creating classes of ground water table data, based on GHG and GLG
* test function `test-bln_format_gtclass` of the function described above

# BLN 0.3.0 2024-12-23

## Added
* optimiser for crop rotation schemes in view of targeted improvements in soil ecosystem services `bln_field_optimiser`
* internal package table for crop rotation schemes `bln_scen_croprotation`

## Updated
* `ppr_tables` in dev to prepare internal package table `bln_scen_croprotation`

# BLN 0.2.0 2024-12-23 

## Added
* SOMERS calculation of emissions in peat soils, function `bln_clim_somers` and associated test function
* internal package table for SOMERS meta-model `bln_somers`

## Updated
* `ppr_tables` in dev to prepare internal package table `bln_somers`

# BLN 0.1.0 2024-11-05
First version of the R package to assess soil quality using BLN framework

## Added
* 1 BLN wrapper function `bln_field` to estimate all indicators and ecosystem services (BLN scores) for agricultural fields
* 18 functions starting with `bln_prod` to evaluate soil quality for its contribution to crop production. This includes soil chemical, physical and biological functions.
* 6 functions starting with `bln_gw` to evaluate soil quality for its contribution to groundwater recharge and groundwater quality
* 4 functions starting with `bln_sw` to evaluate soil quality for its contribution to surface water quality
* 3 functions starting with `bln_clim` to evaluate soil quality for its contribution to carbon sequestration
* 7 helper function in `bln helpers` for water retention, input formatting and weighing function
* 4 evaluating functions in `bln_evaluate` to estimate a BLN indicator from a BLN function
* 2 helper functions `bln_add_management` and `bln_calc_psp` as replacement of OBIC functions in view of speed
* 7 package tables added: `bln_parms`, `bln_lsw`,  `bln_lsw_farm_hf`
* internal package table for crop properties (`bln_crops`) 
* internal package table describint the input variables (`bln_parms`)
* internal package tables for common soil types (`bln_soiltype`) and threshold values to evaluate soil functions (`bln_thresholds`)
* example dataset `bln_farm_hf` for a single farm in te Netherlands

# BLN 0.0.1 2023-08-18
First package setup

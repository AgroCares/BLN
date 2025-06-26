# BLN 0.x.x 2025-xx-xx
## Added
* examples and return descriptions to `bln_clim_*` and RothC functions

## Changed
* increased minimum value for A_SOM_LOI in functions from 0.1 to 0.5
* decreased maximum value for A_SOM_LOI in functions from 100 to 75

## Fixed
* potential bug when running RothC in parrallel if there is only one core available fixes https://github.com/AgroCares/BLN/issues/20

# BLN 0.9.3 2025-04-10
## Deprecated
* function argument A_SOM_LOI in `bln_bbwp_ngw` is deprecated as it was not used by the function

# BLN 0.9.2 2025-04-09

## Added
* improved explanation of which inputs are required for `bln_c_posphor`

# BLN 0.9.1 2025-04-09

## Added
* vignette `bln_ess_groundwater` on the estimation of the ESD for water quantity and water purification
* examples to functions `bln_wat_nretention_gw`,`bln_wat_nrisk_gw`,`bln_wat_groundwater_recharge`, `bln_bbwp_bw` and `bln_bbwp_ngw`

## Changed
* documentation of tables `bln_output_description` and `bln_input_description`

# BLN 0.9.0 2025-03-15

## Changed
* remove dependency on private package `carboncastr`, #BLN-10
* update `bln_rothc_field` and `rothc_initialise`
* package table `bln_crops` expanded with `B_LU_MAKKINK` and `B_LU`

## Added
* function `bln_rothc_sim` to simulate evolution of SOC as function of crop rotation and amendment
* function `bln_rothc` with differential equations for RothC modelling
* function `bln_rothc_input_rmf`, `bln_rothc_input_crop` and `bln_rothc_input_amendment` to facilitate and process RothC inputs
* function `bln_rothc_event`, `bln_rothc_event_crop` and `bln_rothc_event_amendment` to facilitate and process RothC events
* test functions for rothc events, inputs and simulation
* package table `bln_makkink`


# BLN 0.8.1 2025-03-12

## Fixed
* error in crop rotation assignment in `rothc_scenario`, #BLN-11
* add `carboncastr` dependency to suggests

# BLN 0.8.0 2025-02-22

## Added
* github page added via pgkdown to build package website plus articles
* github actions checking pull requests

# BLN 0.7.0 2025-02-22

## Changed
* function `bln_rothc_field` has now argument `scen` to allow users to use rothc for multiple scenarios: BAU, BAUIMPR, CLT and ALL
* function `bln_clim_rothc` runs with default scenarios BAU and ALL
* function `rothc_parallel` and `bln_rothc_multicore` allow users to use rothc for multiple scenarios

## Added
* plot function `plot_bln_rothc_ts` for SOC evolution from RothC
* plot function `plot_bln_map` to plot BLN scores for a set of fields
* plot function `plot_bln_boxplot` to plot classic boxplots for BLN scores, aggregated per ESD type
* test script `test-bln_plots` for testing plot functions

## Fixed
* adapt inconcistent number of simyears in multicore and default RothC calculation in `bln_clim_rothc` 

# BLN 0.6.0 2025-02-18

## Changed
* the argument `outputtype` in `bln_field_optimiser` has new arguments: scores, indicators, bottlenecks, rotation or all, #BLN-6
* `bln_field_optimiser` can give the score per ESD and aggregated BLN function for each of the requested crop rotations (option scores)
* `bln_field_optimiser` can give the score per indicators for each of the requested crop rotations (option indicators)
* `bln_field_optimiser` can give the bottleneck per ESD or aggregated BLN function (option bottlenecks)
* `bln_field_optimiser` can give the best crop rotation per ESD or aggregated BLN function (option rotation)

## Updated
* test for `bln_field_optimiser` given the update in function argumentation

# BLN 0.5.1 2025-01-14

## Changed
* move private package `soilcastor` from imports to suggests
* add warnings / stops for internal functions `bln_rothc_field`, `rothc_initialise`, `rothc_parallel` and `bln_rothc_multicore` using `soilcastor` to avoid installation errors when the `soilcastor` package is not available.
* remove use of internal package tables from `soilcastor` in `rothc_scenario` function
* add stops for internal function `bln_rothc_multicore` for packages `future`, `future.apply` and `parallelly` when packages are not installed 
* set `i_clim_rothc` to NA when carboncastr is not available

## Updated
* add tests for rothc helper functions `rothc_scenario` and `rothc_initialise`
* updated the `bln_crop` package table in `dev` following pandex

# BLN 0.5.0 2025-01-01

## Added
* vignette `bln_intro` given a short intro to the package and the main function to assess soil quality
* vignette `bln_column_description` describing all inputs and outputs of the BLN package
* vignette `bln_ess_crop_production` describing the assessment of soil health in view of crop production
* internal package tables for `bln_input_description` and `bln_output_description`
* wrapper function `bln_field_dt` to apply the `bln_field` function on a data.table
* checkmate on argument `output` in `bln_field`
* add ignore to vignettes to avoid synchronize html pages with git

## Fixed
* ensure that `B_LSW_ID` in the wrapper function `bln_field` is always converted to character
* add `B_AER_CBS` as input in internal table in `bln_c_sulfur`

## Updated
* `bln_lsw_farm_hf` table 

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

#' Calculate and evaluate the index for the microbial biological activity in the Netherlands
#'
#' This function assesses the microbial biological activity (of microbes and fungi) via the Potentially Mineralizable N pool, also called PMN (or SoilLife by Eurofins in the past).
#'
#' @param B_LU_BRP (numeric) The crop code
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_N_PMN (numeric) The potentially mineralizable N pool (mg N / kg soil)
#'
#' @import data.table
#' @import OBIC
#'
#' @examples
#' bln_b_pmn(B_LU_BRP = 256, B_SOILTYPE_AGR = 'dekzand', A_N_PMN = 125)
#' bln_b_pmn(c(256,1027), c('dekzand','rivierklei'), c(125,45))
#'
#' @return
#' the normalized potentially mineralizable Nitrogen pool (mg N / kg), a numeric value, converted to an OSI score.
#'
#' @export
bln_b_pmn <- function(B_LU_BRP, B_SOILTYPE_AGR,A_N_PMN) {

  # add visual bindings
  osi_country = osi_indicator = NULL

  # load and subset thresholds to Dutch situation for PMN
  dt.thresholds <- as.data.table(BLN::bln_thresholds)
  dt.thresholds <- dt.thresholds[bln_country=='NL' & bln_indicator=='i_b_pmn']

  # check length and of arguments
  arg.length <- max(length(A_N_PMN), length(B_LU_BRP), length(B_SOILTYPE_AGR))
  checkmate::assert_numeric(A_N_PMN, lower = 0, upper = 1000, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(BLN::bln_crops$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(BLN::bln_soiltype$bln_soil_cat1), empty.ok = FALSE)
  checkmate::assert_data_table(dt.thresholds,max.rows = 1)

  # calculate the PMN value for the Netherlands using the Dutch OBIC
  value <- OBIC::calc_pmn(B_LU_BRP = B_LU_BRP,
                          B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                          A_N_PMN = A_N_PMN)

  # convert to OSI score
  value <- bln_evaluate_logistic(x = value,
                                 b = dt.thresholds$osi_st_c1,
                                 x0 = dt.thresholds$osi_st_c2,
                                 v = dt.thresholds$osi_st_c3)
  # return value
  return(value)
}


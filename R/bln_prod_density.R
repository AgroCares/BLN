#' Calculate the bulk density
#'
#' This pedotransfer function calculates the bulk density of the soil based on texture and organic matter
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_DENSITY_SA (numeric) The bulk density of the soil (kg/m3)
#'
#' @import data.table
#'
#' @examples
#' bln_p_density(A_SOM_LOI = 6.5, A_CLAY_MI = 28)
#' bln_p_density(A_SOM_LOI = 3.5, A_CLAY_MI = 2)
#' bln_p_density(A_SOM_LOI = c(3.5,8.5),A_CLAY_MI = c(2,28))
#'
#' @return
#' The bulk density of an arable soil (kg / m3) evaluated given the maximum density that limit the root penetration.
#'
#' @export
bln_p_density <- function(A_SOM_LOI, A_CLAY_MI,A_DENSITY_SA = NA_real_) {

  # set visual bindings
  dens.sand = dens.clay = cf = density = crit1 = NULL
  osi_indicator = osi_country = NULL

  # Load in the thresholds
  dt.thresholds <- as.data.table(BLN::bln_thresholds)
  dt.thresholds <- dt.thresholds[bln_country == 'EU' & bln_indicator == 'i_p_dens']

  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_data_table(dt.thresholds,max.rows = 1)

  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_DENSITY_SA = A_DENSITY_SA,
                   value = NA_real_
                   )

  # calculate soil texture dependent density
  dt[, dens.sand := (1 / (0.02525 * A_SOM_LOI + 0.6541)) * 1000]
  dt[, dens.clay :=  (0.00000067*A_SOM_LOI^4 - 0.00007792*A_SOM_LOI^3 + 0.00314712*A_SOM_LOI^2 - 0.06039523*A_SOM_LOI + 1.33932206) * 1000]

  # fraction clay correction
  dt[, cf := pmin(1, A_CLAY_MI/25)]

  # clay dependent density
  dt[, density := fifelse(is.na(A_DENSITY_SA),cf * dens.clay + (1-cf) * dens.sand,A_DENSITY_SA)]

  # calculate critical soil density limiting root penetration (Van den Akker et al., 2020)
  dt[,crit1 := pmin(1.6,1.75 - 0.009 * A_CLAY_MI)*1000]

  # calculate the open soil index socre
  dt[,value := bln_evaluate_logistic(x = density,
                                     b = dt.thresholds$bln_st_c1,
                                     x0 = crit1,
                                     v = dt.thresholds$bln_st_c3,
                                     increasing = FALSE)]

  # return value
  value <- dt[, value]

  return(value)

}

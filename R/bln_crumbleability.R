#' Calculate and evaluate the crumbleability in view of the soils production function
#'
#' This function calculates the crumbleability.
#'
#' @param A_SOM_LOI (numeric) The organic matter content of soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_PH_CC (numeric) The pH of the soil, measured in 0.01M CaCl2
#'
#' @import data.table
#' @import OBIC
#'
#' @return
#' The crumbleability index of a soil, a measure for a physical soil property. A numeric value.
#'
#' @export
bln_p_crumbleability <- function(A_SOM_LOI, A_CLAY_MI, A_PH_CC) {

  # Check input
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(A_PH_CC, lower = 0, upper = 14, any.missing = FALSE, min.len = 1)

  # Setup a table with all the information
  dt <- data.table(A_CLAY_MI = A_CLAY_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   A_PH_CC = A_PH_CC
                  )

  # calculate the crumbleability of the soil
  dt[, D_CR := OBIC::calc_crumbleability(A_SOM_LOI, A_CLAY_MI,A_PH_CC)]

  # evaluates the crumbleability
  dt[, i_p_cr := OBIC::ind_crumbleability(D_CR, B_LU_BRP)]

  # extract the BLN indicator
  value <- dt[, i_p_cr]

  # return the value
  return(value)
}

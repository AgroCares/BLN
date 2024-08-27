#' Calculate and evaluate the crumbleability in view of the soils production function
#'
#' This function calculates the crumbleability.
#'
#' @param B_LU_BRP (numeric) The crop code from the BRP
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
bln_p_crumbleability <- function(B_LU_BRP,A_SOM_LOI, A_CLAY_MI, A_PH_CC) {

  # add visual bindings
  D_CR = i_p_cr = NULL

  # Check input
  arg.length <- max(length(B_LU_BRP),length(A_SOM_LOI),length(A_CLAY_MI),length(A_PH_CC))
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(BLN::bln_crops$crop_code), empty.ok = FALSE)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 0, upper = 14, any.missing = FALSE, min.len = 1, len = arg.length)

  # Setup a table with all the information
  dt <- data.table(B_LU_BRP = B_LU_BRP,
                   A_CLAY_MI = A_CLAY_MI,
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

#' Calculate and evaluate the soil sealing risk
#'
#' This function calculates the risks of soil sealing.
#'
#' @param B_LU_BRP (numeric) The crop code
#' @param A_SOM_LOI (numeric) The organic matter content of soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#'
#' @import data.table
#' @import OBIC
#'
#' @return
#' The risk of soil sealing as affected by the soil organic matter and clay content. A numeric value.
#'
#' @export
bln_p_sealing <- function(B_LU_BRP,A_SOM_LOI, A_CLAY_MI) {

  # Check input
  arg.length <- max(length(B_LU_BRP),length(A_CLAY_MI), length(A_SOM_LOI))
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(BLN::bln_crops$crop_code), empty.ok = FALSE)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, min.len = 1, len = arg.length)

  # Setup a table with all the information
  dt <- data.table(B_LU_BRP = B_LU_BRP,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SOM_LOI = A_SOM_LOI
                  )

  # Calculate the risk of soil surface sealing
  dt[, D_SE := OBIC::calc_sealing_risk(A_SOM_LOI, A_CLAY_MI)]

  # Evaluates the risk of soil sealing
  dt[, i_p_se := OBIC::ind_sealing(D_SE, B_LU_BRP)]

  # extract the BLN indicator
  value <- dt[, i_p_se]

  # return the value
  return(value)
}

#' Function to calculate and evaluate a default SOM balance
#'
#' @param ID (character) A field id
#' @param B_LU_BRP (numeric) value of the BRP crop code
#' @param A_SOM_LOI (numeric) value for the soil organic matter content of the soil
#' @param A_P_AL (numeric) The P-AL content of the soil
#' @param A_P_WA (numeric) The P-content of the soil extracted with water (mg P2O5 / 100 ml soil)
#' @param M_COMPOST (numeric) The frequency that compost is applied (optional, every x years)
#' @param M_GREEN (boolean) A soil measure. Are catch crops sown after main crop (optional)
#
#' @import data.table
#' @import carboncastr
#'
#' @export
bln_clim_cbalance <- function(ID,B_LU_BRP,A_SOM_LOI,A_P_AL,A_P_WA,M_COMPOST = 0,M_GREEN = FALSE){

  # add visual bindings
  D_SOM_BAL = NULL

  # Check inputs
  arg.length <- max(length(B_LU_BRP), length(A_SOM_LOI), length(A_P_AL), length(A_P_WA))
  checkmate::assert_integerish(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(BLN::bln_crops$crop_code), empty.ok = FALSE)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0.1, upper = 100, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_numeric(A_P_AL, lower = 1, upper = 250, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_P_WA, lower = 1, upper = 250, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(M_COMPOST, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(M_GREEN,any.missing = FALSE, len = arg.length)

  # make internal table
  dt <- data.table(ID = ID,
                   B_LU_BRP = B_LU_BRP,
                   A_SOM_LOI = as.numeric(A_SOM_LOI),
                   A_P_AL= as.numeric(A_P_AL),
                   A_P_WA = as.numeric(A_P_WA),
                   M_COMPOST = M_COMPOST,
                   M_GREEN = M_GREEN,
                   value = NA_real_
                   )

  # set compost to zero if NA
  dt[is.na(M_COMPOST), M_COMPOST := 0]
  dt[is.na(M_GREEN), M_GREEN := 0]

  # SOM balance OBIC requires single value for soil properties
  dt[, A_SOM_LOI := mean(A_SOM_LOI),by=ID]
  dt[, A_P_AL := mean(A_P_AL),by=ID]
  dt[, A_P_WA := mean(A_P_WA),by=ID]
  dt[, M_COMPOST := mean(M_COMPOST),by=ID]
  dt[, M_GREEN := M_GREEN[1],by=ID]

  # calculate the SOM balance using OBIC
  dt[, D_SOM_BAL := OBIC::calc_sombalance(B_LU_BRP,A_SOM_LOI,
                                          A_P_AL, A_P_WA,
                                          M_COMPOST,M_GREEN),by=ID]

  # calculate the distance to target
  dt[,value := OBIC::evaluate_logistic(x = D_SOM_BAL,  b = 0.0008144, x0 = 0, v = 0.9077174, increasing = TRUE)]

  # return value i_clim_osb
  value <- dt[, value]

  return(value)

}




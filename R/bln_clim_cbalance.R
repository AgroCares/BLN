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

  # make internal table
  dt <- data.table(ID = ID,
                   B_LU_BRP = B_LU_BRP,
                   A_SOM_LOI = A_SOM_LOI,
                   A_P_AL= A_P_AL,
                   A_P_WA = A_P_WA,
                   M_COMPOST = M_COMPOST,
                   M_GREEN = M_GREEN
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
  dt[,i_clim_osb := OBIC::evaluate_logistic(x = D_SOM_BAL,  b = 0.0008144, x0 = 0, v = 0.9077174, increasing = TRUE)]

  # return value
  value <- dt[, i_clim_osb]

  return(value)

}




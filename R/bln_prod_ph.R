#' Calculate the distance for target for soil pH in view of the BLN production function
#'
#' This functions evaluates the difference between the measured pH and the optimal pH according to the Bemestingsadvies
#'
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The organic matter content of soil in percentage
#' @param A_CLAY_MI (numeric) The percentage A_CLAY_MI present in the soil
#' @param A_PH_CC (numeric) The pH-CaCl2 of the soil
#'
#' @references Handboek Bodem en Bemesting tabel 5.1, 5.2 en 5.3
#'
#' @import data.table
#' @import OBIC
#'
#' @return
#' The bln indicator for the soil pH
#'
#' @export
bln_c_ph <- function(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_PH_CC) {

  # Check inputs
  arg.length <- max(length(A_PH_CC), length(B_SOILTYPE_AGR), length(A_SOM_LOI), length(A_CLAY_MI),
                    length(B_LU_BRP))
  checkmate::assert_numeric(A_PH_CC, lower = 2, upper = 10, any.missing = FALSE, len = arg.length)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices =unique(BLN::bln_soiltype$bln_soil_cat1))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(BLN::bln_crops$crop_code), empty.ok = FALSE)

  # Collect information in table
  dt <- data.table(ID = 1:arg.length,
                   B_LU_BRP = B_LU_BRP,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_CC = A_PH_CC
                  )

  # Calculate the crop rotation fraction
  dt[, D_CP_STARCH := calc_rotation_fraction(ID, B_LU_BRP, crop = "starch")]
  dt[, D_CP_POTATO := calc_rotation_fraction(ID, B_LU_BRP, crop = "potato")]
  dt[, D_CP_SUGARBEET := calc_rotation_fraction(ID, B_LU_BRP, crop = "sugarbeet")]
  dt[, D_CP_GRASS := calc_rotation_fraction(ID, B_LU_BRP, crop = "grass")]
  dt[, D_CP_MAIS := calc_rotation_fraction(ID, B_LU_BRP, crop = "mais")]
  dt[, D_CP_OTHER := calc_rotation_fraction(ID, B_LU_BRP, crop = "other")]
  dt[, D_CP_RUST := calc_rotation_fraction(ID, B_LU_BRP, crop = "rustgewas")]
  dt[, D_CP_RUSTDEEP := calc_rotation_fraction(ID, B_LU_BRP, crop = "rustgewasdiep")]

  # calculate the distance to optimum pH
  dt[, D_PH_DELTA := OBIC::calc_ph_delta(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI,
                                         A_CLAY_MI, A_PH_CC, D_CP_STARCH,
                                         D_CP_POTATO, D_CP_SUGARBEET, D_CP_GRASS,
                                         D_CP_MAIS, D_CP_OTHER)]

  # evaluate the distance to target for BLN indicator
  dt[, i_c_ph := ind_ph(D_PH_DELTA)]

  # extract the BLN indicator
  value <- dt[,i_c_ph]

  # return value
  return(value)

}

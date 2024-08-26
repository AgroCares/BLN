#' Function to calculate and evaluate the nitrogen use efficiency in view of the soils' function to improve nutrient recycling
#'
#' @param ID (character) A field id
#' @param B_LU_BRP (numeric) The crop code
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil (mg N / kg)
#'
#' @import data.table
#' @import OBIC
#'
#' @export
bln_nut_nitrogen <- function(ID, B_LU_BRP,B_SOILTYPE_AGR,A_SOM_LOI,A_N_RT){

  # make internal copy
  blnp <- BLN::bln_parms

  # check inputs B parameters
  arg.length <- max(length(B_LU_BRP),length(B_SOILTYPE_AGR), length(A_SOM_LOI),length(A_N_RT))
  checkmate::assert_subset(B_LU_BRP, choices = unlist(bln_crops$crop_code))
  checkmate::assert_integerish(B_LU_BRP, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unlist(blnp[code == "B_SOILTYPE_AGR", choices]))
  checkmate::assert_character(B_SOILTYPE_AGR, len = arg.length)

  # check inputs A parameters
  checkmate::assert_numeric(A_SOM_LOI, lower = blnp[code == "A_SOM_LOI", value_min], upper = blnp[code == "A_SOM_LOI", value_max],len = arg.length)
  checkmate::assert_numeric(A_N_RT, lower = blnp[code == "A_N_RT", value_min], upper = blnp[code == "A_N_RT", value_max],len = arg.length)

  # make internal table
  dt <- data.table(FIELD_ID = ID,
                   B_LU_BRP = B_LU_BRP,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT,
                   value = NA_real_)

  # add CN ratio
  dt[, A_CN_FR := A_SOM_LOI *10 * 0.5 *1000/ A_N_RT]

  # calculate derivative supporting soil properties
  dt[, D_BDS := OBIC::calc_bulk_density(B_SOILTYPE_AGR,A_SOM_LOI)]
  dt[, D_RD := OBIC::calc_root_depth(B_LU_BRP)]
  dt[, D_OC := OBIC::calc_organic_carbon(A_SOM_LOI, D_BDS, D_RD)]
  dt[, D_GA := OBIC::calc_grass_age(ID=FIELD_ID, B_LU_BRP)]

  # estimate the N supply
  dt[, D_NLV := OBIC::calc_nlv(B_LU_BRP, B_SOILTYPE_AGR, A_N_RT, A_CN_FR, D_OC, D_BDS, D_GA)]

  # estimate the indicator for N efficiency
  dt[, value := bln_evaluate_parabolic(x = D_NLV, x.top = 100, must.plateau = FALSE)]

  # return value i_nut_n
  value <- dt[, value]

  return(value)

}









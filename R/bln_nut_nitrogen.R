#' Function to calculate and evaluate the nitrogen use efficiency in view of the soils' function to improve nutrient recycling
#'
#' @param B_LU_BRP (numeric) The crop code
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil (mg N / kg)
#'
#' @import data.table
#' @import OBIC
#'
#' @export
bln_nut_nitrogen <- function(B_LU_BRP,B_SOILTYPE_AGR,A_SOM_LOI,A_N_RT){

  # load internal table
  dt.lsw <- BLN::bln_lsw[B_LSW_ID == 'lsw_nlmean']
  dt.crop <- BLN::bln_crops[bln_country=='NL']

  # make internal table
  dt <- data.table(id = 1:length(B_LU_BRP),
                   B_LU_BRP = B_LU_BRP,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT)

  # add CN ratio
  dt[, A_CN_FR := A_SOM_LOI *10 * 0.5 *1000/ A_N_RT]

  # calculate derivative supporting soil properties
  dt[, D_BDS := OBIC::calc_bulk_density(B_SOILTYPE_AGR,A_SOM_LOI)]
  dt[, D_RD := OBIC::calc_root_depth(B_LU_BRP)]
  dt[, D_OC := OBIC::calc_organic_carbon(A_SOM_LOI, D_BDS, D_RD)]
  dt[, D_GA := OBIC::calc_grass_age(ID=id, B_LU_BRP)]

  # estimate the N supply
  dt[, D_NLV := OBIC::calc_nlv(B_LU_BRP, B_SOILTYPE_AGR, A_N_RT, A_CN_FR, D_OC, D_BDS, D_GA)]

  # estimate the indicator for N efficiency
  dt[, i_nut_n := bln_evaluate_parabolic(x = D_NLV, x.top = 100, must.plateau = FALSE)]

  # return value
  value <- dt[, i_nut_n]

  return(value)

}









#' Function to calculate and evaluate the N retention in soils in view of water purification for surface quality
#'
#' This function equals the N retention function of the OSI
#'
#' @param B_LU_BRP (numeric) The crop code
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#'
#' @import data.table
#' @import OBIC
#'
#' @export
bln_wat_nretention_sw <- function(B_LU_BRP,B_SOILTYPE_AGR,B_AER_CBS,B_GWL_CLASS,A_SOM_LOI,A_N_RT){

  # make internal table
  dt <- data.table(id = 1:length(B_LU_BRP),
                   B_LU_BRP = B_LU_BRP,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_GWL_CLASS=B_GWL_CLASS,
                   B_AER_CBS=B_AER_CBS,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT
  )

  ### format inputs for OBIC
  dt[, B_GWL_CLASS := OBIC::format_gwt(B_GWL_CLASS)]
  dt[, B_AER_CBS := OBIC::format_aer(B_AER_CBS)]

  # add CN ratio
  dt[, A_CN_FR := A_SOM_LOI *10 * 0.5 *1000/ A_N_RT]

  # calculate derivative supporting soil properties
  dt[, D_BDS := OBIC::calc_bulk_density(B_SOILTYPE_AGR,A_SOM_LOI)]
  dt[, D_RD := OBIC::calc_root_depth(B_LU_BRP)]
  dt[, D_OC := OBIC::calc_organic_carbon(A_SOM_LOI, D_BDS, D_RD)]
  dt[, D_GA := OBIC::calc_grass_age(ID=id, B_LU_BRP)]

  # estimate derivates for availability P, K and acidity
  dt[, D_NLV := OBIC::calc_nlv(B_LU_BRP, B_SOILTYPE_AGR, A_N_RT, A_CN_FR, D_OC, D_BDS, D_GA)]

  # estimate the N leaching to groundwater
  dt[, D_NGW := OBIC::calc_nleach(B_LU_BRP = B_LU_BRP,
                                  B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                                  B_GWL_CLASS = B_GWL_CLASS,
                                  D_NLV = D_NLV,
                                  B_AER_CBS = B_AER_CBS,
                                  leaching_to = "ow")]

  # calculate indicator for N retention in view of surface water quality
  dt[, I_E_NSW := ind_nretention(D_NSW, leaching_to = "sw")]

  # extract value
  value <- dt[, I_E_NSW]

  # return value
  return(value)

}

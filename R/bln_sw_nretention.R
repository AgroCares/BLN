#' Function to calculate and evaluate the N retention in soils in view of water purification for surface quality
#'
#' This function equals the N retention function of the OSI
#'
#' @param ID (character) A field id
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
bln_wat_nretention_sw <- function(ID,B_LU_BRP,B_SOILTYPE_AGR,B_AER_CBS,B_GWL_CLASS,A_SOM_LOI,A_N_RT){

  # make internal copy
  blnp <- BLN::bln_parms

  # Check input
  arg.length <- max(length(B_LU_BRP),length(B_SOILTYPE_AGR), length(B_AER_CBS),
                    length(B_GWL_CLASS),length(A_SOM_LOI),length(A_N_RT))

  checkmate::assert_integerish(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(bln_crops$crop_code), empty.ok = FALSE)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unlist(blnp[code == "B_SOILTYPE_AGR", choices]))
  checkmate::assert_character(B_SOILTYPE_AGR, len = arg.length)
  checkmate::assert_subset(B_AER_CBS, choices = unlist(blnp[code == "B_AER_CBS", choices]))
  checkmate::assert_character(B_AER_CBS, len = arg.length)
  checkmate::assert_subset(B_GWL_CLASS, choices = unlist(blnp[code == "B_GWL_CLASS", choices]))
  checkmate::assert_character(B_GWL_CLASS, len = arg.length)

  # check inputs A parameters
  checkmate::assert_numeric(A_SOM_LOI, lower = blnp[code == "A_SOM_LOI", value_min], upper = blnp[code == "A_SOM_LOI", value_max],len = arg.length)
  checkmate::assert_numeric(A_N_RT, lower = blnp[code == "A_N_RT", value_min], upper = blnp[code == "A_N_RT", value_max],len = arg.length)


  # make internal table
  dt <- data.table(FIELD_ID = ID,
                   B_LU_BRP = B_LU_BRP,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_GWL_CLASS=B_GWL_CLASS,
                   B_AER_CBS=B_AER_CBS,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT,
                   value = NA_real_
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
  dt[, D_GA := OBIC::calc_grass_age(ID=FIELD_ID, B_LU_BRP)]

  # estimate derivates for availability P, K and acidity
  dt[, D_NLV := OBIC::calc_nlv(B_LU_BRP, B_SOILTYPE_AGR, A_N_RT, A_CN_FR, D_OC, D_BDS, D_GA)]

  # estimate the N leaching to groundwater
  dt[, D_NSW := OBIC::calc_nleach(B_LU_BRP = B_LU_BRP,
                                  B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                                  B_GWL_CLASS = B_GWL_CLASS,
                                  D_NLV = D_NLV,
                                  B_AER_CBS = B_AER_CBS,
                                  leaching_to = "ow")]

  # calculate indicator for N retention in view of surface water quality
  dt[, value := OBIC::ind_nretention(D_NSW, leaching_to = "ow")]

  # extract value I_E_NSW
  value <- dt[, value]

  # return value
  return(value)

}

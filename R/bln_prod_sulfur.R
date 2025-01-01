#' Calculate the capacity of soils to supply sulfur for the BLN production function
#'
#' This function calculates a S-balance given the SLV (sulfur supplying capacity) of a soil
#'
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (in percent)
#' @param A_S_RT (numeric) The total Sulpher content of the soil (in mg S per kg)
#'
#' @import data.table
#' @import OBIC
#'
#' @return
#' The capacity of the soil to supply Sulfur (kg S / ha / yr). A numeric value.
#'
#' @export
bln_c_sulfur <- function(B_LU_BRP, B_SOILTYPE_AGR, B_AER_CBS,A_SOM_LOI,A_S_RT) {

  # Add visual bindings
  D_BDS = D_SLV = i_c_s = NULL

  # Check input
  arg.length <- max(length(A_S_RT), length(A_SOM_LOI), length(B_LU_BRP),
                    length(B_SOILTYPE_AGR), length(B_AER_CBS))
  checkmate::assert_numeric(A_S_RT, lower = 0, upper = 10000, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(BLN::bln_crops$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(BLN::bln_soiltype$bln_soil_cat1), empty.ok = FALSE)
  checkmate::assert_character(B_AER_CBS, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_AER_CBS, choices = c('Zuid-Limburg','Zuidelijk Veehouderijgebied','Zuidwest-Brabant',
                                                  'Zuidwestelijk Akkerbouwgebied','Rivierengebied','Hollands/Utrechts Weidegebied',
                                                  'Waterland en Droogmakerijen','Westelijk Holland','IJsselmeerpolders',
                                                  'Centraal Veehouderijgebied','Oostelijk Veehouderijgebied','Noordelijk Weidegebied',
                                                  'Veenkolonien en Oldambt', 'Veenkoloni\xebn en Oldambt','Bouwhoek en Hogeland'), empty.ok = FALSE)

  # Collect data in a table
  dt <- data.table(id = 1:arg.length,
                   B_LU_BRP = B_LU_BRP,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_AER_CBS = B_AER_CBS,
                   A_SOM_LOI = A_SOM_LOI,
                   A_S_RT = A_S_RT,
                   i_c_s = NA_real_
                  )

  # calculate derivative supporting soil properties
  dt[, D_BDS := OBIC::calc_bulk_density(B_SOILTYPE_AGR,A_SOM_LOI)]

  # calculate the capacity of soils to supply S
  dt[, D_SLV := OBIC::calc_slv(B_LU_BRP, B_SOILTYPE_AGR, B_AER_CBS,A_SOM_LOI,A_S_RT, D_BDS)]

  # calculate the indicator
  dt[, i_c_s := OBIC::ind_sulfur(D_SLV, B_LU_BRP, B_SOILTYPE_AGR, B_AER_CBS)]

  # extract the i_c_s bln indicator
  value <- dt[, i_c_s]

  # return S indicator
  return(value)
}

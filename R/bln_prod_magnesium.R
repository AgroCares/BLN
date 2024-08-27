#' Calculate the capacity of soils to supply Magnesium for the BLN production function
#'
#' This function calculates an index for the availability of Magnesium in soil
#'
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_PH_CC (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)
#' @param A_CEC_CO (numeric) The cation exchange capacity of the soil (mmol+ per kg), analyzed via Cobalt-hexamine extraction
#' @param A_K_CO_PO (numeric) The occupation of the CEC with potassium (\%)
#' @param A_MG_CC (numeric) The plant available content of Mg in the soil (mg  Mg per kg) extracted by 0.01M CaCl2
#' @param A_K_CC (numeric) The plant available potassium, extracted with 0.01M CaCl2 (mg per kg),
#'
#' @import data.table
#' @import OBIC
#'
#' @return
#' An index representing the availability of Magnesium in a soil. A numeric value.
#'
#' @export
bln_c_magnesium <- function(B_LU_BRP,B_SOILTYPE_AGR,A_SOM_LOI,A_CLAY_MI,
                            A_PH_CC, A_CEC_CO,A_K_CO_PO,A_MG_CC,A_K_CC) {

  # Add visual bindings
  D_MG = i_c_mg = NULL

  # Check inputs
  arg.length <- max(length(A_MG_CC), length(A_PH_CC), length(A_SOM_LOI), length(A_CEC_CO),
                    length(A_K_CO_PO), length(A_CLAY_MI), length(B_SOILTYPE_AGR), length(B_LU_BRP))
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(BLN::bln_crops$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(BLN::bln_soiltype$bln_soil_cat1), empty.ok = FALSE)
  checkmate::assert_numeric(A_MG_CC, lower = 1, upper = 1100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 10, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CEC_CO, lower = 1, upper = 1000, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_K_CC, lower = 1, upper = 600, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_K_CO_PO, lower = 0.1, upper = 50, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)

  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    B_LU_BRP = B_LU_BRP,
    B_SOILTYPE_AGR = B_SOILTYPE_AGR,
    A_SOM_LOI = A_SOM_LOI,
    A_CLAY_MI = A_CLAY_MI,
    A_PH_CC = A_PH_CC,
    A_CEC_CO = A_CEC_CO,
    A_K_CO_PO = A_K_CO_PO,
    A_MG_CC = A_MG_CC,
    A_K_CC = A_K_CC,
    value = NA_real_
  )

  # estimate Mg availability
  dt[, D_MG := OBIC::calc_magnesium_availability(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI,
                                           A_PH_CC, A_CEC_CO,
                                           A_K_CO_PO, A_MG_CC, A_K_CC)]

  # calculate the Mg supply indicator
  dt[,i_c_mg := OBIC::ind_magnesium(D_MG = D_MG,
                                    B_LU_BRP = B_LU_BRP,
                                    B_SOILTYPE_AGR = B_SOILTYPE_AGR)]

  # extract Mg indicator
  value <- dt[,i_c_mg]

  # return value
  return(value)
}

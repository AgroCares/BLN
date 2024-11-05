#' Function to calculate and evaluate the pesticide retention in soils in view of water purification for surface water quality
#'
#' @param ID (character) A field id
#' @param B_LU_BRP (numeric) The crop code
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param M_GREEN (boolean) A soil measure. Are catch crops sown after main crop (optional, option: yes or no)
#' @param M_MECHWEEDS (boolean) measure. Use of mechanical weed protection (option: yes or no)
#' @param M_PESTICIDES_DST (boolean) measure. Use of DST for pesticides (option: yes or no)
#'
#' @import data.table
#' @import OBIC
#'
#' @export
bln_wat_pesticide <- function(ID,B_LU_BRP,B_SOILTYPE_AGR,
                              A_CLAY_MI,A_SAND_MI, A_SILT_MI, A_SOM_LOI,
                              M_GREEN,M_MECHWEEDS,M_PESTICIDES_DST){

  # add visual bindings
  bln_crops = code = choices = value_min = value_max = FIELD_ID = D_PESTICIDE =D_PSP = NULL

  # make internal copy
  blnp <- BLN::bln_parms

  # length of inpurt arguments
  arg.length <- max(length(B_LU_BRP),length(B_SOILTYPE_AGR),
                    length(A_CLAY_MI),length(A_SAND_MI),length(A_SILT_MI),
                    length(A_SOM_LOI),length(M_GREEN),length(M_MECHWEEDS),length(M_PESTICIDES_DST))

  checkmate::assert_integerish(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(BLN::bln_crops$crop_code), empty.ok = FALSE)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unlist(blnp[code == "B_SOILTYPE_AGR", choices]))
  checkmate::assert_character(B_SOILTYPE_AGR, len = arg.length)

  # check inputs A parameters
  checkmate::assert_numeric(A_CLAY_MI, lower = blnp[code == "A_CLAY_MI", value_min], upper = blnp[code == "A_CLAY_MI", value_max],len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = blnp[code == "A_SAND_MI", value_min], upper = blnp[code == "A_SAND_MI", value_max],len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = blnp[code == "A_SILT_MI", value_min], upper = blnp[code == "A_SILT_MI", value_max],len = arg.length)
  checkmate::assert_numeric(A_SOM_LOI, lower = blnp[code == "A_SOM_LOI", value_min], upper = blnp[code == "A_SOM_LOI", value_max],len = arg.length)
  checkmate::assert_logical(M_GREEN, any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(M_MECHWEEDS, any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(M_PESTICIDES_DST, any.missing = FALSE, len = arg.length)

  # make internal table
  dt <- data.table(FIELD_ID = ID,
                   id = 1:length(B_LU_BRP),
                   B_LU_BRP = B_LU_BRP,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   A_CLAY_MI=A_CLAY_MI,
                   A_SAND_MI=A_SAND_MI,
                   A_SILT_MI=A_SILT_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   M_GREEN = M_GREEN,
                   M_MECHWEEDS=M_MECHWEEDS,
                   M_PESTICIDES_DST = M_PESTICIDES_DST,
                   value = NA_real_)

  # estimate derivatives for precipidation surplus
  dt[, D_PSP := bln_calc_psp(ID = FIELD_ID, B_LU_BRP, M_GREEN)]

  # estimate the pesticide leaching to surface water
  dt[, D_PESTICIDE := OBIC::calc_pesticide_leaching(B_SOILTYPE_AGR,
                                                    A_SOM_LOI,
                                                    A_CLAY_MI,
                                                    A_SAND_MI,
                                                    A_SILT_MI,
                                                    D_PSP,
                                                    M_PESTICIDES_DST,
                                                    M_MECHWEEDS)]

  # calculate indicator for pesticide retention in view of surface water quality
  dt[, value := OBIC::ind_pesticide_leaching(D_PESTICIDE)]

  # extract value I_H_PEST
  value <- dt[, value]

  # return value
  return(value)

}

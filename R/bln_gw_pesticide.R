#' Function to calculate and evaluate the pesticide retention in soils in view of water purification for surface water quality
#'
#' @param ID (character) A field id
#' @param B_LU_BRP (numeric) The crop code
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param A_K_CC (numeric) The plant available K content, extracted with 0.01M CaCl2 (mg / kg)
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
                   M_PESTICIDES_DST = M_PESTICIDES_DST
  )

  # estimate derivatives for precipidation surplus
  dt[, D_PSP := OBIC::calc_psp(B_LU_BRP,M_GREEN), by = FIELD_ID]

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
  dt[, I_H_PEST := OBIC::ind_n_efficiency(D_PESTICIDE,'sw')]

  # extract value
  value <- dt[, I_H_PEST]

  # return value
  return(value)

}

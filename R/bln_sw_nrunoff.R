#' Function to calculate and evaluate the N leaching risk in soils in view of water purification for surface water quality
#'
#' @param B_LU_BRP (numeric) The crop code
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' @param B_DRAIN (boolean) Are drains installed to drain the field (options: yes or no)
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param B_SC_WENR (character) The risk for subsoil compaction as derived from risk assessment study of Van den Akker (2006).
#' @param B_FERT_NORM_FR (numeric) The fraction of the application norm utilized
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param A_P_AL (numeric) The P-AL content of the soil
#' @param A_P_CC (numeric) The plant available P content, extracted with 0.01M CaCl2 (mg / kg)
#' @param A_P_WA (numeric) The P-content of the soil extracted with water (mg P2O5 / 100 ml soil)
#' @param A_PH_CC (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)
#' @param A_CEC_CO (numeric) The cation exchange capacity of the soil (mmol+ / kg), analyzed via Cobalt-hexamine extraction
#' @param A_K_CO_PO (numeric) The occupation of the CEC with K (\%)
#' @param A_K_CC (numeric) The plant available K content, extracted with 0.01M CaCl2 (mg / kg)
#' @param M_GREEN (boolean) A soil measure. Are catch crops sown after main crop (optional, option: yes or no)
#'
#' @import data.table
#' @import OBIC
#'
#' @export
bln_wat_nrunoff <- function(B_LU_BRP,B_SC_WENR,B_GWL_CLASS,B_AER_CBS,B_DRAIN,B_FERT_NORM_FR = 1,
                            A_CLAY_MI,A_SAND_MI, A_SILT_MI, A_SOM_LOI,A_P_AL, A_P_WA, A_P_CC,
                            A_PH_CC, A_CEC_CO,A_K_CO_PO, A_K_CC,
                            M_GREEN){

  # make internal table
  dt <- data.table(id = 1:length(B_LU_BRP),
                   B_LU_BRP = B_LU_BRP,
                   B_SC_WENR=as.character(B_SC_WENR),
                   B_GWL_CLASS=B_GWL_CLASS,
                   B_AER_CBS=B_AER_CBS,
                   B_DRAIN=B_DRAIN,
                   B_FERT_NORM_FR=B_FERT_NORM_FR,
                   A_CLAY_MI=A_CLAY_MI,
                   A_SAND_MI=A_SAND_MI,
                   A_SILT_MI=A_SILT_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   A_P_AL = A_P_AL,
                   A_P_WA = A_P_WA,
                   A_P_CC = A_P_CC,
                   A_PH_CC = A_PH_CC,
                   A_CEC_CO = A_CEC_CO,
                   A_K_CO_PO = A_K_CO_PO,
                   A_K_CC = A_K_CC,
                   M_GREEN = M_GREEN)

  ### format inputs for OBIC
  dt[, B_SC_WENR := OBIC::format_soilcompaction(B_SC_WENR)]
  dt[, B_GWL_CLASS := OBIC::format_gwt(B_GWL_CLASS)]
  dt[, B_AER_CBS := OBIC::format_aer(B_AER_CBS)]

  # Calculate the crop rotation fraction
  dt[, D_CP_STARCH := OBIC::calc_rotation_fraction(ID=id, B_LU_BRP, crop = "starch")]
  dt[, D_CP_POTATO := OBIC::calc_rotation_fraction(ID=id, B_LU_BRP, crop = "potato")]
  dt[, D_CP_SUGARBEET := OBIC::calc_rotation_fraction(ID=id, B_LU_BRP, crop = "sugarbeet")]
  dt[, D_CP_GRASS := OBIC::calc_rotation_fraction(ID=id, B_LU_BRP, crop = "grass")]
  dt[, D_CP_MAIS := OBIC::calc_rotation_fraction(ID=id, B_LU_BRP, crop = "mais")]
  dt[, D_CP_OTHER := OBIC::calc_rotation_fraction(ID=id, B_LU_BRP, crop = "other")]
  dt[, D_CP_RUST := OBIC::calc_rotation_fraction(ID=id, B_LU_BRP, crop = "rustgewas")]
  dt[, D_CP_RUSTDEEP := OBIC::calc_rotation_fraction(ID=id, B_LU_BRP, crop = "rustgewasdiep")]

  # estimate derivates for availability P, K and acidity
  dt[, D_PBI := OBIC::calc_phosphate_availability(B_LU_BRP, A_P_AL, A_P_CC, A_P_WA)]
  dt[, D_K :=  OBIC::calc_potassium_availability(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_PH_CC,
                                                 A_CEC_CO, A_K_CO_PO, A_K_CC)]
  dt[, D_PH_DELTA := OBIC::calc_ph_delta(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_PH_CC, D_CP_STARCH,
                                         D_CP_POTATO, D_CP_SUGARBEET, D_CP_GRASS, D_CP_MAIS, D_CP_OTHER)]

  # estimate the N leaching to surface water
  dt[, D_NLEACH_SW := OBIC::calc_n_efficiency(B_LU_BRP = B_LU_BRP,
                                              B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                                              B_GWL_CLASS = B_GWL_CLASS,
                                              B_AER_CBS = B_AER_CBS,
                                              A_SOM_LOI = A_SOM_LOI,
                                              A_CLAY_MI = A_CLAY_MI,
                                              D_PBI = D_PBI,
                                              D_K = D_K,
                                              D_PH_DELTA = D_PH_DELTA,
                                              leaching_to = 'ow',
                                              M_GREEN = M_GREEN,
                                              B_FERT_NORM_FR = B_FERT_NORM_FR)]

  # calculate indicator for N retention in view of surface water quality
  dt[, I_H_NSW := OBIC::ind_n_efficiency(D_NLEACH_SW,'sw')]

  # extract value
  value <- dt[, I_H_NSW]

  # return value
  return(value)

}

#' Function to calculate and evaluate the groundwater recharche in view of the soils' function to retain water
#'
#' @param B_LU_BRP (numeric) The crop code
#' @param B_DRAIN (boolean) Are drains installed to drain the field (options: yes or no)
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param B_SC_WENR (character) The risk for subsoil compaction as derived from risk assessment study of Van den Akker (2006).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param M_GREEN (boolean) A soil measure. Are catch crops sown after main crop (optional, option: yes or no)
#'
#' @import data.table
#' @import OBIC
#'
#' @export
bln_wat_groundwater_recharge <- function(B_LU_BRP,B_SC_WENR,B_GWL_CLASS,B_DRAIN,
                                         A_CLAY_MI,A_SAND_MI, A_SILT_MI, A_SOM_LOI,M_GREEN){

  # make internal table
  dt <- data.table(id = 1:length(B_LU_BRP),
                   B_LU_BRP = B_LU_BRP,
                   B_SC_WENR=as.character(B_SC_WENR),
                   B_GWL_CLASS=B_GWL_CLASS,
                   B_DRAIN=B_DRAIN,
                   A_CLAY_MI=A_CLAY_MI,
                   A_SAND_MI=A_SAND_MI,
                   A_SILT_MI=A_SILT_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   M_GREEN = M_GREEN)

  ### format inputs for OBIC
  dt[, B_SC_WENR := OBIC::format_soilcompaction(B_SC_WENR)]
  dt[, B_GWL_CLASS := OBIC::format_gwt(B_GWL_CLASS)]

  # estimate derivatives: sealing risk, precipitaiton surplus and saturated permeability
  dt[, D_SE := OBIC::calc_sealing_risk(A_SOM_LOI, A_CLAY_MI)]
  dt[, D_PSP := OBIC::calc_psp(B_LU_BRP,M_GREEN), by = id]
  dt[, D_WRI_K := OBIC::calc_permeability(A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_SOM_LOI)]

  # estimate distance to target for soil compaction and seasling
  dt[, I_P_CO := OBIC::ind_compaction(B_SC_WENR)]
  dt[, I_P_SE := OBIC::ind_sealing(D_SE, B_LU_BRP)]

  # calculate indicator for groundwater recharge
  dt[, I_H_GWR := OBIC::ind_gw_recharge(B_LU_BRP, D_PSP, D_WRI_K, I_P_SE, I_P_CO, B_DRAIN, B_GWL_CLASS)]

  # extract value
  value <- dt[, I_H_GWR]

  # return value
  return(value)

}

#' Function to calculate and evaluate the N leaching risks in soils in view of water purification for groundwater quality
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
bln_wat_nrisk_gw <- function(B_LU_BRP,B_SC_WENR,B_GWL_CLASS,B_AER_CBS,B_DRAIN,B_FERT_NORM_FR = 1,
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

  # estimate the N leaching to groundwater
  dt[, D_NLEACH_GW := OBIC::calc_n_efficiency(B_LU_BRP = B_LU_BRP,
                                              B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                                              B_GWL_CLASS = B_GWL_CLASS,
                                              B_AER_CBS = B_AER_CBS,
                                              A_SOM_LOI = A_SOM_LOI,
                                              A_CLAY_MI = A_CLAY_MI,
                                              D_PBI = D_PBI,
                                              D_K = D_K,
                                              D_PH_DELTA = D_PH_DELTA,
                                              leaching_to = "gw",
                                              M_GREEN = M_GREEN,
                                              B_FERT_NORM_FR = B_FERT_NORM_FR)]

  # calculate indicator for N retention in view of groundwater quality
  dt[, I_H_NGW := OBIC::ind_n_efficiency(D_NLEACH_GW,'gw')]

  # extract value
  value <- dt[, I_H_NGW]

  # return value
  return(value)

}

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
bln_wat_nrisk_sw <- function(B_LU_BRP,B_SC_WENR,B_GWL_CLASS,B_AER_CBS,B_DRAIN,B_FERT_NORM_FR = 1,
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

#' Function to calculate and evaluate the pesticide retention in soils in view of water purification for surface water quality
#'
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
bln_wat_pesticide <- function(B_LU_BRP,B_SOILTYPE_AGR,
                                  A_CLAY_MI,A_SAND_MI, A_SILT_MI, A_SOM_LOI,
                                  M_GREEN,M_MECHWEEDS,M_PESTICIDES_DST){

  # make internal table
  dt <- data.table(id = 1:length(B_LU_BRP),
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
  dt[, D_PSP := OBIC::calc_psp(B_LU_BRP,M_GREEN), by = id]

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

#' Function to calculate and evaluate the N retention in soils in view of water purification for groundwater quality
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
bln_wat_nretention_gw <- function(B_LU_BRP,B_SOILTYPE_AGR,B_AER_CBS,B_GWL_CLASS,A_SOM_LOI,A_N_RT){

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
                                  leaching_to = "gw")]

  # calculate indicator for N retention in view of groundwater quality
  dt[, I_E_NGW := ind_nretention(D_NGW, leaching_to = "gw")]

  # extract value
  value <- dt[, I_E_NGW]

  # return value
  return(value)

}

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

#' Function to calculate and evaluate the N buffering capacity of soils in view of water purification for groundwater quality
#'
#' This function gives the NGW score of the BBWP framework
#'
#' @param B_LU_BRP (numeric) The crop code
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param B_GWP (boolean) is the field located in a groundwater protected area (options: TRUE or FALSE)
#' @param B_SC_WENR (integer) The risk for subsoil compaction as derived from risk assessment study of Van den Akker (2006). Options include: 1,2,3,4,5,10,11,401,901 and 902.
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' @param B_N_RT (numeric) The mean organic nitrogen content of the soil in the LSW region (mg N / kg). Optional.
#' @param B_N_RT_SD (numeric) The variance in organic nitrogen content of the soil in the LSW region (standard deviation) (mg N / kg). Optional.
#' @param penalty (boolean) the option to apply a penalty for high risk BBWP field indicators. Default is TRUE.
#'
#' @import data.table
#' @import OBIC
#'
#' @export
bln_bbwp_ngw <- function(B_LU_BRP,B_SOILTYPE_AGR,B_SC_WENR,B_AER_CBS,B_GW,B_GWL_CLASS,A_SOM_LOI,A_N_RT,
                         B_N_RT = NA_real_,B_N_RT_SD = NA_real_, penalty = TRUE){

  # load internal table
  dt.lsw <- BLN::bln_lsw[B_LSW_ID == 'lsw_nlmean']
  dt.crop <- BLN::bln_crops[bln_country=='NL']
  dt.soil <- BLN::bln_soiltype[bln_country=='NL']

  # make internal table
  dt <- data.table(id = 1:length(B_LU_BRP),
                   B_LU_BRP = B_LU_BRP,
                   B_SC_WENR = B_SC_WENR,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_GWL_CLASS=B_GWL_CLASS,
                   B_AER_CBS=B_AER_CBS,
                   B_GWP = B_GWP,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT,
                   B_N_RT = B_N_RT,
                   B_N_RT_SD = B_N_RT_SD)

  # check on BBWP format Gt
  cols <- c('GtI','GtII','GtIII','GtIV','GtV', 'GtVI','GtVII','GtVIII')
  dt[!B_GWL_CLASS %in% cols & grepl('lg14|limburg',tolower(B_AER_CBS)), B_GWL_CLASS := 'GtVIII']
  dt[!B_GWL_CLASS %in% cols & !grepl('lg14|limburg',tolower(B_AER_CBS)), B_GWL_CLASS := 'GtII']

  # replace missing LSW properties
  dt[is.na(B_N_RT), B_N_RT := dt.lsw$B_N_RT]
  dt[is.na(B_N_RT_SD), B_N_RT_SD := dt.lsw$B_N_RT_SD]

  # merge with crop category
  dt <- merge(dt,dt.crop[,.(crop_code,crop_cat1)],by.x='B_LU_BRP',by.y='crop_code',all.x = TRUE)
  dt <- merge(dt,dt.soil[,.(bln_soil_cat1,bln_soil_cat2)],by.x='B_SOILTYPE_AGR',by.y='bln_soil_cat1',all.x=TRUE)

  # estimate field properties

    # reclassify soil compaction risk (scr) into a numeric value
    # a high value is indicative of high risk of leaching of nitrogen to groundwater
    dt[B_SC_WENR %in% c(902, 901, 401),ngw_scr := 1]
    dt[B_SC_WENR == 1, ngw_scr := 1]
    dt[B_SC_WENR %in% c(2, 10), ngw_scr := 0.8]
    dt[B_SC_WENR == 3, ngw_scr := 0.6]
    dt[B_SC_WENR == 4, ngw_scr := 0.4]
    dt[B_SC_WENR %in% c(5, 11), ngw_scr := 0.2]

    # Re-categorize crop and soil types to match OBIC table
    dt[crop_cat1 == 'arable',crop_cat1_nl := 'akkerbouw']
    dt[crop_cat1 == 'maize',crop_cat1_nl := 'mais']
    dt[crop_cat1 %in% c('grassland','nature'),crop_cat1_nl := 'gras']
    dt[bln_soil_cat2 %in% c('sand','loess'), bln_soil_cat2_nl := 'zand']
    dt[bln_soil_cat2 == 'clay', bln_soil_cat2_nl := 'klei']
    dt[bln_soil_cat2 == 'peat', bln_soil_cat2_nl := 'veen']

    # merge fraction of N leaching into 'dt', based on soil type x crop type x grondwatertrap
    dt <- merge(dt, OBIC::nleach_table[, list(bln_soil_cat2_nl = bodem, crop_cat1_nl = gewas, B_GWL_CLASS = B_GT, nf)],
                by = c("bln_soil_cat2_nl", "crop_cat1_nl", "B_GWL_CLASS"), all.x = TRUE)

    # for situations that nf is unknown
    dt[is.na(nf), nf := 0.5]

    # rank the risk on soil N leaching to groundwater given crop type, soil type and gt
    # a high value means high risks for N leaching
    dt[,ngw_lea := nf / max(OBIC::nleach_table$nf)]

    # rank the nitrogen content as an estimate of total N content: a high value means high risk for N leaching
    dt[,ngw_nlv := pnorm(q = A_N_RT, mean = B_N_RT, sd = B_N_RT_SD)]

  # estimate field indicators

    # columns to be selected
    cols <- colnames(dt)[grepl('ngw_|id',colnames(dt))]

    # melt the data.table to simplify corrections
    dt.melt <- data.table::melt(dt[,mget(cols)], id.vars = 'id',variable.name = 'risk')

    # add correction factor based on risk itself
    dt.melt[,risk_cor := wf(value,type = "indicators",penalty = penalty)]

    # add groups of risk indicators
    dt.melt[,group := gsub('_[a-z]+$','',gsub('d_','',risk))]

    # add manual weighing factor for risks
    dt.melt[,mcf := 1]
    dt.melt[group=='ngw' & grepl('_lea$',risk), mcf := 3]
    dt.melt[group=='ngw' & grepl('_nlv$',risk), mcf := 2]

    # calculate the mean aggregated risk indicators
    dt.ind <- dt.melt[,list(risk = sum(risk_cor * value * mcf)/sum(risk_cor * mcf)),by=c('id','group')]
    dt.ind <- dcast(dt.ind,id~group,value.var='risk')

    # sort output based on id
    setorder(dt.ind,id)

    # add field indicator to the dt
    dt <- merge(dt,dt.ind,by='id')

  # estimate field score

    # correction when field is in a ground water protection zone
    dt[,cfngw := fifelse(B_GWP, 1, 0.5)]

    # lower the regional target for nitrate leaching (compared to the general target 1)
    dt[B_GWL_CLASS %in% c('GtI','GtII','GtIII'), cfngw := cfngw * 0.5]
    dt[B_SOILTYPE_AGR == 'veen', cfngw := cfngw * 0.1]

    # calculate the individual opportunity indexes
    dt[,d_opi_ngw := (0.5 + cfngw/2) * bln_evaluate_logistic(ngw, b=6, x0=0.4, v=.7)]

    # update the field score with measures (assuming no measures to be taken)
    dt[,d_opi_ngw := pmax(0,1 - pmax(0, d_opi_ngw - 0))]

    # # set bbwp ngw score
    dt[,s_bbwp_ngw := d_opi_ngw]

  # extract value
  value <- dt[, round(s_bbwp_ngw,2)]

  # return value
  return(value)

}

#' Function to calculate and evaluate the N buffering capacity of soils in view of water purification for surface quality
#'
#' This function gives the NSW score of the BBWP framework
#'
#' @param B_LU_BRP (numeric) The crop code
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param B_SC_WENR (integer) The risk for subsoil compaction as derived from risk assessment study of Van den Akker (2006). Options include: 1,2,3,4,5,10,11,401,901 and 902.
#' @param B_SLOPE_DEGREE (numeric) The slope of the field (degrees)
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' @param D_SA_W (numeric) The wet perimeter index of the field, fraction that field is surrounded by water
#' @param D_RO_R (numeric) The risk that surface water runs off the parcel
#' @param B_N_RT (numeric) The mean organic nitrogen content of the soil in the LSW region (mg N / kg). Optional.
#' @param B_N_RT_SD (numeric) The variance in organic nitrogen content of the soil in the LSW region (standard deviation) (mg N / kg). Optional.
#' @param D_RO_R (numeric) The mean D_RO_R of the soil in the LSW region. Optional.
#' @param D_RO_R_SD (numeric) The variance in D_RO_R in the LSW region (standard deviation). Optional.
#' @param B_CT_NSW (numeric) the critical target for required reduction in N loss from agriculture (kg N / ha) to reach targets of KRW
#' @param B_CT_NSW_MAX (numeric) the max critical target for N reduction loss (kg N / ha)
#' @param penalty (boolean) the option to apply a penalty for high risk BBWP field indicators. Default is TRUE.
#'
#' @import data.table
#' @import OBIC
#'
#' @export
bln_bbwp_nsw <- function(B_LU_BRP,B_SOILTYPE_AGR,B_SC_WENR,B_AER_CBS,B_GWL_CLASS,B_SLOPE_DEGREE,A_SOM_LOI,A_N_RT,
                         D_RO_R, D_SA_W,
                         B_N_RT = NA_real_,B_RO_R = NA_real_,B_RO_R_SD = NA_real_,B_N_RT_SD = NA_real_,
                         B_CT_NSW, B_CT_NSW_MAX = 5, penalty = TRUE){

  # load internal table
  dt.lsw <- BLN::bln_lsw[B_LSW_ID == 'lsw_nlmean']
  dt.crop <- BLN::bln_crops[bln_country=='NL']
  dt.soil <- BLN::bln_soiltype[bln_country=='NL']

  # make internal table
  dt <- data.table(id = 1:length(B_LU_BRP),
                   B_LU_BRP = B_LU_BRP,
                   B_SC_WENR = B_SC_WENR,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_GWL_CLASS=B_GWL_CLASS,
                   B_AER_CBS=B_AER_CBS,
                   B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT,
                   D_RO_R = D_RO_R,
                   D_SA_W = D_SA_W,
                   B_N_RT = B_N_RT,
                   B_N_RT_SD = B_N_RT_SD,
                   B_RO_R = B_RO_R,
                   B_RO_R_SD = B_RO_R_SD
                   )

  # check on BBWP format Gt
  cols <- c('GtI','GtII','GtIII','GtIV','GtV', 'GtVI','GtVII','GtVIII')
  dt[!B_GWL_CLASS %in% cols & grepl('lg14|limburg',tolower(B_AER_CBS)), B_GWL_CLASS := 'GtVIII']
  dt[!B_GWL_CLASS %in% cols & !grepl('lg14|limburg',tolower(B_AER_CBS)), B_GWL_CLASS := 'GtII']

  # replace missing LSW properties
  dt[is.na(B_N_RT), B_N_RT := dt.lsw$B_N_RT]
  dt[is.na(B_N_RT_SD), B_N_RT_SD := dt.lsw$B_N_RT_SD]
  dt[is.na(B_RO_R), B_RO_R := dt.lsw$B_RO_R]
  dt[is.na(B_RO_R_SD), B_RO_R_SD := dt.lsw$B_RO_R_SD]

  # merge with crop category
  dt <- merge(dt,dt.crop[,.(crop_code,crop_cat1)],by.x='B_LU_BRP',by.y='crop_code',all.x = TRUE)
  dt <- merge(dt,dt.soil[,.(bln_soil_cat1,bln_soil_cat2)],by.x='B_SOILTYPE_AGR',by.y='bln_soil_cat1',all.x=TRUE)

  # estimate field properties

    # reclassify soil compaction risk (scr) into a numeric value
    # a high value is indicative of high risk of leaching of nitrogen to groundwater
    dt[B_SC_WENR %in% c(902, 901, 401),ngw_scr := 1]
    dt[B_SC_WENR == 1, ngw_scr := 1]
    dt[B_SC_WENR %in% c(2, 10), ngw_scr := 0.8]
    dt[B_SC_WENR == 3, ngw_scr := 0.6]
    dt[B_SC_WENR == 4, ngw_scr := 0.4]
    dt[B_SC_WENR %in% c(5, 11), ngw_scr := 0.2]

    # reclassify soil compaction risk (scr) into a numeric value
    dt[,nsw_scr := 1 - ngw_scr]

    # reclassify the groundwater table (gwt) into a numeric value
    dt[B_GWL_CLASS %in% c('GtI', '-'), nsw_gwt := 1]
    dt[B_GWL_CLASS %in% c('GtIIb','GtIIIb','GtVb'), nsw_gwt := 0.9]
    dt[B_GWL_CLASS %in% c('GtII','GtIII','GtV'), nsw_gwt := 0.8]
    dt[B_GWL_CLASS %in% c('GtIV'), nsw_gwt := 0.7]
    dt[B_GWL_CLASS %in% c('GtVI'), nsw_gwt := 0.6]
    dt[B_GWL_CLASS %in% c('GtVII'), nsw_gwt := 0.5]
    dt[B_GWL_CLASS %in% c('GtVIII'), nsw_gwt := 0.4]

    # rank the risk for surface runoff (van Hattum, 2011)
    # higher risk is associated to increased risks for N runoff
    dt[,nsw_ro := pnorm(q = D_RO_R, mean = B_RO_R, sd = B_RO_R_SD)]

    # classify fields with a high slope as extra vulnerable for surface runoff
    # with fields with slope > 2% being vulnerabile (Groenendijk, 2020)
    dt[,nsw_slope := pmax(0.2,pmin(1,B_SLOPE_DEGREE/2))]

    # assess the risk for wet surroundings (Van Gerven, 2018): a high fraction equals a high risk
    # higher risk is associated to increased risks for N runoff
    dt[,nsw_ws := pmin(1,pmax(0,D_SA_W))]

    # rank the nitrogen content as an estimate of total N content: a high value means high risk for N leaching
    dt[,ngw_nlv := pnorm(q = A_N_RT, mean = B_N_RT, sd = B_N_RT_SD)]

    # rank the risk for N pool in soil: higher NLV is associated to increased risks for N runoff
    dt[,nsw_nlv := ngw_nlv]

    # do nlv correction for grassland
    dt[grepl('gras',crop_cat1), nsw_nlv := pmax(0, nsw_nlv - 0.5)]


  # estimate field indicators

    # columns to be selected
    cols <- colnames(dt)[grepl('nsw_|id',colnames(dt))]

    # melt the data.table to simplify corrections
    dt.melt <- data.table::melt(dt[,mget(cols)], id.vars = 'id',variable.name = 'risk')

    # add correction factor based on risk itself
    dt.melt[,risk_cor := wf(value,type = "indicators",penalty = penalty)]

    # add groups of risk indicators
    dt.melt[,group := gsub('_[a-z]+$','',gsub('d_','',risk))]

    # add manual weighing factor for risks
    dt.melt[,mcf := 1]
    dt.melt[group=='nsw' & grepl('_nlv$',risk), mcf := 3]

    # add criteria properties as column (to use as filter)
    dt.melt[,ws := value[risk=='nsw_ws'],by='id']
    dt.melt[,slope := value[risk=='nsw_slope'],by='id']

    # ensure that the final risk after aggregation gets the value 0.1 or 0.01
    dt.melt[ws <= 0.2 & slope < 1 & group %in% c('nsw'), c('mcf','risk_cor','value') :=  list(1,1000,0.1)]
    dt.melt[ws <= 0.1 & slope < 1 & group %in% c('nsw'), c('mcf','risk_cor','value') :=  list(1,1000,0.01)]
    dt.melt[,c('ws','slope') := NULL]

    # calculate the mean aggregated risk indicators
    dt.ind <- dt.melt[,list(risk = sum(risk_cor * value * mcf)/sum(risk_cor * mcf)),by=c('id','group')]
    dt.ind <- dcast(dt.ind,id~group,value.var='risk')

    # sort output based on id
    setorder(dt.ind,id)

    # add field indicator to the dt
    dt <- merge(dt,dt.ind,by='id')

  # estimate field score

    # correction when field is in a region with high target for N load reduction surface water
    dt[,cfnsw := pmax(0,pmin(1,B_CT_NSW / B_CT_NSW_MAX))]

    # replace to max critical limit when no information is ready
    dt[is.na(cfnsw), cfnsw := 1]

    # calculate the individual opportunity indexes
    dt[,d_opi_nsw := (0.5 + cfnsw/2) * bln_evaluate_logistic(ngw, b=6, x0=0.4, v=.7)]

    # update the field score with measures (assuming no measures to be taken)
    dt[,d_opi_nsw := pmax(0,1 - pmax(0, d_opi_nsw - 0))]

    # # set bbwp nsw score
    dt[,s_bbwp_nsw :=  d_opi_nsw]

  # extract value
  value <- dt[, round(s_bbwp_nsw,2)]

  # return value
  return(value)

}

#' Function to calculate and evaluate the P buffering capacity of soils in view of water purification for surface quality
#'
#' This function gives the PSW score of the BBWP framework
#'
#' @param B_LU_BRP (numeric) The crop code
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param B_SC_WENR (integer) The risk for subsoil compaction as derived from risk assessment study of Van den Akker (2006). Options include: 1,2,3,4,5,10,11,401,901 and 902.
#' @param B_SLOPE_DEGREE (numeric) The slope of the field (degrees)
#' @param A_FE_OX (numeric) The aluminium content of soil (mmol / kg)
#' @param A_AL_OX (numeric) The iron content of soil (mmol / kg)
#' @param A_P_CC (numeric) The plant available P content, measured via 0.01M CaCl2 extraction (mg / kg)
#' @param A_P_SG (numeric) The P-saturation index (\%)
#' @param D_SA_W (numeric) The wet perimeter index of the field, fraction that field is surrounded by water
#' @param D_RO_R (numeric) The risk that surface water runs off the parcel
#' @param D_RO_R (numeric) The mean D_RO_R of the soil in the LSW region. Optional.
#' @param D_RO_R_SD (numeric) The variance in D_RO_R in the LSW region (standard deviation). Optional.
#' @param B_CT_PSW (numeric) the critical target for required reduction in P loss from agriculture (kg P / ha) to reach targets of KRW
#' @param B_CT_PSW_MAX (numeric) the max critical target for P reduction loss (kg P / ha)
#' @param penalty (boolean) the option to apply a penalty for high risk BBWP field indicators. Default is TRUE.
#'
#' @import data.table
#' @import OBIC
#'
#' @export
bln_bbwp_psw <- function(B_LU_BRP,B_SC_WENR,B_AER_CBS,B_GWL_CLASS,B_SLOPE_DEGREE,
                         A_P_CC, A_P_SG, A_AL_OX, A_FE_OX,
                         D_RO_R, D_SA_W,
                         B_RO_R = NA_real_,B_RO_R_SD = NA_real_,
                         B_P_CC = NA_real_, B_P_CC_SD = NA_real_, B_P_SG = NA_real_, B_P_SG_SD = NA_real_,
                         B_AL_OX = NA_real_, B_AL_OX_SD = NA_real_, B_FE_OX = NA_real_, B_FE_OX_SD = NA_real_,
                         B_CT_PSW, B_CT_PSW_MAX = 0.5, penalty = TRUE){

  # load internal table
  dt.lsw <- BLN::bln_lsw[B_LSW_ID == 'lsw_nlmean']
  dt.crop <- BLN::bln_crops[bln_country=='NL']

  # make internal table
  dt <- data.table(id = 1:length(B_LU_BRP),
                   B_LU_BRP = B_LU_BRP,
                   B_SC_WENR = B_SC_WENR,
                   B_GWL_CLASS=B_GWL_CLASS,
                   B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                   B_AER_CBS=B_AER_CBS,
                   A_P_CC = A_P_CC,
                   A_P_SG = A_P_SG,
                   A_AL_OX = A_AL_OX,
                   A_FE_OX = A_FE_OX,
                   D_RO_R = D_RO_R,
                   D_SA_W = D_SA_W,
                   B_RO_R = B_RO_R,
                   B_RO_R_SD = B_RO_R_SD,
                   B_P_CC = B_P_CC,
                   B_P_CC_SD = B_P_CC_SD,
                   B_P_SG = B_P_SG,
                   B_P_SG_SD = B_P_SG_SD,
                   B_AL_OX = B_AL_OX,
                   B_AL_OX_SD = B_AL_OX_SD,
                   B_FE_OX = B_FE_OX,
                   B_FE_OX_SD = B_FE_OX_SD
                   )

  # check on BBWP format Gt
  cols <- c('GtI','GtII','GtIII','GtIV','GtV', 'GtVI','GtVII','GtVIII')
  dt[!B_GWL_CLASS %in% cols & grepl('lg14|limburg',tolower(B_AER_CBS)), B_GWL_CLASS := 'GtVIII']
  dt[!B_GWL_CLASS %in% cols & !grepl('lg14|limburg',tolower(B_AER_CBS)), B_GWL_CLASS := 'GtII']

  # replace missing LSW properties
  dt[is.na(B_P_CC), B_P_CC := dt.lsw$B_P_CC]
  dt[is.na(B_P_CC_SD), B_P_CC_SD := dt.lsw$B_P_CC_SD]
  dt[is.na(B_P_SG), B_P_SG := dt.lsw$B_P_SG]
  dt[is.na(B_P_SG_SD), B_P_SG_SD := dt.lsw$B_P_SG_SD]
  dt[is.na(B_AL_OX), B_AL_OX := dt.lsw$B_AL_OX]
  dt[is.na(B_AL_OX_SD), B_AL_OX_SD := dt.lsw$B_AL_OX_SD]
  dt[is.na(B_FE_OX), B_FE_OX := dt.lsw$B_FE_OX]
  dt[is.na(B_FE_OX_SD), B_FE_OX_SD := dt.lsw$B_FE_OX_SD]
  dt[is.na(B_RO_R), B_RO_R := dt.lsw$B_RO_R]
  dt[is.na(B_RO_R_SD), B_RO_R_SD := dt.lsw$B_RO_R_SD]

  # merge with crop category
  dt <- merge(dt,dt.crop[,.(crop_code,crop_cat1)],by.x='B_LU_BRP',by.y='crop_code',all.x = TRUE)

  # estimate field properties

    # reclassify soil compaction risk (scr) into a numeric value
    # a high value is indicative of high risk of leaching of nitrogen to groundwater
    dt[B_SC_WENR %in% c(902, 901, 401),ngw_scr := 1]
    dt[B_SC_WENR == 1, ngw_scr := 1]
    dt[B_SC_WENR %in% c(2, 10), ngw_scr := 0.8]
    dt[B_SC_WENR == 3, ngw_scr := 0.6]
    dt[B_SC_WENR == 4, ngw_scr := 0.4]
    dt[B_SC_WENR %in% c(5, 11), ngw_scr := 0.2]

    # reclassify soil compaction risk (scr) into a numeric value
    dt[,psw_scr := 1 - ngw_scr]

    # reclassify the groundwater table (gwt) into a numeric value
    dt[B_GWL_CLASS %in% c('GtI', '-'), psw_gwt := 1]
    dt[B_GWL_CLASS %in% c('GtIIb','GtIIIb','GtVb'), psw_gwt := 0.9]
    dt[B_GWL_CLASS %in% c('GtII','GtIII','GtV'), psw_gwt := 0.8]
    dt[B_GWL_CLASS %in% c('GtIV'), psw_gwt := 0.7]
    dt[B_GWL_CLASS %in% c('GtVI'), psw_gwt := 0.6]
    dt[B_GWL_CLASS %in% c('GtVII'), psw_gwt := 0.5]
    dt[B_GWL_CLASS %in% c('GtVIII'), psw_gwt := 0.4]

    # rank the risk for surface runoff (van Hattum, 2011)
    # higher risk is associated to increased risks for N runoff
    dt[,psw_ro := pnorm(q = D_RO_R, mean = B_RO_R, sd = B_RO_R_SD)]

    # classify fields with a high slope as extra vulnerable for surface runoff
    # with fields with slope > 2% being vulnerabile (Groenendijk, 2020)
    dt[,psw_slope := pmax(0.2,pmin(1,B_SLOPE_DEGREE/2))]

    # assess the risk for wet surroundings (Van Gerven, 2018): a high fraction equals a high risk
    # higher risk is associated to increased risks for N runoff
    dt[,nsw_ws := pmin(1,pmax(0,D_SA_W))]

    # rank the risk for wet surroundings (Van Gerven, 2018)
    dt[,psw_ws := nsw_ws]

    # rank the risk for P pools in soil
    dt[,psw_pcc := pnorm(q = A_P_CC, mean = B_P_CC, sd = B_P_CC_SD)]
    dt[,psw_psg := pnorm(q = A_P_SG, mean = B_P_SG, sd = B_P_SG_SD)]
    dt[,psw_pret := 1- pnorm(q =  A_AL_OX + A_FE_OX,
                             mean = B_AL_OX + B_FE_OX,
                             sd =  sqrt(B_AL_OX_SD^2 + B_FE_OX_SD^2))]

  # estimate field indicators

    # columns to be selected
    cols <- colnames(dt)[grepl('psw_|id',colnames(dt))]

    # melt the data.table to simplify corrections
    dt.melt <- data.table::melt(dt[,mget(cols)], id.vars = 'id',variable.name = 'risk')

    # add correction factor based on risk itself
    dt.melt[,risk_cor := wf(value,type = "indicators",penalty = penalty)]

    # add groups of risk indicators
    dt.melt[,group := gsub('_[a-z]+$','',gsub('d_','',risk))]

    # add manual weighing factor for risks
    dt.melt[,mcf := 1]
    dt.melt[group=='psw' & grepl('_scr$|_ro$|_ws$',risk), mcf := 2]

    # add criteria properties as column (to use as filter)
    dt.melt[,ws := value[risk=='psw_ws'],by='id']
    dt.melt[,slope := value[risk=='psw_slope'],by='id']

    # ensure that the final risk after aggregation gets the value 0.1 or 0.01
    dt.melt[ws <= 0.2 & slope < 1 & group %in% c('psw'), c('mcf','risk_cor','value') :=  list(1,1000,0.1)]
    dt.melt[ws <= 0.1 & slope < 1 & group %in% c('psw'), c('mcf','risk_cor','value') :=  list(1,1000,0.01)]
    dt.melt[,c('ws','slope') := NULL]

    # calculate the mean aggregated risk indicators
    dt.ind <- dt.melt[,list(risk = sum(risk_cor * value * mcf)/sum(risk_cor * mcf)),by=c('id','group')]
    dt.ind <- dcast(dt.ind,id~group,value.var='risk')

    # sort output based on id
    setorder(dt.ind,id)

    # add field indicator to the dt
    dt <- merge(dt,dt.ind,by='id')

  # estimate field score

  # correction when field is in a region with high target for N load reduction surface water
  dt[,cfpsw := pmax(0,pmin(1,B_CT_PSW / B_CT_PSW_MAX))]

  # replace to max critical limit when no information is ready
  dt[is.na(cfpsw), cfpsw := 1]

  # calculate the individual opportunity indexes
  dt[,d_opi_psw := (0.5 + cfpsw/2) * bln_evaluate_logistic(ngw, b=6, x0=0.4, v=.7)]

  # update the field score with measures (assuming no measures to be taken)
  dt[,d_opi_psw := pmax(0,1 - pmax(0, d_opi_psw - 0))]

  # set bbwp psw score
  dt[,s_bbwp_psw := d_opi_psw]

  # extract value
  value <- dt[, round(s_bbwp_psw,2)]

  # return value
  return(value)

}

#' Function to calculate and evaluate the water buffering capacity of soils in view of water retention in soil
#'
#' This function gives the WB score of the BBWP framework
#'
#' @param B_LU_BRP (numeric) The crop code
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param B_HELP_WENR (character) The soil type abbreviation, derived from 1:50.000 soil map
#' @param B_AREA_DROUGHT (boolean) is the field located in an area with high risks for water deficiencies (options: TRUE or FALSE)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param penalty (boolean) the option to apply a penalty for high risk BBWP field indicators. Default is TRUE.
#'
#' @import data.table
#' @import OBIC
#'
#' @export
bln_bbwp_bw <- function(B_LU_BRP,B_HELP_WENR,B_GWL_CLASS,B_AREA_DROUGHT,A_CLAY_MI, A_SAND_MI, A_SILT_MI, A_SOM_LOI,penalty = TRUE){

  # load internal table
  dt.crop <- BLN::bln_crops[bln_country=='NL']

  # make internal table
  dt <- data.table(id = 1:length(B_LU_BRP),
                   B_LU_BRP = B_LU_BRP,
                   B_HELP_WENR = B_HELP_WENR,
                   B_GWL_CLASS = B_GWL_CLASS,
                   B_AREA_DROUGHT = B_AREA_DROUGHT,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_SOM_LOI = A_SOM_LOI
                   )

  # merge with crop category
  dt <- merge(dt,dt.crop[,.(crop_code,crop_cat1)],by.x='B_LU_BRP',by.y='crop_code')

  # Replace '-' with 'unknown'
  dt[! B_GWL_CLASS %in% c('GtI','GtII','GtIII','GtIV','GtV', 'GtVI','GtVII','GtVIII'), B_GWL_CLASS := '-']

  # estimate field properties

    # calculate the OBIC water risk index for wetstress (% yield reduction)
    dt[, wue_wwri := 1] # When B_HELP_WENR is `unknown`
    if (nrow(dt[B_HELP_WENR != 'unknown',]) > 0) {
      dt[B_HELP_WENR != 'unknown', wue_wwri := OBIC::calc_waterstressindex(
        B_HELP_WENR = B_HELP_WENR,
        B_LU_BRP = B_LU_BRP,
        B_GWL_CLASS = B_GWL_CLASS,
        WSI = 'wetnessstress'
      ) * 0.01]
    }

    # calculate the OBIC water risk index for droughtstress (% yield reduction)
    dt[, wue_wdri := 1] # When B_HELP_WENR is `unknown`
    if (nrow(dt[B_HELP_WENR != 'unknown',]) > 0) {
      dt[B_HELP_WENR != 'unknown', wue_wdri := OBIC::calc_waterstressindex(
        B_HELP_WENR = B_HELP_WENR,
        B_LU_BRP = B_LU_BRP,
        B_GWL_CLASS = B_GWL_CLASS,
        WSI = 'droughtstress'
      ) * 0.01]
    }

    # calculate the possibility to store water (water holding capacity)
    dt[,wue_whc := OBIC::calc_waterretention(A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,A_SILT_MI = A_SILT_MI,A_SOM_LOI = A_SOM_LOI,
                                             type = 'water holding capacity'
                                             )]

    # transform wue_whc to an index between 0 and 1
    dt[,wue_whc := 1 - evaluate_logistic(wue_whc, b = 25, x0 = 0.4,v = 0.35)]

  # estimate field indicator

    # columns to be selected
    cols <- colnames(dt)[grepl('wue_|id',colnames(dt))]

    # melt the data.table to simplify corrections
    dt.melt <- data.table::melt(dt[,mget(cols)], id.vars = 'id',variable.name = 'risk')

    # add correction factor based on risk itself
    dt.melt[,risk_cor := wf(value,type = "indicators",penalty = penalty)]

    # add groups of risk indicators
    dt.melt[,group := gsub('_[a-z]+$','',gsub('d_','',risk))]

    # add manual weighing factor for risks
    dt.melt[,mcf := 1]
    dt.melt[group=='wue' & grepl('_whc$',risk), mcf := 2]

    # calculate the mean aggregated risk indicators
    dt.ind <- dt.melt[,list(risk = sum(risk_cor * value * mcf)/sum(risk_cor * mcf)),by=c('id','group')]
    dt.ind <- dcast(dt.ind,id~group,value.var='risk')

    # sort output based on id
    setorder(dt.ind,id)

    # add field indicator to the dt
    dt <- merge(dt,dt.ind,by='id')

  # estimate field score

    # correction when field is in a region with high water deficiency risks
    dt[,cfwb := fifelse(B_AREA_DROUGHT, 1, 0.5)]

    # calculate the individual opportunity indexes
    dt[,d_opi_wb := (0.5 + cfwb/2) * bln_evaluate_logistic(wue, b=6, x0=0.4, v=.7)]

    # update the field score with measures (assume no measures to be taken)
    dt[,d_opi_wb := pmax(0,1 - pmax(0, d_opi_wb - 0))]

    # Convert form 0-1 to 0-100
    dt[,s_bbwp_wb := 100 * d_opi_wb]

  # return value
  value <- dt[, round(s_bbwp_wb,0)]

  return(value)

}


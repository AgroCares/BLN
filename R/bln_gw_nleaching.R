#' Function to calculate and evaluate the N leaching risks in soils in view of water purification for groundwater quality
#'
#' @param ID (character) A field id
#' @param B_LU_BRP (numeric) The crop code
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
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
bln_wat_nrisk_gw <- function(ID,B_LU_BRP,B_SOILTYPE_AGR,B_AER_CBS,B_GWL_CLASS,B_SC_WENR,B_FERT_NORM_FR = 1,
                             A_CLAY_MI,A_SAND_MI, A_SILT_MI, A_SOM_LOI,A_P_AL, A_P_WA, A_P_CC,
                             A_PH_CC, A_CEC_CO,A_K_CO_PO, A_K_CC,
                             M_GREEN){

  # add visual bindings
  bln_crops = code = choices = value_min = value_max = FIELD_ID = NULL
  D_CP_STARCH = FIELD_ID = D_CP_POTATO = D_CP_SUGARBEET = D_CP_GRASS = D_CP_MAIS = D_CP_OTHER = D_CP_RUST = D_CP_RUSTDEEP = NULL
  D_PBI = D_K = D_PH_DELTA = D_NLEACH_GW = NULL

  # make internal copy
  blnp <- BLN::bln_parms

  # length of input
  arg.length <- max(length(B_LU_BRP),length(B_SOILTYPE_AGR), length(B_AER_CBS),
                    length(B_GWL_CLASS),length(B_SC_WENR),length(B_FERT_NORM_FR),
                    length(A_CLAY_MI),length(A_SAND_MI),length(A_SILT_MI),length(A_SOM_LOI),
                    length(A_P_AL),length(A_P_WA),length(A_P_CC),length(A_PH_CC),
                    length(A_CEC_CO),length(A_K_CO_PO),length(A_K_CC),length(M_GREEN))

  # adjust input
  B_AER_CBS <- bln_format_aer(B_AER_CBS,type='code')
  if(length(B_FERT_NORM_FR)==1){B_FERT_NORM_FR <- rep(B_FERT_NORM_FR,arg.length)}

  # check inputs
  checkmate::assert_integerish(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(BLN::bln_crops$crop_code), empty.ok = FALSE)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unlist(blnp[code == "B_SOILTYPE_AGR", choices]))
  checkmate::assert_character(B_SOILTYPE_AGR, len = arg.length)
  checkmate::assert_subset(B_AER_CBS, choices = unlist(blnp[code == "B_AER_CBS", choices]))
  checkmate::assert_character(B_AER_CBS, len = arg.length)
  checkmate::assert_subset(B_GWL_CLASS, choices = unlist(blnp[code == "B_GWL_CLASS", choices]))
  checkmate::assert_character(B_GWL_CLASS, len = arg.length)
  checkmate::assert_subset(B_SC_WENR, choices = unlist(blnp[code == "B_SC_WENR", choices]))
  checkmate::assert_integerish(B_SC_WENR, len = arg.length)
  checkmate::assert_numeric(B_FERT_NORM_FR, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(M_GREEN,min.len = 1)

  # check inputs A parameters
  checkmate::assert_numeric(A_CLAY_MI, lower = blnp[code == "A_CLAY_MI", value_min], upper = blnp[code == "A_CLAY_MI", value_max],len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = blnp[code == "A_SAND_MI", value_min], upper = blnp[code == "A_SAND_MI", value_max],len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = blnp[code == "A_SILT_MI", value_min], upper = blnp[code == "A_SILT_MI", value_max],len = arg.length)
  checkmate::assert_numeric(A_SOM_LOI, lower = blnp[code == "A_SOM_LOI", value_min], upper = blnp[code == "A_SOM_LOI", value_max],len = arg.length)
  checkmate::assert_numeric(A_P_AL, lower = blnp[code == "A_P_AL", value_min], upper = blnp[code == "A_P_AL", value_max],len = arg.length)
  checkmate::assert_numeric(A_P_WA, lower = blnp[code == "A_P_WA", value_min], upper = blnp[code == "A_P_WA", value_max],len = arg.length)
  checkmate::assert_numeric(A_P_CC, lower = blnp[code == "A_P_CC", value_min], upper = blnp[code == "A_P_CC", value_max],len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = blnp[code == "A_PH_CC", value_min], upper = blnp[code == "A_PH_CC", value_max],len = arg.length)
  checkmate::assert_numeric(A_CEC_CO, lower = blnp[code == "A_CEC_CO", value_min], upper = blnp[code == "A_CEC_CO", value_max],len = arg.length)
  checkmate::assert_numeric(A_K_CO_PO, lower = blnp[code == "A_K_CO_PO", value_min], upper = blnp[code == "A_K_CO_PO", value_max],len = arg.length)
  checkmate::assert_numeric(A_K_CC, lower = blnp[code == "A_K_CC", value_min], upper = blnp[code == "A_K_CC", value_max],len = arg.length)

  # make internal table
  dt <- data.table(FIELD_ID = ID,
                   CROP_ID = 1:length(B_LU_BRP),
                   B_LU_BRP = B_LU_BRP,
                   B_SC_WENR=as.character(B_SC_WENR),
                   B_GWL_CLASS=B_GWL_CLASS,
                   B_AER_CBS=B_AER_CBS,
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
                   M_GREEN = M_GREEN,
                   value = NA_real_)

  ### format inputs for OBIC
  dt[, B_SC_WENR := OBIC::format_soilcompaction(B_SC_WENR)]
  dt[, B_GWL_CLASS := OBIC::format_gwt(B_GWL_CLASS)]
  dt[, B_AER_CBS := OBIC::format_aer(B_AER_CBS)]

  # Calculate the crop rotation fraction
  dt[, D_CP_STARCH := OBIC::calc_rotation_fraction(ID=FIELD_ID, B_LU_BRP, crop = "starch")]
  dt[, D_CP_POTATO := OBIC::calc_rotation_fraction(ID=FIELD_ID, B_LU_BRP, crop = "potato")]
  dt[, D_CP_SUGARBEET := OBIC::calc_rotation_fraction(ID=FIELD_ID, B_LU_BRP, crop = "sugarbeet")]
  dt[, D_CP_GRASS := OBIC::calc_rotation_fraction(ID=FIELD_ID, B_LU_BRP, crop = "grass")]
  dt[, D_CP_MAIS := OBIC::calc_rotation_fraction(ID=FIELD_ID, B_LU_BRP, crop = "mais")]
  dt[, D_CP_OTHER := OBIC::calc_rotation_fraction(ID=FIELD_ID, B_LU_BRP, crop = "other")]
  dt[, D_CP_RUST := OBIC::calc_rotation_fraction(ID=FIELD_ID, B_LU_BRP, crop = "rustgewas")]
  dt[, D_CP_RUSTDEEP := OBIC::calc_rotation_fraction(ID=FIELD_ID, B_LU_BRP, crop = "rustgewasdiep")]

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
  dt[, value := OBIC::ind_n_efficiency(D_NLEACH_GW,'gw')]

  # extract value I_H_NGW
  value <- dt[, value]

  # return value
  return(value)

}

#' Calculate the BLN score for one field
#'
#' This functions wraps the functions of the BLN2 into one main function to calculate the soil quality score for a single field.
#'
#' @param B_LU_BRP (numeric) a series with crop codes given the crop rotation plan (source: the BRP)
#' @param B_LU_BBWP (character) The BBWP category used for allocation of measures to BBWP crop categories
#' @param B_SC_WENR (character) The risk for subsoil compaction as derived from risk assessment study of Van den Akker (2006).
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param B_HELP_WENR (character) The soil type abbreviation, derived from 1:50.000 soil map
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' @param B_GWL_GLG (numeric) The lowest groundwater level averaged over the most dry periods in 8 years in cm below ground level
#' @param B_GWL_GHG (numeric) The highest groundwater level averaged over the most wet periods in 8 years in cm below ground level
#' @param B_GWL_ZCRIT  (numeric) The distance between ground level and groundwater level at which the groundwater can supply the soil surface with 2mm water per day (in cm)
#' @param B_DRAIN (boolean) Are drains installed to drain the field (options: yes or no)
#' @param B_FERT_NORM_FR (numeric) The fraction of the application norm utilized
#' @param B_SLOPE_DEGREE (numeric) The slope of the field (degrees)
#' @param B_GWP (boolean) is the field located in a groundwater protected area (options: TRUE or FALSE)
#' @param B_AREA_DROUGHT (boolean) is the field located in an area with high risks for water deficiencies (options: TRUE or FALSE)
#' @param B_CT_PSW (numeric) the critical target for required reduction in P loss from agriculture (kg P / ha) to reach targets of KRW
#' @param B_CT_NSW (numeric) the critical target for required reduction in N loss from agriculture (kg N / ha) to reach targets of KRW
#' @param B_CT_PSW_MAX (numeric) the max critical target for P reduction loss (kg P / ha)
#' @param B_CT_NSW_MAX (numeric) the max critical target for N reduction loss (kg N / ha)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_SOM_LOI_MLMAX (numeric) The max. percentage organc matter estimated via machine learning model (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#' @param A_DENSITY_SA (numeric) The bulk density of the soil (kg/m3)
#' @param A_FE_OX (numeric) The aluminium content of soil (mmol / kg)
#' @param A_AL_OX (numeric) The iron content of soil (mmol / kg)
#' @param A_PH_CC (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' @param A_CN_FR (numeric) The carbon to nitrogen ratio (-)
#' @param A_S_RT (numeric) The total Sulfur content of the soil (in mg S per kg)
#' @param A_N_PMN (numeric) The potentially mineralizable N pool (mg N / kg soil)
#' @param A_P_AL (numeric) The P-AL content of the soil
#' @param A_P_CC (numeric) The plant available P content, extracted with 0.01M CaCl2 (mg / kg)
#' @param A_P_WA (numeric) The P-content of the soil extracted with water (mg P2O5 / 100 ml soil)
#' @param A_P_SG (numeric) The P-saturation index (\%)
#' @param A_CEC_CO (numeric) The cation exchange capacity of the soil (mmol+ / kg), analyzed via Cobalt-hexamine extraction
#' @param A_CA_CO_PO (numeric) The The occupation of the CEC with Ca (\%)
#' @param A_MG_CO_PO (numeric) The The occupation of the CEC with Mg (\%)
#' @param A_K_CO_PO (numeric) The occupation of the CEC with K (\%)
#' @param A_K_CC (numeric) The plant available K content, extracted with 0.01M CaCl2 (mg / kg)
#' @param A_MG_CC (numeric) The plant available Mg content, extracted with 0.01M CaCl2 (ug / kg)
#' @param A_MN_CC (numeric) The plant available Mn content, extracted with 0.01M CaCl2 (ug / kg)
#' @param A_ZN_CC (numeric) The plant available Zn content, extracted with 0.01M CaCl2 (ug / kg)
#' @param A_CU_CC (numeric) The plant available Cu content, extracted with 0.01M CaCl2 (ug / kg)
#' @param A_EW_BCS (numeric) The presence of earth worms (optional, score 0-1-2)
#' @param A_SC_BCS (numeric) The presence of compaction of subsoil (optional, score 0-1-2)
#' @param A_GS_BCS (numeric) The presence of waterlogged conditions, gley spots (optional, score 0-1-2)
#' @param A_P_BCS (numeric) The presence / occurrence of water puddles on the land, ponding (optional, score 0-1-2)
#' @param A_C_BCS (numeric) The presence of visible cracks in the top layer (optional, score 0-1-2)
#' @param A_RT_BCS (numeric) The presence of visible tracks / rutting or trampling on the land (optional, score 0-1-2)
#' @param A_RD_BCS (integer) The rooting depth (optional, score 0-1-2)
#' @param A_SS_BCS (integer) The soil structure (optional, score 0-1-2)
#' @param A_CC_BCS (integer) The crop cover on the surface (optional, score 0-1-2)
#' @param D_SA_W (numeric) The wet perimeter index of the field, fraction that field is surrounded by water
#' @param D_RO_R (numeric) The risk that surface water runs off the parcel
#' @param M_COMPOST (numeric) The frequency that compost is applied (optional, every x years)
#' @param M_GREEN (boolean) A soil measure. Are catch crops sown after main crop (optional, option: yes or no)
#' @param M_NONBARE (boolean) A soil measure. Is parcel for 80 percent of the year cultivated and 'green' (optional, option: yes or no)
#' @param M_EARLYCROP (boolean) A soil measure. Use of early crop varieties to avoid late harvesting (optional, option: yes or no)
#' @param M_SLEEPHOSE (boolean) A soil measure. Is sleephose used for slurry application (optional, option: yes or no)
#' @param M_DRAIN (boolean) A soil measure. Are under water drains installed in peaty soils (optional, option: yes or no)
#' @param M_DITCH (boolean) A soil measure. Are ditched maintained carefully and slib applied on the land (optional, option: yes or no)
#' @param M_UNDERSEED (boolean) A soil measure. Is grass used as second crop in between maize rows (optional, option: yes or no)
#' @param M_LIME (boolean) measure. Has field been limed in last three years (option: yes or no)
#' @param M_NONINVTILL (boolean) measure. Non inversion tillage (option: yes or no)
#' @param M_SSPM (boolean) measure. Soil Structure Protection Measures, such as fixed driving lines, low pressure tires, and light weighted machinery (option: yes or no)
#' @param M_SOLIDMANURE (boolean) measure. Use of solid manure (option: yes or no)
#' @param M_STRAWRESIDUE (boolean) measure. Application of straw residues (option: yes or no)
#' @param M_MECHWEEDS (boolean) measure. Use of mechanical weed protection (option: yes or no)
#' @param M_PESTICIDES_DST (boolean) measure. Use of DST for pesticides (option: yes or no)
#' @param B_LSW_ID (character) An unique identifier for each Local Surface Water per field
#' @param LSW (data.table) The averaged soil properties (mean and sd) per Local Surface Water. Can be derived from bbwp_lsw_properties.
#' @param ID (character) A field id
#' @param output (character) An optional argument to select output: scores, indicators, or all. (default = all)
#'
#' @import OBIC
#'
#' @details
#' It is assumed that the crop series is a continuous series in decreasing order of years. So most recent year first, oldest year last.
#'
#' @import data.table
#'
#'
#' @export
bln_field <- function(B_LU_BRP,B_LU_BBWP,B_SC_WENR,B_GWL_CLASS,B_SOILTYPE_AGR,B_HELP_WENR,B_AER_CBS,
                      B_GWL_GLG,B_GWL_GHG,B_GWL_ZCRIT,B_DRAIN,B_FERT_NORM_FR,B_SLOPE_DEGREE,B_GWP,
                      B_AREA_DROUGHT,B_CT_PSW,B_CT_NSW,B_CT_PSW_MAX =0.5,B_CT_NSW_MAX = 5.0,
                      A_SOM_LOI,A_SOM_LOI_MLMAX = NA_real_, A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_DENSITY_SA,A_FE_OX,A_AL_OX,A_PH_CC,A_N_RT,
                      A_CN_FR,A_S_RT,A_N_PMN,A_P_AL,A_P_CC,A_P_WA,A_P_SG,A_CEC_CO,A_CA_CO_PO,A_MG_CO_PO,
                      A_K_CO_PO,A_K_CC,A_MG_CC,A_MN_CC,A_ZN_CC,A_CU_CC,
                      A_EW_BCS = NA,A_SC_BCS = NA,A_GS_BCS = NA,A_P_BCS = NA,A_C_BCS = NA,
                      A_RT_BCS = NA,A_RD_BCS = NA,A_SS_BCS = NA,A_CC_BCS = NA,
                      D_SA_W,D_RO_R,
                      M_COMPOST = NA_real_,M_GREEN = NA,M_NONBARE = NA,M_EARLYCROP = NA,
                      M_SLEEPHOSE = NA,M_DRAIN = NA,M_DITCH = NA,M_UNDERSEED = NA,
                      M_LIME = NA,M_NONINVTILL = NA,M_SSPM = NA,M_SOLIDMANURE = NA,
                      M_STRAWRESIDUE = NA,M_MECHWEEDS = NA,M_PESTICIDES_DST = NA,
                      B_LSW_ID = NA_character_,LSW = NULL,ID, output ='all'){

# --- step 1. preprocessing input data ----

  # add visual bindings
  i_c_n = i_c_p = i_c_k = i_c_mg = i_c_s = i_c_ph = NULL
  i_p_cr = i_p_se = i_p_ds = i_p_ws = i_p_du = i_p_co = i_p_whc = i_p_as = i_p_wo = i_p_ro = d_p_co = d_p_cec = NULL
  i_b_di = i_b_sf = i_gw_gwr = i_gw_wb = i_gw_ngw = i_clim_osb = i_clim_rothc = i_clim_csat = NULL
  B_N_RT = B_N_RT_SD = i_gw_pest = i_gw_nret = i_gw_nlea = i_sw_nro = i_sw_nret = i_sw_nsw = i_sw_psw = NULL
  B_RO_R = B_RO_R_SD = B_P_CC = B_P_CC_SD = B_P_SG = B_P_SG_SD = B_AL_OX = B_AL_OX_SD = B_FE_OX = B_FE_OX_SD = NULL
  i_nut_n = i_nut_p = i_nut_k = i_nut_nue = . = crop_code = crop_category = value = indicator = NULL
  cat1 = cat2 = crop_cat = weight = cf = value.w = ncat = cf_yr = NULL

  # make internal table
  dt <- data.table(B_LU_BRP = B_LU_BRP,
                   B_SC_WENR = B_SC_WENR,
                   B_GWL_CLASS = B_GWL_CLASS,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_HELP_WENR = B_HELP_WENR,
                   B_AER_CBS = B_AER_CBS,
                   B_GWL_GLG = B_GWL_GLG,
                   B_GWL_GHG = B_GWL_GHG,
                   B_GWL_ZCRIT = B_GWL_ZCRIT,
                   B_DRAIN = B_DRAIN,
                   B_FERT_NORM_FR = B_FERT_NORM_FR,
                   B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                   B_GWP = B_GWP,
                   B_AREA_DROUGHT = B_AREA_DROUGHT,
                   B_CT_PSW = B_CT_PSW,
                   B_CT_NSW = B_CT_NSW,
                   B_CT_PSW_MAX = B_CT_PSW_MAX,
                   B_CT_NSW_MAX = B_CT_NSW_MAX,
                   A_SOM_LOI = A_SOM_LOI,
                   A_SOM_LOI_MLMAX = A_SOM_LOI_MLMAX,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_DENSITY_SA = A_DENSITY_SA,
                   A_FE_OX = A_FE_OX,
                   A_AL_OX = A_AL_OX,
                   A_PH_CC = A_PH_CC,
                   A_N_RT = A_N_RT,
                   A_CN_FR = A_CN_FR,
                   A_S_RT = A_S_RT,
                   A_N_PMN = A_N_PMN,
                   A_P_AL = A_P_AL,
                   A_P_CC = A_P_CC,
                   A_P_WA = A_P_WA,
                   A_P_SG = A_P_SG,
                   A_CEC_CO = A_CEC_CO,
                   A_CA_CO_PO = A_CA_CO_PO,
                   A_MG_CO_PO = A_MG_CO_PO,
                   A_K_CO_PO = A_K_CO_PO,
                   A_K_CC = A_K_CC,
                   A_MG_CC = A_MG_CC,
                   A_MN_CC = A_MN_CC,
                   A_ZN_CC = A_ZN_CC,
                   A_CU_CC = A_CU_CC,
                   A_EW_BCS = A_EW_BCS,A_SC_BCS = A_SC_BCS,A_GS_BCS = A_GS_BCS,
                   A_P_BCS = A_P_BCS,A_C_BCS = A_C_BCS,
                   A_RT_BCS = A_RT_BCS,A_RD_BCS = A_RD_BCS,A_SS_BCS = A_SS_BCS,A_CC_BCS = A_CC_BCS,
                   D_SA_W = D_SA_W,
                   D_RO_R = D_RO_R,
                   M_COMPOST = M_COMPOST,M_GREEN = M_GREEN,M_NONBARE = M_NONBARE,M_EARLYCROP = M_EARLYCROP,
                   M_SLEEPHOSE = M_SLEEPHOSE,M_DRAIN = M_DRAIN,M_DITCH = M_DITCH,M_UNDERSEED = M_UNDERSEED,
                   M_LIME = M_LIME,M_NONINVTILL = M_NONINVTILL,M_SSPM = M_SSPM,M_SOLIDMANURE = M_SOLIDMANURE,
                   M_STRAWRESIDUE = M_STRAWRESIDUE,M_MECHWEEDS = M_MECHWEEDS,M_PESTICIDES_DST = M_PESTICIDES_DST,
                   B_LSW_ID = B_LSW_ID,LSW = LSW,ID = ID)

  # check formats B_SC_WENR and B_GWL_CLASS
  #dt[, B_SC_WENR := OBIC::format_soilcompaction(B_SC_WENR)]
  dt[, B_GWL_CLASS := OBIC::format_gwt(B_GWL_CLASS)]
  dt[, B_AER_CBS := OBIC::format_aer(B_AER_CBS)]

  # estimate missing data
  dt[is.na(A_DENSITY_SA), A_DENSITY_SA := calc_bulk_density(B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI)]

  # add management when input is missing
  cols <- c('M_GREEN', 'M_NONBARE', 'M_EARLYCROP','M_COMPOST','M_SLEEPHOSE','M_DRAIN','M_DITCH','M_UNDERSEED',
            'M_LIME', 'M_NONINVTILL', 'M_SSPM', 'M_SOLIDMANURE','M_STRAWRESIDUE','M_MECHWEEDS','M_PESTICIDES_DST')
  dt[, c(cols) := OBIC::add_management(ID,B_LU_BRP, B_SOILTYPE_AGR,
                                 M_GREEN, M_NONBARE, M_EARLYCROP,M_COMPOST,M_SLEEPHOSE,M_DRAIN,M_DITCH,M_UNDERSEED,
                                 M_LIME, M_NONINVTILL, M_SSPM, M_SOLIDMANURE,M_STRAWRESIDUE,M_MECHWEEDS,M_PESTICIDES_DST)]

  # add year, assuming that first year is the most recent ones
  dt[,year := 1:.N,by=ID]

  # add LSW properties if missing
  if(is.null(LSW)){

    LSW <- BLN::bln_lsw
    dt[,B_LSW_ID := 'lsw_nlmean']

  } else {

    # desired column names in LSW
    cols <- c("B_LSW_ID","B_SOM_LOI","B_CLAY_MI","B_SAND_MI","B_SILT_MI","B_N_RT","B_P_AL","B_P_CC","B_P_WA","B_P_SG",
              "B_FE_OX","B_AL_OX","B_SA_W","B_RO_R","B_SOM_LOI_SD", "B_CLAY_MI_SD", "B_SAND_MI_SD", "B_SILT_MI_SD", "B_N_RT_SD","B_P_AL_SD","B_P_CC_SD",
              "B_P_WA_SD","B_P_SG_SD","B_FE_OX_SD","B_AL_OX_SD","B_SA_W_SD","B_RO_R_SD")

    # check LSW format and column names
    checkmate::assert_data_table(LSW,nrow = length(unique(B_LSW_ID)))
    checkmate::assert_subset(colnames(LSW),choices = cols)

    # check if all B_LSW_ID are in the LSW data.table
    checkmate::assert_subset(LSW$B_LSW_ID,choices = unique(B_LSW_ID))

  }

  # set internal data.table
  dt <- merge(dt, LSW, by = 'B_LSW_ID',all.x = TRUE)

# --- step 2. calculate BLN indicators ----

  # calculate BLN crop production indicators

    # Calculate indicators for soil chemical functions
    dt[, i_c_n := bln_c_nitrogen(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, A_N_RT)]
    dt[, i_c_p := bln_c_posphor(B_LU_BRP, A_P_AL, A_P_CC, A_P_WA)]
    dt[, i_c_k := bln_c_potassium(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_PH_CC, A_CEC_CO, A_K_CO_PO, A_K_CC)]
    dt[, i_c_mg := bln_c_magnesium(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_PH_CC,A_CEC_CO, A_K_CO_PO, A_MG_CC, A_K_CC)]
    dt[, i_c_s := bln_c_sulfur(B_LU_BRP, B_SOILTYPE_AGR, B_AER_CBS, A_SOM_LOI, A_S_RT)]
    dt[, i_c_ph := bln_c_ph(ID,B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_PH_CC),by=ID]

    # Calculate indicators for soil physical functions
    dt[, i_p_cr := bln_p_crumbleability(A_SOM_LOI, A_CLAY_MI, A_PH_CC)]
    dt[, i_p_se := bln_p_sealing(A_SOM_LOI, A_CLAY_MI)]
    dt[, i_p_ds := bln_p_droughtstress(B_HELP_WENR, B_LU_BRP, B_GWL_CLASS, WSI = "droughtstress")]
    dt[, i_p_ws := bln_p_wetnessstress(B_HELP_WENR, B_LU_BRP, B_GWL_CLASS, WSI = "wetnessstress")]
    dt[, i_p_du := bln_p_windererosion(B_LU_BRP, A_CLAY_MI, A_SILT_MI)]
    dt[, i_p_co := bln_p_compaction(B_SC_WENR)]
    dt[, i_p_whc := bln_p_whc(A_CLAY_MI, A_SAND_MI, A_SILT_MI, A_SOM_LOI, type = "whc")]
    dt[, i_p_as := bln_p_aggstability(B_SOILTYPE_AGR, A_SOM_LOI, A_K_CO_PO, A_CA_CO_PO, A_MG_CO_PO)]
    dt[, i_p_wo := bln_p_workability(A_CLAY_MI, A_SILT_MI, B_LU_BRP, B_SOILTYPE_AGR, B_GWL_GLG,B_GWL_GHG, B_GWL_ZCRIT)]
    dt[, i_p_ro := bln_p_density(A_SOM_LOI, A_CLAY_MI,A_DENSITY_SA)]

    # correction for soil physical functions when data are there from BCS
    dt[,d_p_co := pmax(0,(3 * A_EW_BCS + 3 * A_SC_BCS + 3 * A_RD_BCS  - 2 * A_P_BCS - A_RT_BCS)/18)]
    dt[,i_p_co := fifelse(is.na(d_p_co),i_p_co,d_p_co)]
    dt[,d_p_cec := pmax(0, (3 * A_EW_BCS + 3 * A_SS_BCS - A_C_BCS)/12,0)]
    dt[,i_p_as := fifelse(is.na(d_p_cec),i_p_as,d_p_cec)]

    # Calculate indicators for soil biological functions
    dt[, i_b_di := bln_b_diseaseresistance(A_SOM_LOI)]
    dt[, i_b_sf := bln_b_pmn(B_LU_BRP, B_SOILTYPE_AGR, A_N_PMN)]

  # calculate BLN regulation and purification of water (groundwater)

    # ground water quantity and quality: recharge (I_H_GWR, from OBI)
    dt[, i_gw_gwr := bln_wat_groundwater_recharge(ID,B_LU_BRP, B_SC_WENR, B_GWL_CLASS,
                                                 B_DRAIN, A_CLAY_MI,A_SAND_MI, A_SILT_MI, A_SOM_LOI, M_GREEN)]

    # groundwater quantity and quality: water retention (S_BBWP_WB, from BBWP)
    dt[,i_gw_wb := bln_bbwp_bw(ID,B_LU_BRP, B_HELP_WENR, B_GWL_CLASS, B_AREA_DROUGHT,
                               A_CLAY_MI, A_SAND_MI, A_SILT_MI, A_SOM_LOI)]

    # ground water quantity and quality: N buffering (S_BBWP_NGW, from BBWP)
    dt[, i_gw_ngw := bln_bbwp_ngw(ID,B_LU_BRP, B_SOILTYPE_AGR, B_SC_WENR,
                                  B_AER_CBS, B_GWP,B_GWL_CLASS, A_SOM_LOI, A_N_RT,
                                  B_N_RT, B_N_RT_SD,penalty = TRUE)]

    # groundwater quantity and qality: pesticide leaching (I_H_PEST,from OBI)
    dt[, i_gw_pest := bln_wat_pesticide(ID,B_LU_BRP, B_SOILTYPE_AGR, A_CLAY_MI, A_SAND_MI, A_SILT_MI,
                                        A_SOM_LOI, M_GREEN, M_MECHWEEDS, M_PESTICIDES_DST)]

    # groundwater quantity and qality: nitrogen retention (I_E_NGW, from OBI)
    dt[, i_gw_nret := bln_wat_nretention_gw(ID, B_LU_BRP, B_SOILTYPE_AGR, B_AER_CBS, B_GWL_CLASS, A_SOM_LOI, A_N_RT)]

    # groundwater quantity and quality: nitrogen leaching (I_H_NGW, from OBI). M_GREEN FALSE (YF: otherwise too strong impact)
    dt[, i_gw_nlea := bln_wat_nrisk_gw(ID,B_LU_BRP,B_SOILTYPE_AGR,B_AER_CBS,B_GWL_CLASS,B_SC_WENR,B_FERT_NORM_FR = 1,
                                       A_CLAY_MI,A_SAND_MI, A_SILT_MI, A_SOM_LOI,A_P_AL, A_P_WA, A_P_CC,
                                       A_PH_CC, A_CEC_CO,A_K_CO_PO, A_K_CC,
                                       M_GREEN)]

  # calculate BLN regulation and purification of water (surface water)

    # surface water quality: nitrogen runoff risk (I_H_NSW, from OBI) M_GREEN FALSE (YF: otherwise too strong impact)
    dt[, i_sw_nro := bln_wat_nrunoff(ID,B_LU_BRP, B_SC_WENR, B_GWL_CLASS, B_AER_CBS, B_DRAIN,
                                     B_FERT_NORM_FR = 1, A_CLAY_MI, A_SAND_MI, A_SILT_MI, A_SOM_LOI,
                                     A_P_AL, A_P_WA, A_P_CC, A_PH_CC, A_CEC_CO, A_K_CO_PO, A_K_CC,
                                     M_GREEN = FALSE)]

    # surface water quality: nitrogen retention (I_E_NSW, from OBI)
    dt[, i_sw_nret := bln_wat_nretention_sw(B_LU_BRP, B_SOILTYPE_AGR, B_AER_CBS, B_GWL_CLASS,
                                            A_SOM_LOI, A_N_RT)]

    # surface water quality: nitrogen buffering (S_BBWP_NSW, from BBWP)
    dt[, i_sw_nsw := bln_bbwp_nsw(ID,B_LU_BRP,B_SOILTYPE_AGR,B_SC_WENR,B_AER_CBS,B_GWL_CLASS,B_SLOPE_DEGREE,
                                  A_SOM_LOI,A_N_RT,D_RO_R, D_SA_W,
                                  B_N_RT,B_RO_R,B_RO_R_SD,B_N_RT_SD,B_CT_NSW, B_CT_NSW_MAX)]

    # surface water quality: phosphate buffering (S_BBWP_PSW, from BBWP)
    dt[, i_sw_psw := bln_bbwp_psw(ID,B_LU_BRP,B_SC_WENR,B_AER_CBS,B_GWL_CLASS,B_SLOPE_DEGREE,
                                  A_P_CC, A_P_SG, A_AL_OX, A_FE_OX,D_RO_R, D_SA_W,
                                  B_RO_R,B_RO_R_SD,B_P_CC, B_P_CC_SD, B_P_SG, B_P_SG_SD,
                                  B_AL_OX, B_AL_OX_SD, B_FE_OX, B_FE_OX_SD,
                                  B_CT_PSW, B_CT_PSW_MAX)]

# calculate BLN climate and carbon sequestration

    # estimate the C balance via simple mass balance (OBIC)
    dt[,i_clim_osb := bln_clim_cbalance(ID,B_LU_BRP,A_SOM_LOI,A_P_AL,A_P_WA,M_COMPOST,M_GREEN)]

    # estimate the C saturation via RothC
    dt[,i_clim_rothc := bln_clim_rothc(ID, B_LU_BRP, B_GWL_GLG,A_SOM_LOI, A_CLAY_MI,quiet = TRUE)]

    # estimate the C saturation via ML model
    dt[,i_clim_csat := bln_clim_csat(B_LU_BRP,A_SOM_LOI,A_SOM_LOI_MLMAX)]

# calculate BLN indicators for nutrient clycing

    # nutrient use effiency for soil nitrogen (evalation soil N supply, OBIC)
    dt[,i_nut_n := bln_nut_nitrogen(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, A_N_RT)]

    # nutrient use effiency for soil phosphorus
    dt[,i_nut_p := bln_nut_phosphorus(B_LU_BRP, A_P_AL, A_P_CC, A_P_WA)]

    # nutrient use effiency for soil potassium
    dt[,i_nut_k := bln_nut_potassium(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_PH_CC,
                                     A_CEC_CO, A_K_CO_PO, A_K_CC)]

    # nutrient use effiency for soil N and P given water availability (S_BBWP_NUE from BBWP)
    dt[,i_nut_nue := bln_nut_nue(B_LU_BRP, B_HELP_WENR, B_GWL_CLASS, A_P_AL, A_P_CC,
                                 A_P_WA, A_N_RT, B_N_RT, B_N_RT_SD)]

# --- step 3. Aggregate BLN indicators ----

    # add crop category
    dt <- merge(dt,
                OBIC::crops.obic[,.(crop_code,crop_category)],
                                 by.x='B_LU_BRP',by.y='crop_code',all.x=TRUE)

    # make a data.table for indicators that are not relevant for aggregation
    # if nature make all indicators not relevant
    w <- data.table(crop_cat = c('grasland','grasland'),
                    indicator = c('i_p_du','i_gw_pest'),
                    weight = rep(-1,2))

    # Select all indicators used for scoring, only for I_C, I_P, I_B and I_B
    cols <- colnames(dt)[grepl('^ID$|^i_c|^i_p|^i_b|^i_gw|^i_sw|^i_nut|^i_clim|year|crop_cat',colnames(dt))]

    # Melt dt and assign main categories for OBI
    dt.melt <- melt(dt[,mget(cols)],
                    id.vars = c('year', 'ID','crop_category'),
                    variable.name = 'indicator')

    # remove the indicators that have a NA value
    dt.melt <- dt.melt[!is.na(value)]

    # add main categories relevant for aggregating
    dt.melt[grepl('^i_c|^i_p|^i_b',indicator), cat1 := 'esd_prod']
    dt.melt[grepl('^i_nut',indicator), cat1 := 'esd_nutcycle']
    dt.melt[grepl('^i_gw|^i_sw',indicator), cat1 := 'esd_water']
    dt.melt[grepl('^i_clim',indicator), cat1 := 'esd_climate']

    # add sub categories relevant for aggregating
    dt.melt[grepl('^i_c',indicator), cat2 := 'chemistry']
    dt.melt[grepl('^i_p',indicator), cat2 := 'physics']
    dt.melt[grepl('^i_b',indicator), cat2 := 'biology']
    dt.melt[grepl('^i_nut',indicator), cat2 := 'macronutrient']
    dt.melt[grepl('^i_gw',indicator), cat2 := 'gw_quantity']
    dt.melt[grepl('i_gw_ngw|i_gw_pest|i_gw_nret|i_gw_nlea ',indicator), cat2 := 'gw_quality']
    dt.melt[grepl('^i_sw',indicator), cat2 := 'sw_quality']
    dt.melt[grepl('^i_clim',indicator), cat2 := 'climate']

    # Determine amount of indicators per (sub)category
    dt.melt.ncat <- dt.melt[year==1][,list(ncat = .N),by = .(ID, cat1,cat2)]

    # add weighing factor to indicator values
    dt.melt <- merge(dt.melt,
                     w[,list(crop_category = crop_cat,indicator,weight)],
                     by = c('crop_category','indicator'), all.x = TRUE)

    # calculate correction factor for indicator values (low values have more impact than high values, a factor 5)
    dt.melt[,cf := OBIC::cf_ind_importance(value)]

    # calculate weighted value for crop category
    dt.melt[,value.w := value]
    dt.melt[weight < 0,value.w := -999]

    # subset dt.melt for relevant columns only
    out.score <-  dt.melt[,list(ID, cat1,cat2, year, cf, value = value.w)]

      # calculate weighted average per indicator category per year
      out.score <- out.score[,list(value = sum(cf * pmax(0,value) / sum(cf[value >= 0]))),by = list(ID, cat2,year)]

      # for case that a cat has one indicator or one year and has NA
      out.score[is.na(value), value := -999]

      # calculate correction factor per year; recent years are more important
      out.score[,cf := log(12 - pmin(10,year))]

      # calculate weighted average per indicator category per year
      out.score <- out.score[,list(value = sum(cf * pmax(0,value)/ sum(cf[value >= 0]))), by = list(ID, cat2)]

      # merge out with number per category
      out.score <- merge(out.score,dt.melt.ncat, by=c("ID","cat2"),all.x=TRUE)

      # calculate weighing factor depending on number of indicators
      out.score[,cf := log(ncat + 1)]

      # estimate mean BLN score per ESD
      out.score <- out.score[,list(value = sum(value * cf / sum(cf[value >= 0]))),by= c('ID','cat1')]

      # count number of indicators per cat1
      dt.melt.ncat <- dt.melt[year==1][,list(ncat = .N),by = .(ID, cat1)]

      # merge out with number per category
      out.score <- merge(out.score,dt.melt.ncat, by=c("ID","cat1"),all.x=TRUE)

      # calculate weighing factor depending on number of indicators
      out.score[,cf := log(ncat + 1)]

      # calculate total score over all categories
      out.score.total.bln <- out.score[,list(value = round(sum(value * cf / sum(cf[value >= 0])),3),cat1='bln_total'),by= c('ID')]

      # round at two numbers
      out.score[, value := round(value,3)]

      # combine with BLN score per ESD
      out.score <- rbind(out.score[,.(ID,cat1,value)],out.score.total.bln[,.(ID,cat1,value)])

      # reformat to one line per field
      out.score[value== -999, value := NA]
      out.score <- dcast(out.score,ID~cat1,value.var='value')

    # subset dt.melt for relevant columns only
    out.ind <-  dt.melt[,list(ID, indicator,year,cf,value = value.w)]

    # add cf factor for the year; recent years are more important
    out.ind[,cf_yr := log(12 - pmin(10,year))]

    # calculate weighted average per indicator category per year
    out.ind <- out.ind[,list(value = sum(cf * cf_yr * pmax(0,value) / sum(cf[value >= 0] * cf_yr[value >= 0]))),by = list(ID, indicator)]

    # round at two numbers
    out.ind[, value := round(value,3)]

    # reformat to one line per field
    out.ind[value== -999, value := NA]
    out.ind <- dcast(out.ind,ID~indicator,value.var='value')

  # prepare output, with default
  if(output == 'all') {out <- merge(out.ind,out.score,by='ID',all.x=TRUE)}
  if(output == 'score'){out <- copy(out.score)}
  if(output == 'indicator'){out <- copy(out.ind)}

  # return output
  return(out)

}

# manual testing

    # dt.farm <- BLN::bln_farm_hf
    # ID = dt.farm$ref_id_2022
    # B_LU_BRP = dt.farm$B_LU_BRP
    # B_SC_WENR = dt.farm$B_SC_WENR
    # B_GWL_CLASS = dt.farm$B_GWL_CLASS
    # B_SOILTYPE_AGR = dt.farm$B_SOILTYPE_AGR
    # B_HELP_WENR = dt.farm$B_HELP_WENR
    # B_AER_CBS = dt.farm$B_AER_CBS
    # B_GWL_GLG = dt.farm$B_GWL_GLG
    # B_GWL_GHG = dt.farm$B_GWL_GHG
    # B_GWL_ZCRIT = dt.farm$B_GWL_ZCRIT
    # B_DRAIN = dt.farm$B_DRAIN
    # B_FERT_NORM_FR = dt.farm$B_FERT_NORM_FR
    # B_SLOPE_DEGREE = dt.farm$B_SLOPE_DEGREE
    # B_GWP = dt.farm$B_GWP
    # B_AREA_DROUGHT = dt.farm$B_AREA_DROUGHT
    # B_CT_PSW = dt.farm$B_CT_PSW
    # B_CT_NSW = dt.farm$B_CT_NSW
    # B_CT_PSW_MAX =0.5
    # B_CT_NSW_MAX = 5.0
    # A_SOM_LOI = dt.farm$A_SOM_LOI
    # A_SOM_LOI_MLMAX = dt.farm$a_som_loi_csat_top
    # A_CLAY_MI = dt.farm$A_CLAY_MI
    # A_SAND_MI = dt.farm$A_SAND_MI
    # A_SILT_MI = dt.farm$A_SILT_MI
    # A_DENSITY_SA = NA_real_
    # A_FE_OX = dt.farm$A_FE_OX
    # A_AL_OX = dt.farm$A_AL_OX
    # A_PH_CC = dt.farm$A_PH_CC
    # A_N_RT = dt.farm$A_N_RT
    # A_CN_FR = dt.farm$A_CN_FR
    # A_S_RT = dt.farm$A_S_RT
    # A_N_PMN = dt.farm$A_N_PMN
    # A_P_AL = dt.farm$A_P_AL
    # A_P_CC = dt.farm$A_P_CC
    # A_P_WA = dt.farm$A_P_WA
    # A_P_SG = dt.farm$A_P_SG
    # A_CEC_CO = dt.farm$A_CEC_CO
    # A_CA_CO_PO = dt.farm$A_CA_CO_PO
    # A_MG_CO_PO = dt.farm$A_MG_CO_PO
    # A_K_CO_PO = dt.farm$A_K_CO_PO
    # A_K_CC = dt.farm$A_K_CC
    # A_MG_CC = dt.farm$A_MG_CC
    # A_MN_CC = dt.farm$A_MN_CC
    # A_ZN_CC = dt.farm$A_ZN_CC
    # A_CU_CC = dt.farm$A_CU_CC
    # A_EW_BCS = NA
    # A_SC_BCS = NA
    # A_GS_BCS = NA
    # A_P_BCS = NA
    # A_C_BCS = NA
    # A_RT_BCS = NA
    # A_RD_BCS = NA
    # A_SS_BCS = NA
    # A_CC_BCS = NA
    # D_SA_W = dt.farm$D_SA_W
    # D_RO_R = dt.farm$D_RO_R
    # M_COMPOST = NA_real_
    # M_GREEN = NA
    # M_NONBARE = NA
    # M_EARLYCROP = NA
    # M_SLEEPHOSE = NA
    # M_DRAIN = NA
    # M_DITCH = NA
    # M_UNDERSEED = NA
    # M_LIME = NA
    # M_NONINVTILL = NA
    # M_SSPM = NA
    # M_SOLIDMANURE = NA
    # M_STRAWRESIDUE = NA
    # M_MECHWEEDS = NA
    # M_PESTICIDES_DST = NA
    # B_LSW_ID = NA_character_
    # LSW = NULL
    # output ='all'

# unit test
test_that("bln_field works", {

  # select properties
  dt.farm <- BLN::bln_farm_hf

  # sselect five fields
  dt.farm <- dt.farm[id <6]

  # run BLN
  d1 <- bln_field(ID = dt.farm$ref_id_2022,
                  B_LU_BRP = dt.farm$B_LU_BRP,
                  B_SC_WENR = dt.farm$B_SC_WENR,
                  B_GWL_CLASS = dt.farm$B_GWL_CLASS,
                  B_SOILTYPE_AGR = dt.farm$B_SOILTYPE_AGR,
                  B_HELP_WENR = dt.farm$B_HELP_WENR,
                  B_AER_CBS = dt.farm$B_AER_CBS,
                  B_GWL_GLG = dt.farm$B_GWL_GLG,
                  B_GWL_GHG = dt.farm$B_GWL_GHG,
                  B_GWL_ZCRIT = dt.farm$B_GWL_ZCRIT,
                  B_DRAIN = dt.farm$B_DRAIN,
                  B_FERT_NORM_FR = dt.farm$B_FERT_NORM_FR,
                  B_SLOPE_DEGREE = dt.farm$B_SLOPE_DEGREE,
                  B_GWP = dt.farm$B_GWP,
                  B_AREA_DROUGHT = dt.farm$B_AREA_DROUGHT,
                  B_CT_PSW = dt.farm$B_CT_PSW,
                  B_CT_NSW = dt.farm$B_CT_NSW,
                  B_CT_PSW_MAX =0.5,
                  B_CT_NSW_MAX = 5.0,
                  B_SOMERS_BC = NA_real_,
                  B_DRAIN_SP = NA_real_,
                  B_DRAIN_WP = NA_real_,
                  A_SOM_LOI = dt.farm$A_SOM_LOI,
                  A_SOM_LOI_MLMAX = dt.farm$a_som_loi_csat_top,
                  A_CLAY_MI = dt.farm$A_CLAY_MI,
                  A_SAND_MI = dt.farm$A_SAND_MI,
                  A_SILT_MI = dt.farm$A_SILT_MI,
                  A_DENSITY_SA = NA_real_,
                  A_FE_OX = dt.farm$A_FE_OX,
                  A_AL_OX = dt.farm$A_AL_OX,
                  A_PH_CC = dt.farm$A_PH_CC,
                  A_N_RT = dt.farm$A_N_RT,
                  A_CN_FR = dt.farm$A_CN_FR,
                  A_S_RT = dt.farm$A_S_RT,
                  A_N_PMN = dt.farm$A_N_PMN,
                  A_P_AL = dt.farm$A_P_AL,
                  A_P_CC = dt.farm$A_P_CC,
                  A_P_WA = dt.farm$A_P_WA,
                  A_P_SG = dt.farm$A_P_SG,
                  A_CEC_CO = dt.farm$A_CEC_CO,
                  A_CA_CO_PO = dt.farm$A_CA_CO_PO,
                  A_MG_CO_PO = dt.farm$A_MG_CO_PO,
                  A_K_CO_PO = dt.farm$A_K_CO_PO,
                  A_K_CC = dt.farm$A_K_CC,
                  A_MG_CC = dt.farm$A_MG_CC,
                  A_MN_CC = dt.farm$A_MN_CC,
                  A_ZN_CC = dt.farm$A_ZN_CC,
                  A_CU_CC = dt.farm$A_CU_CC,
                  A_EW_BCS = NA,A_SC_BCS = NA,A_GS_BCS = NA,A_P_BCS = NA,A_C_BCS = NA,
                  A_RT_BCS = NA,A_RD_BCS = NA,A_SS_BCS = NA,A_CC_BCS = NA,
                  D_SA_W = dt.farm$D_SA_W,
                  D_RO_R = dt.farm$D_RO_R,
                  M_COMPOST = NA_real_,M_GREEN = NA,M_NONBARE = NA,M_EARLYCROP = NA,
                  M_SLEEPHOSE = NA,M_DRAIN = NA,M_DITCH = NA,M_UNDERSEED = NA,
                  M_LIME = NA,M_NONINVTILL = NA,M_SSPM = NA,M_SOLIDMANURE = NA,
                  M_STRAWRESIDUE = NA,M_MECHWEEDS = NA,M_PESTICIDES_DST = NA,
                  B_LSW_ID = NA_character_,LSW = NULL, output ='all',
                  runrothc = TRUE,
                  mc = FALSE)

  # test for dimensions dataset
  expect_equal(dim(d1), expected = c(5,49), tolerance = 0.1 )

  # test for colnames
  cols <- c("ID","i_b_di","i_b_sf","i_c_k","i_c_mg","i_c_n","i_c_p","i_c_ph","i_c_s","i_clim_csat"  ,"i_clim_osb","i_clim_rothc", "i_gw_gwr","i_gw_ngw","i_gw_nlea","i_gw_nret","i_gw_pest",
            "i_gw_wb","i_nut_k","i_nut_n","i_nut_nue","i_nut_p","i_p_as","i_p_co","i_p_cr","i_p_ds","i_p_du","i_p_ro","i_p_se","i_p_whc","i_p_wo","i_p_ws","i_sw_nret","i_sw_nro",
            "i_sw_nsw","i_sw_psw",'s_bln_esd_clim','s_bln_esd_nut', 's_bln_esd_prod', 's_bln_esd_water', 's_bln_prod_b', 's_bln_prod_c', 's_bln_clim',
            's_bln_gw_quality', 's_bln_gw_quantity', 's_bln_nut', 's_bln_prod_p', 's_bln_sw_quality', 's_bln_total')
  expect_equal(colnames(d1), expected = cols, tolerance = 0.1 )

  # test BLN score
  expect_equal(d1$s_bln_total, expected = c(0.74,0.74,0.72,0.61,0.67), tolerance = 0.01)

  # test BLN soil quality score ESD production
  expect_equal(d1$s_bln_esd_prod, expected = c(0.8,0.8,0.82,0.8,0.79), tolerance = 0.01)

  # test BLN soil quality score ESD water quality
  expect_equal(d1$s_bln_esd_water, expected = c(0.69,0.65,0.53,0.33,0.46), tolerance = 0.01)

  # test BLN soil quality score ESD climate
  expect_equal(d1$s_bln_esd_clim, expected = c(0.88,0.88,0.87,0.73,0.83), tolerance = 0.01)

  # test BLN soil quality score ESD nutrient cycle
  expect_equal(d1$s_bln_esd_nut, expected = c(0.57,0.64,0.67,0.56,0.63), tolerance = 0.01)

})

test_that('All GWL classes known to BLN are accepted by all functions.', {
  # select properties
  dt.farm <- BLN::bln_farm_hf

  # repeat 1 field once for each value of B_GWL_CLASS
  dt.farm <- rbindlist(rep(list(dt.farm[1]), times = length(
    unlist(BLN::bln_parms[code == "B_GWL_CLASS", choices])
  )))
  dt.farm[, B_GWL_CLASS := unlist(BLN::bln_parms[code == "B_GWL_CLASS", choices])]


  expect_no_error(
    bln_field(ID = dt.farm$ref_id_2022,
              B_LU_BRP = dt.farm$B_LU_BRP,
              B_SC_WENR = dt.farm$B_SC_WENR,
              B_GWL_CLASS = dt.farm$B_GWL_CLASS,
              B_SOILTYPE_AGR = dt.farm$B_SOILTYPE_AGR,
              B_HELP_WENR = dt.farm$B_HELP_WENR,
              B_AER_CBS = dt.farm$B_AER_CBS,
              B_GWL_GLG = dt.farm$B_GWL_GLG,
              B_GWL_GHG = dt.farm$B_GWL_GHG,
              B_GWL_ZCRIT = dt.farm$B_GWL_ZCRIT,
              B_DRAIN = dt.farm$B_DRAIN,
              B_FERT_NORM_FR = dt.farm$B_FERT_NORM_FR,
              B_SLOPE_DEGREE = dt.farm$B_SLOPE_DEGREE,
              B_GWP = dt.farm$B_GWP,
              B_AREA_DROUGHT = dt.farm$B_AREA_DROUGHT,
              B_CT_PSW = dt.farm$B_CT_PSW,
              B_CT_NSW = dt.farm$B_CT_NSW,
              B_CT_PSW_MAX =0.5,
              B_CT_NSW_MAX = 5.0,
              B_SOMERS_BC = NA_real_,
              B_DRAIN_SP = NA_real_,
              B_DRAIN_WP = NA_real_,
              A_SOM_LOI = dt.farm$A_SOM_LOI,
              A_SOM_LOI_MLMAX = dt.farm$a_som_loi_csat_top,
              A_CLAY_MI = dt.farm$A_CLAY_MI,
              A_SAND_MI = dt.farm$A_SAND_MI,
              A_SILT_MI = dt.farm$A_SILT_MI,
              A_DENSITY_SA = NA_real_,
              A_FE_OX = dt.farm$A_FE_OX,
              A_AL_OX = dt.farm$A_AL_OX,
              A_PH_CC = dt.farm$A_PH_CC,
              A_N_RT = dt.farm$A_N_RT,
              A_CN_FR = dt.farm$A_CN_FR,
              A_S_RT = dt.farm$A_S_RT,
              A_N_PMN = dt.farm$A_N_PMN,
              A_P_AL = dt.farm$A_P_AL,
              A_P_CC = dt.farm$A_P_CC,
              A_P_WA = dt.farm$A_P_WA,
              A_P_SG = dt.farm$A_P_SG,
              A_CEC_CO = dt.farm$A_CEC_CO,
              A_CA_CO_PO = dt.farm$A_CA_CO_PO,
              A_MG_CO_PO = dt.farm$A_MG_CO_PO,
              A_K_CO_PO = dt.farm$A_K_CO_PO,
              A_K_CC = dt.farm$A_K_CC,
              A_MG_CC = dt.farm$A_MG_CC,
              A_MN_CC = dt.farm$A_MN_CC,
              A_ZN_CC = dt.farm$A_ZN_CC,
              A_CU_CC = dt.farm$A_CU_CC,
              A_EW_BCS = NA,A_SC_BCS = NA,A_GS_BCS = NA,A_P_BCS = NA,A_C_BCS = NA,
              A_RT_BCS = NA,A_RD_BCS = NA,A_SS_BCS = NA,A_CC_BCS = NA,
              D_SA_W = dt.farm$D_SA_W,
              D_RO_R = dt.farm$D_RO_R,
              M_COMPOST = NA_real_,M_GREEN = NA,M_NONBARE = NA,M_EARLYCROP = NA,
              M_SLEEPHOSE = NA,M_DRAIN = NA,M_DITCH = NA,M_UNDERSEED = NA,
              M_LIME = NA,M_NONINVTILL = NA,M_SSPM = NA,M_SOLIDMANURE = NA,
              M_STRAWRESIDUE = NA,M_MECHWEEDS = NA,M_PESTICIDES_DST = NA,
              B_LSW_ID = NA_character_,LSW = NULL, output ='all',
              runrothc = TRUE,
              mc = FALSE)
  )
})

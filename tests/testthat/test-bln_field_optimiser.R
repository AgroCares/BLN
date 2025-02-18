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
    # B_SOMERS_BC = NA_real_
    # B_DRAIN_SP = NA_real_
    # B_DRAIN_WP = NA_real_
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
    # i_clim_rothc = NA_real_
    # LSW = NULL
    # output ='all'
    # foptim = list(scenarios = NULL, b_lu_brp = NULL, outputtype = 'rotation',mc = TRUE,runrothc = TRUE)

    # unit test
test_that("bln_field works", {

  # select properties
  dt.farm <- BLN::bln_farm_hf

  # sselect five fields
  dt.farm <- dt.farm[id ==1]

  # run BLN
  d1 <- bln_field_optimiser(ID = dt.farm$ref_id_2022,
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
                            D_SA_W = dt.farm$D_SA_W,
                            D_RO_R = dt.farm$D_RO_R,
                            M_COMPOST = NA_real_,M_GREEN = NA,
                            i_clim_rothc = NA_real_,
                            B_LSW_ID = NA_character_,
                            LSW = NULL,
                            foptim = list(scenarios = NULL, b_lu_brp = NULL, outputtype = 'scores',mc = TRUE,runrothc = TRUE))

  # test for dimensions dataset
  expect_equal(dim(d1), expected = c(1,157), tolerance = 0.1 )

  # test for colnames
  cols <- c("ID","bld_arable_int_s_esd_nut_hs","bld_arable_prot_s_esd_clim_hs",
            "bld_int_s_bln_prod_c_hs","current_s_bln_prod_p_hs","sms_permanent_s_esd_nut_hs")
  expect_equal(colnames(d1)[c(1,25,37,85,125,155)], expected = cols, tolerance = 0.1 )

  # test BLN score
  expect_equal(d1$bld_arable_int_s_bln_total_hs, expected = c(0.66), tolerance = 0.01)

  # test BLN soil quality score ESD production
  expect_equal(d1$bld_gld_collaboration_s_bln_total_hs, expected = 0.65, tolerance = 0.01)


  # run BLN
  d1 <- bln_field_optimiser(ID = dt.farm$ref_id_2022,
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
                            D_SA_W = dt.farm$D_SA_W,
                            D_RO_R = dt.farm$D_RO_R,
                            M_COMPOST = NA_real_,M_GREEN = NA,
                            i_clim_rothc = NA_real_,
                            B_LSW_ID = NA_character_,
                            LSW = NULL,
                            foptim = list(scenarios = NULL, b_lu_brp = NULL, outputtype = 'rotation',mc = TRUE,runrothc = TRUE))

  # test for dimensions dataset
  expect_equal(dim(d1), expected = c(1,14), tolerance = 0.1 )

  # test for colnames
  cols <- c("ID",  "s_bln_clim_blu","s_bln_esd_clim_blu","s_bln_esd_nut_blu",
            "s_bln_esd_prod_blu","s_bln_esd_water_blu","s_bln_gw_quality_blu" , "s_bln_gw_quantity_blu",
            "s_bln_nut_blu","s_bln_prod_b_blu","s_bln_prod_c_blu","s_bln_prod_p_blu",
            "s_bln_sw_quality_blu" , "s_bln_total_blu"  )
  expect_equal(colnames(d1), expected = cols, tolerance = 0.1 )

  # test BLN score
  expect_equal(d1$s_bln_prod_c_blu , expected = c('sms_permanent'), tolerance = 0.01)


})

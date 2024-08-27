test_that("bln_bbwp_psw works", {

  # test for potato crop on sandy soil
  expect_equal(
    bln_bbwp_psw(
      ID = 15,
      B_LU_BRP = 3732,
      B_SC_WENR = 902,
      B_AER_CBS = 'LG14',
      B_GWL_CLASS = 'GtVI',
      B_SLOPE_DEGREE = 2.5,
      A_FE_OX = 500,
      A_AL_OX = 150,
      A_P_CC = 5,
      A_P_SG = 25,
      B_CT_PSW = 0.25,
      D_RO_R = 0.8,
      D_SA_W = 0.8,
      penalty = TRUE
    ),
    expected = c(0.357),
    tolerance = 0.1
  )

  # test for potato on peat soil
  expect_equal(
    bln_bbwp_psw(
      ID = 15,
      B_LU_BRP = 265,
      B_SC_WENR = 902,
      B_AER_CBS = 'LG14',
      B_GWL_CLASS = 'GtII',
      B_SLOPE_DEGREE = 2.5,
      A_FE_OX = 500,
      A_AL_OX = 150,
      A_P_CC = 5,
      A_P_SG = 75,
      B_CT_PSW = 0.5,
      D_RO_R = 1,
      D_SA_W = 1,
      penalty = TRUE
    ),
    expected = c(0.0665),
    tolerance = 0.1
  )


  set.seed(123)
  a= sample(24:100,10)

  # test for potato
  expect_equal(
    bln_bbwp_psw(
      ID = 15,
      B_LU_BRP = rep(3732,10),
      B_SC_WENR = rep(902,10),
      B_AER_CBS = rep('LG14',10),
      B_GWL_CLASS = rep('GtII',10),
      B_SLOPE_DEGREE = rep(2,10),
      A_FE_OX = seq(100,1000,length.out = 10),
      A_AL_OX = seq(10,150,length.out = 10),
      A_P_CC = seq(1,5.8,length.out = 10),
      A_P_SG = seq(5,85,length.out = 10),
      B_CT_PSW = rep(0.5,10),
      D_RO_R = rep(1,10),
      D_SA_W = rep(1,10),
      B_AL_OX = NA_real_,
      B_AL_OX_SD = NA_real_,
      penalty = TRUE
    ),
    expected = c(0.09,0.096,0.0929,0.087,0.08,0.073,0.07,0.0675,0.066,0.064),
    tolerance = 0.01
  )

  # test for potato
  expect_equal(
    bln_bbwp_psw(
      ID = 15,
      B_LU_BRP = rep(265,10),
      B_SC_WENR = rep(902,10),
      B_AER_CBS = rep('LG14',10),
      B_GWL_CLASS = rep('GtVI',10),
      B_SLOPE_DEGREE = rep(2,10),
      A_FE_OX = seq(100,400,length.out = 10),
      A_AL_OX = seq(10,150,length.out = 10),
      A_P_CC = seq(1.5,2.58,length.out = 10),
      A_P_SG = seq(6.5,8.5,length.out = 10),
      B_CT_PSW = rep(0.2,10),
      D_RO_R = rep(1,10),
      D_SA_W = rep(1,10),
      B_AL_OX = NA_real_,
      B_AL_OX_SD = NA_real_,
      penalty = TRUE
    ),
    expected = c(0.37,0.37,0.37,0.37,0.37,0.37,0.37,0.37,0.37,0.37),
    tolerance = 0.01
  )


})

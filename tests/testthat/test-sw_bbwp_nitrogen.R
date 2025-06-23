test_that("bln_bbwp_nsw works", {

  # test for potato crop on sandy soil
  expect_equal(
    bln_bbwp_nsw(
      ID = 15,
      B_LU_BRP = 3732,
      B_SOILTYPE_AGR = 'dekzand',
      B_SC_WENR = 902,
      B_AER_CBS = 'LG14',
      B_GWL_CLASS = 'VI',
      B_SLOPE_DEGREE = 2.5,
      A_SOM_LOI = 7.92,
      A_N_RT = 2300,
      B_CT_NSW = 3.5,
      B_N_RT = NA_real_,
      B_N_RT_SD = NA_real_,
      D_RO_R = 0.8,
      D_SA_W = 0.8,
      penalty = TRUE
    ),
    expected = c(0.31),
    tolerance = 0.1
  )

  # test for potato on peat soil
  expect_equal(
    bln_bbwp_nsw(
      ID = 15,
      B_LU_BRP = 3732,
      B_SOILTYPE_AGR = 'veen',
      B_SC_WENR = 902,
      B_AER_CBS = 'LG14',
      B_GWL_CLASS = 'II',
      B_SLOPE_DEGREE = 2.5,
      A_SOM_LOI = 7.92,
      A_N_RT = 14300,
      B_CT_NSW = 5,
      B_N_RT = NA_real_,
      B_N_RT_SD = NA_real_,
      D_RO_R = 1,
      D_SA_W = 1,
      penalty = TRUE
    ),
    expected = c(0.0496),
    tolerance = 0.1
  )


  set.seed(123)
  a= sample(24:100,10)

  # test for potato
  expect_equal(
    bln_bbwp_nsw(
      ID = 15,
      B_LU_BRP = rep(3732,10),
      B_SOILTYPE_AGR = rep('dekzand',10),
      B_SC_WENR = rep(902,10),
      B_AER_CBS = rep('LG14',10),
      B_GWL_CLASS = rep('II',10),
      B_SLOPE_DEGREE = rep(2,10),
      A_SOM_LOI = seq(1,15,length.out = 10),
      A_N_RT = seq(1000,9000,length.out = 10),
      D_RO_R = rep(1,10),
      D_SA_W = rep(1,10),
      B_CT_NSW = rep(4,10),
      B_N_RT = NA_real_,
      B_N_RT_SD = NA_real_,
      penalty = TRUE
    ),
    expected = c(0.20,0.199,0.195,0.19,0.183,0.176,0.167,0.16,0.15,0.15),
    tolerance = 0.01
  )

  # test for potato
  expect_equal(
    bln_bbwp_nsw(
      ID = 15,
      B_LU_BRP = rep(3732,10),
      B_SOILTYPE_AGR = rep('dekzand',10),
      B_SC_WENR = rep(902,10),
      B_AER_CBS = rep('LG14',10),
      B_GWL_CLASS = rep('II',10),
      B_SLOPE_DEGREE = rep(2,10),
      A_SOM_LOI = seq(1,15,length.out = 10),
      A_N_RT = seq(1000,9000,length.out = 10),
      D_RO_R = rep(1,10),
      D_SA_W = rep(1,10),
      B_CT_NSW = rep(0.4,10),
      B_N_RT = NA_real_,
      B_N_RT_SD = NA_real_,
      penalty = TRUE
    ),
    expected = c(0.52,0.52,0.52,0.51,0.51,0.51,0.50,0.496,0.493,0.49),
    tolerance = 0.01
  )


})

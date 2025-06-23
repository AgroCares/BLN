test_that("bln_bbwp_ngw works", {

  # test for potato crop on sandy soil
  expect_equal(
    bln_bbwp_ngw(
      ID = 15,
      B_LU_BRP = 3732,
      B_SOILTYPE_AGR = 'dekzand',
      B_SC_WENR = 902,
      B_AER_CBS = 'LG14',
      B_GWP = FALSE,
      B_GWL_CLASS = 'VI',
      A_N_RT = 2300,
      B_N_RT = NA_real_,
      B_N_RT_SD = NA_real_,
      penalty = TRUE
    ),
    expected = c(0.43),
    tolerance = 0.1
  )

  # test for potato on peat soil
  expect_equal(
    bln_bbwp_ngw(
      ID = 15,
      B_LU_BRP = 3732,
      B_SOILTYPE_AGR = 'veen',
      B_SC_WENR = 902,
      B_AER_CBS = 'LG14',
      B_GWP = FALSE,
      B_GWL_CLASS = 'II',
      A_N_RT = 8300,
      B_N_RT = NA_real_,
      B_N_RT_SD = NA_real_,
      penalty = TRUE
    ),
    expected = c(0.55),
    tolerance = 0.1
  )


  set.seed(123)
  a= sample(24:100,10)

  # test for potato
  expect_equal(
    bln_bbwp_ngw(
      ID = 15,
      B_LU_BRP = rep(3732,10),
      B_SOILTYPE_AGR = rep('dekzand',10),
      B_SC_WENR = rep(902,10),
      B_AER_CBS = rep('LG14',10),
      B_GWP = rep(TRUE,10),
      B_GWL_CLASS = rep('II',10),
      A_N_RT = seq(1000,9000,length.out = 10),
      B_N_RT = NA_real_,
      B_N_RT_SD = NA_real_,
      penalty = TRUE
    ),
    expected = c(0.51,0.5,0.48,0.46,0.43,0.4,0.38,0.35,0.34,0.33),
    tolerance = 0.01
  )

  # test for potato
  expect_equal(
    bln_bbwp_ngw(
      ID =  c(rep(15,5),rep(4,5)),
      B_LU_BRP = rep(3732,10),
      B_SOILTYPE_AGR = c(rep('dekzand',9),'veen'),
      B_SC_WENR = rep(902,10),
      B_AER_CBS = rep('LG14',10),
      B_GWP = rep(TRUE,10),
      B_GWL_CLASS = c(rep('V',9),'II'),
      A_N_RT = seq(1000,19000,length.out = 10),
      B_N_RT = NA_real_,
      B_N_RT_SD = NA_real_,
      penalty = TRUE
    ),
    expected = c(0.28,0.25,0.19,0.13,0.09,0.08,0.08,0.08,0.08,0.52),
    tolerance = 0.01
  )

})

test_that("bln_nut_nue works", {

  # test for various crops on river clay
  expect_equal(
    bln_nut_nue(
      B_LU_BRP = rep(3732,10),
      B_HELP_WENR = c('Mn15C','Mn15A','bMn15A','gMn85C','gMn83C','kMn63C',rep('Mn35A',4)),
      B_GWL_CLASS = rep('II',10),
      A_P_AL = c(35,54,rep(150,8)),
      A_P_CC = c(2.5,4.5,seq(5,60,length.out = 8)),
      A_P_WA = c(3.5,6.5,rep(45,8)),
      A_N_RT = seq(1000,9000,length.out = 10),
      B_N_RT = NA_real_,
      B_N_RT_SD = NA_real_,
      penalty = TRUE
    ),
    expected = c(0.69,0.76,0.34,0.35,0.35,0.36,0.36,0.36,0.36,0.36),
    tolerance = 0.1
  )

  # test for grassland
  expect_equal(
    bln_nut_nue(
      B_LU_BRP = rep(265,10),
      B_HELP_WENR = c('Mn15C','Mn15A','bMn15A','gMn85C','gMn83C','kMn63C',rep('Mn35A',4)),
      B_GWL_CLASS = rep('II',10),
      A_P_AL = c(35,54,rep(150,8)),
      A_P_CC = c(2.5,4.5,seq(5,60,length.out = 8)),
      A_P_WA = c(3.5,6.5,rep(45,8)),
      A_N_RT = seq(1000,9000,length.out = 10),
      B_N_RT = NA_real_,
      B_N_RT_SD = NA_real_,
      penalty = TRUE
    ),
    expected = c(0.32,0.31,0.34,0.31,0.32,0.32,0.32,0.33,0.33,0.33),
    tolerance = 0.1
  )


})

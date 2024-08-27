test_that("bln_wat_nrunoff works", {

  # test for various crops on river clay
  expect_equal(
    bln_wat_nrunoff(
      ID = 15,
      B_LU_BRP = c(233,259,2014,308),
      B_SOILTYPE_AGR = rep('rivierklei',4),
      B_GWL_CLASS = rep('GtIII',4),
      B_AER_CBS = rep('LG06',4),
      B_SC_WENR = rep(902,4),
      B_FERT_NORM_FR = rep(1,4),
      A_SOM_LOI = rep(3,4),
      A_CLAY_MI = rep(20,4),
      A_SAND_MI = rep(20,4),
      A_SILT_MI = rep(60,4),
      A_P_AL = c(25, 20,10,15),
      A_P_CC = c(1.5, 3,4,6),
      A_P_WA = c(24,20,15,10),
      A_PH_CC = c(5.6,7.04,5.65,6.22),
      A_CEC_CO = c(122,191,157,142),
      A_K_CC = c(95,107,73,89),
      A_K_CO_PO = c(1.6,2.9,3.1,2.4),
      M_GREEN = FALSE
      ),
    expected = c(0.98,0.94,0.54,0.995),
    tolerance = 0.1
  )

  # test for various crops on river clay
  expect_equal(
    bln_wat_nrunoff(
      ID = 15,
      B_LU_BRP = c(233,259,2014,308),
      B_SOILTYPE_AGR = rep('loess',4),
      B_GWL_CLASS = rep('GtVII',4),
      B_AER_CBS = rep('LG06',4),
      B_SC_WENR = rep(902,4),
      B_FERT_NORM_FR = rep(1,4),
      A_SOM_LOI = rep(3,4),
      A_CLAY_MI = rep(20,4),
      A_SAND_MI = rep(20,4),
      A_SILT_MI = rep(60,4),
      A_P_AL = c(2.5, 2.0,1.0,1.5),
      A_P_CC = c(1.5, .3,.4,.6),
      A_P_WA = c(2.4,2.0,1.5,1.0),
      A_PH_CC = c(5.6,7.04,5.65,6.22),
      A_CEC_CO = c(122,191,157,142),
      A_K_CC = c(9.5,1.07,7.3,8.9),
      A_K_CO_PO = c(1.6,2.9,3.1,2.4),
      M_GREEN = FALSE
    ),
    expected = c(0.976,0.91,0.65,0.99),
    tolerance = 0.1
  )




})

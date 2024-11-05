test_that("bln_wat_nretention_gw works", {

  # test for various crops on river clay
  expect_equal(
    bln_wat_nretention_gw(
      ID = 15,
      B_LU_BRP = c(233,259,2014,308),
      B_SOILTYPE_AGR = rep('rivierklei',4),
      B_GWL_CLASS = rep('GtIII',4),
      B_AER_CBS = rep('LG06',4),
      A_SOM_LOI = rep(3,4),
      A_N_RT = c(1000,2500,3500,9800),
      A_CN_FR = c(25,15,10,5.5)
      ),
    expected = c(0.99,0.99,0.99,0.61),
    tolerance = 0.1
  )

  # test for various crops on loess
  expect_equal(
    bln_wat_nretention_gw(
      ID = 1:4,
      B_LU_BRP = c(233,259,2014,308),
      B_SOILTYPE_AGR = rep('loess',4),
      B_GWL_CLASS = rep('GtVII',4),
      B_AER_CBS = rep('LG14',4),
      A_SOM_LOI = rep(3,4),
      A_N_RT = c(1000,2500,3500,9800),
      A_CN_FR = c(25,15,10,5.5)
    ),
    expected = c(0.99,0.51,0.09,0),
    tolerance = 0.1
  )




})

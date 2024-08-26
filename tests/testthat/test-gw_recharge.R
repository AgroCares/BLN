test_that("bln_wat_groundwater_recharge works", {

  # test for various crops on river clay
  expect_equal(
    bln_wat_groundwater_recharge(
      ID = 15,
      B_LU_BRP = c(233,259,2014,308),
      B_SC_WENR = rep(11,4),
      B_GWL_CLASS = rep('GtVI',4),
      B_DRAIN = rep(TRUE,4),
      A_CLAY_MI = rep(20,4),
      A_SAND_MI = rep(15,4),
      A_SILT_MI = rep(10,4),
      A_SOM_LOI = c(2,3,5,8),
      M_GREEN = rep(FALSE,4)
      ),
    expected = c(0.68,0.71,0.78,0.79),
    tolerance = 0.1
  )

  # test for various crops on loess
  expect_equal(
    bln_wat_groundwater_recharge(
      ID = 1:4,
      B_LU_BRP = c(233,259,2014,308),
      B_SC_WENR = rep(11,4),
      B_GWL_CLASS = rep('GtII',4),
      B_DRAIN = rep(TRUE,4),
      A_CLAY_MI = rep(20,4),
      A_SAND_MI = rep(15,4),
      A_SILT_MI = rep(60,4),
      A_SOM_LOI = c(2,3,15,18),
      M_GREEN = rep(FALSE,4)
    ),
    expected = c(0.69,0.71,0.80,0.80),
    tolerance = 0.1
  )




})

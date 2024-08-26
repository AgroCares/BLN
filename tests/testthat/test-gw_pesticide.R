test_that("bln_wat_pesticide works", {

  # test for various crops on river clay
  expect_equal(
    bln_wat_pesticide(
      ID = 15,
      B_LU_BRP = c(233,259,2014,308),
      B_SOILTYPE_AGR = rep('dekzand',4),
      A_SOM_LOI = c(2,3,5,8),
      A_CLAY_MI = rep(20,4),
      A_SAND_MI = rep(15,4),
      A_SILT_MI = rep(10,4),
      M_PESTICIDES_DST = rep(FALSE,4),
      M_MECHWEEDS = rep(FALSE,4),
      M_GREEN = rep(FALSE,4)
      ),
    expected = c(0.006,0.0189,0.0871,0.221),
    tolerance = 0.1
  )

  # test for various crops on loess
  expect_equal(
    bln_wat_pesticide(
      ID = 1:4,
      B_LU_BRP = c(233,259,2014,308),
      B_SOILTYPE_AGR = rep('loess',4),
      A_SOM_LOI = c(2,3.5,5,18),
      A_CLAY_MI = rep(20,4),
      A_SAND_MI = rep(15,4),
      A_SILT_MI = rep(65,4),
      M_PESTICIDES_DST = rep(FALSE,4),
      M_MECHWEEDS = rep(FALSE,4),
      M_GREEN = rep(FALSE,4)
    ),
    expected = c(0.005,0.0289,0.0688,0.9074),
    tolerance = 0.1
  )




})

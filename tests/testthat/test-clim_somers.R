test_that("bln_clim_somers works", {
  expect_equal(

    # test for single peat soil
    bln_clim_somers(
      ID = 1,
      B_SOILTYPE_AGR = 'veen',
      A_SOM_LOI = 25,
      B_SOMERS_BC = 25,
      B_DRAIN_SP = 0.63,
      B_DRAIN_WP = 0.49,
      B_DRAIN_SP_CHANGE = 0.2
    ),
    expected = c(0.584),
    tolerance = 0.1
  )

  # test for single peat soil
  expect_equal(
  bln_clim_somers(
    ID = 1,
    B_SOILTYPE_AGR = 'veen',
    A_SOM_LOI = 25,
    B_SOMERS_BC = 25,
    B_DRAIN_SP = 0.40,
    B_DRAIN_WP = 0.15,
    B_DRAIN_SP_CHANGE = 0.2
  ),
  expected = c(0.66),
  tolerance = 0.1
  )

  # test for ten peat soils
  expect_equal(
    bln_clim_somers(
      ID = 1:10,
      B_SOILTYPE_AGR = rep('veen',10),
      A_SOM_LOI = rep(25,10),
      B_SOMERS_BC = rep(25,10),
      B_DRAIN_SP = seq(1,0.1,length.out = 10),
      B_DRAIN_WP = seq(1,0.1,length.out = 10) -0.15,
      B_DRAIN_SP_CHANGE = 0.2
    ),
    expected = c(0.18,0.20,0.24,0.55,0.596,0.63,0.66,0.58,0.47,0.47),
    tolerance = 0.1
  )

})

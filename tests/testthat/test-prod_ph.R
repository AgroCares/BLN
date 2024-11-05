test_that("bln_c_ph works", {

  # test 1. single fields with more crops
  expect_equal(
    bln_c_ph(
      ID = 15,
      B_LU_BRP = c(233,259,2014,308),
      B_SOILTYPE_AGR = rep('loess',4),
      A_SOM_LOI = rep(3,4),
      A_CLAY_MI = rep(20,4),
      A_PH_CC = c(4,5,6,3)
      ),
    expected = c(0,0,0,0),
    tolerance = 0.1
  )

  # test 2. multiple fields with more crops
  expect_equal(
    bln_c_ph(
      ID = 15:18,
      B_LU_BRP = c(233,259,2014,308),
      B_SOILTYPE_AGR = rep('loess',4),
      A_SOM_LOI = rep(3,4),
      A_CLAY_MI = rep(20,4),
      A_PH_CC = c(4,5,6,3)
    ),
    expected = c(0,0.8,0.32,0),
    tolerance = 0.1
  )

  # test 3. multiple fields with different soil types
  expect_equal(
    bln_c_ph(
      ID = 1:5,
      A_PH_CC = c(6, 4.0, 6.2, 4.5, 5.0),
      B_SOILTYPE_AGR = c("rivierklei", "veen", "veen", "loess", "dekzand"),
      A_CLAY_MI = c(20, 5, 8, 12, 5),
      A_SOM_LOI = c(5, 20, 23, 8, 10),
      B_LU_BRP = c(265, 265, 265, 265, 800)
    ),
    expected = c(0.99,0.0,0.999,0.03,0.96),
    tolerance = 0.1
  )

  # test 4. multiple fields with low pH, very low scoring then
  expect_equal(
    bln_c_ph(
      ID = 1:5,
      A_PH_CC = c(3,3,3,3,3),
      B_SOILTYPE_AGR = c("rivierklei", "veen", "veen", "loess", "dekzand"),
      A_CLAY_MI = c(20, 5, 8, 12, 5),
      A_SOM_LOI = c(5, 20, 23, 8, 10),
      B_LU_BRP = c(265, 265, 265, 265, 800)
    ),
    expected = c(0,0,0,0,0),
    tolerance = 0.1
  )

})

test_that("bln_bbwp_bw works", {

  # test for various crops on river clay
  expect_equal(
    bln_bbwp_bw(
      ID = 15,
      B_LU_BRP = c(233,259,2014,308),
      B_HELP_WENR = c('gMn53C', 'Mn25C','U0102nr108','nSn13A'),
      B_GWL_CLASS = rep('GtVI',4),
      B_AREA_DROUGHT = rep(TRUE,4),
      A_CLAY_MI = rep(20,4),
      A_SAND_MI = rep(15,4),
      A_SILT_MI = rep(10,4),
      A_SOM_LOI = c(2,3,5,8),
      penalty = TRUE
      ),
    expected = c(0.92,0.94,0.92,0.96),
    tolerance = 0.1
  )

  # test for various crops on various soils
  expect_equal(
    bln_bbwp_bw(
      ID = 1:4,
      B_LU_BRP = c(233,259,2014,308),
      B_HELP_WENR = c('gMn53C', 'Mn25C','U0102nr108','nSn13A'),
      B_GWL_CLASS = rep('GtVI',4),
      B_AREA_DROUGHT = rep(TRUE,4),
      A_CLAY_MI = rep(20,4),
      A_SAND_MI = rep(15,4),
      A_SILT_MI = rep(60,4),
      A_SOM_LOI = c(0.72,1.3,0.5,0.8),
      penalty = TRUE
    ),
    expected = c(0.78,0.84,0.73,0.81),
    tolerance = 0.1
  )




})

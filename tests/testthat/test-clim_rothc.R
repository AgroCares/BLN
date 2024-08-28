test_that("bln_clim_rothc works", {

  # test for potato crop on sandy soil
  expect_equal(
    bln_clim_rothc(
      ID = 15,
      A_SOM_LOI = 7.92,
      B_GWL_GLG = 75,
      A_CLAY_MI = 3.5,
      B_LU_BRP = 3732,
      quiet = TRUE,
      mc = FALSE
    ),
    expected = c(0.867),
    tolerance = 0.1
  )

  # test for potato on peat soil
  expect_equal(
    bln_clim_rothc(
      ID = 15,
      A_SOM_LOI = 27.92,
      B_GWL_GLG = 95,
      A_CLAY_MI = 3.5,
      B_LU_BRP = 3732,
      quiet = TRUE,
      mc = FALSE
    ),
    expected = c(0.675),
    tolerance = 0.1
  )


  set.seed(123)
  a= sample(24:100,10)

  # test for potato
  expect_equal(
    bln_clim_rothc(
      ID = 15,
      A_SOM_LOI = seq(1,15,length.out = 10),
      B_GWL_GLG = rep(95,10),
      A_CLAY_MI = a,
      B_LU_BRP = rep(3732,10),
      quiet = TRUE,
      mc = FALSE
    ),
    expected = rep(0.866,10),
    tolerance = 0.1
  )

  # test for potato
  expect_equal(
    bln_clim_rothc(
      ID = c(rep(15,5),rep(4,5)),
      A_SOM_LOI = seq(1,15,length.out = 10),
      B_GWL_GLG = rep(95,10),
      A_CLAY_MI = a,
      B_LU_BRP = rep(3732,10),
      quiet = TRUE,
      mc = FALSE
    ),
    expected = c(rep(0.844,5),rep(0.902,5)),
    tolerance = 0.1
  )

  # test for grassland and potato, two fields
  expect_equal(
    bln_clim_rothc(
      ID = c(rep(15,5),rep(4,5)),
      A_SOM_LOI = seq(1,15,length.out = 10),
      B_GWL_GLG = rep(95,10),
      A_CLAY_MI = a,
      B_LU_BRP = c(rep(265,5),rep(3732,5)),
      quiet = TRUE,
      mc = FALSE
    ),
    expected = c(rep(0.943,5),rep(0.902,5)),
    tolerance = 0.1
  )

  # test for 10 fields with grassland and potato
  expect_equal(
    bln_clim_rothc(
      ID = 1:10,
      A_SOM_LOI = seq(1,15,length.out = 10),
      B_GWL_GLG = rep(95,10),
      A_CLAY_MI = a,
      B_LU_BRP = rep(c(3732,265),5),
      quiet = TRUE,
      mc = FALSE
    ),
    expected = c(0.778,0.949,0.742,0.94,0.779,0.94,0.813,0.94,0.84,0.944),
    tolerance = 0.1
  )

  # test for 10 fields with grassland and potato
  expect_equal(
    bln_clim_rothc(
      ID = 1:10,
      A_SOM_LOI = seq(1,15,length.out = 10),
      B_GWL_GLG = rep(95,10),
      A_CLAY_MI = a,
      B_LU_BRP = rep(c(3732,265),5),
      quiet = TRUE,
      mc = TRUE
    ),
    expected = c(0.78,0.95,0.74,0.94,0.78,0.94,0.81,0.94,0.84,0.94),
    tolerance = 0.1
  )
})

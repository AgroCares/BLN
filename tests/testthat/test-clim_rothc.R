test_that("bln_clim_rothc works", {

  # test for potato crop on sandy soil
  expect_equal(
    bln_clim_rothc(
      ID = 15,
      A_SOM_LOI = 7.92,
      B_GWL_GLG = 75,
      A_CLAY_MI = 3.5,
      B_LU_BRP = 3732,
      quiet = TRUE
    ),
    expected = c(0.92),
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
      quiet = TRUE
    ),
    expected = c(0.68),
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
      quiet = TRUE
    ),
    expected = c(0.88,0.88,0.88,0.88,0.88,0.88,0.88,0.88,0.88,0.88),
    tolerance = 0.01
  )

  # test for potato
  expect_equal(
    bln_clim_rothc(
      ID = c(rep(15,5),rep(4,5)),
      A_SOM_LOI = seq(1,15,length.out = 10),
      B_GWL_GLG = rep(95,10),
      A_CLAY_MI = a,
      B_LU_BRP = rep(3732,10),
      quiet = TRUE
    ),
    expected = c(rep(0.91,5),rep(0.90,5)),
    tolerance = 0.01
  )

  # test for grassland and potato, two fields
  expect_equal(
    bln_clim_rothc(
      ID = c(rep(15,5),rep(4,5)),
      A_SOM_LOI = seq(1,15,length.out = 10),
      B_GWL_GLG = rep(95,10),
      A_CLAY_MI = a,
      B_LU_BRP = c(rep(265,5),rep(3732,5)),
      quiet = TRUE
    ),
    expected = c(rep(0.93,5),rep(0.97,5)),
    tolerance = 1
  )

  # test for 10 fields with grassland and potato, two fields
  expect_equal(
    bln_clim_rothc(
      ID = 1:10,
      A_SOM_LOI = seq(1,15,length.out = 10),
      B_GWL_GLG = rep(95,10),
      A_CLAY_MI = a,
      B_LU_BRP = rep(c(3732,265),5),
      quiet = TRUE
    ),
    expected = c(0.83,0.96,0.82,0.96,0.85,0.96,0.88,0.96,0.90,0.96),
    tolerance = 1
  )
})

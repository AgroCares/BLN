test_that("bln_clim_csat works", {

  # test for potato crop
  expect_equal(
    bln_clim_csat(
      A_SOM_LOI = 7.92,
      A_CLAY_MI = 3.5,
      B_LU_BRP = 3732,
      A_SOM_LOI_MLMAX = 14
    ),
    expected = c(0.566),
    tolerance = 0.1
  )

  # test for potato without no max given for C saturation
  expect_equal(
    bln_clim_csat(
      A_SOM_LOI = 7.92,
      A_CLAY_MI = 3.5,
      B_LU_BRP = 3732
    ),
    expected = c(0.79),
    tolerance = 0.1
  )


  set.seed(123)
  a= sample(24:100,10)

  # test for potato
  expect_equal(
    bln_clim_csat(
      A_SOM_LOI = seq(1,20,length.out = 10),
      A_CLAY_MI = a,
      B_LU_BRP = rep(3732,10),
      A_SOM_LOI_MLMAX = 14
    ),
    expected = c(.07,0.22,0.37,0.52,0.675,0.825,0.976,1,1,1),
    tolerance = 0.01
  )

  # test for grassland
  expect_equal(
    bln_clim_csat(
      A_SOM_LOI = seq(1,20,length.out = 10),
      A_CLAY_MI = a,
      B_LU_BRP = rep(265,10),
      A_SOM_LOI_MLMAX = 14
    ),
    expected = c(.07,0.22,0.37,0.52,0.675,0.825,0.976,1,1,1),
    tolerance = 0.01
  )

  # test for grassland, two fields
  expect_equal(
    bln_clim_csat(
      A_SOM_LOI = seq(1,20,length.out = 10),
      A_CLAY_MI = a,
      B_LU_BRP = c(rep(265,5),rep(3732,5)),
      A_SOM_LOI_MLMAX = NA_real_
    ),
    expected = c(0.22,0.47,0.67,0.69,0.78,0.51,0.57,0.52,0.71,0.63),
    tolerance = 1
  )

})

test_that("bln_clim_cbalance works", {
  expect_equal(
    # test for potato
    bln_clim_cbalance(
      ID = 1,
      A_SOM_LOI = 7.92,
      A_P_AL = 63,
      A_P_WA = 65,
      B_LU_BRP = 3732,
      M_COMPOST = 3,
      M_GREEN = TRUE
    ),
    expected = c(0.689),
    tolerance = 0.1
  )
  set.seed(123)
  a= sample(24:100,10)

  # test for potato
  expect_equal(
    bln_clim_cbalance(
      ID = 1:10,
      A_SOM_LOI = seq(1,20,length.out = 10),
      A_P_AL = a,
      A_P_WA = a*1.05,
      B_LU_BRP = rep(3732,10),
      M_COMPOST = rep(3,10),
      M_GREEN = rep(TRUE,10)
    ),
    expected = c(0.86,0.78,0.77,0.70,0.67,0.65,0.64,0.63,0.67,0.62),
    tolerance = 1
  )

  # test for grassland
  expect_equal(
    bln_clim_cbalance(
      ID = 1:10,
      A_SOM_LOI = seq(1,20,length.out = 10),
      A_P_AL = a,
      A_P_WA = a*1.05,
      B_LU_BRP = rep(265,10),
      M_COMPOST = rep(3,10),
      M_GREEN = rep(TRUE,10)
    ),
    expected = c(0.99,0.99,0.99,0.98,0.98,0.97,0.97,0.97,0.98,0.97),
    tolerance = 1
  )

  # test for grassland, two fields
  expect_equal(
    bln_clim_cbalance(
      ID = rep(1:5,2),
      A_SOM_LOI = seq(1,20,length.out = 10),
      A_P_AL = a,
      A_P_WA = a*1.05,
      B_LU_BRP = rep(265,10),
      M_COMPOST = rep(3,10),
      M_GREEN = rep(TRUE,10)
    ),
    expected = c(0.99,0.99,0.99,0.98,0.98,0.97,0.97,0.97,0.98,0.97),
    tolerance = 1
  )

})

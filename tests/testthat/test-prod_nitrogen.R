test_that("bln_c_nitrogen works", {

  # test 1
  expect_equal(
    bln_c_nitrogen(
      B_LU_BRP = 256, B_SOILTYPE_AGR = 'dekzand',A_SOM_LOI = 4.5,A_N_RT = 2500
      ),
    expected = c(1),
    tolerance = 0.1
  )

  # test 2
  expect_equal(
    bln_c_nitrogen(
      B_SOILTYPE_AGR = c('dekzand','zeeklei','rivierklei','loess'),
      B_LU_BRP = rep(3732,4),
      A_SOM_LOI = rep(5,4),
      A_N_RT = seq(1000,2000,length.out = 4)
    ),
    expected = c(.24,0.56,0.81,0.99),
    tolerance = 0.1
  )

})

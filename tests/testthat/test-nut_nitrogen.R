test_that("bln_nut_nitrogen works", {

  # test for various crops on river clay
  expect_equal(
    bln_nut_nitrogen(
      ID = 15,
      B_LU_BRP = c(233,259,2014,308,rep(3732,6)),
      B_SOILTYPE_AGR = rep('rivierklei',10),
      A_SOM_LOI = seq(1,15,length.out = 10),
      A_N_RT = seq(1000,9000,length.out = 10)
      ),
    expected = c(0.88,0.995,0.798,0.37,rep(0,6)),
    tolerance = 0.1
  )

  # test for various crops on various soils
  expect_equal(
    bln_nut_nitrogen(
      ID = 1:10,
      B_LU_BRP = c(233,259,2014,308,rep(3732,6)),
      B_SOILTYPE_AGR = rep('loess',10),
      A_SOM_LOI = rep(5,10),
      A_N_RT = seq(1000,4500,length.out = 10)
    ),
    expected = c(0.24,0.68,0.93,0.999,0.88,0.58,0.10,rep(0,3)),
    tolerance = 0.1
  )


})

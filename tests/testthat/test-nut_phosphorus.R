test_that("bln_nut_phosphorus works", {

  # test for various crops on river clay
  expect_equal(
    bln_nut_phosphorus(
      B_LU_BRP = c(233,259,2014,308,rep(3732,6)),
      A_P_AL = c(35,54,rep(150,8)),
      A_P_CC = c(2.5,4.5,seq(5,60,length.out = 8)),
      A_P_WA = c(3.5,6.5,rep(45,8))
      ),
    expected = c(0.01,0.98,0.96,0.96,0.96,0.96,0.96,0.96,0.96,0.96),
    tolerance = 0.1
  )

  # test for various crops on various soils
  expect_equal(
    bln_nut_phosphorus(
      B_LU_BRP = c(233,259,2014,308,rep(3732,6)),
      A_P_AL = seq(1.5,105,length.out = 10),
      A_P_CC = seq(.1,10.5,length.out = 10),
      A_P_WA = c(35,65,rep(45,8))
    ),
    expected = c(0.85,0.29,0.96,0.96,0.96,0.96,0.960,0.9,0.96,0.96),
    tolerance = 0.1
  )


})

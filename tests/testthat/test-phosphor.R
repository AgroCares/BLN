test_that("bln_c_posphor works", {
  expect_equal(
    bln_c_posphor(B_LU = 265, A_P_AL = 45, A_P_CC = 2.5),
    expected = c(0.91),
    tolerance = 0.01
  )
  expect_equal(
    bln_c_posphor(B_LU = 265, A_P_AL = 15, A_P_CC = 1.5),
    expected = c(0.74),
    tolerance = 0.01
  )

  expect_equal(
    bln_c_posphor(B_LU = c(265,1019),A_P_AL = c(35,10),A_P_CC = c(2.5,0.5), A_P_WA = c(35,15)),
    expected = c(0.92,0.20),
    tolerance = 0.01
  )



})

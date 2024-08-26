test_that("bln_b_pmn works", {

  # test 1
  expect_equal(
    bln_b_pmn(
      A_N_PMN = 65,
      B_LU_BRP = 265,
      B_SOILTYPE_AGR = "rivierklei"
      ),
    expected = c(.99998),
    tolerance = 0.1
  )

  # test 2
  expect_equal(
    bln_b_pmn(
      A_N_PMN = seq(15,75,length.out = 8),
      B_LU_BRP = c(rep(265,4), rep(235,4)),
      B_SOILTYPE_AGR = rep(c("rivierklei", "veen", "veen", "dekzand"),2)
    ),
    expected = c(.43,0.72,0.93,0.98,0.797,0.999,0.9999,0.9997),
    tolerance = 0.1
  )

})

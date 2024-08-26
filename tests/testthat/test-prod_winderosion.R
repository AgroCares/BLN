test_that("bln_p_windererosion works", {

  # test 1
  expect_equal(
    bln_p_windererosion( A_CLAY_MI=25,
               A_SILT_MI=15,
               B_LU_BRP = 3732),
    expected = c(0.73),
    tolerance = 0.1
  )

  # test 2
  a = c(14, 22, 25, 26, 5, 19, 9, 3, 8, 7)
  expect_equal(
    bln_p_windererosion(A_CLAY_MI=a,
              A_SILT_MI=rev(a),
              B_LU_BRP = rep(3732,10)),
    expected = c(0.544,0.65,0.63,0.69,0.58,0.58,0.69,0.63,0.65,0.54),
    tolerance = 0.1
  )

})

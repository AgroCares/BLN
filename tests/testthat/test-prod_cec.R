test_that("bln_c_cec works", {

  # test 1
  expect_equal(
    bln_c_cec(
      A_CEC_CO = seq(10,500,length.out = 5)
      ),
    expected = c(.1,1,1,1,1),
    tolerance = 0.1
  )

  # test 2
  expect_equal(
    bln_c_cec(
      A_CEC_CO = seq(1,150,length.out = 5)
    ),
    expected = c(.01,0.382,0.755,1,1),
    tolerance = 0.1
  )


})

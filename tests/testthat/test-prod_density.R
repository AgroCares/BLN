test_that("bln_p_density works", {

  # test 1
  expect_equal(
    bln_p_density(
      A_SOM_LOI = 6.5, A_CLAY_MI = 28
      ),
    expected = c(0.95),
    tolerance = 0.1
  )

  # test 2
  expect_equal(
    bln_p_density(
      A_SOM_LOI = 3.5, A_CLAY_MI = 2
    ),
    expected = c(.84),
    tolerance = 0.1
  )

  # test 3
  expect_equal(
    bln_p_density(
      A_SOM_LOI = c(3.5,8.5),A_CLAY_MI = c(2,28)
    ),
    expected = c(.84,.96),
    tolerance = 0.1
  )


})

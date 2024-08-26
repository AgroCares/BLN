test_that("bln_p_whc works", {

  # test 1
  expect_equal(
    bln_p_whc(A_CLAY_MI = 20.5,A_SAND_MI = 65,A_SILT_MI = 14.5,A_SOM_LOI = 3.5),
    expected = c(0.827),
    tolerance = 0.1
  )

  # test 2
  expect_equal(
    bln_p_whc(A_CLAY_MI = 5,A_SAND_MI = 15,A_SILT_MI = 80,A_SOM_LOI = 6.5),
    expected = c(0.825),
    tolerance = 0.1
  )

  # test 3
  expect_equal(
    bln_p_whc(A_CLAY_MI = 5,A_SAND_MI = 15,A_SILT_MI = 80,A_SOM_LOI = 6.5, type = 'water holding capacity'),
    expected = c(0.825),
    tolerance = 0.1
  )

  # test 3
  expect_equal(
    bln_p_whc(A_CLAY_MI = rep(5,3),
              A_SAND_MI = c(15,30,45),
              A_SILT_MI = c(80,30,40),
              A_SOM_LOI = c(3,6.5,9),
              type = 'water holding capacity'),
    expected = c(0.754,0.837,0.864),
    tolerance = 0.1
  )
})

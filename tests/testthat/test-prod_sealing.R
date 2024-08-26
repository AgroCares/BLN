test_that("bln_p_sealing works", {

  # test 1
  expect_equal(
    bln_p_sealing(B_LU_BRP = 2014,
                  A_CLAY_MI = 20,
                  A_SOM_LOI = 5),
    expected = c(.99998),
    tolerance = 0.1
  )

  # test 2
  expect_equal(
    bln_p_sealing(B_LU_BRP = c(2014,265,3732),
                    A_CLAY_MI = c(25, 20,15),
                    A_SOM_LOI = c(1.5, 8,4)),
    expected = c(0.96,1,.85),
    tolerance = 0.1
  )

})

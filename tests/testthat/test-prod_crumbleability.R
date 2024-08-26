test_that("bln_p_crumbleability works", {

  # test 1
  expect_equal(
    bln_p_crumbleability(
      B_LU_BRP = 2014,
      A_CLAY_MI = 10,
      A_SOM_LOI = 5,
      A_PH_CC = 6
      ),
    expected = c(1),
    tolerance = 0.1
  )

  # test 2
  expect_equal(
    bln_p_crumbleability(
      B_LU_BRP = rep(1042,11),
      A_CLAY_MI = seq(2,60,length.out = 11),
      A_SOM_LOI =seq(1.2,25,length.out = 11),
      A_PH_CC = seq(5.5,7.5,length.out = 11)
    ),
    expected = c(1,1,1,0.64,0.47,0.49,1,1,1,1,1),
    tolerance = 0.1
  )


})

test_that("bln_c_potassium works", {

  # test 1
  expect_equal(
    bln_c_potassium(B_LU_BRP = 265, B_SOILTYPE_AGR = 'dekzand',
                    A_SOM_LOI = 4, A_CLAY_MI = 11,A_PH_CC = 5.4, A_CEC_CO = 125,
                    A_K_CO_PO = 8.5, A_K_CC = 145),
    expected = c(.99998),
    tolerance = 0.1
  )

  # test 2
  expect_equal(
    bln_c_potassium(265, 'dekzand',4, 11,5.4,  125,8.5, 145),
    expected = c(0.9997),
    tolerance = 0.1
  )

  # test 3
  expect_equal(
    bln_c_potassium(c(265,1019), rep('dekzand',2),c(4,6), c(11,14),
                    c(5.4,5.6),  c(125,145),c(8.5,3.5), c(145,180)),
    expected = c(0.999,0.995),
    tolerance = 0.1
  )

  # test 4
  expect_equal(
    bln_c_potassium(265, 'dekzand',4, 11,5.4,  125,1.5, 4.5),
    expected = c(0.25),
    tolerance = 0.1
  )

})

test_that("bln_c_magnesium works", {

  # test 1
  expect_equal(
    bln_c_magnesium(
      A_MG_CC = 122,
      A_PH_CC = 5.6,
      A_SOM_LOI = 7.9,
      A_CEC_CO = 122,
      A_K_CC = 95,
      A_K_CO_PO = 1.56,
      A_CLAY_MI = 1.73,
      B_SOILTYPE_AGR = 'dekzand',
      B_LU_BRP = 3732
      ),
    expected = c(0.99),
    tolerance = 0.1
  )

  # test 2
  expect_equal(
    bln_c_magnesium(
      A_MG_CC = c(122,146,291,160),
      A_PH_CC = c(5.6,7.04,5.65,6.22),
      A_SOM_LOI = c(7.9,4.2,6.12,3.57),
      A_CEC_CO = c(122,191,157,142),
      A_K_CC = c(95,107,73,89),
      A_K_CO_PO = c(1.6,2.9,3.1,2.4),
      A_CLAY_MI = c(1.7,26,31,14),
      B_SOILTYPE_AGR = c('dekzand','zeeklei','rivierklei','loess'),
      B_LU_BRP = rep(3732,4)
    ),
    expected = c(0.99,1,1,1),
    tolerance = 0.1
  )

  # test 3
  expect_equal(
    bln_c_magnesium(
      A_MG_CC = c(122,146,291,160),
      A_PH_CC = c(5.6,7.04,5.65,6.22),
      A_SOM_LOI = c(7.9,4.2,6.12,3.57),
      A_CEC_CO = c(122,191,157,142),
      A_K_CC = c(95,107,73,89),
      A_K_CO_PO = c(1.6,2.9,3.1,2.4),
      A_CLAY_MI = c(1.7,26,31,14),
      B_SOILTYPE_AGR = c('dekzand','zeeklei','rivierklei','loess'),
      B_LU_BRP = rep(265,4)
    ),
    expected = c(0.98,0.59,0.98,0.999),
    tolerance = 0.1
  )

})

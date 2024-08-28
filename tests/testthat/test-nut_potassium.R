test_that("bln_nut_potassium works", {

  # test for various crops on river clay
  expect_equal(
    bln_nut_potassium(
      A_PH_CC = c(5.6,7.04,5.65,6.22),
      A_SOM_LOI = c(7.9,4.2,6.12,3.57),
      A_CEC_CO = c(122,191,157,142),
      A_K_CC = c(95,107,73,89),
      A_K_CO_PO = c(1.6,2.9,3.1,2.4),
      A_CLAY_MI = c(1.7,26,31,14),
      B_SOILTYPE_AGR = c('dekzand','zeeklei','rivierklei','loess'),
      B_LU_BRP = rep(3732,4)
      ),
    expected = c(0.61,0.92,0.998,0.999),
    tolerance = 0.1
  )

  # test for various crops on river clay
  expect_equal(
    bln_nut_potassium(
      A_PH_CC = c(5.6,7.04,5.65,6.22),
      A_SOM_LOI = c(7.9,4.2,6.12,3.57),
      A_CEC_CO = c(122,191,157,142),
      A_K_CC = c(95,107,73,89)*5,
      A_K_CO_PO = c(1.6,2.9,3.1,2.4),
      A_CLAY_MI = c(1.7,26,31,14),
      B_SOILTYPE_AGR = c('dekzand','zeeklei','rivierklei','loess'),
      B_LU_BRP = rep(3732,4)
    ),
    expected = c(0.82,0.001,0.02,0.91),
    tolerance = 0.1
  )


})

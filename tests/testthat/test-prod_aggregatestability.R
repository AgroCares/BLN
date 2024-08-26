test_that("bln_p_aggstability works", {

  # test for sandy soils
  expect_equal(
    bln_p_aggstability(
      A_K_CO_PO = rep(5,5),
      A_MG_CO_PO = rep(2.8,5),
      A_CA_CO_PO = seq(35,92,length.out = 5),
      A_SOM_LOI = rep(5, 5),
      B_SOILTYPE_AGR = rep('dekzand', 5)
      ),
    expected = c(.64,0.75,0.86,0.95,0.895),
    tolerance = 0.1
  )

  # test for river clay soils
  expect_equal(
    bln_p_aggstability(
      A_K_CO_PO = rep(5,5),
      A_MG_CO_PO = rep(2.8,5),
      A_CA_CO_PO = seq(35,92,length.out = 5),
      A_SOM_LOI = rep(5, 5),
      B_SOILTYPE_AGR = rep('rivierklei', 5)
    ),
    expected = c(0.598,0.711,0.823,0.93,0.93),
    tolerance = 0.1
  )


})

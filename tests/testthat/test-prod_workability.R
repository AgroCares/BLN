test_that("bln_p_workability works", {

  # test 1
  expect_equal(
    bln_p_workability( A_CLAY_MI = c(15.6,13.6, 4.3, 22.6, 1.9, 2.9, 3.1, 4.3, 15.6, 1.9),
                       A_SILT_MI = c(16.7,30.5, 11.8, 36.6, 9.2, 8.6, 10.6, 11.8, 16.7, 9.2),
                       B_LU_BRP = c(233, 234, 236, 256, 259, 265, 265, 317, 2014, 259),
                       B_SOILTYPE_AGR = c('zeeklei','zeeklei', 'dekzand','zeeklei', 'dekzand', 'dekzand', 'veen', 'dekzand', 'zeeklei', 'maasklei'),
                       B_GWL_GLG = c(173,139, 106, 144, 115, 113, 42, 106, 173, 115),
                       B_GWL_GHG = c(21, 18, 62, 70, 49, 81, 9, 62, 21, 49),
                       B_GWL_ZCRIT = c(400, 400, 400, 400, 400, 400, 400, 400, 400, 400)),
    expected = c(0.06,0.011,0.041,0.792,0.67,1,0.01,0.53,0.20,0.20),
    tolerance = 0.1
  )


})

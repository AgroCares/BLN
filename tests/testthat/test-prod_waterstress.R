test_that("bln_p_waterstress works", {

  # test 1
  expect_equal(
    bln_p_waterstress( B_HELP_WENR= c('gMn25C','bMn15A','gMn25C','bMn15A','gMn25C','bMn15A'),
                       B_LU_BRP = c(3732,265,258,172,343,2709),
                       B_GWL_CLASS = c('IV','V','III','II','VI','IV')
                 ),
    expected = c(0.97,0.95,0.99,0.99,0.99,0.97),
    tolerance = 0.1
  )

  # test 2
  expect_equal(
    bln_p_waterstress(B_HELP_WENR= c('gMn25C','bMn15A','gMn25C','bMn15A','gMn25C','bMn15A'),
                      B_LU_BRP = c(3732,265,258,172,343,2709),
                      B_GWL_CLASS = c('III','III','III','III','III','III')),
    expected = c(0.56,0.95,0.99,0.99,0.99,0.56),
    tolerance = 0.1
  )


})

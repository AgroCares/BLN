test_that("bln_evaluate_logistic works", {
  expect_equal(
    bln_evaluate_logistic(x = 5, b = 2, x0 = 3, v = 2.6),
    expected = c(0.99),
    tolerance = 0.01
  )
  expect_equal(
    bln_evaluate_logistic(x = 1.5, b = 2, x0 = 3, v = 2.6),
    expected = c(0.31),
    tolerance = 0.01
  )

  expect_equal(
    bln_evaluate_logistic(x = c(0.1,0.5,1.5,3.5), b = 2, x0 = 3, v = 2.6),
    expected = c(0.107,0.146,0.310,0.886),
    tolerance = 0.01
  )



})

test_that("bln_evaluate_parabolic works", {
  expect_equal(
    bln_evaluate_parabolic(x = c(0.1,0.5,1.5,3.5), x.top = 6.5),
    expected = c(0.031,0.150,0.408,0.787),
    tolerance = 0.01
  )
  expect_equal(
    bln_evaluate_parabolic(x = 5, x.top = 8),
    expected = c(0.859),
    tolerance = 0.01
  )


})

test_that("cf_ind_importance works", {
  expect_equal(
    cf_ind_importance(x = c(0.1,0.5,1.5)),
    expected = c(3.33,1.43,0.59),
    tolerance = 0.01
  )
  expect_equal(
    cf_ind_importance(x = 0.5),
    expected = c(1.43),
    tolerance = 0.01
  )


})

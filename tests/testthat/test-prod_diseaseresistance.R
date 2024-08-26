test_that("bln_b_diseaseresistance works", {

  # test 1
  expect_equal(
    bln_b_diseaseresistance(
      A_SOM_LOI = 3.5
      ),
    expected = c(0.76),
    tolerance = 0.1
  )

  # test 2
  expect_equal(
    bln_b_diseaseresistance(
      A_SOM_LOI = c(3.5,5.5,15,25)
    ),
    expected = c(.76,0.97,0.999,1),
    tolerance = 0.1
  )


})

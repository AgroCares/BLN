test_that("bln_format_gtclass works", {

  expect_equal(
    bln_format_gtclass(
      B_GWL_GHG = c(5, 16, 135, 34, 41, 98),
      B_GWL_GLG = c(41, 60, 157, 156, 55, 135)
    ),
    expected = c("I", "II", "VII", "Vb", "-", "VII"),
    tolerance = 0
  )
})

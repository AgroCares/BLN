test_that("bln_p_compaction works", {

  # test 1
  expect_equal(
    bln_p_compaction(
      B_SC_WENR = c("Bebouwing en infrastructuur","Groot","Zeer groot","Matig","Water",
                    "Glastuinbouw, niet beoordeeld","Beperkt door veenlagen","Van nature dicht" ,
                    "Beperkt", "Zeer beperkt")

      ),
    expected = c(1,0.4,0.2,0.6,1,1,0.8,0.2,0.8,1),
    tolerance = 0.1
  )

  # test 2
  expect_equal(
    bln_p_compaction(
      B_SC_WENR = c('Zeer beperkt')
    ),
    expected = c(1),
    tolerance = 0.1
  )

  # test 3
  expect_equal(
    bln_p_compaction(
      B_SC_WENR = c("1", "2", "3", "4", "5", "10", "11", "401","901", "902")
    ),
    expected = c(1,0.8,0.6,0.4,0.2,0.8,0.2,1,1,1),
    tolerance = 0.1
  )

})

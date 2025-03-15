test_that("bln_clim_rothc works", {

  # test for potato crop on sandy soil
  expect_equal(
    bln_clim_rothc(
      ID = 15,
      A_SOM_LOI = 7.92,
      B_GWL_GLG = 75,
      A_CLAY_MI = 3.5,
      B_LU_BRP = 3732,
      quiet = TRUE,
      mc = FALSE
    ),
    expected = c(0.867),
    tolerance = 0.1
  )

  # check on RothC initialisation
  expect_equal(
    rothc_initialise(
      B_LU_BRP = 3732,
      A_SOM_LOI = 7.92,
      A_CLAY_MI = 3.5
    ),
    expected = c(fr_IOM = 0.1604, fr_DPM = 0.0048,fr_RPM = 0.1394,fr_BIO = 0.0118),
    tolerance = 0.1
  )

  # test scenario
  out <- rothc_scenario(B_LU_BRP = 3732, scen = 'BAU')
  expect_equal(names(out),
    expected = c('rotation','amendment'),
    tolerance = 0.1
  )
  expect_equal(as.numeric(out$rotation[,c(2,3,4)]),
               expected = c(875,0,0.22),
               tolerance = 0.1
  )
  out <- rothc_scenario(B_LU_BRP = 3732, scen = 'ALL')
  expect_equal(as.numeric(out$rotation[,c(2,3,4)]),
               expected = c(1640,990,0.31),
               tolerance = 0.1
  )
  expect_equal(as.numeric(out$amendment$P_DOSE),
               expected = c(44333,3182),
               tolerance = 0.1
  )


  # test for potato on peat soil
  expect_equal(
    bln_clim_rothc(
      ID = 15,
      A_SOM_LOI = 27.92,
      B_GWL_GLG = 95,
      A_CLAY_MI = 3.5,
      B_LU_BRP = 3732,
      quiet = TRUE,
      mc = FALSE
    ),
    expected = c(0.675),
    tolerance = 0.1
  )


  set.seed(123)
  a= sample(24:100,10)

  # test for potato
  expect_equal(
    bln_clim_rothc(
      ID = 15,
      A_SOM_LOI = seq(1,15,length.out = 10),
      B_GWL_GLG = rep(95,10),
      A_CLAY_MI = a,
      B_LU_BRP = rep(3732,10),
      quiet = TRUE,
      mc = FALSE
    ),
    expected = rep(0.866,10),
    tolerance = 0.1
  )

  # test for potato
  expect_equal(
    bln_clim_rothc(
      ID = c(rep(15,5),rep(4,5)),
      A_SOM_LOI = seq(1,15,length.out = 10),
      B_GWL_GLG = rep(95,10),
      A_CLAY_MI = a,
      B_LU_BRP = rep(3732,10),
      quiet = TRUE,
      mc = FALSE
    ),
    expected = c(rep(0.844,5),rep(0.902,5)),
    tolerance = 0.1
  )

  # test for grassland and potato, two fields
  expect_equal(
    bln_clim_rothc(
      ID = c(rep(15,5),rep(4,5)),
      A_SOM_LOI = seq(1,15,length.out = 10),
      B_GWL_GLG = rep(95,10),
      A_CLAY_MI = a,
      B_LU_BRP = c(rep(265,5),rep(3732,5)),
      quiet = TRUE,
      mc = FALSE
    ),
    expected = c(rep(0.943,5),rep(0.902,5)),
    tolerance = 0.1
  )

  # test for 10 fields with grassland and potato
  expect_equal(
    bln_clim_rothc(
      ID = 1:10,
      A_SOM_LOI = seq(1,15,length.out = 10),
      B_GWL_GLG = rep(95,10),
      A_CLAY_MI = a,
      B_LU_BRP = rep(c(3732,265),5),
      quiet = TRUE,
      mc = FALSE
    ),
    expected = c(0.778,0.949,0.742,0.94,0.779,0.94,0.813,0.94,0.84,0.944),
    tolerance = 0.1
  )

  # test for 10 fields with grassland and potato
  expect_equal(
    bln_clim_rothc(
      ID = 1:10,
      A_SOM_LOI = seq(1,15,length.out = 10),
      B_GWL_GLG = rep(95,10),
      A_CLAY_MI = a,
      B_LU_BRP = rep(c(3732,265),5),
      quiet = TRUE,
      mc = TRUE
    ),
    expected = c(0.78,0.95,0.74,0.94,0.78,0.94,0.81,0.94,0.84,0.94),
    tolerance = 0.1
  )
})

test_that("bln_rothc_field works", {

  out <- bln_rothc_field(A_SOM_LOI = 7.92,
                         A_CLAY_MI = 3.5,
                         B_LU_BRP = 3732,
                         simyears = 50,
                         init = TRUE,
                         scen = c("BAU", "ALL"),
                         spinup = 10
                        )

  # test for potato crop on sandy soil
  expect_equal(dim(out),expected = c(52,3),tolerance = 0.1)
  expect_equal(colnames(out),expected=c('year','A_SOM_LOI_ALL','A_SOM_LOI_BAU'))
  expect_equal(as.numeric(out[50,2:3]),c(6.606,6.389),tolerance = 0.01)

  out <- bln_rothc_field(A_SOM_LOI = 7.92,
                         A_CLAY_MI = 3.5,
                         B_LU_BRP = 3732,
                         simyears = 50,
                         init = TRUE,
                         scen = c("BAU", "ALL",'CLT'),
                         spinup = 10
  )

  # test for potato crop on sandy soil
  expect_equal(dim(out),expected = c(52,4),tolerance = 0.1)
  expect_equal(colnames(out),expected=c('year','A_SOM_LOI_ALL','A_SOM_LOI_BAU','A_SOM_LOI_CLT'))
  expect_equal(as.numeric(out[50,2:4]),c(6.65,6.44,6.38),tolerance = 0.01)

})

test_that("bln_rothc_multicore works", {

  out <- bln_rothc_multicore(ID = 1:10,
                             B_LU_BRP = rep(c(3732,265),5),
                             B_GWL_GLG = rep(95, 10),
                             A_SOM_LOI = seq(1,15,length.out = 10),
                             A_CLAY_MI = rep(4.5,10),
                             scen = c('BAU','ALL'),
                             quiet = FALSE)

  # test for potato crop on sandy soil
  expect_equal(dim(out),expected = c(10,3),tolerance = 0.1)
  expect_equal(colnames(out),expected=c('ID','A_SOM_LOI_BAU','A_SOM_LOI_ALL'))
  expect_equal(as.numeric(out[10,2:3]),c(15.04,15.691),tolerance = 0.01)

  out <- bln_rothc_multicore(ID = 1:10,
                             B_LU_BRP = rep(c(3732,265),5),
                             B_GWL_GLG = rep(95, 10),
                             A_SOM_LOI = seq(1,15,length.out = 10),
                             A_CLAY_MI = rep(4.5,10),
                             scen = c('BAU','ALL', 'BAUIMPR','CLT'),
                             quiet = FALSE)

  # test for potato crop on sandy soil
  expect_equal(dim(out),expected = c(10,5),tolerance = 0.1)
  expect_equal(colnames(out),expected=c('ID','A_SOM_LOI_BAU','A_SOM_LOI_ALL','A_SOM_LOI_BAUIMPR','A_SOM_LOI_CLT'))
  expect_equal(as.numeric(out[10,2:5]),c(15.04447,15.691,15.691,15.43029),tolerance = 0.01)


})




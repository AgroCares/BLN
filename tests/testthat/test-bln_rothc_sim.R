# test bln rothc inputs functions
test_that("bln_rothc_sim works", {

  # get some input files
  b_lu_brp <- BLN::bln_scen_croprotation[b_aer_cbs=='LG01' & soiltype=='clay' & scen=='bld_arable_int',b_lu_brp]
  scen.inp <- rothc_scenario(B_LU_BRP = b_lu_brp, scen = 'BAU')
  rothc_rotation <- scen.inp$rotation
  rothc_amendment <- scen.inp$amendment

  # run bln_rothc_sim
  out <- bln_rothc_sim(A_SOM_LOI = 4.5,
                       A_CLAY_MI = 3.5,
                       A_DEPTH = 0.3,
                       B_DEPTH = 0.3,
                       cf_yield = 1,
                       M_TILLAGE_SYSTEM = 'CT',
                       rothc_rotation = rothc_rotation,
                       rothc_amendment = rothc_amendment,
                       rothc_parms = list(simyears = 50, init = FALSE,spinup = 10,method='adams'))

  # test dimensions and value
  expect_equal(dim(out),expected = c(61,2),tolerance = 0.1)
  expect_equal(as.numeric(out[61,2]),expected = c(3.6325),tolerance=0.1)
  expect_equal(colnames(out),expected = c('year','A_SOM_LOI'))

  })


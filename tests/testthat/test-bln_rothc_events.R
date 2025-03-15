# test bln rothc inputs functions
test_that("bln_rothc_event works", {

  # get some input files
  b_lu_brp <- BLN::bln_scen_croprotation[b_aer_cbs=='LG01' & soiltype=='clay' & scen=='bld_arable_int',b_lu_brp]
  scen.inp <- rothc_scenario(B_LU_BRP = b_lu_brp, scen = 'BAU')
  rothc_rotation <- scen.inp$rotation
  rothc_amendment <- scen.inp$amendment

  # run bln_rothc_event
  dt.crop <- bln_rothc_input_crop(dt = rothc_rotation,B_LU_BRP = NULL,cf_yield = 1)
  dt.org <- bln_rothc_input_amendment(dt = rothc_amendment,B_LU_BRP = NULL)
  out <- bln_rothc_event(crops = dt.crop,amendment = dt.org,A_CLAY_MI = 4.5,simyears = 50)

  # test dimensions and value
  expect_equal(dim(out),expected = c(164,4),tolerance = 0.1)
  expect_equal(as.numeric(out[164,c(1,4)]),expected = c(49.75,33.157),tolerance=0.1)
  expect_equal(as.character(out[164,.(as.character(var),method)]),expected = c('CHUM','add'),tolerance=0)
  expect_equal(colnames(out),expected = c('time','var','method','value'))

  })

test_that("bln_rothc_event_crop works", {

  # get some input files
  b_lu_brp <- BLN::bln_scen_croprotation[b_aer_cbs=='LG01' & soiltype=='clay' & scen=='bld_arable_int',b_lu_brp]
  scen.inp <- rothc_scenario(B_LU_BRP = b_lu_brp, scen = 'BAU')
  rothc_rotation <- scen.inp$rotation
  rothc_amendment <- scen.inp$amendment

  # run bln_rothc_event
  dt.crop <- bln_rothc_input_crop(dt = rothc_rotation,B_LU_BRP = NULL,cf_yield = 1)
  out <- bln_rothc_event_crop(crops = dt.crop,A_CLAY_MI = 4.5)

  # test dimensions and value
  expect_equal(dim(out),expected = c(8,4),tolerance = 0.1)
  expect_equal(as.numeric(out[8,c(1,3)]),expected = c(3.75,1127.49),tolerance=0.1)
  expect_equal(as.character(out[8,.(as.character(var),method)]),expected = c('CRPM','add'),tolerance=0)
  expect_equal(colnames(out),expected = c('time','var','value','method'))

})

test_that("bln_rothc_event_amendment works", {

  # get some input files
  b_lu_brp <- BLN::bln_scen_croprotation[b_aer_cbs=='LG01' & soiltype=='clay' & scen=='bld_arable_int',b_lu_brp]
  scen.inp <- rothc_scenario(B_LU_BRP = b_lu_brp, scen = 'BAU')
  rothc_rotation <- scen.inp$rotation
  rothc_amendment <- scen.inp$amendment

  # run bln_rothc_event
  dt.crop <- bln_rothc_input_crop(dt = rothc_rotation,B_LU_BRP = NULL,cf_yield = 1)
  dt.org <- bln_rothc_input_amendment(dt = rothc_amendment,B_LU_BRP = NULL)
  out <- bln_rothc_event_amendment(crops = dt.crop,amendment = dt.org)

  # test dimensions and value
  expect_equal(dim(out),expected = c(9,4),tolerance = 0.1)
  expect_equal(as.numeric(out[8,c(1,3)]),expected = c(1.75,33.157),tolerance=0.1)
  expect_equal(as.character(out[8,.(as.character(var),method)]),expected = c('CHUM','add'),tolerance=0)
  expect_equal(colnames(out),expected = c('time','var','value','method'))

})

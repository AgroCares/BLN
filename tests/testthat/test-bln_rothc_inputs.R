# test bln rothc inputs functions
test_that("bln_rothc_input_crop works", {

  # get some input files
  b_lu_brp <- BLN::bln_scen_croprotation[b_aer_cbs=='LG01' & soiltype=='clay' & scen=='bld_arable_int',b_lu_brp]
  scen.inp <- rothc_scenario(B_LU_BRP = b_lu_brp, scen = 'BAU')
  rothc_rotation <- scen.inp$rotation
  rothc_amendment <- scen.inp$amendment

  # run bln_rothc_input_crop
  out <- bln_rothc_input_crop(dt = rothc_rotation,B_LU_BRP = NULL,cf_yield = 1)

  # test dimensions and value
  expect_equal(dim(out),expected = c(4,11),tolerance = 0.1)
  expect_equal(as.numeric(out$cin_crop_dpm),expected = c(1206,1518,469,1518),tolerance = 1)
  expect_equal(colnames(out),expected = c("year","B_LU","cin_crop_dpm","cin_crop_rpm","cin_res_dpm","cin_res_rpm","M_GREEN_TIMING" ,
                                          "M_IRRIGATION","M_CROPRESIDUE" , "M_RENEWAL","cf_yield" ))

  # run bln_rothc_input_crop
  out <- bln_rothc_input_crop(dt = NULL,B_LU_BRP = b_lu_brp,cf_yield = 1)

  # test dimensions and value
  expect_equal(dim(out),expected = c(4,11),tolerance = 0.1)
  expect_equal(as.numeric(out$cin_crop_dpm),expected = c(1206,1518,469,1518),tolerance = 1)
  expect_equal(colnames(out),expected = c("year","B_LU","cin_crop_dpm","cin_crop_rpm","cin_res_dpm","cin_res_rpm","M_GREEN_TIMING" ,
                                          "M_IRRIGATION","M_CROPRESIDUE" , "M_RENEWAL","cf_yield" ))

  })

test_that("bln_rothc_input_amendment works", {

  # get some input files
  b_lu_brp <- BLN::bln_scen_croprotation[b_aer_cbs=='LG01' & soiltype=='clay' & scen=='bld_arable_int',b_lu_brp]
  scen.inp <- rothc_scenario(B_LU_BRP = b_lu_brp, scen = 'BAU')
  rothc_rotation <- scen.inp$rotation
  rothc_amendment <- scen.inp$amendment

  # run bln_rothc_input_crop
  out <- bln_rothc_input_amendment(dt = rothc_amendment,B_LU_BRP = NULL)

  # test dimensions and value
  expect_equal(dim(out),expected = c(4,7),tolerance = 0.1)
  expect_equal(as.numeric(out$cin_dpm),expected = c(540,540,0,540),tolerance = 1)
  expect_equal(colnames(out),expected = c("p_name","year","cin_tot","cin_hum","cin_dpm","cin_rpm","fr_eoc_p"))

  # run bln_rothc_input_crop
  out <- bln_rothc_input_amendment(dt = NULL,B_LU_BRP = b_lu_brp)

  # test dimensions and value
  expect_equal(dim(out),expected = c(4,7),tolerance = 0.1)
  expect_equal(as.numeric(out$cin_dpm),expected = c(540,540,0,540),tolerance = 1)
  expect_equal(colnames(out),expected = c("p_name","year","cin_tot","cin_hum","cin_dpm","cin_rpm","fr_eoc_p"))

})


test_that("bln_rothc_input_rmf works", {

  # get some input files
  b_lu_brp <- BLN::bln_scen_croprotation[b_aer_cbs=='LG01' & soiltype=='clay' & scen=='bld_arable_int',b_lu_brp]
  scen.inp <- rothc_scenario(B_LU_BRP = b_lu_brp, scen = 'BAU')
  rothc_rotation <- scen.inp$rotation

  # run bln_rothc_input_crop
  dt.crop <- bln_rothc_input_crop(dt = rothc_rotation,B_LU_BRP = NULL,cf_yield = 1)
  out <- bln_rothc_input_rmf(dt = dt.crop,B_LU_BRP = NULL,A_CLAY_MI=4.5)

  # test dimensions and value
  expect_equal(length(out),expected = 3)
  expect_equal(names(out),expected = c('R1','abc','d'))
  expect_equal(as.character(sapply(out,class)),expected = c('numeric','function','function'),tolerance = 1)

})

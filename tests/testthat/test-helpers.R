test_that("pF_curve works", {

  # test 1
  expect_equal(
    pF_curve(head = 2.2, thetaR = 0.01, thetaS = 0.35, alfa = 0.3,n = 1.6),
    expected = c(0.301),
    tolerance = 0.1
  )

  # test 2
  expect_equal(
    pF_curve(head = 4.2, thetaR = 0.01, thetaS = 0.35, alfa = 0.3,n = 1.6),
    expected = c(0.253),
    tolerance = 0.1
  )

})

test_that("pFpara_ptf_Wosten1999 works", {

  # test 1
  expect_equal(
    pFpara_ptf_Wosten1999(Pklei = 25, Psilt = 15, Psom = 4.5, Bovengrond = 1),
    expected = data.table(Dichtheid = 1.12,ThetaR = 0.01,ThetaS = 0.51,alfa = 0.054, n = 1.17,ksat = 11.59),
    tolerance = 0.1
  )

  # test 2
  expect_equal(
    pFpara_ptf_Wosten1999(Pklei = 45, Psilt = 3, Psom = 4.5, Bovengrond = 1),
    expected = data.table(Dichtheid = 1.12,ThetaR = 0.01,ThetaS = 0.51,alfa = 0.042, n = 1.17,ksat = 17.646),
    tolerance = 0.1
  )

})

test_that("pFpara_ptf_Wosten2001 works", {

  # test 1
  expect_equal(
    pFpara_ptf_Wosten2001(Pklei = 25, Pleem = 15, Psom = 4.5,M50 = 150, Bovengrond = 1),
    expected = data.table(Dichtheid = 1.296,ThetaR = 0.01,ThetaS = 0.49,alfa = 0.051, n = 1.128,ksat = 82.7775,l=-3.524),
    tolerance = 0.1
  )

  # test 2
  expect_equal(
    pFpara_ptf_Wosten2001(Pklei = 45, Pleem = 3, Psom = 4.5,M50 = 150,Bovengrond = 1),
    expected = data.table(Dichtheid = 1.186,ThetaR = 0.01,ThetaS = 0.54,alfa = 0.0703, n = 1.084,ksat = 89.664, l= -5.345),
    tolerance = 0.1
  )

})

test_that("pFpara_class works", {

  # test 1
  expect_equal(
    pFpara_class(Pklei = 25, Pleem = 15, Psom = 4.5,M50 = 150),
    expected = data.table(ThetaR = 0.01,ThetaS = 0.43,alfa = 0.0064, n = 1.21,ksat = 0.7),
    tolerance = 0.1
  )

  # test 2
  expect_equal(
    pFpara_class(Pklei = 45, Pleem = 3, Psom = 4.5,M50 = 150),
    expected = data.table(ThetaR = 0.01,ThetaS = 0.59,alfa = 0.0195, n = 1.109,ksat = 4.53),
    tolerance = 0.1
  )

})


test_that("wf works", {

  # test 1
  expect_equal(
    wf(c(0.3,0.6,0.9), type = "indicators", penalty = TRUE),
    expected = c(1.11,1.667,3.33),
    tolerance = 0.1
  )

  # test 2
  expect_equal(
    wf(c(0.3,0.6,0.9), type = "score", penalty = TRUE),
    expected = c(4.93,4.854,4.785),
    tolerance = 0.1
  )

})



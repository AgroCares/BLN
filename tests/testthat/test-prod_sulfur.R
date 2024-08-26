test_that("bln_c_sulfur works", {

  # test 1
  expect_equal(
    bln_c_sulfur(A_SOM_LOI = 7.9,
                  A_S_RT = 563,
                  B_LU_BRP = 3732,
                  B_SOILTYPE_AGR ='dekzand',
                  B_AER_CBS = 'Oostelijk Veehouderijgebied'
                 ),
    expected = c(0.9845),
    tolerance = 0.1
  )

  # test 2
  expect_equal(
    bln_c_sulfur(A_SOM_LOI = c(7.9,4.2,6.13,6.9,3.57,39.8),
                  A_S_RT = c(563,390,614,485,236,3664),
                  B_LU_BRP = rep(3732,6),
                  B_SOILTYPE_AGR = c('dekzand','zeeklei','rivierklei','dalgrond','loess','veen'),
                  B_AER_CBS = c('Oostelijk Veehouderijgebied','IJsselmeerpolders','Rivierengebied',
                                'Oostelijk Veehouderijgebied','Zuid-Limburg','Hollands/Utrechts Weidegebied')
                  ),
    expected = c(0.98,0.02,0.998,0.9446,0.132,1),
    tolerance = 0.1
  )

  # test 3
  expect_equal(
    bln_c_sulfur(A_SOM_LOI = c(7.9,4.2,6.13,6.9,3.57,39.8),
                 A_S_RT = c(563,390,614,485,236,3664),
                 B_LU_BRP = rep(265,6),
                 B_SOILTYPE_AGR = c('dekzand','zeeklei','rivierklei','dalgrond','loess','veen'),
                 B_AER_CBS = c('Oostelijk Veehouderijgebied','IJsselmeerpolders','Rivierengebied',
                               'Oostelijk Veehouderijgebied','Zuid-Limburg','Hollands/Utrechts Weidegebied')
    ),
    expected = c(0.47,0.28,0.47,0.3988,0.195,0.999),
    tolerance = 0.1
  )


})

#' Function to calculate and evaluate the potassium use efficiency in view of the soils' function to improve nutrient recycling
#'
#' @param B_LU_BRP (numeric) The crop code
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_PH_CC (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)
#' @param A_CEC_CO (numeric) The cation exchange capacity of the soil (mmol+ / kg), analyzed via Cobalt-hexamine extraction
#' @param A_K_CO_PO (numeric) The occupation of the CEC with potassium (\%)
#' @param A_K_CC (numeric) The plant available potassium, extracted with 0.01M CaCl2 (mg / kg),
#'
#' @import data.table
#'
#' @export
bln_nut_potassium <- function(B_LU_BRP, B_SOILTYPE_AGR,A_SOM_LOI, A_CLAY_MI,A_PH_CC,
                              A_CEC_CO, A_K_CO_PO, A_K_CC) {

  # add visual bindings
  id = crop_category = soiltype.n = crop_code = soiltype = NULL
  b = cF = kindex1 = kindex2 = A_PH_KCL = A_K_CO = NULL
  bln_country =bln_indicator = crop_cat1 =bln_soil_cat1 =bln_soil_cat2 = value = NULL
  bln_threshold_cropcat =bln_threshold_soilcat = i_c_k =bln_st_c1 =bln_st_c2 =bln_st_c3 = NULL

  # Load in the datasets
  dt.crops <- as.data.table(bln_crops[bln_country=='NL'])
  dt.soils <- as.data.table(bln_soiltype[bln_country=='NL'])

  # Check inputs
  arg.length <- max(length(A_PH_CC), length(A_SOM_LOI), length(A_CEC_CO), length(A_K_CO_PO),
                    length(A_K_CC), length(A_CLAY_MI), length(B_SOILTYPE_AGR), length(B_LU_BRP))

  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(dt.soils$osi_soil_cat1), empty.ok = FALSE)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 10, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_K_CC, lower = 0, upper = 800, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_K_CO_PO, lower = 0.1, upper = 50, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CEC_CO, lower = 1, upper = 1000, any.missing = FALSE, len = arg.length)

  # Collect the data
  dt <- data.table(id = 1:arg.length,
                   B_LU_BRP = B_LU_BRP,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_CC = A_PH_CC,
                   A_CEC_CO = A_CEC_CO,
                   A_K_CO_PO = A_K_CO_PO,
                   A_K_CC = A_K_CC,
                   value = NA_real_
  )

  # merge with crop and soil classification tables
  dt <- merge(dt, dt.crops[, list(crop_code, crop_cat1)],
              by.x = "B_LU_BRP", by.y = "crop_code", all.x = TRUE)
  dt <- merge(dt, dt.soils[, list(bln_soil_cat1,bln_soil_cat2)],
              by.x = "B_SOILTYPE_AGR", by.y = "bln_soil_cat1",all.x = TRUE)

  # calculate the K-availability index
  dt[, D_K := OBIC::calc_potassium_availability(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_PH_CC,
                                                A_CEC_CO, A_K_CO_PO, A_K_CC)]

  #  evaluate nutrient cycling indicators for K
  dt[grepl('gras|maiz',crop_cat1),i_nut_k := bln_evaluate_logistic_gaus_down(D_K, b = 8, x0 = 2.5, v = 8, optimum = 20, optimum_ofset = 1.5)]
  dt[grepl('arabl',crop_cat1) & grepl("zand|dal|veen",B_SOILTYPE_AGR),i_nut_k := bln_evaluate_logistic_gaus_down(D_K, b = 0.3, x0 = 9, v = 1.1, optimum = 20, optimum_ofset = 1.5)]
  dt[grepl('arabl',crop_cat1) & grepl("klei",B_SOILTYPE_AGR) & A_SOM_LOI > 10,i_nut_k := bln_evaluate_logistic_gaus_down(D_K, b = 0.4, x0 = 11.5, v = 1.1, optimum = 20, optimum_ofset = 1.5)]
  dt[grepl('arabl',crop_cat1) & (grepl("loess",B_SOILTYPE_AGR)|grepl("klei",B_SOILTYPE_AGR) & A_SOM_LOI <= 10),i_nut_k := bln_evaluate_logistic_gaus_down(D_K, b = 0.5, x0 = 11.5, v = 1.1, optimum = 20, optimum_ofset = 1.5)]
  dt[is.na(i_nut_k),i_nut_k := bln_evaluate_logistic_gaus_down(D_K, b = 8, x0 = 2.5, v = 8, optimum = 20, optimum_ofset = 1.5)]

  # select the output variable
  out <- dt[,i_nut_k]

  # return the OSI score
  return(out)
}
